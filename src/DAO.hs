{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}

module DAO where

import Control.Monad.State (evalStateT, forM)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time (Day)
import Data.Maybe (fromMaybe)
import Data.Text
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.List.Split as S (splitOn)
import qualified Model.Data as MD
import qualified Data.Time.Format as DTF
import GHC.Generics

-- | [Input]: The BillingID
-- [Output]: Empty IO
deleteExpense :: Bool -> Int -> IO ()
deleteExpense isMock id = do
    conn <- open "split.db"
    execute_ conn (creatStm isMock)
    execute conn (deleteStm isMock) (Only id)
    close conn
    return ()

-- | [Input]: Activity table name
-- [Output]: Empty IO
addExpense :: Bool -> MD.ExpenseRecord -> IO ()
addExpense isMock er = do
    conn <- open "split.db"
    execute_ conn (creatStm isMock)
    execute conn (insertStm isMock) (ExpenseField (MD.title er) (fromMaybe "No description" (MD.description er)) (MD.creditor er) (serializeList (MD.debtors er)) (MD.amount er) (show (MD.createDate er)))
    close conn
    return ()

-- | [Input]: Activity table name
-- [Output]: A list of MD.ExpenseRecord values
batchQuery :: Bool -> IO [MD.ExpenseRecord]
batchQuery isMock = do
    conn <- open "split.db"
    execute_ conn (creatStm isMock)
    r <- query_ conn (queryStm isMock)
    close conn
    forM r $ \(billingID, title, description, creditor, debtors, amount, createDate) -> return (MD.ExpenseRecord {
                                                                                            MD.billingID = billingID,
                                                                                            MD.title = title,
                                                                                            MD.description = if description == "No description" then Nothing else Just description,
                                                                                            MD.creditor = creditor,
                                                                                            MD.debtors = deserializeToList debtors,
                                                                                            MD.amount = amount,
                                                                                            MD.createDate = (read createDate) :: Day
                                                                                        })
    

-- [Output]: Empty IO
loadMockData :: IO ()
loadMockData = do
    today <- utctDay <$> getCurrentTime
    let date = fromMaybe today (parseDay "2022-11-20")
    let date2 = fromMaybe today (parseDay "2022-12-05")
    let date3 = fromMaybe today (parseDay "2022-12-06")
    let date4 = fromMaybe today (parseDay "2022-11-25")

    -- let record1 = MD.ExpenseRecord {
    --     MD.billingID = 1,
    --     MD.title = "KFC",
    --     MD.description = Just "payment for KFC",
    --     MD.creditor = "JasonKing",
    --     MD.debtors = ["JasonKing", "Hao", "Bill", "Joey"],
    --     MD.amount = 93.03,
    --     MD.createDate = date
    -- }
    -- let record2 = MD.ExpenseRecord {
    --     MD.billingID = 2,
    --     MD.title = "Uber",
    --     MD.description = Just "Uber fare",
    --     MD.creditor = "Hao",
    --     MD.debtors = ["Hao", "Bill", "Joey"],
    --     MD.amount = 36.07,
    --     MD.createDate = date
    -- }
    -- let record3 = MD.ExpenseRecord {
    --     MD.billingID = 3,
    --     MD.title = "Grocery",
    --     MD.description = Just "Grocery payment",
    --     MD.creditor = "Bill",
    --     MD.debtors = ["Bill", "JasonKing", "Hao"],
    --     MD.amount = 26.17,
    --     MD.createDate = date
    -- }

    let record1 = MD.ExpenseRecord {
        MD.billingID = 1,
        MD.title = "Dinner at KFC",
        MD.description = Just "Had dinner togeter at KFC, the fried chicken was very juicy",
        MD.creditor = "Fred",
        MD.debtors = ["Bob", "David", "Ema"],
        MD.amount = 30,
        MD.createDate = date
    }
    let record2 = MD.ExpenseRecord {
        MD.billingID = 2,
        MD.title = "Uber",
        MD.description = Just "payment for Uber drive, from La Jolla to Downtown",
        MD.creditor = "Fred",
        MD.debtors = ["Charile"],
        MD.amount = 30,
        MD.createDate = date2
    }
    let record3 = MD.ExpenseRecord {
        MD.billingID = 3,
        MD.title = "Lego",
        MD.description = Just "bought lego as the birthday gift",
        MD.creditor = "Gabe",
        MD.debtors = ["Bob"],
        MD.amount = 30,
        MD.createDate = date2
    }
    let record4 = MD.ExpenseRecord {
        MD.billingID = 4,
        MD.title = "another Lego",
        MD.description = Just "another Lego for David",
        MD.creditor = "Gabe",
        MD.debtors = ["David"],
        MD.amount = 10,
        MD.createDate = date3
    }
    let record5 = MD.ExpenseRecord {
        MD.billingID = 5,
        MD.title = "Noodle House",
        MD.description = Just "payment for the lunch at Noodle House",
        MD.creditor = "Bob",
        MD.debtors = ["Charile"],
        MD.amount = 40,
        MD.createDate = date2
    }
    let record6 = MD.ExpenseRecord {
        MD.billingID = 6,
        MD.title = "ticket of national park",
        MD.description = Just "payment for park ticket",
        MD.creditor = "Charile",
        MD.debtors = ["David"],
        MD.amount = 20,
        MD.createDate = date4
    }
    let record7 = MD.ExpenseRecord {
        MD.billingID = 7,
        MD.title = "brutal death metal band tour",
        MD.description = Just "Tickets for Brutal Metal Band Tour at Brick by brick!",
        MD.creditor = "David",
        MD.debtors = ["Ema"],
        MD.amount = 50,
        MD.createDate = date4
    }
    let dropTable = "DROP TABLE IF EXISTS mock"
    execDDL dropTable
    addExpense True record1
    addExpense True record2
    addExpense True record3
    addExpense True record4
    addExpense True record5
    addExpense True record6
    addExpense True record7

    -- debug
    -- let record1 = MD.ExpenseRecord {
    --     MD.billingID = 1,
    --     MD.title = "KFC",
    --     MD.description = Just "Had dinner togeter at KFC after coming back from the park, the fried chicken was very juicy",
    --     MD.creditor = "Bob",
    --     MD.debtors = ["Alice", "Bob"],
    --     MD.amount = 40,
    --     MD.createDate = date
    -- }
    -- let record2 = MD.ExpenseRecord {
    --     MD.billingID = 2,
    --     MD.title = "Noodle House",
    --     MD.description = Just "Payment for Noodle House",
    --     MD.creditor = "Bob",
    --     MD.debtors = ["Carol","Dave"],
    --     MD.amount = 20,
    --     MD.createDate = date
    -- }
    -- -- let record3 = MD.ExpenseRecord {
    -- --     MD.billingID = 3,
    -- --     MD.title = "KFC",
    -- --     MD.description = Just "payment for KFC",
    -- --     MD.creditor = "Bob",
    -- --     MD.debtors = ["Dave"],
    -- --     MD.amount = 10,
    -- --     MD.createDate = date
    -- -- }
    -- let record4 = MD.ExpenseRecord {
    --     MD.billingID = 4,
    --     MD.title = "Boba Milk Tea",
    --     MD.description = Just "help advance payment",
    --     MD.creditor = "Carol",
    --     MD.debtors = ["Alice"],
    --     MD.amount = 20,
    --     MD.createDate = date
    -- }
    -- let record5 = MD.ExpenseRecord {
    --     MD.billingID = 5,
    --     MD.title = "KFC",
    --     MD.description = Just "KFC again",
    --     MD.creditor = "Dave",
    --     MD.debtors = ["Carol"],
    --     MD.amount = 10,
    --     MD.createDate = date
    -- }
    -- let record6 = MD.ExpenseRecord {
    --     MD.billingID = 6,
    --     MD.title = "Beer",
    --     MD.description = Just "payment for Beer",
    --     MD.creditor = "Dave",
    --     MD.debtors = ["Alice"],
    --     MD.amount = 20,
    --     MD.createDate = date
    -- }
    -- let record7 = MD.ExpenseRecord {
    --     MD.billingID = 7,
    --     MD.title = "Tickets",
    --     MD.description = Just "Tickets for Brutal Metal Band Tour at Brick by brick!",
    --     MD.creditor = "Alice",
    --     MD.debtors = ["Dave"],
    --     MD.amount = 20,
    --     MD.createDate = date
    -- }
    -- let dropTable = "DROP TABLE IF EXISTS mock"
    -- execDDL dropTable
    -- addExpense True record1
    -- addExpense True record2
    -- -- addExpense True record3
    -- addExpense True record4
    -- addExpense True record5
    -- addExpense True record6
    -- addExpense True record7

-------------------------------------------------------------
-- Below are private functions supposed not to be exported --
-------------------------------------------------------------

parseDay :: String -> Maybe Day
parseDay s = DTF.parseTimeM True DTF.defaultTimeLocale "%Y-%-m-%-d" s

creatStm:: Bool -> Query
creatStm isMock
    | isMock == False = "CREATE TABLE IF NOT EXISTS splitWise (billingID INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, description TEXT, creditor TEXT, debtors TEXT, amount REAL, createDate TEXT)"
    | otherwise = "CREATE TABLE IF NOT EXISTS mock (billingID INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, description TEXT, creditor TEXT, debtors TEXT, amount REAL, createDate TEXT)"

insertStm:: Bool -> Query
insertStm isMock
    | isMock == False = "INSERT INTO splitWise (title, description, creditor, debtors, amount, createDate) VALUES (?, ?, ?, ?, ?, ?)"
    | otherwise = "INSERT INTO mock (title, description, creditor, debtors, amount, createDate) VALUES (?, ?, ?, ?, ?, ?)"

deleteStm:: Bool -> Query
deleteStm isMock
    | isMock == False = "DELETE FROM splitWise WHERE billingID = ?"
    | otherwise = "DELETE FROM mock WHERE billingID = ?"

queryStm:: Bool -> Query
queryStm isMock
    | isMock == False = "SELECT billingID, title, description, creditor, debtors, amount, createDate from splitWise"
    | otherwise = "SELECT billingID, title, description, creditor, debtors, amount, createDate from mock"

data ExpenseField = ExpenseField {
    title :: String,
    description:: String,
    creditor :: String,
    debtors :: String,
    amount :: Float,
    createDate :: String
} deriving (Show, Eq, Generic)

instance FromRow ExpenseField where
    fromRow = ExpenseField <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow ExpenseField where
  toRow (ExpenseField title description creditor debtors amount createDate) = toRow (title, description, creditor, debtors, amount, createDate)

serializeList :: [String] -> String
serializeList (e:[]) = e
serializeList (e:ls) = e ++ "," ++ serializeList ls
serializeList _ = ""

deserializeToList :: String -> [String]
deserializeToList s = S.splitOn "," s

execDDL :: Query -> IO()
execDDL q = do
    conn <- open "split.db"
    execute_ conn q
    close conn