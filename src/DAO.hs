{-# LANGUAGE OverloadedStrings #-}

module DAO where

import Control.Monad.State (evalStateT)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time (Day)
import Data.Maybe (fromMaybe)
import qualified Model.Data as MD
import qualified Model.Storage as MS
import qualified Data.Time.Format as DTF

-- | [Input]: Activity name
-- [Output]: A list of MD.ExpenseRecord values
batchQuery :: String -> IO [MD.ExpenseRecord]
batchQuery s = evalStateT (do {MS.init}) MS.dictionary


-- | [Input]: The BillingID
-- [Output]: Empty IO
deleteExpense :: Int -> IO [MD.ExpenseRecord]
deleteExpense s = evalStateT (do {MS.init}) MS.dictionary


-- [Output]: A list of mock expense records
fechMockData :: IO [MD.ExpenseRecord]
fechMockData = do
    today <- utctDay <$> getCurrentTime
    let date = fromMaybe today (parseDay "2010-1-1")
    let record1 = MD.ExpenseRecord {
        MD.billingID = 1,
        MD.title = "KFC",
        MD.description = Just "payment for KFC",
        MD.creditor = "JasonKing",
        MD.debtors = ["JasonKing", "HAO", "Bill", "Joey"],
        MD.amount = 93.03,
        MD.createDate = date
    }
    let record2 = MD.ExpenseRecord {
        MD.billingID = 2,
        MD.title = "Uber",
        MD.description = Just "Uber fare",
        MD.creditor = "HAO",
        MD.debtors = ["HAO", "Bill", "Joey"],
        MD.amount = 36.07,
        MD.createDate = date
    }
    let record3 = MD.ExpenseRecord {
        MD.billingID = 3,
        MD.title = "Grocery",
        MD.description = Just "Grocery payment",
        MD.creditor = "Bill",
        MD.debtors = ["Bill", "JasonKing", "Hao"],
        MD.amount = 26.17,
        MD.createDate = date
    }
    return [record1, record2, record3]

parseDay :: String -> Maybe Day
parseDay s = DTF.parseTimeM True DTF.defaultTimeLocale "%Y-%-m-%-d" s