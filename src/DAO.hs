{-# LANGUAGE OverloadedStrings #-}

module DAO where

import           Control.Monad.State (evalStateT)
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
-- [Output]: Maybe Day (Nothing | Just Day), indicating the date when the input repo was marked or Nothing
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
        MD.description = Just "payment for for KFC",
        MD.creditor = "JasonKing",
        MD.debtors = ["HAO", "Bill", "Joey"],
        MD.amount = 93.03,
        MD.createDate = date
    }
    return [record1]

parseDay :: String -> Maybe Day
parseDay s = DTF.parseTimeM True DTF.defaultTimeLocale "%Y-%-m-%-d" s