{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Data where
import Data.Time (Day)
import GHC.Generics


data ExpenseRecord = ExpenseRecord {
    billingID :: Int,
    title :: String,
    description:: Maybe String,
    creditor :: String,
    debtors :: [String],
    amount :: Float,
    createDate :: Day
} deriving (Show, Eq, Generic)


data SplitSuggestion = SplitSuggestion {
    debtor :: String,
    paymentSuggestions :: [(String, Float)]
} deriving (Show, Eq, Generic)