{-# LANGUAGE OverloadedStrings #-}

module DAO where

import           Control.Monad.State (evalStateT)
import qualified Model.Data as MD
import qualified Model.Storage as MS

-- | [Input]: Activity name
-- [Output]: A list of MD.ExpenseRecord values
batchQuery :: String -> IO [MD.ExpenseRecord]
batchQuery s = evalStateT (do {MS.init}) MS.dictionary


-- | [Input]: The BillingID
-- [Output]: Maybe Day (Nothing | Just Day), indicating the date when the input repo was marked or Nothing
deleteExpense :: Int -> IO [MD.ExpenseRecord]
deleteExpense s = evalStateT (do {MS.init}) MS.dictionary