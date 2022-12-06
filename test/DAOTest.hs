{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module DAOTest where

import qualified Model.Data as MD
import DAO (serializeList, deserializeToList, execDDL, addExpense, batchQuery, deleteExpense)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time (Day)
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.Split

date = (read "2022-11-11") :: Day
record1 = MD.ExpenseRecord {
    MD.billingID = 1,
    MD.title = "KFC",
    MD.description = Just "payment for KFC",
    MD.creditor = "JasonKing",
    MD.debtors = ["JasonKing", "HAO", "Bill", "Joey"],
    MD.amount = 93.03,
    MD.createDate = date
}

record2 = MD.ExpenseRecord {
    MD.billingID = 2,
    MD.title = "Uber",
    MD.description = Just "Uber fare",
    MD.creditor = "HAO",
    MD.debtors = ["HAO", "Bill", "Joey"],
    MD.amount = 36.07,
    MD.createDate = date
}

record3 = MD.ExpenseRecord {
    MD.billingID = 3,
    MD.title = "Grocery",
    MD.description = Just "Grocery payment",
    MD.creditor = "Bill",
    MD.debtors = ["Bill", "JasonKing", "Hao"],
    MD.amount = 26.17,
    MD.createDate = date
}

testDebtors = ["JasonKing", "HAO", "Bill", "Joey"]

serializeTest :: TestTree
serializeTest = testCase "Testing list serialization and deserialization for database interaction" $ 
    assertEqual "deserialize serialized list should be the same as the input list" testDebtors $ deserializeToList $ serializeList testDebtors

sqliteTest :: TestTree
sqliteTest = testGroup "Testing database insertion, deletion, query operations"
    [
        -- testCase "query after insertion" $ do
        --     let dropTable = "DROP TABLE IF EXISTS mock"
        --     execDDL dropTable
        --     addExpense True record1
        --     addExpense True record2
        --     addExpense True record3
        --     r <- batchQuery True
        --     (length $ r) @?= 3,

        testCase "query after insertion and deletion" $ do
            let dropTable = "DROP TABLE IF EXISTS mock"
            execDDL dropTable
            addExpense True record1
            addExpense True record2
            addExpense True record3
            deleteExpense True 1
            r <- batchQuery True
            (length $ r) @?= 2
    ]

daoTest :: TestTree
daoTest = testGroup "DAO Test" [serializeTest, sqliteTest]