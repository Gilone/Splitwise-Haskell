{-# LANGUAGE OverloadedStrings #-}

module AddExpenseTest where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time (parseTimeOrError)
import Data.Time.Format (defaultTimeLocale)
import Data.Time (Day)

import qualified View.AddExpense as AE
import qualified Model.Data as MD

addExpenseHelperFuncTest :: TestTree
addExpenseHelperFuncTest = testGroup "Testing addExpenseHelperFunc"
    [
        testCase "Testing trim, should delete whitespace correctly" $ do
            let originStr = " abc \n "
            let targetStr = "abc"
            let newStr = AE.trim' originStr
            newStr @?= targetStr,

        testCase "Testing parseDay, should parse valid input date correctly" $ do
            let input = "2022-12-5"
            let targetDay = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2022-12-05"
            case AE.parseDay input of
                Just d -> assertEqual ("Day parse error: " ++ show d ++ " expected: " ++ show targetDay) d targetDay
                Nothing -> assertFailure ("Could not parse date: " ++ input),

        testCase "Testing parseDay, should not parse invalid input" $ do
            let input = "asdfasf"
            case AE.parseDay input of
                Just d -> assertFailure ("Should not parse invalid date: " ++ input ++ " to: " ++ show d)
                Nothing -> return (),

        testCase "Testing truncate, should round the amount for given digits correctly" $ do
            let originNum = 98.23123
            let targetNum = 98.23
            let newNum = AE.truncate' originNum 2
            show (newNum) @?= show (targetNum),

        testCase "Testing isNotValid, should return false" $ do
            let date = (read "2022-11-11") :: Day
            let record = MD.ExpenseRecord {
                MD.billingID = 1,
                MD.title = "KFC",
                MD.description = Just "payment for KFC",
                MD.creditor = "JasonKing",
                MD.debtors = ["JasonKing", "HAO", "Bill", "Joey"],
                MD.amount = 93.03,
                MD.createDate = date
            }
            let newValue = AE.isNotValid' record
            newValue @?= False,

        testCase "Testing isNotValid, should return true if title is empty" $ do
            let date = (read "2022-11-11") :: Day
            let record = MD.ExpenseRecord {
                MD.billingID = 1,
                MD.title = "",
                MD.description = Just "payment for KFC",
                MD.creditor = "JasonKing",
                MD.debtors = ["JasonKing", "HAO", "Bill", "Joey"],
                MD.amount = 93.03,
                MD.createDate = date
            }
            let newValue = AE.isNotValid' record
            newValue @?= True,

        testCase "Testing isNotValid, should return true if creditor is empty" $ do
            let date = (read "2022-11-11") :: Day
            let record = MD.ExpenseRecord {
                MD.billingID = 1,
                MD.title = "KFC",
                MD.description = Just "payment for KFC",
                MD.creditor = "",
                MD.debtors = ["JasonKing", "HAO", "Bill", "Joey"],
                MD.amount = 93.03,
                MD.createDate = date
            }
            let newValue = AE.isNotValid' record
            newValue @?= True,

        testCase "Testing isNotValid, should return true if debtors is empty" $ do
            let date = (read "2022-11-11") :: Day
            let record = MD.ExpenseRecord {
                MD.billingID = 1,
                MD.title = "KFC",
                MD.description = Just "payment for KFC",
                MD.creditor = "JasonKing",
                MD.debtors = [],
                MD.amount = 93.03,
                MD.createDate = date
            }
            let newValue = AE.isNotValid' record
            newValue @?= True
    ]

addExpenseTest :: TestTree
addExpenseTest = testGroup "Add Expense Test" [addExpenseHelperFuncTest]