module View.State where

import DAO
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Model.Data as MD

data AppState = AppState {
    expenseRecords :: (L.List () MD.ExpenseRecord),
    shouldExit :: Bool,
    -- 0: Dashboard Page, 1: AddExpense Page, 2: Settlement Page
    pageDisplay :: Int,
    mockData :: Bool
}

getAppState :: (L.List () MD.ExpenseRecord)-> Bool ->Int->Bool -> IO (AppState)
getAppState l s p m = do
    -- _ <- DAO.loadMockData
    ers <- DAO.batchQuery True
    return $  AppState (L.list () (Vec.fromList ers) 1) False 0 True

getInitAppState :: Bool -> Bool -> IO (AppState)
getInitAppState isMock isFirstTime
    | isMock == False = do
        ers <- DAO.batchQuery False
        return $  AppState  (L.list () (Vec.fromList ers) 1) False 0 isMock
    | isFirstTime == True = do
        _ <- DAO.loadMockData
        ers <- DAO.batchQuery True
        return $  AppState  (L.list () (Vec.fromList ers) 1) False 0 isMock
    | otherwise = do 
        ers <- DAO.batchQuery True
        return $  AppState  (L.list () (Vec.fromList ers) 1) False 0 isMock

updateAppState :: AppState -> IO (AppState)
updateAppState (AppState _ f p m) =
    do
        ers <- DAO.batchQuery m
        return $  AppState  (L.list () (Vec.fromList ers) 1) f p m

getEmptyAppState :: Bool -> Int -> IO (AppState)
getEmptyAppState flag pageNum = return (AppState (L.list () (Vec.fromList []) 1) flag pageNum False)