module View.State where

import DAO
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Model.Data as MD

data AppState = AppState {
    expenseRecords :: (L.List () MD.ExpenseRecord),
    shouldExit :: Bool,
    mockData :: Bool
}

getInitAppState :: Bool -> IO (AppState)
getInitAppState isMock
    | isMock == False = do
        ers <- DAO.batchQuery ""
        return $  AppState  (L.list () (Vec.fromList ers) 1) False isMock
    | otherwise = do
        ers <- DAO.fechMockData
        return $  AppState  (L.list () (Vec.fromList ers) 1) False isMock

updateAppState :: AppState -> IO (AppState)
updateAppState (AppState _ f m) =
    do
        ers <- DAO.batchQuery ""
        return $  AppState  (L.list () (Vec.fromList ers) 1) f m

getEmptyAppState :: Bool -> IO (AppState)
getEmptyAppState flag = return (AppState (L.list () (Vec.fromList []) 1) flag False)