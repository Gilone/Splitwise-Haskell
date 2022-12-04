module View.State where

import DAO
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Model.Data as MD

data AppState = AppState {
    expenseRecords :: (L.List () MD.ExpenseRecord),
    shouldExit :: Bool
}

getInitAppState :: IO (AppState)
getInitAppState =
    do
        ers <- DAO.batchQuery ""
        return $  AppState  (L.list () (Vec.fromList ers) 1) False

updateAppState :: AppState -> IO (AppState)
updateAppState (AppState _ f) =
    do
        ers <- DAO.batchQuery ""
        return $  AppState  (L.list () (Vec.fromList ers) 1) False

getEmptyAppState ::  Bool -> IO (AppState)
getEmptyAppState flag = return (AppState (L.list () (Vec.fromList []) 1) flag)