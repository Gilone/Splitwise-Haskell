module Main (main) where

import qualified View.State as VS
import qualified View.ActivityDashboard as VD
import qualified View.AddExpense as VE
import qualified View.Settlement as VT
import qualified Model.Data as MD
import qualified Control.Monad

loop :: IO ()
loop= do
  q <- VE.startFilter
  currentState <- VD.startDashboard True False
  Control.Monad.unless (VS.shouldExit currentState) loop

main :: IO ()
main = do
  currentState <- VD.startDashboard True True
  Control.Monad.unless (VS.shouldExit currentState) loop
