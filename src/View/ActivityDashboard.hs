{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module View.ActivityDashboard where

import DAO
import Lens.Micro ((^.))
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , str
  , vLimit
  , hLimit
  , vBox
  , hBox
  , withAttr
  ,txt
  )
import  Brick.Widgets.Table
import Brick.Util (fg, on)
import Control.Monad.RWS.Lazy (MonadIO(liftIO))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Brick.Widgets.Border as WB
import qualified Brick.Widgets.Center as C
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A

import qualified Model.Data as MD
import qualified View.State as VS
import qualified Data.Text as DT
import qualified Brick.Main as M

drawDashboard :: VS.AppState -> [Widget ()]
drawDashboard (VS.AppState l _ _ _) = [ui]
    where
        label = str "Activity Dashboard: item " <+> cur <+> str " of " <+> total
        cur = case l^.L.listSelectedL of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.L.listElementsL
        box1 = WB.borderWithLabel label $
              hLimit 205 $
              vLimit 50 $
              drawTable l
        box2 = WB.borderWithLabel (str "Expense Details") $
              hLimit 87 $
              vLimit 3$
              L.renderList listDrawElement True l
        ui = C.hCenter $ vBox [ box1, box2, str "Press \"Esc\" to exit, \"a\" to add an expense, \"s\" to go to the settlement page, \"d\" to delete an expense." ]


dashboardEvent :: VS.AppState -> T.BrickEvent () e -> T.EventM () (T.Next VS.AppState)
dashboardEvent s@(VS.AppState l _ _ m) (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> do
            newState <- liftIO $ VS.getEmptyAppState True 0
            M.halt newState

        V.EvKey (V.KChar 'a') [] -> do
            newState <- liftIO $ VS.getEmptyAppState False 1
            M.halt newState

        V.EvKey (V.KChar 's') [] -> do
            newState <- liftIO $ VS.getEmptyAppState False 2
            M.halt newState

        V.EvKey (V.KChar 'd')  [] -> 
            case l^.L.listSelectedL of
                Nothing -> M.continue s
                Just i -> do        
                    _ <- liftIO $ DAO.deleteExpense m (MD.billingID ((l^.L.listElementsL) Vec.! i))
                    newState <- liftIO $ VS.updateAppState s
                    M.continue newState 

        ev -> M.continue =<< handleTrendingList ev s
dashboardEvent s _ = M.continue s

handleTrendingList :: V.Event -> VS.AppState -> T.EventM () (VS.AppState)
handleTrendingList e s@(VS.AppState theList f p m) = do
    nextList <- L.handleListEvent e theList
    return $ VS.AppState nextList f p m


listDrawElement :: Bool -> MD.ExpenseRecord -> Widget ()
listDrawElement selected (MD.ExpenseRecord billingID title description creditor debtors amount createDate) = 
    if selected
    then case description of
        Just s -> renderTable $ surroundingBorder False $ alignCenter 1 $ table [[txt (DT.pack title)],[txt (DT.pack s) ]]
        Nothing -> renderTable $ surroundingBorder False $ alignCenter 1 $ table [[txt (DT.pack title)],[txt (DT.pack "No description") ]]
    else renderTable $ surroundingBorder False $ alignCenter 1 $ table [[txt (DT.pack title)]]


drawTable :: (L.List () MD.ExpenseRecord) -> Widget ()
drawTable l = renderTable $  innerTable (Vec.toList (l^.L.listElementsL))

innerTable :: [MD.ExpenseRecord] -> Table ()
innerTable rs = 
    surroundingBorder False $ 
    table $  [txt "Date", txt "Title", txt "Amount", txt "Creditor", txt "Debtors"] : map expenseToTable rs

expenseToTable :: MD.ExpenseRecord -> [Widget ()]
expenseToTable (MD.ExpenseRecord billingID title description creditor debtors amount createDate) = 
    [ txt (DT.pack (show createDate)), txt(DT.pack title), txt(DT.pack (show amount)), txt (DT.pack (show creditor)), txt (DT.pack (show debtors))]   


customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.green)
    , (L.listSelectedAttr,    V.white `on` V.green)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App VS.AppState e ()
theApp =
    M.App { M.appDraw = drawDashboard
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = dashboardEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }


startDashboard :: Bool -> Bool -> IO VS.AppState
startDashboard isMock isFirstTime = do
    as <- VS.getInitAppState isMock isFirstTime
    M.defaultMain theApp as