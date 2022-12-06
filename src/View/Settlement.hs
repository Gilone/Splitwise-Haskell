{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module View.Settlement where

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
import qualified SplitAlgorithm as SS
import qualified Data.Text as DT
import qualified Brick.Main as M

drawSettlement :: VS.AppState -> [Widget ()]
drawSettlement (VS.AppState l _ _ _) = [ui]
    where
        
        ll = Vec.toList (l^.L.listElementsL)
        ers = SS.getSuggestionsGreedy ll
        er = L.list () (Vec.fromList ers) 1
        -- ers = DAO.batchQuery True
        label = str "show settlement"
        box1 = WB.borderWithLabel label $
              hLimit 205 $
              vLimit 50 $
              drawTable er
        ui = C.hCenter $ vBox [ box1, str "Press \"Esc\" to exit" ]


settlementEvent :: VS.AppState -> T.BrickEvent () e -> T.EventM () (T.Next VS.AppState)
settlementEvent s@(VS.AppState l _ _ m) (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> do
            state <- liftIO $ VS.getAppState l False 2 m
            M.halt state

        -- ev -> M.continue =<< handleTrendingList ev s
settlementEvent s _ = M.continue s

handleTrendingList :: V.Event -> VS.AppState -> T.EventM () (VS.AppState)
handleTrendingList e s@(VS.AppState theList f p m) = do
    nextList <- L.handleListEvent e theList
    return $ VS.AppState nextList f p m


drawTable :: (L.List () MD.SplitSuggestion) -> Widget ()
drawTable l = renderTable $  innerTable (Vec.toList (l^.L.listElementsL))

innerTable :: [MD.SplitSuggestion] -> Table ()
innerTable rs = 
    surroundingBorder False $ 
    table $  [txt "suggestAmount", txt "suggestCreditor", txt "Debtor"] : map expenseToTable rs

expenseToTable :: MD.SplitSuggestion -> [Widget ()]
expenseToTable (MD.SplitSuggestion debtor suggestCreditor suggestAmount) = 
    [ txt(DT.pack (show suggestAmount)), txt (DT.pack (show suggestCreditor)), txt (DT.pack (show debtor))]   


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
    M.App { M.appDraw = drawSettlement
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = settlementEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }


-- startSettlement :: Bool -> IO VS.AppState
-- startSettlement isMock = do
--     as <- VS.getInitAppState isMock
--     M.defaultMain theApp as
