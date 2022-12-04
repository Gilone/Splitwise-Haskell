{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Model.Storage where

import           Control.Monad.State
import           System.IO
import qualified Model.Data as MD
import qualified Data.Map.Strict as Map

type MyMap = Map.Map String MD.ExpenseRecord

dictionary :: MyMap
dictionary = Map.empty


init :: (MonadState MyMap m, MonadIO m) => m [MD.ExpenseRecord]
init = do 
    return []