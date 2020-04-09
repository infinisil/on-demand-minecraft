{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Timeout (Timeout(..), timeout, runTimeout) where

import Polysemy
import Polysemy.Final

import qualified System.Timeout as T

data Timeout m a where
  Timeout :: Int -> m a -> Timeout m (Maybe (Maybe a))

makeSem ''Timeout

runTimeout :: Member (Final IO) r => Sem (Timeout ': r) a -> Sem r a
runTimeout = interpretFinal $ \case
  Timeout micros action -> do
    ins <- getInspectorS
    m' <- runS action
    liftS $ T.timeout micros (inspect ins <$> m')
