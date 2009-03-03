{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Termlib.Trs.XmlParser where

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Control.Monad.Error
import Control.Monad (liftM, forM)
import Control.Monad.Trans (lift)
import Control.Monad.RWS.Lazy
import Control.Monad.Reader
import Control.Monad.State

import qualified Termlib.Rule as R
import qualified Termlib.Term as T
import qualified Termlib.Trs as Trs
import qualified Termlib.Variable as V
import qualified Termlib.FunctionSymbol as F
import Termlib.Trs




              

                       