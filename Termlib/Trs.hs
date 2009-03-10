module Termlib.Trs
  -- (
  -- Trs,
  -- isEmpty, 
  -- empty,
  -- makeTrs, 
  -- allrules,
  -- definedSymbols,
  -- constructors,
  -- rewrites,
  -- topRewrites,
  -- reduced,
  -- topReduced,
  -- duplicating,
  -- nonduplicating,
  -- flat,
  -- shallow,
  -- linear,
  -- ground,
  -- leftFlat,
  -- leftShallow,
  -- leftLinear,
  -- leftGround,
  -- rightFlat,
  -- rightShallow,
  -- rightLinear,
  -- rightGround,
  -- ) 
where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Control.Monad.State.Lazy as State

import qualified Termlib.Rule as R
import qualified Termlib.FunctionSymbol as F
import Termlib.FunctionSymbol (Symbol, Signature)

import qualified Termlib.Variable as V
import Termlib.Variable (Variables, Variable)
import qualified Termlib.Signature as Signature

type Rules = [R.Rule]
data Trs = Trs {rules :: Rules
               , signature :: Signature
               , variables :: Variables}
           deriving Show

instance Eq Trs where
  trs1 == trs2 = undefined


type TrsMonad a = State.State Trs a

empty = Trs [] F.emptySignature V.emptyVariables

isEmpty trs = rules trs == [] 

allrules f trs = all f $ rules trs

makeTrs = Trs


runTrs :: TrsMonad a -> Trs -> (a, Trs)
runTrs  = State.runState

getTrs :: TrsMonad Trs
getTrs = State.get

getRules :: TrsMonad Rules
getRules = getTrs >>= return . rules

getSignature :: TrsMonad Signature
getSignature = getTrs >>= return . signature

getVariables :: TrsMonad Variables
getVariables = getTrs >>= return . variables


modifyTrs :: (Trs -> Trs) -> TrsMonad ()
modifyTrs f = do trs <- getTrs
                 putTrs $ f trs
                 return ()
                 
modifyRules :: (Rules -> Rules) -> TrsMonad ()
modifyRules f = modifyTrs $ \ trs -> trs{rules=f $ rules trs}

modifySignature :: (Signature -> Signature) -> TrsMonad ()
modifySignature f = modifyTrs $ \ trs -> trs{signature=f $ signature trs}

modifyVariables :: (Variables -> Variables) -> TrsMonad ()
modifyVariables f = modifyTrs $ \ trs -> trs{variables=f $ variables trs}


putTrs :: Trs -> TrsMonad ()
putTrs trs = modifyTrs $ \ _ ->  trs

putSignature :: Signature -> TrsMonad ()
putSignature sig = modifySignature $ \ _ ->  sig

putVariables :: Variables -> TrsMonad ()
putVariables vars = modifyVariables $ \ _ ->  vars

freshSymbol ::  F.Attributes -> TrsMonad Symbol
freshSymbol attribs = do sig <- getSignature
                         let (fresh, sig') = F.fresh attribs $ sig
                         putSignature sig'
                         return fresh

freshVariable ::  String -> TrsMonad Variable
freshVariable name = do vars <- getVariables
                        let (fresh, vars') = V.fresh name $ vars
                        putVariables vars'
                        return fresh



rewrites s t trs = any (R.rewrites s t) $ rules trs

topRewrites s t trs = any (R.topRewrites s t) $ rules trs

reduced = allrules . R.reduced

topReduced = allrules . R.topReduced

duplicating trs = any R.duplicating $ rules trs

nonduplicating = allrules R.nonduplicating

flat = allrules R.flat

shallow = allrules R.shallow

linear = allrules R.linear

ground = allrules R.ground

leftFlat = allrules R.leftFlat

rightFlat = allrules R.rightFlat

leftShallow = allrules R.leftShallow

rightShallow = allrules R.rightShallow

leftLinear = allrules R.leftLinear

rightLinear = allrules R.rightLinear

leftGround = allrules R.leftGround

rightGround = allrules R.rightGround


definedSymbols :: Trs -> Set.Set F.Symbol
definedSymbols _ = undefined 

constructors :: Trs -> Set.Set F.Symbol
constructors _ = undefined 