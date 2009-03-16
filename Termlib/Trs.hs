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
import Data.Set (Set)
import qualified Control.Monad.State.Lazy as State

import qualified Termlib.Rule as R
import qualified Termlib.Term as T
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

empty :: Trs

empty = Trs [] F.emptySignature V.emptyVariables

isEmpty :: Trs -> Bool
isEmpty trs = rules trs == [] 

allrules f trs = all f $ rules trs

makeTrs = Trs


runTrs :: TrsMonad a -> Trs -> (a, Trs)
runTrs  = State.runState

getTrs :: TrsMonad Trs
getTrs = State.get

putTrs :: Trs -> TrsMonad ()
putTrs = State.put

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

putSignature :: Signature -> TrsMonad ()
putSignature sig = modifySignature $ \ _ ->  sig

putVariables :: Variables -> TrsMonad ()
putVariables vars = modifyVariables $ \ _ ->  vars

addRule :: R.Rule -> TrsMonad ()
addRule r = modifyRules $ ((:) r . (List.delete r))

freshSymbol' :: (Signature -> F.Attributes -> (Symbol, Signature)) -> F.Attributes -> TrsMonad Symbol
freshSymbol' f attribs = do sig <- getSignature
                            let (fresh, sig') = f sig attribs 
                            putSignature sig'
                            return fresh

freshSymbol ::  F.Attributes -> TrsMonad Symbol
freshSymbol = freshSymbol' F.fresh

getSymbol :: F.Attributes -> TrsMonad Symbol
getSymbol = freshSymbol' F.getSymbol

freshVariable' :: (String -> Variables -> (Variable, Variables)) -> String -> TrsMonad Variable
freshVariable' f name = do vars <- getVariables
                           let (fresh, vars') = f name vars
                           putVariables vars'
                           return fresh

freshVariable ::  String -> TrsMonad Variable
freshVariable = freshVariable' V.fresh

getVariable :: String -> TrsMonad Variable
getVariable = freshVariable' V.getVariable

isVariable :: String -> TrsMonad Bool
isVariable n = do v <- getVariables
                  return $ V.isVariable n v

symbols :: Trs -> Set F.Symbol 
symbols = Signature.symbols . signature

definedSymbols :: Trs -> Set F.Symbol
definedSymbols trs = foldl f Set.empty (rules trs) 
  where f s (R.Rule l _) = case T.root l of 
                             Left _  -> error "Trs.definedSymbols. Variable as lhs"
                             Right r -> Set.insert r s

constructors :: Trs -> Set F.Symbol
constructors _ = undefined 

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
