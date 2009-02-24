module Termlib.Trs
  (
  Trs,
  empty,
  allrules,
  definedSymbols,
  constructors,
  rewrites,
  topRewrites,
  reduced,
  topReduced,
  duplicating,
  nonduplicating,
  flat,
  shallow,
  linear,
  ground,
  leftFlat,
  leftShallow,
  leftLinear,
  leftGround,
  rightFlat,
  rightShallow,
  rightLinear,
  rightGround,
  ) where

import qualified Termlib.Rule as R
import qualified Termlib.Signature as Sig
import Termlib.Signature (Signature)
import qualified Data.List as List

newtype Trs = Trs [R.Rule] deriving Show

empty = Trs []

allrules f (Trs rs) = all f rs

rewrites s t (Trs rs) = any (R.rewrites s t) rs

topRewrites s t (Trs rs) = any (R.topRewrites s t) rs

reduced = allrules . R.reduced

topReduced = allrules . R.topReduced

duplicating (Trs rs) = any R.duplicating rs

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


definedSymbols :: Trs -> Signature
definedSymbols (Trs _) = undefined 

constructors :: Trs -> Signature
constructors (Trs _) = undefined 