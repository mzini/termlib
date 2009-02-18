module Termlib.Trs
  (
  allrules,
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
  TRS
  ) where

import qualified Termlib.Rule as R
import qualified Data.List as List

newtype TRS = TRS [R.Rule] deriving Show

allrules f (TRS rs) = all f rs

rewrites s t (TRS rs) = any (R.rewrites s t) rs

topRewrites s t (TRS rs) = any (R.topRewrites s t) rs

reduced = allrules . R.reduced

topReduced = allrules . R.topReduced

duplicating (TRS rs) = any R.duplicating rs

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