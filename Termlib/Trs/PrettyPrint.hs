{-# LANGUAGE FlexibleInstances #-}
module Termlib.Trs.PrettyPrint where

import Termlib.Trs hiding (empty)
import qualified Termlib.Rule as R
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Variable as V
import Termlib.Term
import qualified Termlib.Signature as Sig
import Termlib.FunctionSymbol (Signature)
import Termlib.Variable (Variables)
import Text.PrettyPrint.HughesPJ
import Control.Monad (forM)
import Termlib.Utils

instance PrettyPrintable (Trs, Signature, Variables) where 
  pprint (trs, sig, var) = braces $ pprs (rules trs) 
      where pprs []  = text ""
            pprs [r] = pprint (r, sig, var)
            pprs rs  = vcat $ [com <+> pprint (r, sig, var) | (com,r) <- zip (text " " : repeat (text ",")) rs]

instance PrettyPrintable (R.Rule, Signature, Variables) where
  pprint ((R.Rule l r), sig, var) = fsep [pprint (l, sig, var), text "->", pprint (r, sig, var)]


instance PrettyPrintable (Term, Signature, Variables) where
  pprint ((Var x), _, var)      = text $ V.variableName x var
  pprint ((Fun f ts), sig, var) = pprint (f,sig)
                                <> parens (sep $ punctuate (text ",") [pprint (t_i, sig, var) | t_i <- ts])
