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
            pprs [r] = pprintRule r sig var
            pprs rs  = vcat $ [com <+> pprintRule r sig var | (com,r) <- zip (text " " : repeat (text ",")) rs]

pprintRule :: R.Rule -> Signature -> Variables -> Doc
pprintRule (R.Rule l r) sig var = fsep [pprintTerm l sig var, text "->", pprintTerm r sig var]


pprintTerm :: Term -> Signature -> Variables -> Doc
pprintTerm (Var x) _ var = text $ V.variableName x var
pprintTerm (Fun f ts) sig var = pprint (f,sig) 
                                <> parens (sep $ punctuate (text ",") [pprintTerm t_i sig var | t_i <- ts])
                                    
