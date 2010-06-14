{-
This file is part of the Haskell Term Rewriting Library.

The Haskell Term Rewriting Library is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

The Haskell Term Rewriting Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the Haskell Term Rewriting Library.  If not, see <http://www.gnu.org/licenses/>.
-}

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


pprintTrs :: (a -> Doc) -> [a] -> Doc
pprintTrs ppRule trs = braces $ pprs trs 
      where pprs []  = text ""
            pprs [r] = ppRule r
            pprs rs  = vcat $ [com <+> ppRule r | (com,r) <- zip (text " " : repeat (text ",")) rs]

instance PrettyPrintable Trs where
    pprint trs = pprintTrs pprint (rules trs)

instance PrettyPrintable (Trs, Signature, Variables) where 
    pprint (trs, sig, var) = pprintTrs (\ r -> pprint (r, sig, var)) (rules trs)

instance PrettyPrintable (R.Rule, Signature, Variables) where
    pprint ((R.Rule l r), sig, var) = fsep [pprint (l, sig, var), text "->", pprint (r, sig, var)]


instance PrettyPrintable (Term, Signature, Variables) where
  pprint ((Var x), _, var)      = text $ V.variableName x var
  pprint ((Fun f ts), sig, var) = pprint (f,sig)
                                <> parens (sep $ punctuate (text ",") [pprint (t_i, sig, var) | t_i <- ts])

instance PrettyPrintable Int where pprint = int
