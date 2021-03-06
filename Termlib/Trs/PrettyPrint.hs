
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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Termlib.Trs.PrettyPrint where

import Termlib.Trs hiding (empty)
import qualified Termlib.Trs as Trs
import qualified Termlib.Rule as R
import qualified Termlib.Variable as V
import Termlib.Term
import Termlib.FunctionSymbol (Signature)
import Termlib.Variable (Variables)
import Text.PrettyPrint.HughesPJ
import Termlib.Utils


pprintTrs :: (a -> Doc) -> [a] -> Doc
pprintTrs ppRule trs = pprs trs 
      where pprs []  = text ""
            pprs [r] = braces $ text " " <> ppRule r <> text " "
            pprs rs  = vcat [ com <+> ppRule r
                            | (com,r) <- zip (text "{" : repeat (text ",")) rs ]
                       <+> text "}"

instance PrettyPrintable Trs where
    pprint trs = pprintTrs pprint (rules trs)

instance PrettyPrintable (Trs, Signature, Variables) where 
    pprint (trs, sig, var) = pprintTrs (\ r -> pprint (r, sig, var)) (rules trs)


pprintNamedTrs :: Signature -> Variables -> String -> Trs -> Doc
pprintNamedTrs sig vars n trs  
    | Trs.isEmpty trs = empty 
    | otherwise       = block n $ pprint (trs, sig, vars)

instance PrettyPrintable (R.Rule, Signature, Variables) where
    pprint ((R.Rule l r), sig, var) = fsep [pprint (l, sig, var), text "->", pprint (r, sig, var)]


instance PrettyPrintable (Term, Signature, Variables) where
  pprint ((Var x), _, var)      = text $ V.variableName x var
  pprint ((Fun f ts), sig, var) = pprint (f,sig)
                                <> parens (sep $ punctuate (text ",") [pprint (t_i, sig, var) | t_i <- ts])

instance PrettyPrintable Int where pprint = int
