module Termlib.Trs.PrettyPrint where
 
import Termlib.Trs hiding (empty)
import qualified Termlib.Rule as R
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Variable as V
import Termlib.Term
import qualified Termlib.Signature as Sig
import Text.PrettyPrint.HughesPJ
import Control.Monad (forM)
import Termlib.Utils

instance PrettyPrintable Trs where 
  pprint trs = fst $ runTrs (forM (rules trs) pprintRule >>= return . sep) trs

pprintRule :: R.Rule -> TrsMonad Doc
pprintRule (R.Rule l r) = do ppl <- pprintTerm l
                             ppr <- pprintTerm r 
                             return $ sep [ppl, text "->", ppr]


pprintTerm :: Term -> TrsMonad Doc
pprintTerm (Var x) = getVariables >>= return . (Sig.attribute V.ident x) >>= return . text
pprintTerm (Fun f ts) = do sig <- getSignature
                           ppts <- forM ts pprintTerm
                           return $ pprint (Sig.attribute id f sig) 
                                    <> parens (sep $ punctuate (text ",") ppts)
