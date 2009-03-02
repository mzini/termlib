module Termlib.Trs.PrettyPrint where
 
import Termlib.Trs hiding (empty)
import qualified Termlib.Rule as R
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Variable as V
import Termlib.Term
import qualified Termlib.Signature as Sig
import Text.PrettyPrint.HughesPJ
import Control.Monad (forM)

pprintTrs :: Trs -> Doc
pprintTrs trs = fst $ runTrs (forM (rules trs) pprintRule >>= return . sep) trs

pprintRule :: R.Rule -> TrsMonad Doc
pprintRule (R.Rule l r) = do ppl <- pprintTerm l
                             ppr <- pprintTerm r 
                             return $ sep [ppl, text "->", ppr]


pprintTerm :: Term -> TrsMonad Doc
pprintTerm (Var x) = getVariables >>= return . (Sig.attribute V.ident x) >>= return . text
pprintTerm (Fun f ts) = do sig <- getSignature
                           ppts <- forM ts pprintTerm
                           let  attribs = Sig.attribute id f sig
                                ppsym = ppname <> ppmark <> pplabel  
                                ppname = text $ F.ident attribs
                                ppmark = if F.isMarked attribs then text "^#" else empty
                                pplabel = case F.label attribs of 
                                            Just l  -> text "_" <> int l
                                            Nothing -> empty
                           return $ ppsym <> parens (sep $ punctuate (text ",") ppts)
