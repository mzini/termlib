{-# LANGUAGE ParallelListComp #-}
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

{-# LANGUAGE FlexibleContexts #-}
module Termlib.Utils where 

-- import Control.Monad.Identity ()
import Text.PrettyPrint.HughesPJ
import Data.List (transpose)
import qualified Control.Monad.State.Lazy as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String ()
import qualified Text.PrettyPrint.HughesPJ as PP

-- * Parsing and Printing

class PrettyPrintable a where
  pprint :: a -> Doc

instance PrettyPrintable Doc where
  pprint = id

columns :: Int -> [[Doc]] -> Doc
columns spce cols = columns' [(width cs, cs) | cs <- cols]
    where width col = spce + maximum (0 : [docLength e | e <- col])

columns' :: [(Int, [Doc])] -> Doc
columns' cols = vcat [ pprow row | row <- rows]
    where rows      = transpose [ [ (i,c) | c <- cs ] | (i,cs) <- cols']
          pprow row = vcat [ text (concat l) | l <- transpose rowLines]
              where rowLines  = [[ pad len l | l <- ls ++ take (h - length ls) (repeat "")]  | (len,ls) <- cs]
                    cs = [ (len, lines (show e)) | (len,e) <- row ]
                    h  = maximum $ 1 : [length ls | (_,ls) <- cs]
                    pad len s = s ++ take (len - length s) (repeat ' ')
          numrows = maximum $ 0 : [length cs | (_,cs) <- cols ]
          cols'   = [ (i, cs ++ take (numrows - length cs) (repeat empty)) | (i,cs) <- cols]

data PPTree a = PPTree { pptRoots :: [a]
                       , pptSuc :: a -> [a]}

printTree :: Int -- label offset
             -> ([a] -> a -> Doc) -- printing of nodes
             -> ([a] -> a -> Doc) -- printing of labels
             -> PPTree a
             -> Doc
printTree offset ppNode ppLabel tree  = printTree' True [] [] (pptRoots tree)
    where printTree' top edges pth ns = vcat $ [ let pth' = pth ++ [n] 
                                                 in printEdges False edges
                                                 $+$ ((printEdges isLast edges <> (text "->" <> ppNode pth' n)) $$ nest offset (ppLabel pth' n))
                                                 $+$ printTree' False (edges' isLast) pth' (pptSuc tree n)
                                                 $+$ if top then text "" else PP.empty
                                                 | n <- ns  
                                                 | isLast <- (take (length ns - 1) (repeat False)) ++ [True]]
              where edges' True =  True : flp edges
                    edges' False = True : edges
                    flp (False : edges) = (True : edges)
                    flp (True : edges) = (False : edges)
                    flp [] = []

          printEdges b (True :  es) = printEdges False es <> text (spce ++ (if b then "`" else "|"))
          printEdges _ (False : es) = printEdges False es <> text (spce ++ " ")
          printEdges _ [] = PP.empty
          spce = "   "

docLength :: Doc -> Int
docLength d = maximum $ 0 : [ length l | l <- lines $ show d]

docHeight :: Doc -> Int
docHeight d = length $ lines $ show d

padToLength :: Int -> Doc -> Doc
padToLength len d = d <> text padding
    where padding = take (len - docLength d) $ repeat ' '

padToHeight :: Int -> Doc -> Doc
padToHeight h d = d $$ padding
    where padding = hcat [ text "" | _ <- [1..(h - docHeight d)]]


class Parsable a where
  parse :: Stream s m Char => ParsecT s u m a

parseFromString :: Parsable a => String -> Either ParseError a
parseFromString s = runParser parse () "/dev/stderr" s


-- * Memoisation

type MemoAction k a = State.State (Map.Map k a)

memo :: (Ord k) => k -> (MemoAction k a a) -> (MemoAction k a a)
memo k  m = do s <- State.get 
               case Map.lookup k s of
                 Just old -> return old
                 Nothing  -> do { new <- m;
                                 State.modify (Map.insert k new);
                                 return new}


runMemoAction :: (Ord k) => MemoAction k a b -> b
runMemoAction ma = fst $ State.runState ma Map.empty

liftMemo :: (Ord k) => (k -> a) -> (k -> MemoAction k a a)
liftMemo f k = memo k (return $ f k)

-- * List Utility Functions

listProduct :: [[a]] -> [[a]]
listProduct []             = [[]]
listProduct (xs:xss) = foldl f [] xs
  where f a x = map (\ xs' -> x:xs') (listProduct xss) ++ a


snub :: Ord a => [a] -> [a]
snub = Set.toList . Set.fromList


-- * Monad Utilities

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  x >>= f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
  fail _ = MaybeT $ return Nothing


eitherM :: Monad m => (a -> m c) -> (b -> m c) -> m (Either a b) -> m c
eitherM ma mb me = do e <- me 
                      either ma mb  e

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t e = do g <- b
               if g then t else e

-- * Parsec Utility Functions


($++$) :: Doc -> Doc -> Doc
a $++$ b = a $+$ text "" $+$ b

paragraph :: String -> Doc
paragraph s = vcat [ fsep [text w | w <- words l] | l <- lines s]

underline :: Doc -> Doc
underline p = p $+$ text (take (length $ show p) $ repeat '-')

enumerated :: [Doc] -> [Doc] -> Doc
enumerated indices ds = vcat [ i <> text ")" <+> d | d <- ds | i <- indices]

pprintInt :: Int -> Doc
pprintInt = text . show

pprintChar :: Char -> Doc
pprintChar c = text [c]

block :: String -> Doc -> Doc
block h doc   = hang (text (h ++ ":")) 2 $ doc


-- * Misc
class Enumerateable a where
  enum :: a -> Int
  invEnum :: Int -> a
