{-# LANGUAGE TypeSynonymInstances #-}
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

module Termlib.Repl 
       ( 
         -- | This Module provides enlists most common used functions.
         
         
         -- * Data Structures containing terms
         WithTerms(..)
         , isSubtermOf
         -- | 't ``isSubtermOf`` a' checks if 't' occurs in the list of subterms of 'a'
         , nonVariableSubterms           
         -- | filters variables from the list of subterms 
           
         -- * Terms
         , Term.Term(..)
         -- ** Querying
         , Term.depth
           -- | returns the depth of a term. Variables and constants admit depth '0'
         , Term.size
           -- | returns the size of a term. Variables and constants admit size '1'           
         , Term.fsize
           -- | returns the number of function symbols
           
         , Term.root
           -- | returns the root of the term
         , Term.immediateSubterms
           -- | returns the list of direct subterms or '[]' if argument is a variable
         , Term.properSubterms           
           -- | returns the list of subterms, excluding the argument
         , Term.isSupertermOf
           -- | converse of 'isSubtermOf'
         , Term.cardinality
           -- | returns the number of occurences of the first argument
         , Subst.isUnifiable           
           -- | returns 'True' iff the arguments are unifiable
         , Subst.isRenamedUnifiable           
           -- | returns 'True' iff renamings without common variables are unifiable
         , Subst.matches
           -- | 's ``matches`` t' returns 'True' iff 't' is an instance of 's'
         , Subst.subsumes
           -- | inverse of matches
         , Subst.encompasses
           -- | 's ``encomapsses`` t' returns 'True' iff 's' encompasses 't'
         , Subst.variant           
           -- | two terms are variants if they subsume eatch other
           
         , Term.isVariable 
           -- | returns 'True' if the argument is a variable
         
         -- * Term Rewriting Rule
         , Rule.Rule(..)
         -- ** Predicates
         -- | See module "Termlib.Rule" for further predicates. Predicates can be lifted
         -- with @all@ and @any@ from module 'Data.Foldable' to 'Trs.Trs's.
           
         , Rule.isNonErasing
           -- | rule is non-erasing if every occurence of a variable in the left-hand
           -- side occurs in the right-hand side 
         , Rule.isErasing           
           -- | inverse of 'isNonErasing'
         , Rule.isNonDuplicating
           -- | a rule is non-duplicating if no variable appears more often 
           -- in the right-hand side than in the left-hand side
         , Rule.isDuplicating
           -- | inverse of 'isNonDuplicating'
           
         -- ** Modification
         , Rule.invert 
           -- | convert a rule 'l -> r' to 'r -> l'
         , Rule.canonise
           -- | renames variables to a canonical form.
           -- Two rules 'r1' and 'r2' are equal modulo variable renaming, 
           -- iff 'canonise r1' and 'canonise r2' are syntactically equal.
           
         -- * Term Rewrite System
         , Trs.Trs
           -- | A TRS is a list of 'Rule.Rule'.
         , emptyTrs 
           -- | The empty 'Trs.Trs'..
         , Trs.fromRules
           -- | translates a list of rewrite rules to a 'Trs.Trs'.
         , Trs.toRules
           -- | returns the list of rewrite rules in the TRS          
           
         -- ** Set Operations
         , Trs.union 
           -- | union operator on the set of rules, removing duplicates
         , Trs.append 
           -- | like union, but does not remove duplicates
         , (Trs.\\)
           -- | difference on the set of rules
         , Trs.intersect
           -- | intersection on the set of rules
         , Trs.member
           -- | checks if a rule is contained in the TRS, does not perform
           -- variable conversion
         , Trs.insert
           -- | inserts a rule into a TRS, if the rule is not already contained
                  
         -- ** Querying  
         , Trs.lhss
           -- | returns the list of left-hand sides
         , Trs.rhss
           -- | returns the list of left-hand sides
         , Trs.definedSymbols
           -- | returns roots of right-hand sides
         , Trs.constructors
           -- | returns all symbols which are not roots of right-hand sides
           
         -- ** Predicates           
           -- | Many predicates are defined in "Termlib.Rule". These can be lifted to  
           -- 'Trs.Trs' by 'Fold.all' and 'Fold.any'
           
         , Trs.isEmpty
           -- | checks if the list of rules is empty
         , Trs.wellFormed
           -- | checks that no left-hand side is a variable
           -- and all variables of a right-hand side are included
           -- in the corresponding left-hand side
         , Trs.isOverlapping
           -- | returns 'True' iff it contains two overlapping rules
         , Trs.isOverlay
           -- | returns 'True' iff all overlaps are only root overlaps
         , Trs.isOrthogonal
           -- | returns 'True' iff the given TRS is orthogonal           
         , Trs.isNestedRecursive
           -- | returns 'True' iff there exists a rule 'f(..) -> C[f(..C[f(..)]..)]'.
           
         -- ** Modification
         , Trs.filterRules
           -- | removes rules from the TRS matching the predicate
         , Trs.mapRules
           -- | map the given function over the rules
           
           -- * Complexity Problem
         , Prob.Problem(..)
         , Prob.StartTerms (..)
         , Prob.Strategy (..)
           -- ** Querying
         , Prob.weakComponents
           -- | returns weak dependency pairs and weak rewrite rules
         , Prob.strictComponents
           -- | returns strict dependency pairs and strict rewrite rules
         , Prob.dpComponents           
           -- | returns all dependency pairs
         , Prob.trsComponents           
           -- | returns all dependency rules which are not dependency pairs
         , Prob.allComponents
           -- | returns all dependency pairs and rules
           -- ** Predicates
         , Prob.isRCProblem
           -- | returns 'True' iff the set of start-terms is basic
         , Prob.isDCProblem
           -- | converse of 'isRCProblem'
         , Prob.isDPProblem
           -- | returns 'True' iff the set of start-terms is basic, and all 
           -- defined symbols are marked in the set of basic terms
           
           -- ** Modification
         , Prob.withFreshCompounds           
           -- | replaces all compound symbols by fresh compound symbols, 
           -- and removes unary compound symbols
           
           -- * Parsing Utilities 
         , parseFromString
         ) where


import qualified Termlib.Term.Parser as TParser
import qualified Termlib.Problem as Prob
import qualified Termlib.Trs as Trs
import qualified Termlib.Substitution as Subst
import qualified Termlib.FunctionSymbol as F
import qualified Termlib.Variable as V
import qualified Termlib.Term as Term
import qualified Termlib.Rule as Rule

import qualified Data.Foldable as Fold
import Data.Set (Set)
import qualified Data.Set as Set
import Termlib.Utils (PrettyPrintable(..))

class WithTerms a where
  -- | extracts the set of variables
  vars    :: a -> Set V.Variable 
  -- | extracts the set of function symbols  
  symbols :: a -> Set F.Symbol 
  -- | returns the list of subterms
  subterms :: a -> Set Term.Term
  
  isLinear :: a -> Bool 
  isGround :: a -> Bool  
  isFlat   :: a -> Bool
  isShallow :: a -> Bool   
  
nonVariableSubterms :: WithTerms a => a -> Set Term.Term
nonVariableSubterms = Set.filter (not . Term.isVariable) . subterms

isSubtermOf :: WithTerms a => Term.Term -> a -> Bool
isSubtermOf t a = t `Set.member` subterms a


instance WithTerms Term.Term where
  vars = Term.variables
  symbols = Term.functionSymbols
  subterms = Set.fromList . Term.subterms
  isLinear = Term.isLinear
  isGround = Term.isGround  
  isFlat = Term.isFlat
  isShallow = Term.isShallow
  

instance WithTerms Rule.Rule where
  vars = Rule.variables
  symbols = Rule.functionSymbols
  subterms r = subterms (Rule.lhs r) `Set.union` subterms (Rule.rhs r)
  isLinear = Rule.isLinear
  isGround = Rule.isGround  
  isFlat = Rule.isFlat
  isShallow = Rule.isShallow  

instance WithTerms Trs.Trs where
  vars = Trs.variables
  symbols = Trs.functionSymbols
  subterms r = Fold.foldl (\ ss rl -> subterms rl `Set.union` ss) Set.empty r
  isLinear = Fold.all isLinear
  isGround = Fold.all isGround
  isFlat = Fold.all isFlat
  isShallow = Fold.all isShallow

emptyTrs :: Trs.Trs
emptyTrs = Trs.empty

parseFromString :: TParser.TermParser a -> String -> Prob.Problem  -> (a,Prob.Problem)
parseFromString parser str prob = 
  case TParser.parseFromString (Prob.signature prob) (Prob.variables prob) parser str of
    Right ((t,fs,vs),_) -> (t, prob { Prob.variables = vs, Prob.signature = fs })
    Left e      -> error $ "Failed Parsing:\n" ++ show (pprint e)
                                     

