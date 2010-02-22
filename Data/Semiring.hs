module Data.Semiring where

import Data.Monoid

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Sequence ( Seq, (><), ViewL(..), ViewR(..) )
import qualified Data.Sequence as Seq

import qualified Data.Foldable as Fold

infixr 6 .+.
infixr 7 .*.

class CommutativeMonoid m
 where zero  :: m
       (.+.) :: m -> m -> m

class CommutativeMonoid s => Semiring s
 where one   :: s
       (.*.) :: s -> s -> s

instance CommutativeMonoid Bool
 where zero  = False
       (.+.) = (||)

instance Semiring Bool
 where one   = True
       (.*.) = (&&)

instance CommutativeMonoid Int
 where zero  = 0
       (.+.) = (+)

instance Semiring Int
 where one   = 1
       (.*.) = (*)

instance (CommutativeMonoid a, CommutativeMonoid b) => CommutativeMonoid (a,b)
 where zero                = (zero   , zero   )
       (x1,y1) .+. (x2,y2) = (x1.+.x2, y1.+.y2)

instance (Semiring a, Semiring b) => Semiring (a,b)
 where one                 = (one    , one    )
       (x1,y1) .*. (x2,y2) = (x1.*.x2, y1.*.y2)

instance CommutativeMonoid b => CommutativeMonoid (a -> b)
 where zero  = \_ -> zero
       f.+.g = \x -> f x .+. g x

instance Semiring b => Semiring (a -> b)
 where one   = \_ -> one
       f.*.g = \x -> f x .*. g x

instance Ord a => CommutativeMonoid (Set a)
 where zero  = Set.empty
       (.+.) = Set.union

instance (Ord a, Monoid a) => Semiring (Set a)
 where one   = Set.singleton mempty
       s.*.t = Set.fromList [mappend m n | m <- Set.toList s, n <- Set.toList t]

newtype Tuple a = Tuple { getTuple :: Seq a }
 deriving (Ord, Eq, Show)

tuple :: [a] -> Tuple a
tuple = Tuple . Seq.fromList

fromTuple :: Tuple a -> [a]
fromTuple = Fold.toList . getTuple

instance Monoid a => Monoid (Tuple a)
 where
  mempty = Tuple (Seq.singleton mempty)

  mappend (Tuple s) (Tuple t) =
    Tuple (case (Seq.viewr s, Seq.viewl t) of
             (xs :> x, y :< ys) -> xs >< Seq.singleton (mappend x y) >< ys)
