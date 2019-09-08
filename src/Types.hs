module Types where

import Data.Monoid
import Data.Char

data Counts =
    Counts { charCount :: !(Sum Int)
           , wordCount :: !(Flux Bool)
           , lineCount :: !(Sum Int)
           }
    deriving (Show)

instance Semigroup Counts where
  (Counts a b c) <> (Counts a' b' c') = Counts (a <> a') (b <> b') (c <> c')

instance Monoid Counts where
  mempty = Counts mempty mempty mempty


data Pair a = Pair !a !a
    deriving (Show, Eq)

data Flux a = Flux
  -- We keep track of the last value we saw on the left and right sides of the accumulated
  -- sequence; `Nothing` is used in the identity case meaning no elements have yet
  -- been encountered
  { sides :: !(Maybe (Pair a))
  -- We have a counter which increments each time we mappend another Flux who's
  -- left doesn't match our right or vice versa depending on which side it is mappended onto.
  , getFlux :: !Int
  } deriving (Show, Eq)

-- | Embed a single value into a Flux;
-- number of changes starts at 0.
flux :: a -> Flux a
flux a = Flux (Just (Pair a a)) 0

instance (Eq a) => Semigroup (Flux a) where
  Flux Nothing _ <> f = f
  f <> Flux Nothing _ = f
  Flux (Just (Pair l r)) n <> Flux (Just (Pair l' r')) n'
    | r == l' = Flux (Just (Pair l r')) (n + n')
    | otherwise = Flux (Just (Pair l r')) (n + n' + 1)

instance (Eq a) => Monoid (Flux a) where
  mempty = Flux Nothing 0

countChar :: Char -> Counts
countChar c =
    Counts { charCount = 1
           , wordCount = flux (isSpace c)
           , lineCount = if (c == '\n') then 1 else 0
           }
