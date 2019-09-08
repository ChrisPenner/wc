module Types where

import Data.Monoid
import Data.Char

data Counts =
    Counts { charCount :: !(Sum Int)
           , wordCount :: !Flux
           , lineCount :: !(Sum Int)
           }
    deriving (Show)

instance Semigroup Counts where
  (Counts a b c) <> (Counts a' b' c') = Counts (a <> a') (b <> b') (c <> c')

instance Monoid Counts where
  mempty = Counts mempty mempty mempty


data Pair a = Pair !a !a
    deriving (Show, Eq)

data CharType = IsSpace | NotSpace
    deriving Show
data Flux = Flux !CharType !Int !CharType
    deriving Show

instance Semigroup Flux where
  Flux l n NotSpace <> Flux NotSpace n' r = Flux l (n + n' - 1) r
  Flux l n _ <> Flux _ n' r = Flux l (n + n') r

instance Monoid Flux where
  mempty = Flux IsSpace 0 IsSpace

flux :: Char -> Flux
flux c | isSpace c = mempty
       | otherwise = Flux NotSpace 1 NotSpace

countChar :: Char -> Counts
countChar c =
    Counts { charCount = 1
           , wordCount = flux c
           , lineCount = if (c == '\n') then 1 else 0
           }

getFlux :: Flux -> Int
getFlux (Flux _ n _) = n
