{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Fractran where

import Control.Monad (when)
import Data.Primitive.Types
import Control.Monad.ST (ST, runST)
import GHC.Err
import GHC.Exts
import Data.Primitive.PrimArray
import Prelude hiding (Rational)
import qualified Text.Read.Lex as L
import Text.ParserCombinators.ReadPrec hiding (get)
import GHC.Read
import Text.Read (readMaybe)

data Rational = Rat !Int !Int
  deriving Show
  -- fix show instance
  --

instance Read Rational where
  readPrec =
    parens
      ( prec 7
        ( do x <- step readPrec
             expectP (L.Symbol "/")
             y <- step readPrec
             return (Rat x y)
        )
      )

instance Prim Rational where
  sizeOf# _ = 2# *# sizeOf# (undefined :: Int)
  alignment# _ = alignment# (undefined :: Int)
  indexByteArray# arr# i# =
    let x = indexByteArray# arr# (2# *# i#)
        y = indexByteArray# arr# (2# *# i# +# 1#)
    in Rat x y
  readByteArray# arr# i# =
    \s0 -> case readByteArray# arr# (2# *# i#) s0 of
       (# s1#, x #) -> case readByteArray# arr# (2# *# i# +# 1#) s1# of
          (# s2#, y #) -> (# s2#, Rat x y #)
  writeByteArray# arr# i# (Rat a b) =
    \s0 -> case writeByteArray# arr# (2# *# i#) a s0 of
       s1 -> case writeByteArray# arr# (2# *# i# +# 1#) b s1 of
         s2 -> s2
  setByteArray# = defaultSetByteArray#
  indexOffAddr# addr# i# =
    let x = indexOffAddr# addr# (2# *# i#)
        y = indexOffAddr# addr# (2# *# i# +# 1#)
    in Rat x y
  readOffAddr# addr# i# =
    \s0 -> case readOffAddr# addr# (2# *# i#) s0 of
       (# s1, x #) -> case readOffAddr# addr# (2# *# i# +# 1#) s1 of
         (# s2, y #) -> (# s2, Rat x y #)
  writeOffAddr# addr# i# (Rat a b) =
    \s0 -> case writeOffAddr# addr# (2# *# i#) a s0 of
       s1 -> case writeOffAddr# addr# (2# *# i# +# 1#) b s1 of
         s2 -> s2
  setOffAddr# = defaultSetOffAddr#
  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}

-- | Result of parsing a FRACTRAN program
data PResult = PResult
  { initial :: !Int -- ^ n
  , size    :: !Int -- ^ size of program
  , fracs   :: ![Rational] -- program 
  }

get :: FilePath -> IO String
get !fp = readFile fp >>= pure

eval :: FilePath -> IO ()
eval !fp = get fp >>= (pure . parse) >>= either print print

parse :: String -> Either String [Int]
parse [] = Left $ invalid "EMPTY"
parse [x] = Left $ invalid "SINGLE CHAR"
parse (x:xs) = case readMaybe [x] :: Maybe Int of
  Nothing -> Left $ invalid "FIRST CHARACTER NOT AN INT"
  Just i  -> case readMaybe xs :: Maybe [Rational] of
    Nothing -> Left $ invalid "READ ERROR ON FRACTIONS"
    Just rs -> let res = PResult i (length rs) rs
                   prog = construct res
               in Right $ run prog

invalid :: String -> String
invalid s = "INVALID FRACTRAN: " ++ s

construct :: PResult -> Fractran
construct (PResult !n s fracs) = Fractran n (primArrayFromListN s fracs)

data Fractran = Fractran !Int !(PrimArray Rational)

data Check
  = Terminate
  | First
    { _new :: !Int -- ^ new 'n' value
    , _ix  :: !Int -- ^ index at which it occurs
    }

reduce :: Int -> Int -> Rational
reduce x y = Rat (x `quotInt` d) (y `quotInt` d)
  where
    d = gcd x y
--  | b == 0 = error "divbyzero"
--  | b == (-1) && a == minBound = error "overFlow"
     
quotInt :: Int -> Int -> Int
(I# x) `quotInt` (I# y) = I# (x `quotInt#` y)
{-# INLINE quotInt #-}

-- | returns the first position at which nf is an integer
check :: Fractran -> Check
check (Fractran !n !pa) = runST run where
  run :: forall s. ST s Check
  run = do
    let !sz = sizeofPrimArray pa 
    mpa <- newPrimArray sz
    let go :: Int -> ST s Check
        go !ix = if ix < sz
          then do
            !(Rat x y) <- readPrimArray mpa ix
            if (n `mod` y == 0)
              then
                let (Rat x' y') = reduce (n * x) y
                in pure $! First x' ix
              else go (ix + 1)
          else pure Terminate
    go 0
{-# INLINE check #-}

run :: Fractran -> [Int]
run f = go 0 where
  go !ix =
    let c = check f
    in case c of
      Terminate -> []
      First r _ -> r : go (ix + 1)
