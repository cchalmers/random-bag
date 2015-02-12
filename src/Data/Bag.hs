{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- |
--  Module      : Data.Bag
--  Copyright   : Copyright (C) 2015 Christopher Chalmers
--  License     : BSD3
--
--  Maintainer  : Christopher Chalmers <c.chalmers@me.com>
--  Stability   : experimental
--  Portability : GHC
--
--  Simple structure with O(1) insert, delete, random read and random modify.
--
--  All Operations are in a `PrimMonad` for efficiency.
--

module Data.Bag where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Data
import           Data.PRef
import qualified Data.Vector.Binary          ()
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

import           System.Random.PCG.Class

data Bag m a where
  Bag :: Generator g m
      => !g
      -> !(PRef (PrimState m) Int)
      -> !(M.MVector (PrimState m) a)
      -> Bag m a
  deriving Typeable

-- | Contruct a bag from a list, using the given generator
mkBag :: (PrimMonad m, M.Unbox a, Generator g m) => g -> [a] -> m (Bag m a)
mkBag g as = do
  let v = V.fromList as
  thawBag g 0.2 v

type IOBag   = Bag IO
type STBag s = Bag (ST s)

-- | Check if a bag is empty.
empty :: PrimMonad m => Bag m a -> m Bool
empty (Bag _ r _) = do
  m <- readPRef r
  return $ m == 0

-- | Select a random value from the bag. Returns an error if the bag is
--   empty.
luckyDip :: (PrimMonad m, M.Unbox a) => Bag m a -> m a
luckyDip (Bag g r v) = do
  m <- readPRef r
  when (m == 0) $ error "luckyDip: empty bag"
  i <- uniformR (0, m - 1) g
  M.read v i

{-
-- | Take a random element from the bag so it's no longer in the bag.
luckyDip :: (PrimMonad m, MVector v a) => Bag m v a -> m (Maybe a)

-- | Look at random element in the bag but don't take it out.
luckyPeek :: (PrimMonad m, MVector v a) => Bag m v a -> m (Maybe a)

-- | Look at random element in the bag but don't take it out. Throws an
--   error if the bag is empty.
unsafePeek :: (PrimMonad m, MVector v a) => Bag m v a -> m a

-- | O(n). Shuffle the bag, returning a vector of each element of the
--   bag in random order.
shuffle :: Bag m v a -> m (v (PrimState m) a)

-- | O(n). Shuffle the bag, returning a vector of up to @n@ elements of
--   the bag in random order.
shuffleN :: Int -> Bag m v a -> m (v (PrimState m) a)

-- | O(n). Fold the elements of the bag in a random order.
foldM :: (b -> a -> m b) -> b -> Bag m v a -> m b

-- | O(n). Take @n@ random elements from the bag to form a new bag. The
--   elements are no longer in the first bag.
take :: Int -> Bag m v a -> m (Bag m v a)

-- | O(n). @add a b@ adds elements from bag @a@ to bag @b@.
combine :: Bag m v a -> Bag m v a -> m ()

-- | O(n). Find an element in a bag.
lookup :: Eq a => a -> Bag m v a -> m (Maybe a)

-- | O(n). Find an element in a bag.
find :: (a -> Bool) -> Bag m v a -> m (Maybe a)

filter :: (a -> Bool) -> Bag m v a -> m ()

overVector  :: Bag m v a -> (v a -> v b) -> m (Bag m v a)
overVectorM :: Bag m v a -> (v a -> m (v b)) -> m (Bag m v a)

withVector  :: Bag m v a -> (v a -> v a) -> m ()
withVectorM :: Bag m v a -> (v a -> m (v a)) -> m ()

overMVector :: Bag m v a -> (Mutable v a -> m (Mutable v a)) -> m ()

-}

-- | What to do with a modified item in the bag.
data Modify a
  = Keep
  | Remove
  | Modify !a

withElement :: (PrimMonad m, M.Unbox a) => Bag m a -> (a -> m b) -> m (Maybe b)
withElement bag@(Bag _ r _) f = do
  m <- readPRef r
  if m == 0
    then return Nothing
    else do
      a <- luckyDip bag
      b <- f a
      return (Just b)

-- | Modify some random element of a bag. Allows removal, keeping or
--   modifying. Does nothing if the bag is empty.
rModify :: (PrimMonad m, M.Unbox a) => Bag m a -> (a -> Modify a) -> m ()
rModify (Bag g r v) f = do
  m <- readPRef r
  if m == 0
    then return ()
    else do
      i <- uniformR (0, m - 1) g
      a <- M.read v i
      case f a of
        Keep     -> return ()
        Modify b -> M.write v i b
        Remove   -> do
          l <- M.read v (m - 1)
          M.write v i l
          modifyPRef r (subtract 1)

-- | Modify some element of a bag. Allows removal, keeping or modifying.
--   Does nothing if the bag is empty.
rModifyM :: (PrimMonad m, M.Unbox a) => Bag m a -> (a -> m (Modify a)) -> m ()
rModifyM (Bag g r v) f = do
  m <- readPRef r
  if m == 0
    then return ()
    else do
      i <- uniformR (0, m - 1) g
      a <- M.read v i
      f a >>= \case
        Keep     -> return ()
        Modify b -> M.write v i b
        Remove   -> do
          l <- M.read v (m - 1)
          M.write v i l
          modifyPRef r (subtract 1)

-- | Helper to only modify when @b@ is 'True'.
modifyWhen :: Monad m => Bool -> m (Modify a) -> m (Modify a)
modifyWhen b a = if b then a else return Keep

-- | Helper to only modify when @b@ is 'False'.
modifyUnless :: Monad m => Bool -> m (Modify a) -> m (Modify a)
modifyUnless b a = if b then return Keep else a

elem :: (PrimMonad m, Eq a, M.Unbox a) => Bag m a -> a -> m Bool
elem bag a = do
  v <- unsafeFreezeBag bag
  return $ a `V.elem` v

-- | O(n) Replace one element with another. Returns an error if the
--   element isn't present.
replace :: (PrimMonad m, Eq a, M.Unbox a)
        => Bag m a -> a -> a -> m ()
replace bag@(Bag _ _ mv) a b = do
  v <- unsafeFreezeBag bag
  let mi = V.elemIndex a v
  case mi of
    Nothing -> error "replace: element not in bag"
    Just i  -> M.write mv i b

-- | Insert an element in the bag. If the vector isn't large enough this
--   returns an error.
--
--   Be aware the currently the underlying vector has no way to grow, so
--   if you add too many elements, an error is throw. This should be
--   fixed soon.
insert :: (PrimMonad m, M.Unbox a) => Bag m a -> a -> m ()
insert (Bag _ r v) a = do
  m <- readPRef r
  when (m > M.length v) $ error "insert: Bag not large enough!"
  -- TODO figure out a nice way to grow the vector

  M.write v m a
  plusOne r

-- immutable -----------------------------------------------------------

-- | Freeze a bag to retreive it's internal vector.
freezeBag :: (PrimMonad m, M.Unbox a) => Bag m a -> m (V.Vector a)
freezeBag (Bag _ r v) = do
  m <- readPRef r
  let v' = M.take m v
  V.freeze v'

-- | Freeze a bag to retreive it's internal vector.
unsafeFreezeBag :: (PrimMonad m, M.Unbox a) => Bag m a -> m (V.Vector a)
unsafeFreezeBag (Bag _ r v) = do
  m <- readPRef r
  let v' = M.take m v
  V.unsafeFreeze v'

-- | Thaw a vector with a `Generator` and initial increase factor for
--   the internal vector. This allows inserting elements.
thawBag :: (PrimMonad m, Generator g m, M.Unbox a) => g -> Double -> V.Vector a -> m (Bag m a)
thawBag g x v = do
  m <- V.thaw v
  m' <- M.grow m $ ceiling (fromIntegral (V.length v) * x + 10)
  r <- newPRef $ V.length v
  return $ Bag g r m'

-- ------------------------------------------------------------------------
-- -- Internal
-- ------------------------------------------------------------------------

-- <&> :: Functor f => f a -> (a -> b) -> f b
-- <&> = flip fmap

-- bagVector :: Lens (Bag s a) (Bag s b) (M.MVector s a) (M.MVector s b)
-- bagVector f (Bag r v) = f v <&> Bag r

-- bagSize :: PrimMonad m => Bag (PrimState m) a -> m Int
-- bagSize (Bag r _) = readPRef r

