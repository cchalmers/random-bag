{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- |
--  Module      : Data.Bag
--  Copyright   : Copyright (c) 2015 Christopher Chalmers
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

module Data.RandomBag where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Data
import           Data.Primitive.ByteArray
import           Data.Vector.Unboxed         (Unbox, Vector)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M
import           Foreign.Storable            (sizeOf)

import           System.Random.PCG.Class

data Bag m a where
  Bag :: Generator g m
      => !g
      -> !(MutableByteArray (PrimState m))
      -> !(M.MVector (PrimState m) a)
      -> Bag m a
  deriving Typeable

-- | Contruct a bag from a list, using the given generator
mkBag :: (PrimMonad m, Unbox a, Generator g m) => g -> [a] -> m (Bag m a)
mkBag g as = do
  let v = V.fromList as
  thawBag g 0.2 v

type IOBag   = Bag IO
type STBag s = Bag (ST s)

size :: PrimMonad m => Bag m a -> m Int
size (Bag _ r _) = readByteArray r 0
{-# INLINE size #-}

resize :: PrimMonad m => Bag m a -> Int -> m ()
resize (Bag _ r _) = \i -> writeByteArray r 0 i
{-# INLINE resize #-}

-- internal ------------------------------------------------------------

bagUniform :: PrimMonad m => Bag m a -> Int -> m Int
bagUniform (Bag g _ _) i = uniformB i g
{-# INLINE bagUniform #-}

unsafeRead :: (PrimMonad m, Unbox a) => Bag m a -> Int -> m a
unsafeRead (Bag _ _ v) = \a -> M.unsafeRead v a
{-# INLINE unsafeRead #-}

randomIndex :: PrimMonad m => Bag m a -> m Int
randomIndex bag = size bag >>= bagUniform bag
{-# INLINE randomIndex #-}

withRandom :: PrimMonad m
           => Bag m a
           -> (M.MVector (PrimState m) a -> Int -> Int -> m b)
           -> m (Maybe b)
withRandom (Bag g r mv) f = do
  n <- readByteArray r 0
  if n == 0
    then return Nothing
    else do
      i <- uniformB n g
      b <- f mv i n
      return $! Just $! b

vRemove :: (PrimMonad m, Unbox a)
        => M.MVector (PrimState m) a
        -> Int -- last element
        -> Int -- element to remove
        -> m ()
vRemove mv i n = M.read mv (n - 1) >>= M.write mv i

-- Exposed -------------------------------------------------------------

-- | Take a random element from the bag.
luckyDip :: (PrimMonad m, Unbox a) => Bag m a -> m (Maybe a)
luckyDip bag = liftM join . withRandom bag $ \mv i n -> do
  a <- M.read mv i
  vRemove mv i n
  resize bag (n - 1)
  return (Just $! a)
{-# INLINE luckyDip #-}

-- | Take a random element from the bag and
luckyPeek :: (PrimMonad m, Unbox a)
         => Bag m a -> m (Maybe a)
luckyPeek bag = withRandom bag $
  \mv i _ -> M.read mv i
{-# INLINE luckyPeek #-}

-- | Check if a bag is empty.
empty :: PrimMonad m => Bag m a -> m Bool
empty = liftM (==0) . size
{-# INLINE empty #-}

-- -- | Select a random value from the bag. Returns nothing
-- --   empty.
-- luckyDip :: (PrimMonad m, Unbox a) => Bag m a -> m (Maybe a)
-- luckyDip (Bag g r v) = do
--   n <- size
--   if n == 0
--     then Nothing
--     else do
--       i <- bagUniform bag n
--       a <- unsafeRead bag i
--       return (Just $! a)
-- {-# INLINE luckyDip #-}

-- | Select a random value from the bag.
unsafeLuckyDip :: (PrimMonad m, Unbox a) => Bag m a -> m a
unsafeLuckyDip bag = randomIndex bag >>= unsafeRead bag
{-# INLINE unsafeLuckyDip #-}

{-
-- | O(n). Fold the elements of the bag in a random order.
foldM :: (b -> a -> m b) -> b -> Bag m v a -> m b

-- | O(n). Take @n@ random elements from the bag to form a new bag. The
--   elements are no longer in the first bag.
take :: Int -> Bag m v a -> m (Bag m v a)

-- | O(n). @add a b@ adds elements from bag @a@ to bag @b@.
combine :: Bag m v a -> Bag m v a -> m ()

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

-- | Helper to only modify when @b@ is 'True'.
modifyWhen :: Monad m => Bool -> m (Modify a) -> m (Modify a)
modifyWhen b a = if b then a else return Keep
{-# INLINE modifyWhen #-}

-- | Helper to only modify when @b@ is 'False'.
modifyUnless :: Monad m => Bool -> m (Modify a) -> m (Modify a)
modifyUnless b a = if b then return Keep else a
{-# INLINE modifyUnless #-}

modify :: (PrimMonad m, Unbox a) => Bag m a -> (a -> Modify a) -> m ()
modify b f = modifyM b (return . f)
{-# INLINE modify #-}

modifyM :: (PrimMonad m, Unbox a) => Bag m a -> (a -> m (Modify a)) -> m ()
modifyM bag f = liftM (const ()) . withRandom bag $
  \mv i n -> do
    a <- M.read mv i
    f a >>= \case
      Keep     -> return ()
      Remove   -> vRemove mv i n >> resize bag (n - 1)
      Modify b -> M.write mv i b
{-# INLINE modifyM #-}

alter :: (PrimMonad m, Unbox a) => Bag m a -> (a -> a) -> m ()
alter bag f = modify bag (Modify . f)
{-# INLINE alter #-}

alterM :: (PrimMonad m, Unbox a) => Bag m a -> (a -> m a) -> m ()
alterM bag f = modifyM bag (liftM Modify . f)
{-# INLINE alterM #-}

-- vectors -------------------------------------------------------------

-- | Use the frozen underlying vector. This is useful for making use of
--   vector's many functions. The result has to be forced to ensure
--   changes to the bag don't change the result.
withVector :: (PrimMonad m, Unbox a, NFData b)
           => (Vector a -> b) -> Bag m a -> m b
withVector f bag = (force . f) `liftM` unsafeFreezeBag bag
{-# INLINE withVector #-}

-- | Use the frozen underlying vector. This is useful for making use of
--   vector's many functions. The result has to be forced to ensure
--   changes to the bag don't change the result.
withVectorM :: (PrimMonad m, Unbox a, NFData b)
            => (Vector a -> m b) -> Bag m a -> m b
withVectorM f bag = unsafeFreezeBag bag >>= liftM force . f
{-# INLINE withVectorM #-}

-- | /O(n)/ Check if the bag contains an element.
inBag :: (PrimMonad m, Unbox a, Eq a)
     => a -> Bag m a -> m Bool
inBag a = withVector (V.elem a)
{-# INLINE inBag #-}

-- | /O(n)/ Check if the bag does not contain an element (inverse of 'elem').
notInBag :: (PrimMonad m, Unbox a, Eq a)
     => a -> Bag m a -> m Bool
notInBag a = withVector (V.notElem a)
{-# INLINE notInBag #-}

-- | /O(n)/ Yield 'Just' the first element matching the predicate or
--   'Nothing' if no such element exists.
find :: (PrimMonad m, Unbox a, NFData a)
     => (a -> Bool) -> Bag m a -> m (Maybe a)
find f = withVector (V.find f)
{-# INLINE find #-}
-- this could be implimented without the need for @deepseq a@

-- | /worst case O(n)/ Find the first element that matches the predicate
--   and modify it.
findModify :: (PrimMonad m, Unbox a)
           => Bag m a -> (a -> Bool) -> (a -> Modify a) -> m ()
findModify bag@(Bag _ r mv) p f = do
  n  <- readByteArray r 0
  mi <- V.findIndex p `liftM` unsafeFreezeBag bag
  case mi of
    Just i  -> M.unsafeRead mv i >>= \a ->
      case f a of
        Keep     -> return ()
        Remove   -> vRemove mv i n >> resize bag (n - 1)
        Modify b -> M.write mv i b
    Nothing -> return ()
{-# INLINE findModify #-}

-- | /O(n)/ Find the all elements that matche the predicate
--   and modify them.
-- findModifyAll :: (PrimMonad m, Unbox a)
--            => Bag m a -> (a -> Bool) -> (a -> Modify a) -> m ()
-- findModifyAll bag@(Bag _ r mv) p f = do
--   n  <- readByteArray 0 r
--   is <- V.findIndicies p `liftM` unsafeFreezeBag bag
--   forM_ is $ \i -> M.unsafeRead mv i >>= \a ->
--       case f a of
--         Keep     -> return ()
--         Remove   -> vRemove mv i n >> resize bag (n - 1)
--         Modify b -> M.write mv i b

-- random shuffle ------------------------------------------------------

-- | /O(n)/ Perform a random shuffle on the bag, emptying it. Takes upto
--   @m@ random elements.
unsafeRandomShuffle :: (PrimMonad m, Unbox a) => Bag m a -> m (Vector a)
unsafeRandomShuffle = unsafeRandomShuffleN maxBound
{-# INLINE unsafeRandomShuffle #-}

-- | /O(max n m)/ Perform a random shuffle on the bag. Takes upto
--   @m@ random elements. The underlying vector of the bag is also
--   randomised.
unsafeRandomShuffleN :: (PrimMonad m, Unbox a) => Int -> Bag m a -> m (Vector a)
unsafeRandomShuffleN m (Bag g r mv) = do
  n <- readByteArray r 0
  let n' = max n m
  for 0 n' $ \nn -> do
    i <- uniformR (n, nn) g
    M.swap mv i n

  V.unsafeFreeze $ M.take (n' + 1) mv
{-# INLINE unsafeRandomShuffleN #-}

-- | O(n) Replace one element with another. Returns an error if the
--   element isn't present.
replace :: (PrimMonad m, Eq a, Unbox a)
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
insert :: (PrimMonad m, Unbox a) => Bag m a -> a -> m ()
insert (Bag _ r v) a = do
  n <- readByteArray r 0
  when (n > M.length v) $ error "insert: Bag not large enough!"
  -- TODO figure out a nice way to grow the vector

  M.write v n a
  writeByteArray r 0 (n + 1)

-- immutable -----------------------------------------------------------

-- | Freeze a bag to retreive it's internal vector by copying it.
freezeBag :: (PrimMonad m, Unbox a) => Bag m a -> m (V.Vector a)
freezeBag (Bag _ r v) = do
  n <- readByteArray r 0
  let v' = M.take n v
  V.freeze v'
{-# INLINE freezeBag #-}

-- | Freeze a bag to retreive it's internal vector without copying it.
--   Any modifications to the original bag are unsafe.
unsafeFreezeBag :: (PrimMonad m, Unbox a) => Bag m a -> m (V.Vector a)
unsafeFreezeBag (Bag _ r v) = do
  n <- readByteArray r 0
  let v' = M.slice 0 n v
  V.unsafeFreeze v'
{-# INLINE unsafeFreezeBag #-}

-- | Thaw a vector with a `Generator` and initial increase factor for
--   the internal vector. This allows inserting elements.
thawBag :: (PrimMonad m, Generator g m, Unbox a) => g -> Double -> V.Vector a -> m (Bag m a)
thawBag g x v = do
  m  <- V.thaw v
  m' <- M.grow m $ ceiling (fromIntegral (V.length v) * x + 10)
  let n = V.length v
  r  <- newByteArray $ sizeOf n
  writeByteArray r 0 n
  return $! Bag g r m'
{-# INLINE thawBag #-}

-- ------------------------------------------------------------------------
-- -- Internal
-- ------------------------------------------------------------------------

-- <&> :: Functor f => f a -> (a -> b) -> f b
-- <&> = flip fmap

-- bagVector :: Lens (Bag s a) (Bag s b) (M.MVector s a) (M.MVector s b)
-- bagVector f (Bag r v) = f v <&> Bag r

-- bagSize :: PrimMonad m => Bag (PrimState m) a -> m Int
-- bagSize (Bag r _) = readByteArray 0 r

-- | Simple for loop.  Counts from /start/ to /end/-1.
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for n0 !n f = loop n0
  where
    loop i | i == n    = return ()
           | otherwise = f i >> loop (i+1)
{-# INLINE for #-}
