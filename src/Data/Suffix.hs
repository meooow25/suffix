{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

-- |
--
-- == Implementation
--
-- Suffix array construction uses the SAIS algorithm described by
--
-- * Ge Nong, Sen Zhang, and Wai Hong Chan,
-- /\"Linear Suffix Array Construction by Almost Pure Induced-Sorting\"/,
-- 2009 Data Compression Conference,
-- https://doi.org/10.1109/DCC.2009.42
--
-- LCP array construction uses the \(\varPhi\)-algorithm described by
--
-- * Juha Kärkkäinen, Giovanni Manzini, and Simon J. Puglisi
-- /\"Permuted Longest-Common-Prefix Array\"/,
-- Annual Symposium on Combinatorial Pattern Matching, 2009,
-- https://doi.org/10.1007/978-3-642-02441-2_17
--
-- The search algorithms used are described by
--
-- * Udi Manber and Gene Myers,
-- /\"Suffix arrays: a new method for on-line string searches\"/,
-- First annual ACM-SIAM symposium on Discrete algorithms, 1990, pp. 319-327,
-- https://dl.acm.org/doi/10.5555/320176.320218

module Data.Suffix
  (
    -- * Suffix array
    buildSuffixArray
  , SuffixArray(..)
  , search

    -- * Longest common prefix array
  , buildLCPArray
  , LCPArray(..)

    -- * LLCP and RLCP arrays
  , buildLRLCPArray
  , LRLCPArrays(..)
  , searchLRLCP

   -- * Suffix tree
  , foldSuffixTree

    -- * Pull
  , Pull(..)
  , pullFromByteString
  , pullFromPrimArray
  , pullFromArray
  , pullFromArrayLike

    -- * Intn
  , Intn
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), (.|.), unsafeShiftL, unsafeShiftR)
import Data.Foldable (for_)
import Data.Int (Int32)
import qualified Data.Primitive.Array as A
import qualified Data.Primitive.PrimArray as PA
import Data.Primitive.Types (Prim)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS.Unsafe

#include "MachDeps.h"

----------
-- Links
----------

-- Nong's site with papers and code:
-- https://code.google.com/archive/p/ge-nong/downloads
-- The implementation here largely mirrors Nong's C++ implementation.

-- Useful explanation of SAIS, see "Constructing Suffix Arrays":
-- https://web.stanford.edu/class/archive/cs/cs166/cs166.1196/

-- Highly optimized C implementations to perhaps adopt optimizations from:
-- https://sites.google.com/site/yuta256/sais
-- https://github.com/IlyaGrebnov/libsais

----------
-- Types
----------

-- | Suffix array.
newtype SuffixArray i = SuffixArray (PA.PrimArray i)
  deriving (Eq, Ord, Show)

instance NFData (SuffixArray i) where
  rnf !_ = ()

-- | Longest common prefix array.
newtype LCPArray i = LCPArray (PA.PrimArray i)
  deriving (Eq, Ord, Show)

instance NFData (LCPArray i) where
  rnf !_ = ()

-- | LLCP and RLCP arrays.
data LRLCPArrays i = LRLCPArrays
  {-# UNPACK #-} !(PA.PrimArray i)
  {-# UNPACK #-} !(PA.PrimArray i)
  deriving (Eq, Ord, Show)

instance NFData (LRLCPArrays i) where
  rnf !_ = ()

-----------------
-- Suffix array
-----------------

-- | \(O(n + k)\). Build a suffix array from a sequence.
--
-- On 64-bit systems, a @SuffixArray Int32@ requires half the memory as a
-- @SuffixArray Int@.
buildSuffixArray
  :: Intn i
  => Int            -- ^ The alphabet size \(k\).
  -> Pull Int       -- ^ Input sequence of length \(n\). Indexing is assumed to
                    --   be \(O(1)\). Elements must be in @[0..k-1]@.
  -> SuffixArray i  -- ^ Output type @i@ can be 'Int' or 'Int32'. If @i@ is
                    --   @Int32@, \(n\) must be @<= (maxBound :: Int32)@.
buildSuffixArray !k p@(Pull n _)
  | n < 0 || k < 0 = errBuildSANKNegative
  | otherwise = SuffixArray $ PA.runPrimArray $ do
    out <- newPA n
    when (n > 0) $
      sais k p out n
    pure out
{-# INLINE buildSuffixArray #-}

errBuildSANKNegative :: a
errBuildSANKNegative =
  error "Data.Suffix.buildSuffixArray: n and k must be >= 0"

-- Manually specialize saisPrimArray with rules because SPECIALIZE doesn't work
-- https://gitlab.haskell.org/ghc/ghc/-/issues/25117
saisPrimArray
  :: Intn i
  => Int -> Int -> PA.PrimArray i -> PA.MutablePrimArray s i -> Int -> ST s ()
saisPrimArray k n a out outn = sais k (Pull n (toInt . indexPA a)) out outn
{-# NOINLINE saisPrimArray #-}

saisPrimArrayInt
  :: Int
  -> Int
  -> PA.PrimArray Int
  -> PA.MutablePrimArray s Int
  -> Int
  -> ST s ()
saisPrimArrayInt k n a out outn = sais k (Pull n (indexPA a)) out outn

saisPrimArrayInt32
  :: Int
  -> Int
  -> PA.PrimArray Int32
  -> PA.MutablePrimArray s Int32
  -> Int
  -> ST s ()
saisPrimArrayInt32 k n a out outn = sais k (Pull n (toInt . indexPA a)) out outn

{-# RULES
"saisPrimArrayInt"   saisPrimArray = saisPrimArrayInt
"saisPrimArrayInt32" saisPrimArray = saisPrimArrayInt32
#-}

-- Inline sais into buildSuffixArray, saisPrimArrayInt, saisPrimArrayInt32. In
-- buildSuffixArray it should get optimized for whatever input sequence is used.
-- In saisPrimArray it is optimized for PrimArray Int/Int32.
{-# INLINE sais #-}
-- Precondition: n > 0
sais
  :: Intn i
  => Int                      -- alphabet size
  -> Pull Int                 -- input sequence
  -> PA.MutablePrimArray s i  -- buffer, also the output array
  -> Int                      -- length of `out` available
  -> ST s ()
sais !k p@(Pull n at) !out !outn = do

  -- L = False, S = True
  typ <- do
    typm <- newClearedBitMA n
    for_ (Decr (n-2) 0) $ \sufId -> do
      nxt <- readBitMA typm (sufId + 1)
      when (at sufId < at (sufId + 1) || (at sufId == at (sufId + 1) && nxt)) $
        setBitMA typm sufId
    unsafeFrzBitMA typm
  let isLMS i = indexBitA typ i && not (indexBitA typ (i-1))

  T2 buckets outn1 <- sharedOrNewSlice n out outn k
  fillBuckets k p buckets
  T2 bucketIdx _ <- sharedOrNewSlice n out outn1 k

  setPA out 0 n (frInt emptyValue)
  copyMutSlice bucketIdx 0 buckets 0 k
  for_ (Decr (n-1) 1) $ \sufId ->
    when (isLMS sufId) $ do
      modifyMutSliceM bucketIdx (at sufId) $ \outIdx -> do
        let outIdx' = outIdx - 1
        outIdx' <$ writePA out (toInt outIdx') (frInt sufId)

  let doFillL = fillL k p typ buckets bucketIdx out
      doFillS = fillS k p typ buckets bucketIdx out

  doFillL
  doFillS

  numLMS <- foldlM (Incr 0 (n-1)) 0 $ \j i -> do
    sufId <- readPA out i
    if sufId /= 0 && isLMS (toInt sufId)
    then (j+1) <$ writePA out j sufId
    else pure j

  when (numLMS > 0) $ do
    let !ndiv2 = n `unsafeShiftR` 1
        mapr i = ndiv2 + (i `unsafeShiftR` 1)

    setPA out ndiv2 (n-ndiv2) (frInt emptyValue)

    do
      sufId0 <- readPA out 0
      writePA out (mapr (toInt sufId0)) 0
    lastName <- foldlM (Incr 1 (numLMS-1)) 0 $ \prvName i -> do
      prvSufId <- toInt <$> readPA out (i-1)
      sufId <- toInt <$> readPA out i
      let eqLoop !i1 !i2
            | i1 >= n || i2 >= n = False
            | isLMS i1 || isLMS i2 = at i1 == at i2
            | otherwise = at i1 == at i2 && eqLoop (i1+1) (i2+1)
          name = if at prvSufId == at sufId && eqLoop (prvSufId+1) (sufId+1)
                 then prvName
                 else prvName + 1
      writePA out (mapr sufId) (frInt name)
      pure name
    let numNames = lastName + 1

    when (numNames < numLMS) $ do
      lastj <- foldlM (Decr (n-1) ndiv2) n $ \j i -> do
        name <- readPA out i
        if toInt name == emptyValue
        then pure j
        else do
          let j' = j-1
          j' <$ writePA out j' name

      newa <- PA.freezePrimArray out lastj (n-lastj)
      saisPrimArray numNames numLMS newa out outn1

      _ <- foldlM (Decr (n-1) 1) (n-1) $ \j sufId ->
        if isLMS sufId
        then (j-1) <$ writePA out j (frInt sufId)
        else pure j
      for_ (Incr 0 (numLMS-1)) $ \i -> do
        j <- readPA out i
        sufId <- readPA out (n - numLMS + toInt j)
        writePA out i sufId

  copyMutSlice bucketIdx 0 buckets 0 k
  setPA out numLMS (n-numLMS) (frInt emptyValue)
  for_ (Decr (numLMS-1) 0) $ \i -> do
    sufId <- readPA out i
    writePA out i (frInt emptyValue)
    modifyMutSliceM bucketIdx (at (toInt sufId)) $ \outIdx -> do
      let outIdx' = outIdx - 1
      outIdx' <$ writePA out (toInt outIdx') sufId

  doFillL
  doFillS

emptyValue :: Int
emptyValue = -1

sharedOrNewSlice
  :: Intn i
  => Int
  -> PA.MutablePrimArray s i
  -> Int
  -> Int
  -> ST s (T2 (MutSlice s i) Int)
sharedOrNewSlice n out outn want
  | outn - n >= want =
    let !outn' = outn - want
    in pure (T2 (MutSlice outn' out) outn')
  | otherwise = do
    a <- newPA want
    pure (T2 (MutSlice 0 a) outn)
{-# INLINE sharedOrNewSlice #-}

fillBuckets :: Intn i => Int -> Pull Int -> MutSlice s i -> ST s ()
fillBuckets k (Pull n at) buckets = do
  setMutSlice buckets 0 k 0
  for_ (Incr 0 (n-1)) $ \i ->
    modifyMutSlice buckets (at i) (+1)
  _ <- foldlM (Incr 0 (k-1)) 0 $ \acc i -> do
    v <- readMutSlice buckets i
    let acc' = acc + v
    writeMutSlice buckets i acc'
    pure acc'
  pure ()
{-# INLINE fillBuckets #-}

fillL
  :: Intn i
  => Int
  -> Pull Int
  -> BitA
  -> MutSlice s i
  -> MutSlice s i
  -> PA.MutablePrimArray s i
  -> ST s ()
fillL k (Pull n at) typ buckets bucketIdx out = do
  writeMutSlice bucketIdx 0 0
  copyMutSlice bucketIdx 1 buckets 0 (k-1)
  modifyMutSliceM bucketIdx (at (n-1)) $ \outIdx ->
    (outIdx+1) <$ writePA out (toInt outIdx) (frInt n - 1)
  for_ (Incr 0 (n-1)) $ \i -> do
    sufId <- toInt <$> readPA out i
    when (sufId /= emptyValue) $ do
      let sufIdL = sufId - 1
      when (sufIdL >= 0 && not (indexBitA typ sufIdL)) $ do
        modifyMutSliceM bucketIdx (at sufIdL) $ \outIdx -> do
          (outIdx+1) <$ writePA out (toInt outIdx) (frInt sufIdL)
{-# INLINE fillL #-}

fillS
  :: Intn i
  => Int
  -> Pull Int
  -> BitA
  -> MutSlice s i
  -> MutSlice s i
  -> PA.MutablePrimArray s i
  -> ST s ()
fillS k (Pull n at) typ buckets bucketIdx out = do
  copyMutSlice bucketIdx 0 buckets 0 k
  for_ (Decr (n-1) 0) $ \i -> do
    sufId <- toInt <$> readPA out i
    when (sufId /= emptyValue) $ do
      let sufIdL = sufId - 1
      when (sufIdL >= 0 && indexBitA typ sufIdL) $ do
        modifyMutSliceM bucketIdx (at sufIdL) $ \outIdx -> do
          let outIdx' = outIdx - 1
          outIdx' <$ writePA out (toInt outIdx') (frInt sufIdL)
{-# INLINE fillS #-}

-----------
-- Search
-----------

-- | \(O(m \log n)\). Search for a pattern in a sequence using its suffix array.
--
-- Note: For typical inputs, the worst case is unlikely and the running time is
-- close to \(O(m + \log n)\). To get guaranteed \(O(m + \log n)\) running time
-- consider using 'searchLRLCP' instead.
search
  :: (Ord a, Intn i)
  => Pull a           -- ^ Sequence of length \(n\). Indexing is assumed to
                      --   be \(O(1)\). @compare@ for @a@ is assumed to be
                      --   \(O(1)\).
  -> SuffixArray i    -- ^ Suffix array for the above sequence
  -> Pull a           -- ^ Pattern of length \(m\)
  -> (Int, Int)       -- ^ An @(offset, length)@ pair, denoting a slice of
                      --   the suffix array. Beginning at @offset@, @length@
                      --   suffixes start with the pattern. @length@ is 0 if the
                      --   pattern does not occur in the sequence.
search s@(Pull n _) (SuffixArray sa) !t
  | n /= PA.sizeofPrimArray sa = errSearchSizeMismatch
  | otherwise = let T3 l _ _ = binarySearch n (T2 0 0) nxtl
                    T3 _ r _ = binarySearch n (T2 0 0) nxtr
                    !off = l+1
                    !len = r-off
                in (off, len)
  where
    doCmpSuffix suf i = cmpSuffix s suf t i
    {-# NOINLINE doCmpSuffix #-}
    -- ^ Not inlining this apparently helps greatly, benchmark time reduces
    -- by 45-50%.

    nxtl !m (T2 llcp rlcp) = case doCmpSuffix suf (min llcp rlcp) of
      T2 LT lcp' -> R (T2 lcp' rlcp)
      T2 _ lcp' -> L (T2 llcp lcp')
      where
        suf = toInt (indexPA sa m)
    nxtr !m (T2 llcp rlcp) = case doCmpSuffix suf (min llcp rlcp) of
      T2 GT lcp' -> L (T2 llcp lcp')
      T2 _ lcp' -> R (T2 lcp' rlcp)
      where
        suf = toInt (indexPA sa m)
{-# INLINE search #-}

errSearchSizeMismatch :: a
errSearchSizeMismatch = error "Data.Suffix.search: size mismatch"

cmpSuffix :: Ord a => Pull a -> Int -> Pull a -> Int -> T2 Ordering Int
cmpSuffix (Pull n at) !suf (Pull n2 at2) = loop
  where
    loop i | i >= n2 = T2 EQ i
           | suf+i >= n = T2 LT i
           | otherwise = case compare (at (suf+i)) (at2 i) of
             EQ -> loop (i+1)
             o -> T2 o i
{-# INLINE cmpSuffix #-}

getMid :: Int -> Int -> Int
getMid l h = w2i (i2w (l + h) `unsafeShiftR` 1)
  where
    -- To Word to avoid overflow
    i2w :: Int -> Word
    i2w = fromIntegral
    w2i :: Word -> Int
    w2i = fromIntegral

binarySearch :: Int -> s -> (Int -> s -> E s s) -> T3 Int Int s
binarySearch n s0 nxt = go (-1) n s0
  where
    go !l !r !s
      | l + 1 >= r = T3 l r s
      | otherwise =
        let !m = getMid l r
        in case nxt m s of
          L s' -> go l m s'
          R s' -> go m r s'
{-# INLINE binarySearch #-}

--------------
-- LCP Array
--------------

-- | \(O(n)\). Build a longest common prefix array from a sequence and its
-- suffix array.
--
-- The LCP array has the same length as the sequence, \(n\). The \(0\)-th
-- element of the LCP array is \(0\). The \(i\)-th element of the LCP array for
-- \(0 < i < n\) is the longest common prefix of the \(i\)-th and \((i-1)\)-th
-- suffix in the suffix array.
buildLCPArray
  :: (Eq a, Intn i)
  => Pull a         -- ^ Sequence of length @n@. Indexing is assumed to be
                    --   \(O(1)\). @compare@ for @a@ is assumed to be \(O(1)\).
  -> SuffixArray i  -- ^ Suffix array for the above sequence
  -> LCPArray i
buildLCPArray (Pull n at) (SuffixArray sa)
  | n /= PA.sizeofPrimArray sa = errBuildLCPASizeMismatch
  | otherwise = LCPArray $ PA.runPrimArray $ do
    phi <- newPA n
    when (n > 0) $
      writePA phi (toInt (indexPA sa 0)) (-1)
    for_ (Incr 1 (n-1)) $ \i ->
      writePA phi (toInt (indexPA sa i)) (indexPA sa (i-1))
    mplcpa <- newPA n
    _ <- foldlM (Incr 0 (n-1)) 0 $ \l i -> do
      j <- toInt <$> readPA phi i
      let diff d = i+d >= n || j+d >= n || at (i+d) /= at (j+d)
          l' = if j == -1
               then 0
               else until diff (+1) l
      writePA mplcpa i (frInt l')
      pure $ max 0 (l' - 1)
    plcpa <- PA.unsafeFreezePrimArray mplcpa
    lcpa <- newPA n
    for_ (Incr 0 (n-1)) $ \i ->
      writePA lcpa i (indexPA plcpa (toInt (indexPA sa i)))
    pure lcpa
{-# INLINE buildLCPArray #-}

errBuildLCPASizeMismatch :: a
errBuildLCPASizeMismatch = error "Data.Suffix.buildLCPArray: size mismatch"

--------------
-- LLCP RLCP
--------------

-- | \(O(n)\). Build LLCP and RLCP arrays from an LCP array, for use in
-- @searchLRLCP@.
buildLRLCPArray :: Intn i => LCPArray i -> LRLCPArrays i
buildLRLCPArray (LCPArray lcpa) = runST $ do
  llcpa <- newPA n
  rlcpa <- newPA n
  let go l r
        | l + 1 >= r = if r >= n
                       then pure 0
                       else pure $! toInt (indexPA lcpa r)
        | otherwise = do
          let m = getMid l r
          llcp <- go l m
          writePA llcpa m (frInt llcp)
          rlcp <- go m r
          writePA rlcpa m (frInt rlcp)
          pure $! min llcp rlcp
  _ <- go (-1) n
  LRLCPArrays
    <$> PA.unsafeFreezePrimArray llcpa
    <*> PA.unsafeFreezePrimArray rlcpa
  where
    n = PA.sizeofPrimArray lcpa
{-# SPECIALIZE buildLRLCPArray :: LCPArray Int -> LRLCPArrays Int #-}
{-# SPECIALIZE buildLRLCPArray :: LCPArray Int32 -> LRLCPArrays Int32 #-}

-- | \(O(m + \log n)\). Search for a pattern in a sequence using its suffix
-- array and LLCP and RLCP arrays.
searchLRLCP
  :: (Ord a, Intn i)
  => Pull a           -- ^ Sequence of length \(n\). Indexing is assumed to
                      --   be \(O(1)\). @compare@ for @a@ is assumed to be
                      --   \(O(1)\).
  -> SuffixArray i    -- ^ Suffix array for the above sequence
  -> LRLCPArrays i    -- ^ LLCP and RLCP arrays for the above sequence
  -> Pull a           -- ^ Pattern sequence of length \(m\)
  -> (Int, Int)       -- ^ An @(offset, length)@ pair, denoting a slice of
                      --   the suffix array. Beginning at @offset@, @length@
                      --   suffixes start with the pattern. @length@ is 0 if the
                      --   pattern does not occur in the sequence.
searchLRLCP s@(Pull n _) (SuffixArray sa) (LRLCPArrays llcpa rlcpa) !t
  | n /= PA.sizeofPrimArray sa ||
    n /= PA.sizeofPrimArray llcpa ||
    n /= PA.sizeofPrimArray rlcpa
  = errSearchLRLCPSizeMismatch
  | otherwise = let T3 l _ _ = binarySearch n (T2 0 0) nxtl
                    T3 _ r _ = binarySearch n (T2 0 0) nxtr
                    !off = l+1
                    !len = r-off
                in (off, len)
  where
    doCmpSuffix suf i = cmpSuffix s suf t i
    {-# NOINLINE doCmpSuffix #-}
    -- ^ Not inlining this apparently helps greatly, benchmark time reduces
    -- by 45-50%.

    nxtl !m (T2 llcp rlcp)
      | llcp >= rlcp =
        let mllcp = toInt (indexPA llcpa m)
        in if llcp > mllcp
           then L (T2 llcp mllcp)
           else case doCmpSuffix suf llcp of
             T2 LT lcp' -> R (T2 lcp' rlcp)
             T2 _ lcp' -> L (T2 llcp lcp')
      | otherwise =
        let mrlcp = toInt (indexPA rlcpa m)
        in if rlcp > mrlcp
           then R (T2 mrlcp rlcp)
           else case doCmpSuffix suf rlcp of
             T2 LT lcp' -> R (T2 lcp' rlcp)
             T2 _ lcp' -> L (T2 llcp lcp')
      where
        suf = toInt (indexPA sa m)

    nxtr !m (T2 llcp rlcp)
      | llcp >= rlcp =
        let mllcp = toInt (indexPA llcpa m)
        in if llcp > mllcp
           then L (T2 llcp mllcp)
           else case doCmpSuffix suf llcp of
             T2 GT lcp' -> L (T2 llcp lcp')
             T2 _ lcp' -> R (T2 lcp' rlcp)
      | otherwise =
        let mrlcp = toInt (indexPA rlcpa m)
        in if rlcp > mrlcp
           then R (T2 mrlcp rlcp)
           else case doCmpSuffix suf rlcp of
             T2 GT lcp' -> L (T2 llcp lcp')
             T2 _ lcp' -> R (T2 lcp' rlcp)
      where
        suf = toInt (indexPA sa m)
{-# INLINE searchLRLCP #-}

errSearchLRLCPSizeMismatch :: a
errSearchLRLCPSizeMismatch = error "Data.Suffix.searchLRLCP: size mismatch"

----------------
-- Suffix tree
----------------

-- | \(O(n)\). Bottom-up strict monadic fold of the suffix tree formed by the
-- given suffix array and LCP array.
--
-- The tree folded over can be considered to be equivalent to the following
-- definition.
--
-- @
-- data SuffixTree
--   = Leaf
--       Int          -- ^ The suffix index
--   | Branch
--       Int          -- ^ The string depth of this node
--       [SuffixTree] -- ^ The children of this node
-- @
--
-- This tree has exactly \(n+1\) leaves and at most \(n\) internal nodes.
-- The suffix with index \(n\), i.e. the empty suffix, is a leaf in this tree,
-- though it is not present in the suffix array.
--
-- The calls to the leaf function and the internal node finalizer taken together
-- form a post-order traversal of the tree. For an internal node, the values
-- from its children are combined left-to-right.
--
-- The \(O(n)\) bound assumes that monadic bind and all given functions are
-- \(O(1)\).
foldSuffixTree
  :: (Intn i, Monad m)
  => (Int -> m a)           -- ^ Leaf. The @Int@ is the suffix index.
  -> (Int -> m b)           -- ^ Internal node, initialize. The @Int@ is the
                            --   node's string depth.
  -> (Int -> b -> a -> m b) -- ^ Internal node, combine with the value from a
                            --   child. The @Int@ is the node's string depth.
  -> (Int -> b -> m a)      -- ^ Internal node, finalize. The @Int@ is the
                            --   node's string depth.
  -> SuffixArray i          -- ^ A suffix array derived from a sequence
  -> LCPArray i             -- ^ The LCP array for the same sequence and suffix
                            --   array
  -> m a
foldSuffixTree
  leaf branchInit branchCombine branchFinish (SuffixArray sa) (LCPArray lcpa)
  | n /= PA.sizeofPrimArray lcpa = errFoldSTSizeMismatch
  | n == 0 = leaf n
  | otherwise = do
    !aLeft <- leaf n
    !b <- branchInit 0
    !b' <- branchCombine 0 b aLeft
    down 0 (T3 0 b' Nil)
  where
    n = PA.sizeofPrimArray sa
    down !i (T3 d b stk1) = case compare lcpCur lcpNxt of
      LT | addLeft -> do
           !aLeft <- leaf (toInt (indexPA sa (i-1)))
           !b' <- branchCombine d b aLeft
           !b1 <- branchInit lcpNxt
           down (i+1) (T3 lcpNxt b1 (Push d b' stk1))
         | otherwise -> do
           !b1 <- branchInit lcpNxt
           down (i+1) (T3 lcpNxt b1 (Push d b stk1))
      EQ | addLeft -> do
           !aLeft <- leaf (toInt (indexPA sa (i-1)))
           !b' <- branchCombine d b aLeft
           down (i+1) (T3 d b' stk1)
         | otherwise -> down (i+1) (T3 d b stk1)
      GT -> do
        !b' <-
              if addLeft
              then do
                !aLeft <- leaf (toInt (indexPA sa (i-1)))
                branchCombine d b aLeft
              else pure b
        !aRight <- leaf (toInt (indexPA sa i))
        !b'' <- branchCombine d b' aRight
        !a <- branchFinish d b''
        up (i+1) lcpNxt a stk1
      where
        lcpPrv = if i > 0 then toInt (indexPA lcpa (i-1)) else n
        lcpCur = toInt (indexPA lcpa i)
        lcpNxt = if i < n-1 then toInt (indexPA lcpa (i+1)) else (-1)
        addLeft = lcpPrv <= lcpCur

    up !i !dep = go
      where
        go !a stk@(Push d b stk1) = do
          case compare d dep of
            LT -> do
              !b1 <- branchInit dep
              !b1' <- branchCombine dep b1 a
              down i (T3 dep b1' stk)
            EQ -> do
              !b' <- branchCombine d b a
              down i (T3 d b' stk1)
            GT -> do
              !b' <- branchCombine d b a
              !a' <- branchFinish d b'
              go a' stk1
        go !a Nil = pure a
{-# INLINE foldSuffixTree #-}

errFoldSTSizeMismatch :: a
errFoldSTSizeMismatch = error "Data.Suffix.foldSuffixTree: size mismatch"

data Stack a
  = Push {-# UNPACK #-} !Int !a !(Stack a)
  | Nil

---------
-- Pull
---------

-- | A pull array. Serves as a simple interface for array-like structures.
--
-- Note: For good performance, create @Pull@s right before supplying them to
-- functions in this module.
data Pull a = Pull
  !Int       -- ^ Length \(n\). \(n\) must be \(\geq 0\).
  (Int -> a) -- ^ Indexing function. Must be valid for inputs in @[0 .. n-1]@

instance Functor Pull where
  fmap f (Pull n at) = Pull n (f . at)
  x <$ Pull n _ = Pull n (const x)

-- | Pull from a @ByteString@ (from the bytestring package).
pullFromByteString :: BS.ByteString -> Pull Word8
pullFromByteString =
  pullFromArrayLike
    BS.length
#ifdef CHECKS
    BS.index
#else
    BS.Unsafe.unsafeIndex
#endif
{-# INLINE pullFromByteString #-}

-- | Pull from a @PrimArray@ (from the primitive package).
pullFromPrimArray :: Prim a => PA.PrimArray a -> Pull a
pullFromPrimArray = pullFromArrayLike PA.sizeofPrimArray indexPA
{-# INLINE pullFromPrimArray #-}

-- | Pull from an @Array@ (from the primitive package).
pullFromArray :: A.Array a -> Pull a
pullFromArray =
  pullFromArrayLike
    A.sizeofArray
#ifdef CHECKS
    (\a i -> check "pullFromArray"
                   (0 <= i && i < A.sizeofArray a)
                   (A.indexArray a i))
#else
    A.indexArray
#endif
{-# INLINE pullFromArray #-}

-- | Pull elements from any array-like structure.
pullFromArrayLike
  :: (arr -> Int)      -- ^ Size function
  -> (arr -> Int -> a) -- ^ Indexing function
  -> arr               -- ^ The structure
  -> Pull a
pullFromArrayLike size index !a = Pull (size a) (index a)
{-# INLINE pullFromArrayLike #-}

----------
-- Intn
----------

-- | Allows for a choice between @Int@ and @Int32@.
class (Prim i, Integral i) => Intn i where
  toInt :: i -> Int
  frInt :: Int -> i

instance Intn Int where
  toInt = id
  frInt = id

instance Intn Int32 where
  toInt = fromIntegral
  frInt = fromIntegral

--------------
-- Bit array
--------------

newtype BitMA s = BitMA (PA.MutablePrimArray s Int)
newtype BitA = BitA (PA.PrimArray Int)

wshift, wmask :: Int
#if SIZEOF_HSWORD == 4
wshift = 5
wmask = 31
#elif SIZEOF_HSWORD == 8
wshift = 6
wmask = 63
#else
#error "unsupported word size"
#endif

newClearedBitMA :: Int -> ST s (BitMA s)
newClearedBitMA n = do
  let wn = (n + wmask) `unsafeShiftR` wshift
  a <- newPA wn
  setPA a 0 wn 0
  pure (BitMA a)
{-# INLINE newClearedBitMA #-}

setBitMA :: BitMA s -> Int -> ST s ()
setBitMA (BitMA a) i = do
  let j = i `unsafeShiftR` wshift
  x <- readPA a j
  writePA a j (x .|. (1 `unsafeShiftL` (i .&. wmask)))
{-# INLINE setBitMA #-}

readBitMA :: BitMA s -> Int -> ST s Bool
readBitMA (BitMA a) i = do
  let j = i `unsafeShiftR` wshift
  x <- readPA a j
  pure $ x .&. (1 `unsafeShiftL` (i .&. wmask)) /= 0
{-# INLINE readBitMA #-}

unsafeFrzBitMA :: BitMA s -> ST s BitA
unsafeFrzBitMA (BitMA a) = BitA <$> PA.unsafeFreezePrimArray a
{-# INLINE unsafeFrzBitMA #-}

indexBitA :: BitA -> Int -> Bool
indexBitA (BitA a) i =
  let j = i `unsafeShiftR` wshift
  in indexPA a j .&. (1 `unsafeShiftL` (i .&. wmask)) /= 0
{-# INLINE indexBitA #-}

-------------
-- MutSlice
-------------

data MutSlice s a = MutSlice
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !(PA.MutablePrimArray s a)

readMutSlice :: Prim a => MutSlice s a -> Int -> ST s a
readMutSlice (MutSlice off a) i = readPA a (off+i)
{-# INLINE readMutSlice #-}

writeMutSlice :: Prim a => MutSlice s a -> Int -> a -> ST s ()
writeMutSlice (MutSlice off a) i = writePA a (off+i)
{-# INLINE writeMutSlice #-}

modifyMutSlice :: Prim a => MutSlice s a -> Int -> (a -> a) -> ST s ()
modifyMutSlice (MutSlice off a) i = modifyPA a (off+i)
{-# INLINE modifyMutSlice #-}

setMutSlice :: Prim a => MutSlice s a -> Int -> Int -> a -> ST s ()
setMutSlice (MutSlice off a) i = setPA a (off+i)
{-# INLINE setMutSlice #-}

copyMutSlice
  :: Prim a => MutSlice s a -> Int -> MutSlice s a -> Int -> Int -> ST s ()
copyMutSlice (MutSlice doff dst) di (MutSlice soff src) si =
  copyMutPA dst (doff+di) src (soff+si)
{-# INLINE copyMutSlice #-}

modifyMutSliceM :: Prim a => MutSlice s a -> Int -> (a -> ST s a) -> ST s ()
modifyMutSliceM (MutSlice off a) i = modifyPAM a (off+i)
{-# INLINE modifyMutSliceM #-}

----------------------
-- Primitive helpers
----------------------

indexPA :: Prim a => PA.PrimArray a -> Int -> a
indexPA a i =
#ifdef CHECKS
  check "indexPA" (0 <= i && i < PA.sizeofPrimArray a) $
#endif
  PA.indexPrimArray a i
{-# INLINE indexPA #-}

newPA :: Prim a => Int -> ST s (PA.MutablePrimArray s a)
newPA n =
#ifdef CHECKS
  check "newPA" (n >= 0) $
#endif
  PA.newPrimArray n
{-# INLINE newPA #-}

readPA :: Prim a => PA.MutablePrimArray s a -> Int -> ST s a
readPA a i = do
#ifdef CHECKS
  sz <- PA.getSizeofMutablePrimArray a
  check "readPA" (0 <= i && i < sz) $
#endif
    PA.readPrimArray a i
{-# INLINE readPA #-}

writePA :: Prim a => PA.MutablePrimArray s a -> Int -> a -> ST s ()
writePA a i x = do
#ifdef CHECKS
  sz <- PA.getSizeofMutablePrimArray a
  check "writePA" (0 <= i && i < sz) $
#endif
    PA.writePrimArray a i x
{-# INLINE writePA #-}

setPA :: Prim a => PA.MutablePrimArray s a -> Int -> Int -> a -> ST s ()
setPA a i n x = do
#ifdef CHECKS
  sz <- PA.getSizeofMutablePrimArray a
  check "setPA" (0 <= i && 0 <= n && i + n <= sz) $
#endif
    PA.setPrimArray a i n x
{-# INLINE setPA #-}

copyMutPA
  :: Prim a
  => PA.MutablePrimArray s a -> Int
  -> PA.MutablePrimArray s a -> Int
  -> Int
  -> ST s ()
copyMutPA dst dstoff src srcoff n = do
#ifdef CHECKS
  dstSz <- PA.getSizeofMutablePrimArray dst
  srcSz <- PA.getSizeofMutablePrimArray src
  check "copyMutPA"
        ( 0 <= dstoff &&
          dstoff + n <= dstSz &&
          0 <= srcoff &&
          srcoff + n <= srcSz ) $
#endif
    PA.copyMutablePrimArray dst dstoff src srcoff n
{-# INLINE copyMutPA #-}

modifyPA
  :: Prim a => PA.MutablePrimArray s a -> Int -> (a -> a) -> ST s ()
modifyPA a i f = readPA a i >>= writePA a i . f
{-# INLINE modifyPA #-}

modifyPAM
  :: Prim a => PA.MutablePrimArray s a -> Int -> (a -> ST s a) -> ST s ()
modifyPAM a i f = readPA a i >>= f >>= writePA a i
{-# INLINE modifyPAM #-}

#ifdef CHECKS
check :: String -> Bool -> a -> a
check msg b x = if not b then error ("Data.Suffix." ++ msg) else x
#endif

----------
-- Utils
----------

data T2 a b = T2 !a !b
data T3 a b c = T3 !a !b !c
data E a b = L !a | R !b

foldlM :: (Foldable f, Monad m) => f a -> b -> (b -> a -> m b) -> m b
foldlM xs z0 f = foldr c z xs z0
  where
    z !y = pure y
    c x k !y = f y x >>= k
    {-# INLINE c #-}
{-# INLINE foldlM #-}

data Incr a where
  Incr :: !Int -> !Int -> Incr Int

instance Foldable Incr where
  foldr f z (Incr i0 j) = go i0
    where
      go i | i > j = z
           | otherwise = f i (go (i+1))
  {-# INLINE foldr #-}

data Decr a where
  Decr :: !Int -> !Int -> Decr Int

instance Foldable Decr where
  foldr f z (Decr i0 j) = go i0
    where
      go i | i < j = z
           | otherwise = f i (go (i-1))
  {-# INLINE foldr #-}
