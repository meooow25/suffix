-- |
--
-- == Implementation
--
-- See "Data.Suffix" for implementation details.
module Data.Suffix.ByteString
  (
    -- * Suffix array
    buildSuffixArray
  , Suf.SuffixArray(..)
  , search

    -- * Longest common prefix array
  , buildLCPArray
  , Suf.LCPArray(..)

    -- * LLCP and RLCP arrays
  , Suf.buildLRLCPArray
  , Suf.LRLCPArrays(..)
  , searchLRLCP

    -- * Suffix tree
  , Suf.foldSuffixTree

    -- * Intn
  , Intn

    -- * Examples
    -- $examples
  ) where

import qualified Data.ByteString as B
import Data.Int
import Data.Suffix (Intn, SuffixArray, LCPArray, LRLCPArrays)
import qualified Data.Suffix as Suf

-- | \(O(n)\). Build a suffix array from a @ByteString@.
--
-- On 64-bit systems, a @SuffixArray Int32@ requires half the memory as a
-- @SuffixArray Int@.
buildSuffixArray
  :: Intn i
  => B.ByteString   -- ^ Input @ByteString@ of length \(n\).
  -> SuffixArray i  -- ^ Output type @i@ can be @Int@ or @Int32@. If @i@ is
                    --   @Int32@, \(n\) must be @<= (maxBound :: Int32)@.
buildSuffixArray b =
  Suf.buildSuffixArray 256 (fromIntegral <$> Suf.pullFromByteString b)
{-# SPECIALIZE buildSuffixArray :: B.ByteString -> SuffixArray Int #-}
{-# SPECIALIZE buildSuffixArray :: B.ByteString -> SuffixArray Int32 #-}

-- | \(O(m \log n)\). Search for a pattern in a @ByteString@ using its suffix
-- array.
--
-- Note: For typical inputs, the worst case is unlikely and the running time is
-- close to \(O(m + \log n)\). To get guaranteed \(O(m + \log n)\) running time
-- consider using 'searchLRLCP' instead.
search
  :: Intn i
  => B.ByteString   -- ^ @ByteString@ of length \(n\)
  -> SuffixArray i  -- ^ Suffix array for the above @ByteString@
  -> B.ByteString   -- ^ Pattern @ByteString@ of length \(m\)
  -> (Int, Int)     -- ^ An @(offset, length)@ pair, denoting a slice of the
                    --   suffix array. Beginning at @offset@, @length@ suffixes
                    --   start with the pattern. @length@ is 0 if the pattern
                    --   does not occur in the sequence.
search b sa b2 =
  Suf.search (Suf.pullFromByteString b) sa (Suf.pullFromByteString b2)
{-# SPECIALIZE search :: B.ByteString -> SuffixArray Int -> B.ByteString -> (Int, Int) #-}
{-# SPECIALIZE search :: B.ByteString -> SuffixArray Int32 -> B.ByteString -> (Int, Int) #-}

-- | \(O(n)\). Build a longest common prefix array from a @ByteString@ and its
-- suffix array.
--
-- The LCP array has the same length as the sequence, \(n\). The \(0\)-th
-- element of the LCP array is \(0\). The \(i\)-th element of the LCP array for
-- \(0 < i < n\) is the longest common prefix of the \(i\)-th and \((i-1)\)-th
-- suffix in the suffix array.
buildLCPArray :: Intn i => B.ByteString -> SuffixArray i -> LCPArray i
buildLCPArray b sa = Suf.buildLCPArray (Suf.pullFromByteString b) sa
{-# SPECIALIZE buildLCPArray :: B.ByteString -> SuffixArray Int -> LCPArray Int #-}
{-# SPECIALIZE buildLCPArray :: B.ByteString -> SuffixArray Int32 -> LCPArray Int32 #-}

-- | \(O(m + \log n)\). Search for a pattern in a sequence using its suffix
-- array and LLCP and RLCP arrays.
searchLRLCP
  :: Intn i
  => B.ByteString   -- ^ @ByteString@ of length \(n\)
  -> SuffixArray i  -- ^ Suffix array for the above @ByteString@
  -> LRLCPArrays i  -- ^ LLCP and RLCP arrays for the above @ByteString@
  -> B.ByteString   -- ^ Pattern @ByteString@ of length \(m\)
  -> (Int, Int)     -- ^ An @(offset, length)@ pair, denoting a slice of the
                    --   suffix array. Beginning at @offset@, @length@ suffixes
                    --   start with the pattern. @length@ is 0 if the pattern
                    --   does not occur in the sequence.
searchLRLCP b sa lrlcp b2 =
  Suf.searchLRLCP
    (Suf.pullFromByteString b)
    sa
    lrlcp
    (Suf.pullFromByteString b2)
{-# SPECIALIZE searchLRLCP :: B.ByteString -> SuffixArray Int -> LRLCPArrays Int -> B.ByteString -> (Int, Int) #-}
{-# SPECIALIZE searchLRLCP :: B.ByteString -> SuffixArray Int32 -> LRLCPArrays Int32 -> B.ByteString -> (Int, Int) #-}

-- $examples
--
-- === Build a suffix array and LCP array
--
-- @
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as BC
--
-- import Data.Suffix.ByteString
--
-- banana :: ByteString
-- banana = BC.pack \"BANANA\"
-- @
--
-- >>> let sa = buildSuffixArray banana :: SuffixArray Int
-- >>> sa
-- SuffixArray [5,3,1,0,4,2]
-- >>> let lcpa = buildLCPArray banana sa
-- >>> lcpa
-- LCPArray [0,1,3,0,0,2]
--
-- === Tabulate a suffix array
--
-- @
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as BC
-- import Data.Primitive.PrimArray (indexPrimArray)
-- import Text.Printf (printf)
--
-- import Data.Suffix.ByteString
--
-- tabulatePrint :: ByteString -> IO ()
-- tabulatePrint bs = putStrLn $ unlines $ header : map row [0 .. n-1]
--   where
--     header = "SA  LCP   Suffix"
--     n = BC.length bs
--     sa@('SuffixArray' sa_) = 'buildSuffixArray' bs :: SuffixArray Int
--     'LCPArray' lcpa_ = 'buildLCPArray' bs sa
--     row i = printf "%2d   %2d   %s" sufIdx lcp suffix
--       where
--         suffixId = indexPrimArray sa_ i
--         lcp = indexPrimArray lcpa_ i
--         suffix = BC.unpack (BC.drop suffixId bs)
-- @
--
-- >>> tabulatePrint (BC.pack "mississippi")
-- SA  LCP   Suffix
-- 10    0   i
--  7    1   ippi
--  4    1   issippi
--  1    4   ississippi
--  0    0   mississippi
--  9    0   pi
--  8    1   ppi
--  6    0   sippi
--  3    2   sissippi
--  5    1   ssippi
--  2    3   ssissippi
--
-- === Search
--
-- @
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as BC
-- import Data.Foldable (for_)
-- import Data.Primitive.PrimArray (indexPrimArray)
--
-- import Data.Suffix.ByteString
--
-- searchAndPrint :: ByteString -> 'SuffixArray' Int -> ByteString -> IO ()
-- searchAndPrint bs sa@('SuffixArray' sa_) pat = case 'search' bs sa pat of
--   (_, 0) -> putStrLn "not found"
--   (off, len) -> for_ [off .. off+len-1] $ \\i -> do
--     let suffixId = indexPrimArray sa_ i
--     putStrLn $ BC.unpack bs
--     putStrLn $ replicate suffixId ' ' ++ replicate (BC.length pat) \'^\'
-- @
--
-- >>> let str = BC.pack "shikanokonokonokokoshitantan"
-- >>> let sa = buildSuffixArray str
-- >>> search str sa (BC.pack "nokonoko")
-- (14,2)
-- >>> searchAndPrint str sa (BC.pack "nokonoko")
-- shikanokonokonokokoshitantan
--          ^^^^^^^^
-- shikanokonokonokokoshitantan
--      ^^^^^^^^
-- >>> search str sa (BC.pack "shika senbei")
-- (24,0)
-- >>> searchAndPrint str sa (BC.pack "shika senbei")
-- not found
--
-- === Visualize a suffix tree
--
-- @
-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Writer.Lazy
-- import Control.Monad.Trans.State.Strict
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as BC
-- import Data.Maybe (fromJust)
--
-- import Data.Suffix.ByteString
--
-- -- Output a tree in Graphviz DOT format
-- suffixTreeDot :: ByteString -> String
-- suffixTreeDot bs =
--   execWriter $ flip evalStateT (0 :: Int) $ do
--     writeln "digraph suffixtree {"
--     _ <- 'Suf.foldSuffixTree' l1 b1 b2 b3 sa lcpa
--     writeln "}"
--   where
--     n = BC.length bs
--     sa = 'buildSuffixArray' bs :: 'SuffixArray' Int
--     lcpa = 'buildLCPArray' bs sa
--
--     fresh = state $ \\i -> (i, i+1)
--     writeln = lift . tell . (++ "\\n")
--
--     l1 suffixId = do
--       nodeId <- fresh
--       writeln $ concat [show nodeId, " [label=\\"", show suffixId, "\\", shape=square];"]
--       let depth = n - suffixId
--       pure (depth, suffixId, nodeId)
--     b1 _ = do
--       nodeId <- fresh
--       writeln $ concat [show nodeId, " [label=\\"\\", shape=circle];"]
--       pure (Nothing, nodeId)
--     b2 depth (_, nodeId) (chDepth, chSuffixId, chNodeId) = do
--       let label = BC.unpack $ BC.drop depth $ BC.take chDepth $ BC.drop chSuffixId bs
--           label' = if null label then "ϵ" else ' ':label
--       writeln $ concat [show nodeId, " -> ", show chNodeId, " [label=\\"", label', "\\"];"]
--       pure (Just chSuffixId, nodeId)
--     b3 depth (mSuffixId, nodeId) =
--       pure (depth, fromJust mSuffixId, nodeId)
-- @
--
-- >>> let bs = BC.pack "mississippi"
-- >>> writeFile "suffixtree.dot" (suffixTreeDot bs)
-- >>> :! dot -Tsvg -o suffixtree.svg suffixtree.dot
--
-- Generates the image:
--
-- ![suffix tree](images/suffixtree.svg)
--
