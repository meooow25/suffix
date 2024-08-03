{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.ByteString.Char8 as BC
import Data.Functor.Identity (Identity(..))
import Data.Int (Int32)
import qualified Data.List as L
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Primitive.Array as A
import qualified Data.Primitive.PrimArray as PA
import Data.Primitive.Types (Prim)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Suffix
  ( Intn
  , LCPArray(..)
  , LRLCPArrays(..)
  , Pull(..)
  , SuffixArray(..)
  , buildLCPArray
  , buildLRLCPArray
  , buildSuffixArray
  , foldSuffixTree
  , pullFromArray
  , pullFromByteString
  , search
  , searchLRLCP
  )

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testGroup "buildSuffixArray"
    [ testCase "banana" $
        buildSuffixArray @Int 3 (pullBC bananaRank banana) @?= bananaSA
    , testCase "dna" $
        buildSuffixArray @Int 4 (pullBC dnaRank dna) @?= dnaSA
    , testCase "shika" $
        buildSuffixArray @Int 256 (fromIntegral <$> pullBS shika) @?=
        shikaSA
    , testGroup "random"
      [ testArbBuildSuffixArray @Bool (Proxy @Int) "Bool Int" 2 fromEnum
      , testArbBuildSuffixArray @Bool (Proxy @Int32) "Bool Int32" 2 fromEnum
      , testArbBuildSuffixArray @Biased (Proxy @Int) "Biased Int" 2 fromEnum
      , testArbBuildSuffixArray @Biased (Proxy @Int32) "Biased Int32" 2 fromEnum
      , testArbBuildSuffixArray @Word8 (Proxy @Int) "Word8 Int" 256 fromEnum
      , testArbBuildSuffixArray @Word8 (Proxy @Int32) "Word8 Int32" 256 fromEnum
      ]
    ]
  , testGroup "buildLCPArray"
    [ testCase "banana" $
        buildLCPArray (pullBC bananaRank banana) bananaSA @?= bananaLCP
    , testCase "dna" $
        buildLCPArray (pullBC dnaRank dna) dnaSA @?= dnaLCP
    , testCase "shika" $
        buildLCPArray (pullBS shika) shikaSA @?= shikaLCP
    , testGroup "random"
      [ testArbBuildLCPArray @Bool (Proxy @Int) "Bool Int" 2 fromEnum
      , testArbBuildLCPArray @Bool (Proxy @Int32) "Bool Int32" 2 fromEnum
      , testArbBuildLCPArray @Biased (Proxy @Int) "Biased Int" 2 fromEnum
      , testArbBuildLCPArray @Biased (Proxy @Int32) "Biased Int32" 2 fromEnum
      , testArbBuildLCPArray @Word8 (Proxy @Int) "Word8 Int" 256 fromEnum
      , testArbBuildLCPArray @Word8 (Proxy @Int32) "Word8 Int32" 256 fromEnum
      ]
    ]
  , testGroup "search"
    [ testGroup "shika"
      [ testCase name $
          search (pullBS shika) shikaSA (pullBS pat) @?= result
      | (name, pat, result) <- shikaSearchTests
      ]
    , testGroup "random"
      [ testArbSearch @Bool (Proxy @Int) "Bool Int" 2 fromEnum
      , testArbSearch @Bool (Proxy @Int32) "Bool Int32" 2 fromEnum
      , testArbSearch @Biased (Proxy @Int) "Biased Int" 2 fromEnum
      , testArbSearch @Biased (Proxy @Int32) "Biased Int32" 2 fromEnum
      , testArbSearch @Word8 (Proxy @Int) "Word8 Int" 256 fromEnum
      , testArbSearch @Word8 (Proxy @Int32) "Word8 Int32" 256 fromEnum
      ]
    ]
  , testGroup "searchLRLCP"
    [ testGroup "shika"
      [ testCase name $
          searchLRLCP (pullBS shika) shikaSA shikaLRLCP (pullBS pat) @?= result
      | (name, pat, result) <- shikaSearchTests
      ]
    , testGroup "random"
      [ testArbSearchLRLCP @Bool (Proxy @Int) "Bool Int" 2 fromEnum
      , testArbSearchLRLCP @Bool (Proxy @Int32) "Bool Int32" 2 fromEnum
      , testArbSearchLRLCP @Biased (Proxy @Int) "Biased Int" 2 fromEnum
      , testArbSearchLRLCP @Biased (Proxy @Int32) "Biased Int32" 2 fromEnum
      , testArbSearchLRLCP @Word8 (Proxy @Int) "Word8 Int" 256 fromEnum
      , testArbSearchLRLCP @Word8 (Proxy @Int32) "Word8 Int32" 256 fromEnum
      ]
    ]
  , testGroup "foldSuffixTree"
    [ testCase "banana" $
        mkSuffixTree bananaSA bananaLCP @?= bananaST
    , testCase "dna" $
        mkSuffixTree dnaSA dnaLCP @?= dnaST
    , testGroup "random"
      [ testArbBuildSuffixTree @Bool (Proxy @Int) "Bool Int" 2 fromEnum
      , testArbBuildSuffixTree @Bool (Proxy @Int32) "Bool Int32" 2 fromEnum
      , testArbBuildSuffixTree @Biased (Proxy @Int) "Biased Int" 2 fromEnum
      , testArbBuildSuffixTree @Biased (Proxy @Int32) "Biased Int32" 2 fromEnum
      , testArbBuildSuffixTree @Word8 (Proxy @Int) "Word8 Int" 256 fromEnum
      , testArbBuildSuffixTree @Word8 (Proxy @Int32) "Word8 Int32" 256 fromEnum
      ]
    ]
  ]

-------------------
-- Property tests
-------------------

testArbBuildSuffixArray
  :: (Arbitrary a, Ord a, Show a, Intn i, Show i)
  => Proxy i -> String -> Int -> (a -> Int) -> TestTree
testArbBuildSuffixArray (_ :: Proxy i) name k toInt =
  testProperty name $ \(xs :: [a]) ->
    let a = A.arrayFromList xs
    in buildSuffixArray @i k (toInt <$> pullFromArray a) ===
       fst (naive xs)

testArbBuildLCPArray
  :: (Arbitrary a, Ord a, Show a, Intn i, Show i)
  => Proxy i -> String -> Int -> (a -> Int) -> TestTree
testArbBuildLCPArray (_ :: Proxy i) name k toInt =
  testProperty name $ \(xs :: [a]) ->
    let a = A.arrayFromList xs
        sa = buildSuffixArray @i k (toInt <$> pullFromArray a)
    in buildLCPArray (pullFromArray a) sa === snd (naive xs)

testArbSearch
  :: (Arbitrary a, Ord a, Show a, Intn i)
  => Proxy i -> String -> Int -> (a -> Int) -> TestTree
testArbSearch (_ :: Proxy i) name k toInt =
  testProperty name $ \(xs :: [a]) (ys :: [a]) ->
    let a = A.arrayFromList xs
        sa = buildSuffixArray @i k (toInt <$> pullFromArray a)

        b = A.arrayFromList ys
        ex = naiveSearch xs ys
        lbl = case snd ex of
          0 -> "no match"
          _ -> case A.sizeofArray b of
            n2 | n2 == 0 -> "match; 0"
               | n2 <= 4 -> "match; [1..4]"
               | n2 <= 16 -> "match; [5..16]"
               | otherwise -> "match; [17..]"

    in label lbl $
         search (pullFromArray a) sa (pullFromArray b) === ex

testArbSearchLRLCP
  :: (Arbitrary a, Ord a, Show a, Intn i)
  => Proxy i -> String -> Int -> (a -> Int) -> TestTree
testArbSearchLRLCP (_ :: Proxy i) name k toInt =
  testProperty name $ \(xs :: [a]) (ys :: [a]) ->
    let a = A.arrayFromList xs
        sa = buildSuffixArray @i k (toInt <$> pullFromArray a)
        lcpa = buildLCPArray (pullFromArray a) sa
        lrlcpa = buildLRLCPArray lcpa

        b = A.arrayFromList ys
        ex = naiveSearch xs ys
        lbl = case snd ex of
          0 -> "no match"
          _ -> case A.sizeofArray b of
            n2 | n2 == 0 -> "match; 0"
               | n2 <= 4 -> "match; [1..4]"
               | n2 <= 16 -> "match; [5..16]"
               | otherwise -> "match; [17..]"

    in label lbl $
         searchLRLCP (pullFromArray a) sa lrlcpa (pullFromArray b) === ex

testArbBuildSuffixTree
  :: (Arbitrary a, Ord a, Show a, Intn i)
  => Proxy i -> String -> Int -> (a -> Int) -> TestTree
testArbBuildSuffixTree (_ :: Proxy i) name k toInt =
  testProperty name $ \(xs :: [a]) ->
    let a = A.arrayFromList xs
        sa = buildSuffixArray @i k (toInt <$> pullFromArray a)
        lcpa = buildLCPArray (pullFromArray a) sa
    in mkSuffixTree sa lcpa === naiveSuffixTree xs

----------------
-- Naive impls
----------------

mkSuffixTree :: Intn i => SuffixArray i -> LCPArray i -> SuffixTree
mkSuffixTree sa lcpa = runIdentity $
  foldSuffixTree leaf branchInit branchCombine branchFinish sa lcpa
  where
    leaf = pure . Leaf
    branchInit _ = pure []
    branchCombine _ ys y = pure (y:ys)
    branchFinish d ys = pure (Branch d (reverse ys))

naive :: (Ord a, Prim i, Integral i) => [a] -> (SuffixArray i, LCPArray i)
naive s = (SuffixArray sa, LCPArray lcpa)
  where
    sorted = L.sortBy (comparing snd) $ zip [0..] $ init $ L.tails s
    lcp xs ys = fromIntegral $ length $ takeWhile id $ zipWith (==) xs ys
    sa = PA.primArrayFromList (map fst sorted)
    lcpa =
      if PA.sizeofPrimArray sa == 0
      then PA.emptyPrimArray
      else PA.primArrayFromList (0 : (zipWith lcp <*> tail) (map snd sorted))

naiveSearch :: Ord a => [a] -> [a] -> (Int, Int)
naiveSearch xs ys = (off, len)
  where
    n = length xs
    m = length ys
    (SuffixArray sa',_) = naive xs
    off = first ge
    len = first gt - off
    ge = (/= LT) . (`compare` ys) . take m
    gt = (== GT) . (`compare` ys) . take m
    first f = fromMaybe n $ L.find g [0..n-1]
      where
        g = f . flip drop xs . PA.indexPrimArray sa'

data SuffixTree
  = Leaf Int
  | Branch Int [SuffixTree]
  deriving (Eq, Show)

data Suf a = Suf { id_ :: !Int, suf_ :: ![a] } deriving Show

naiveSuffixTree :: Ord a => [a] -> SuffixTree
naiveSuffixTree xs
  | null xs = Leaf 0
  | otherwise = go 0 sufs0
  where
    sufs0 = zipWith Suf [0..] (L.tails xs)

    go _ [] = error "go: []"
    go _ [suf] = Leaf (id_ suf)
    go d sufs = Branch (l+d) (map (go (l+d)) grps)
      where
        (sufs', l) = lcp sufs
        (eSufs, neSufs) = L.partition (null . suf_) sufs'
        grps = [eSufs | not (null eSufs)] ++ groupByHead neSufs

    lcp sufs = until (not . sameHead . map suf_ . fst)
                     (\(sufs', d) -> (map tailSuf sufs', d+1))
                     (sufs, 0)
    tailSuf suf = suf { suf_ = tail (suf_ suf) }

    sameHead = \case
      [] -> error "sameHead: empty"
      ys:yss -> all ((listToMaybe ys ==) . listToMaybe) yss

    groupByHead sufs = M.elems $ M.fromListWith (++) [(head (suf_ suf), [suf]) | suf <- sufs]

--------------------
-- Small test data
--------------------

pullBC :: (Char -> Int) -> BC.ByteString -> Pull Int
pullBC f b = f <$> Pull (BC.length b) (BC.index b)

pullBS :: BC.ByteString -> Pull Word8
pullBS = pullFromByteString

saFromList :: Prim a => [a] -> SuffixArray a
saFromList = SuffixArray . PA.primArrayFromList

lcpaFromList :: Prim a => [a] -> LCPArray a
lcpaFromList = LCPArray . PA.primArrayFromList

banana :: BC.ByteString
banana = BC.pack "BANANA"

bananaRank :: Char -> Int
bananaRank = \case
  'A' -> 0
  'B' -> 1
  'N' -> 2
  _ -> error "bananaRank"

bananaSA :: SuffixArray Int
bananaSA = saFromList [5,3,1,0,4,2]

bananaLCP :: LCPArray Int
bananaLCP = lcpaFromList [0,1,3,0,0,2]

bananaST :: SuffixTree
bananaST =
  Branch 0
    [ Leaf 6
    , Branch 1
        [ Leaf 5
        , Branch 3
            [ Leaf 3
            , Leaf 1 ] ]
    , Leaf 0
    , Branch 2
        [ Leaf 4
        , Leaf 2 ] ]

dna :: BC.ByteString
dna = BC.pack "GTCCCGATGTCATGTCAGGA"

dnaRank :: Char -> Int
dnaRank = \case
  'A' -> 0
  'C' -> 1
  'G' -> 2
  'T' -> 3
  _ -> error "dnaRank"

dnaSA :: SuffixArray Int
dnaSA = saFromList [19,16,11,6,15,10,2,3,4,18,5,17,13,8,0,14,9,1,12,7]

dnaLCP :: LCPArray Int
dnaLCP = lcpaFromList [0,1,1,6,0,2,1,2,1,0,2,1,1,4,3,0,3,2,1,5]

dnaST :: SuffixTree
dnaST =
  Branch 0
    [ Leaf 20
    , Branch 1
        [ Leaf 19
        , Leaf 16
        , Branch 6
            [ Leaf 11
            , Leaf 6 ] ]
    , Branch 1
        [ Branch 2
            [ Leaf 15
            , Leaf 10 ]
        , Branch 2
            [ Leaf 2
            , Leaf 3 ]
        , Leaf 4 ]
    , Branch 1
        [ Branch 2
            [ Leaf 18
            , Leaf 5 ]
        , Leaf 17
        , Branch 3
            [ Branch 4
                [ Leaf 13
                , Leaf 8 ]
            , Leaf 0 ] ]
    , Branch 1
        [ Branch 2
            [ Branch 3
                [ Leaf 14
                , Leaf 9 ]
            , Leaf 1 ]
        , Branch 5
            [ Leaf 12
            , Leaf 7 ] ] ]

shika :: BC.ByteString
shika = BC.pack "shikanokonokonokokoshitantan"

shikaSA :: SuffixArray Int
shikaSA = saFromList
  [26,4,23,1,20,2,21,3,15,11,7,17,27,13,9,5,24,14,10,6,16,12,8,18,0,19,25,22]

shikaLCP :: LCPArray Int
shikaLCP = lcpaFromList
  [0,2,2,0,2,0,1,0,1,2,6,2,0,1,4,8,1,0,3,7,3,1,5,1,0,3,0,3]

shikaLRLCP :: LRLCPArrays Int
shikaLRLCP = buildLRLCPArray shikaLCP

shikaSearchTests :: [(String, BC.ByteString, (Int, Int))]
shikaSearchTests =
  [ (,,) "1" (BC.pack "shika") (24,1)
  , (,,) "2" (BC.pack "shika senbei") (24,0)
  , (,,) "3" shika (24,1)
  , (,,) "4" (BC.pack "nokonoko") (14,2)
  , (,,) "5" (BC.pack "ko") (8,4)
  , (,,) "6" (BC.pack "m") (12,0)
  , (,,) "7" (BC.pack "o") (17,7)
  ]
----------------
-- Other stuff
----------------

-- Like Bool but biased in order to get longer matches at random.
data Biased = B0 | B1 deriving (Eq, Ord, Show, Enum)

instance Arbitrary Biased where
  arbitrary = frequency [(19, pure B0), (1, pure B1)]
