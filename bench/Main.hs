{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.Bits ((.&.))
import qualified Data.ByteString as B
import Data.Functor.Identity (Identity(..))
import Data.Int (Int32)
import qualified Data.List as L
import Data.Proxy (Proxy(..))
import Test.Tasty.Bench

import Data.Suffix.ByteString (Intn)
import qualified Data.Suffix.ByteString as SufB

main :: IO ()
main = defaultMain
  [ bgroup "buildSuffixArray"
    [ bgroup "Int" $ map (benchBuildSuffixArray (Proxy @Int)) benchInputs
    , bgroup "Int32" $ map (benchBuildSuffixArray (Proxy @Int32)) benchInputs
    ]
  , bgroup "search"
    [ bgroup "Int" $ map (benchSearch (Proxy @Int)) benchInputs
    , bgroup "Int32" $ map (benchSearch (Proxy @Int32)) benchInputs
    ]
  , bgroup "buildLCPArray"
    [ bgroup "Int" $ map (benchBuildLCPArray (Proxy @Int)) benchInputs
    , bgroup "Int32" $ map (benchBuildLCPArray (Proxy @Int32)) benchInputs
    ]
  , bgroup "buildLRLCPArray"
    [ bgroup "Int" $ map (benchBuildLRLCPArray (Proxy @Int)) benchInputs
    , bgroup "Int32" $ map (benchBuildLRLCPArray (Proxy @Int32)) benchInputs
    ]
  , bgroup "searchLRLCP"
    [ bgroup "Int" $ map (benchSearchLRLCP (Proxy @Int)) benchInputs
    , bgroup "Int32" $ map (benchSearchLRLCP (Proxy @Int32)) benchInputs
    ]
  , bgroup "foldSuffixTree"
    [ bgroup "Int" $ map (benchFoldSuffixTree (Proxy @Int)) benchInputs
    , bgroup "Int32" $ map (benchFoldSuffixTree (Proxy @Int32)) benchInputs
    ]
  ]

benchBuildSuffixArray :: Intn i => Proxy i -> Input -> Benchmark
benchBuildSuffixArray (_ :: Proxy i) input = env (prepareInput input) $ \bs ->
  bench (nameInput input) $ whnf (SufB.buildSuffixArray @i) bs

benchSearch :: Intn i => Proxy i -> Input -> Benchmark
benchSearch (_ :: Proxy i) input = env e $ \ ~(bs,sa) ->
  bench (nameInput input) $
    flip whnf (bs,sa) $ \(bs',sa') -> SufB.search bs' sa' bs'
  where
    e = do
      bs <- prepareInput input
      let sa = SufB.buildSuffixArray @i bs
      pure (bs,sa)

benchBuildLCPArray :: Intn i => Proxy i -> Input -> Benchmark
benchBuildLCPArray (_ :: Proxy i) input = env e $ \ ~(bs,sa) ->
  bench (nameInput input) $
    flip whnf (bs,sa) $ \(bs',sa') -> SufB.buildLCPArray bs' sa'
  where
    e = do
      bs <- prepareInput input
      let sa = SufB.buildSuffixArray @i bs
      pure (bs,sa)

benchBuildLRLCPArray :: Intn i => Proxy i -> Input -> Benchmark
benchBuildLRLCPArray (_ :: Proxy i) input = env e $ \lcpa ->
  bench (nameInput input) $ whnf SufB.buildLRLCPArray lcpa
  where
    e = do
      bs <- prepareInput input
      let sa = SufB.buildSuffixArray @i bs
      pure $ SufB.buildLCPArray bs sa

benchSearchLRLCP :: Intn i => Proxy i -> Input -> Benchmark
benchSearchLRLCP (_ :: Proxy i) input = env e $ \ ~(bs,sa,lrlcpa) ->
  bench (nameInput input) $
    flip whnf (bs,sa) $ \(bs',sa') -> SufB.searchLRLCP bs' sa' lrlcpa bs'
  where
    e = do
      bs <- prepareInput input
      let sa = SufB.buildSuffixArray @i bs
          lcpa = SufB.buildLCPArray bs sa
          lrlcpa = SufB.buildLRLCPArray lcpa
      pure (bs,sa,lrlcpa)

benchFoldSuffixTree :: Intn i => Proxy i -> Input -> Benchmark
benchFoldSuffixTree (_ :: Proxy i) input = env e $ \ ~(sa,lcpa) ->
  bench (nameInput input) $
    flip whnf (sa,lcpa) $ \ ~(sa',lcpa') ->
      -- This is meant to measure only the time spent traversing the tree.
      let l1 _ = pure ()
          b1 _ = pure ()
          b2 _ _ _ = pure ()
          b3 _ _ = pure ()
      in runIdentity (SufB.foldSuffixTree l1 b1 b2 b3 sa' lcpa')
  where
    e = do
      bs <- prepareInput input
      let sa = SufB.buildSuffixArray @i bs
          lcpa = SufB.buildLCPArray bs sa
      pure (sa,lcpa)

-- Get the files from the Pizza&Chilli corpus and truncate to 1MB.
-- https://pizzachili.dcc.uchile.cl/texts.html

benchInputs :: [Input]
benchInputs =
  [ Random (1024 * 1024)
  , Repeat (1024 * 1024)
  , File "bench/data/sources.1MB"
  , File "bench/data/pitches.1MB"
  , File "bench/data/proteins.1MB"
  , File "bench/data/dna.1MB"
  , File "bench/data/english.1MB"
  , File "bench/data/dblp.xml.1MB"
  ]

data Input
  = Random !Int
  | Repeat !Int
  | File !String

nameInput :: Input -> String
nameInput = \case
  Random n -> "random " ++ show n
  Repeat n -> "repeat " ++ show n
  File path -> "file " ++ name path
  where
    name = reverse . takeWhile (/= '/') . reverse

prepareInput :: Input -> IO B.ByteString
prepareInput = \case
  Random n -> pure $ B.pack (map fromIntegral (randomInts n))
  Repeat n -> pure $ B.pack (replicate n 65)
  File name -> B.readFile name

-- LCG
randomInts :: Int -> [Int]
randomInts n =
  take n $ L.iterate' (\i -> 0xffffffff .&. (i * 1103515245 + 12345)) n
{-# INLINE randomInts #-}
