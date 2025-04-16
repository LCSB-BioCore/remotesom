{-
 - Copyright (c) 2025 University of Luxembourg
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network
import Numeric.RemoteSOM
import Numeric.RemoteSOM.IO
  ( arrayMatrix
  , arraySummary
  , matrixArray
  , readArrayStorable
  , summaryArray
  )
import Opts

import Control.Concurrent.Async (forConcurrently)
import Control.Monad (unless)
import qualified Data.Aeson as J
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import qualified Data.Array.Accelerate.LLVM.Native as LL
import Data.Foldable (foldlM)
import System.Random

main :: IO ()
main = parseOpts >>= run

{-
 - IO helpers
 -}
decodeFile :: J.FromJSON a => FilePath -> IO a
decodeFile file = do
  x <- J.eitherDecodeFileStrict file
  case x of
    Left err -> error $ "error loading " ++ file ++ ": " ++ err
    Right x' -> pure x'

readPoints :: InputOpts -> Int -> IO (A.Matrix Float)
readPoints iopts dim =
  readArrayStorable (Z :. inputPoints iopts :. dim) (inputData iopts)

withJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
withJust (Just x) m = m x
withJust _ _ = pure ()

{-
 - Interpreter for the command from the args
 -}
run :: Cmd -> IO ()
run (GenCmd opts so to) = do
  (s, t) <- runGen opts
  J.encodeFile so $ matrixArray s
  J.encodeFile to $ matrixArray t
run (TrainCmd opts iopts) = do
  (som0, topo) <- trainStartSOM opts
  let (Z :. _ :. dim) = A.arrayShape som0
  points <- readPoints iopts dim
  let som =
        foldl
          (\s sigma -> somIterLL points topo s (scalar sigma))
          som0
          (trainSigmas opts)
  J.encodeFile (trainSomOut opts) (matrixArray som)
run (StatsCmd opts iopts) = do
  som <- arrayMatrix <$> decodeFile (statsSomIn opts)
  let (Z :. _ :. dim) = A.arrayShape som
  points <- readPoints iopts dim
  let (sums, sqsums, counts) = somSumSqsumCountsLL points som
  withJust (statsMeansOut opts) $ \o -> do
    J.encodeFile o . matrixArray $ somMeansLL sums counts
  withJust (statsCountsOut opts) $ \o -> do
    J.encodeFile o $ A.toList counts
  withJust (statsVariancesOut opts) $ \o -> do
    J.encodeFile o . matrixArray $ somVariancesLL sums sqsums counts
  withJust (statsMedians opts) $ \mo -> do
    let (lb, ub) = mediansBounds mo
        bs0 = somMedianInitLL (scalar lb) (scalar ub) som
        cs = somClosestLL points som
        step bs =
          let med = somMedianMedLL bs
              ltcs = somLtCountsLL points cs med
           in somMedianCountStepLL ltcs counts med bs
    J.encodeFile (mediansOut mo) . matrixArray . somMedianMedLL
      $ iterate step bs0 !! mediansIters mo
run (SummaryCmd opts iopts) = do
  som <- arrayMatrix <$> decodeFile (summarySomIn opts)
  let (Z :. _ :. dim) = A.arrayShape som
  points <- readPoints iopts dim
  J.encodeFile (summaryOut opts) . uncurry summaryArray
    $ somSumCountsLL points som
run (AggregateCmd opts) = do
  (sumss, countss) <-
    unzip
      <$> traverse (fmap arraySummary . decodeFile) (aggregateSummaryIn opts)
  topo <- arrayMatrix <$> decodeFile (aggregateTopoIn opts)
  let sums0 =
        case sumss of
          [] -> error "no data to aggregate"
          (s:_) -> s
      (Z :. nsom :. dim) = A.arrayShape sums0
      som =
        matrixArray
          $ aggregate nsom dim sumss countss topo (aggregateSigma opts)
  J.encodeFile (aggregateSomOut opts) som
run (ServerCmd sopts iopts dim) = do
  points <- readPoints iopts dim
  interactServer sopts $ \query -> do
    let som = either error arrayMatrix $ J.eitherDecode query
        (Z :. _ :. dim') = A.arrayShape som
    unless (dim == dim') $ error "som dimensions do not match"
    pure . J.encode . uncurry summaryArray $ somSumCountsLL points som
run (ClientTrainCmd servers copts opts) = do
  unless (not $ null servers) $ error "no servers to connect to"
  (som0, topo) <- trainStartSOM opts
  let (Z :. nsom :. dim) = A.arrayShape som0
      trainWithClients som sigma = do
        let q = J.encode $ matrixArray som
        (sumss, countss) <-
          fmap unzip . forConcurrently servers $ \s ->
            either error arraySummary . J.eitherDecode
              <$> runClientQuery s copts q
        pure $ aggregate nsom dim sumss countss topo sigma
  som <- foldlM trainWithClients som0 (trainSigmas opts)
  J.encodeFile (trainSomOut opts) (matrixArray som)
run _ = error "not implemented yet"

{-
 - Accelerate.LLVM.Native adaptors
 -}
scalar :: A.Elt a => a -> A.Scalar a
scalar x = A.fromList Z [x]

arraySumIntVecLL :: A.Vector Int -> A.Vector Int -> A.Vector Int
arraySumIntVecLL = LL.runN arraySum

arraySumFloatMtxLL :: A.Matrix Float -> A.Matrix Float -> A.Matrix Float
arraySumFloatMtxLL = LL.runN arraySum

somIterLL ::
     A.Matrix Float
  -> A.Matrix Float
  -> A.Matrix Float
  -> A.Scalar Float
  -> A.Matrix Float
somIterLL = LL.runN somIter

somClosestLL :: A.Matrix Float -> A.Matrix Float -> A.Vector Int
somClosestLL = LL.runN somClosest

somLtCountsLL ::
     A.Matrix Float -> A.Vector Int -> A.Matrix Float -> A.Matrix Int
somLtCountsLL = LL.runN somLtCounts

somMedianInitLL ::
     A.Scalar Float
  -> A.Scalar Float
  -> A.Matrix Float
  -> (A.Matrix Float, A.Matrix Float)
somMedianInitLL = LL.runN somMedianInit

somMedianMedLL :: (A.Matrix Float, A.Matrix Float) -> A.Matrix Float
somMedianMedLL = LL.runN somMedianMed

somMedianCountStepLL ::
     A.Matrix Int
  -> A.Vector Int
  -> A.Matrix Float
  -> (A.Matrix Float, A.Matrix Float)
  -> (A.Matrix Float, A.Matrix Float)
somMedianCountStepLL = LL.runN somMedianCountStep

somVariancesLL ::
     A.Matrix Float -> A.Matrix Float -> A.Vector Int -> A.Matrix Float
somVariancesLL = LL.runN somVariances

somMeansLL :: A.Matrix Float -> A.Vector Int -> A.Matrix Float
somMeansLL = LL.runN somMeans

somAggregateLL ::
     A.Scalar Int
  -> A.Scalar Int
  -> A.Matrix Float
  -> A.Vector Int
  -> A.Matrix Float
  -> A.Scalar Float
  -> A.Matrix Float
somAggregateLL = LL.runN somAggregate

somSumCountsLL ::
     A.Matrix Float -> A.Matrix Float -> (A.Matrix Float, A.Vector Int)
somSumCountsLL = LL.runN somSumCounts

somSumSqsumCountsLL ::
     A.Matrix Float
  -> A.Matrix Float
  -> (A.Matrix Float, A.Matrix Float, A.Vector Int)
somSumSqsumCountsLL = LL.runN somSumSqsumCounts

{-
 - Base operations
 -}
aggregate ::
     Int
  -> Int
  -> [A.Matrix Float]
  -> [A.Vector Int]
  -> A.Matrix Float
  -> Float
  -> A.Matrix Float
aggregate nsom dim sumss countss topo sigma
  | all ((== (Z :. nsom :. dim)) . A.arrayShape) sumss
      && all ((== (Z :. nsom)) . A.arrayShape) countss =
    let sums = foldl1 arraySumFloatMtxLL sumss
        counts = foldl1 arraySumIntVecLL countss
     in somAggregateLL
          (scalar nsom)
          (scalar dim)
          sums
          counts
          topo
          (scalar sigma)
  | otherwise = error "summary dimensions do not match"

trainStartSOM :: TrainOpts -> IO (A.Matrix Float, A.Matrix Float)
trainStartSOM opts =
  case trainSomIn opts of
    Left (gopts, to) -> do
      (sa, ta) <- runGen gopts
      J.encodeFile to (matrixArray ta)
      pure (sa, ta)
    Right (si, ti) -> do
      (,)
        <$> (arrayMatrix <$> decodeFile si)
        <*> (arrayMatrix <$> decodeFile ti)

runGen :: GenOpts -> IO (A.Matrix Float, A.Matrix Float)
runGen opts = do
  let somn = genX opts * genY opts
      coords = (`divMod` genX opts)
      somsqdist i j =
        let (a, b) = coords i
            (c, d) = coords j
         in (a - c) * (a - c) + (b - d) * (b - d)
      topology =
        A.fromFunction
          (Z :. somn :. somn)
          (\(Z :. i :. j) -> fromIntegral $ somsqdist i j)
  centroids <-
    A.fromList (Z :. somn :. genDim opts)
      . fst
      . uniformListR (somn * genDim opts) (-1, 1)
      <$> case genSeed opts of
            Just s -> pure $ mkStdGen s
            Nothing -> initStdGen
  pure (centroids, topology)
