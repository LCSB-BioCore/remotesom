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
import Numeric.RemoteSOM.IO
import Numeric.RemoteSOM.RunN
import Opts

import Control.Concurrent.Async (forConcurrently)
import Control.Monad (unless)
import qualified Data.Aeson as J
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import Data.Foldable (foldlM)
import Data.List (foldl1')
import System.Random

main :: IO ()
main = parseOpts >>= run

{-
 - IO and helpers
 -}
decodeFile :: J.FromJSON a => FilePath -> IO a
decodeFile file = do
  x <- J.eitherDecodeFileStrict file
  case x of
    Left err -> error $ "error loading " ++ file ++ ": " ++ err
    Right x' -> pure x'

withMmapPoints :: InputOpts -> Int -> (A.Matrix Float -> IO a) -> IO a
withMmapPoints iopts dim =
  withMmapArray (Z :. inputPoints iopts :. dim) (inputData iopts)

withJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
withJust (Just x) m = m x
withJust _ _ = pure ()

scalar :: A.Elt a => a -> A.Scalar a
scalar x = A.fromList Z [x]

uncurry3 :: (a -> b -> c -> r) -> (a, b, c) -> r
uncurry3 f (a, b, c) = f a b c

iterateN :: (a -> a) -> Int -> (a -> a)
iterateN f n a = foldr (const f) a [1 .. n]

iterateNM :: Monad m => (a -> m a) -> Int -> (a -> m a)
iterateNM f n a = foldlM (const . f) a [1 .. n]

{-
 - Interpreter for the command from the args
 -}
run :: Cmd -> IO ()
-- generate a new SOM
run (GenCmd opts so to) = do
  (s, t) <- runGen opts
  J.encodeFile so $ matrixArray s
  J.encodeFile to $ matrixArray t
-- local training
run (TrainCmd opts iopts) = do
  (som0, topo) <- trainStartSOM opts
  let (Z :. _ :. dim) = A.arrayShape som0
  withMmapPoints iopts dim $ \points ->
    let som =
          foldl
            (\s sigma -> somIterLL points topo s (scalar sigma))
            som0
            (trainSigmas opts)
     in J.encodeFile (trainSomOut opts) (matrixArray som)
-- local statistics output
run (StatsCmd opts iopts) = do
  som <- arrayMatrix <$> decodeFile (statsSomIn opts)
  let (Z :. _ :. dim) = A.arrayShape som
  withMmapPoints iopts dim $ \points -> do
    let (sums, sqsums, counts) = somSumSqsumCountsLL points som
    outputSSCStats opts sums sqsums counts
    withJust (statsMedians opts) $ \mo -> do
      let (lb, ub) = mediansBounds mo
          bs0 = somMedianInitLL (scalar lb) (scalar ub) som
          cs = somClosestLL points som
          step bs =
            let med = somMedianMedLL bs
                ltcs = somLtCountsLL points cs med
             in somMedianCountStepLL ltcs counts med bs
      J.encodeFile (mediansOut mo) . matrixArray . somMedianMedLL
        $ iterateN step (mediansIters mo) bs0
-- manual training summary step
run (SummaryCmd opts iopts) = do
  som <- arrayMatrix <$> decodeFile (summarySomIn opts)
  let (Z :. _ :. dim) = A.arrayShape som
  withMmapPoints iopts dim
    $ J.encodeFile (summaryOut opts)
        . uncurry summaryArray
        . flip somSumCountsLL som
-- manual training aggregation step
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
-- run a server
run (ServerCmd sopts iopts dim) =
  withMmapPoints iopts dim $ \points ->
    interactServer sopts $ \q ->
      case either error id $ J.eitherDecode q of
        QueryDataSummary som' -> do
          let som = arrayMatrix som'
              (Z :. _ :. dim') = A.arrayShape som
          unless (dim == dim') $ error "som dimensions do not match"
          pure . J.encode . uncurry summaryArray $ somSumCountsLL points som
        QueryStats som' -> do
          let som = arrayMatrix som'
              (Z :. _ :. dim') = A.arrayShape som
          unless (dim == dim') $ error "som dimensions do not match"
          pure . J.encode . uncurry3 statsSummaryArray
            $ somSumSqsumCountsLL points som
        QueryLessThan som' med' -> do
          let som = arrayMatrix som'
              med = arrayMatrix med'
              sh@(Z :. nsom :. dim') = A.arrayShape som
          unless (sh == A.arrayShape med && dim == dim')
            $ error "sizes do not match"
          let cs = somClosestLL points som
              counts = somCountsLL (scalar nsom) cs
              ltcs = somLtCountsLL points cs med
          pure . J.encode $ ltCsArray ltcs counts
-- run the training client
run (ClientTrainCmd servers copts opts) = do
  unless (not $ null servers) $ error "no servers to connect to"
  (som0, topo) <- trainStartSOM opts
  let (Z :. nsom :. dim) = A.arrayShape som0
      trainWithClients som sigma = do
        let q = J.encode . QueryDataSummary $ matrixArray som
        (sumss, countss) <-
          fmap unzip . forConcurrently servers $ \s ->
            either error arraySummary . J.eitherDecode
              <$> runClientQuery s copts q
        pure $ aggregate nsom dim sumss countss topo sigma
  som <- foldlM trainWithClients som0 (trainSigmas opts)
  J.encodeFile (trainSomOut opts) (matrixArray som)
-- run the stats-gathering client
run (ClientStatsCmd servers copts opts) = do
  unless (not $ null servers) $ error "no servers to connect to"
  som <- arrayMatrix <$> decodeFile (statsSomIn opts)
  (sumss, sqsumss, countss) <-
    fmap unzip3 . forConcurrently servers $ \s ->
      either error arrayStatsSummary . J.eitherDecode
        <$> runClientQuery
              s
              copts
              (J.encode $ QueryStats (matrixArray som :: [[Float]]))
  let sums = foldl1' arraySumFloatMtxLL sumss
      sqsums = foldl1' arraySumFloatMtxLL sqsumss
      counts = foldl1' arraySumIntVecLL countss
  outputSSCStats opts sums sqsums counts
  withJust (statsMedians opts) $ \mo -> do
    let (lb, ub) = mediansBounds mo
        bs0 = somMedianInitLL (scalar lb) (scalar ub) som
        step bs = do
          let med = somMedianMedLL bs
          (ltcss, mcountss) <-
            fmap unzip . forConcurrently servers $ \s ->
              either error arrayLtCs . J.eitherDecode
                <$> runClientQuery
                      s
                      copts
                      (J.encode
                         $ QueryLessThan (matrixArray som) (matrixArray med))
          let ltcs = foldl1' arraySumIntMtxLL ltcss
              mcounts = foldl1' arraySumIntVecLL mcountss
          pure $ somMedianCountStepLL ltcs mcounts med bs
    bs' <- iterateNM step (mediansIters mo) bs0
    J.encodeFile (mediansOut mo) . matrixArray $ somMedianMedLL bs'

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
    let sums = foldl1' arraySumFloatMtxLL sumss
        counts = foldl1' arraySumIntVecLL countss
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

outputSSCStats ::
     StatsOpts -> A.Matrix Float -> A.Matrix Float -> A.Vector Int -> IO ()
outputSSCStats opts sums sqsums counts = do
  withJust (statsMeansOut opts) $ \o -> do
    J.encodeFile o . matrixArray $ somMeansLL sums counts
  withJust (statsCountsOut opts) $ \o -> do
    J.encodeFile o $ A.toList counts
  withJust (statsVariancesOut opts) $ \o -> do
    J.encodeFile o . matrixArray $ somVariancesLL sums sqsums counts
