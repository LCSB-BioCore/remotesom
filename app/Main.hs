{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network
import Numeric.RemoteSOM (somAggregate, somIter, somSumCounts)
import Numeric.RemoteSOM.IO
  ( arraySOM
  , arraySummary
  , arrayTopo
  , readArrayStorable
  , somArray
  , summaryArray
  , topoArray
  )
import Opts

import Control.Concurrent.Async (forConcurrently)
import Control.Monad (unless)
import qualified Data.Aeson as J
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import qualified Data.Array.Accelerate.LLVM.Native as LL
import Data.Foldable (foldlM)

main :: IO ()
main = parseOpts >>= run

decodeFile :: J.FromJSON a => FilePath -> IO a
decodeFile file = do
  x <- J.eitherDecodeFileStrict file
  case x of
    Left err -> error $ "error loading " ++ file ++ ": " ++ err
    Right x' -> pure x'

readPoints :: InputOpts -> Int -> IO (A.Matrix Float)
readPoints iopts dim =
  readArrayStorable (Z :. inputPoints iopts :. dim) (inputData iopts)

run :: Cmd -> IO ()
run (GenCmd opts so to) = do
  let (s, t) = runGen opts
  J.encodeFile so $ somArray s
  J.encodeFile to $ topoArray t
run (TrainCmd opts iopts) = do
  (som0, topo) <- trainStartSOM opts
  let (Z :. _ :. dim) = A.arrayShape som0
  points <- readPoints iopts dim
  let som =
        foldl
          (\s sigma -> LL.run $ somIter points (A.use topo) s sigma)
          som0
          (trainSigmas opts)
  J.encodeFile (trainSomOut opts) (somArray som)
run (SummaryCmd opts iopts) = do
  som <- arraySOM <$> decodeFile (summarySomIn opts)
  let (Z :. _ :. dim) = A.arrayShape som
  points <- readPoints iopts dim
  let (s, c) = somSumCounts points som
  J.encodeFile (summaryOut opts) $ summaryArray (LL.run s) (LL.run c)
run (AggregateCmd opts) = do
  (sumss, countss) <-
    unzip
      <$> traverse (fmap arraySummary . decodeFile) (aggregateSummaryIn opts)
  topo <- arrayTopo <$> decodeFile (aggregateTopoIn opts)
  let sums0 =
        case sumss of
          [] -> error "no data to aggregate"
          (s:_) -> s
      (Z :. nsom :. dim) = A.arrayShape sums0
      som =
        somArray $ doAggregate nsom dim sumss countss topo (aggregateSigma opts)
  J.encodeFile (aggregateSomOut opts) som
run (ServerCmd sopts iopts dim) = do
  points <- readPoints iopts dim
  interactServer sopts $ \query -> do
    let som = either error arraySOM $ J.eitherDecode query
        (Z :. _ :. dim') = A.arrayShape som
    unless (dim == dim') $ error "som dimensions do not match"
    let (s, c) = somSumCounts points som
    pure . J.encode $ summaryArray (LL.run s) (LL.run c)
run (ClientTrainCmd servers copts opts) = do
  unless (not $ null servers) $ error "No servers to connect to."
  (som0, topo) <- trainStartSOM opts
  let (Z :. nsom :. dim) = A.arrayShape som0
      trainWithClients som sigma = do
        let q = J.encode $ somArray som
        (sumss, countss) <-
          fmap unzip . forConcurrently servers $ \s ->
            either error arraySummary . J.eitherDecode
              <$> runClientQuery s copts q
        pure $ doAggregate nsom dim sumss countss topo sigma
  som <- foldlM trainWithClients som0 (trainSigmas opts)
  J.encodeFile (trainSomOut opts) (somArray som)
run _ = putStrLn "Not implemented yet."

trainStartSOM :: TrainOpts -> IO (A.Matrix Float, A.Matrix Float)
trainStartSOM opts =
  case trainSomIn opts of
    Left (gopts, to) -> do
      let (sa, ta) = runGen gopts
          ts = topoArray ta
      J.encodeFile to ts
      pure (sa, ta)
    Right (si, ti) -> do
      (,) <$> (arraySOM <$> decodeFile si) <*> (arrayTopo <$> decodeFile ti)

doAggregate ::
     Int
  -> Int
  -> [A.Matrix Float]
  -> [A.Vector Int]
  -> A.Matrix Float
  -> Float
  -> A.Matrix Float
doAggregate nsom dim sumss countss topo sigma
  | all ((== (Z :. nsom :. dim)) . A.arrayShape) sumss
      && all ((== (Z :. nsom)) . A.arrayShape) countss =
    let sums = foldl1 (A.zipWith (+)) (map A.use sumss)
        counts = foldl1 (A.zipWith (+)) (map A.use countss)
     in LL.run $ somAggregate nsom dim sums counts (A.use topo) sigma
  | otherwise = error "summary dimensions do not match"

runGen :: GenOpts -> (A.Matrix Float, A.Matrix Float)
runGen opts = (centroids, topology)
  where
    somn = genX opts * genY opts
    coords = (`divMod` genX opts)
    somsqdist i j =
      let (a, b) = coords i
          (c, d) = coords j
       in (a - c) * (a - c) + (b - d) * (b - d)
    centroids = A.fromFunction (Z :. somn :. genDim opts) (const 0)
    topology =
      A.fromFunction
        (Z :. somn :. somn)
        (\(Z :. i :. j) -> fromIntegral $ somsqdist i j)
