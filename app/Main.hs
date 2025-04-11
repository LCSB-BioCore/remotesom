{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Numeric.RemoteSOM
import Numeric.RemoteSOM.IO
import Opts

import qualified Data.Aeson as J

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import qualified Data.Array.Accelerate.Interpreter as AI
import qualified Data.Array.Accelerate.LLVM.Native as LL
import System.Environment

floatArray :: A.Array sh Float -> A.Array sh Float
floatArray = id

main = parseOpts >>= run

decodeFile file = do
  x <- J.eitherDecodeFileStrict file
  case x of
    Left err -> error $ "error loading " ++ file ++ ": " ++ err
    Right x -> pure x

readPoints iopts dim =
  readArrayStorable (Z :. inputPoints iopts :. dim) (inputData iopts)

run (GenCmd opts so to) = do
  let (s, t) = runGen opts
  J.encodeFile so $ somArray s
  J.encodeFile to $ topoArray t
run (TrainCmd opts iopts) = do
  (som0, topo) <-
    case trainSomIn opts of
      Left (gopts, to) -> do
        let (sa, ta) = runGen gopts
            ts = topoArray ta
        J.encodeFile to ts
        pure (sa, ta)
      Right (si, ti) -> do
        (,) <$> (arraySOM <$> decodeFile si) <*> (arrayTopo <$> decodeFile ti)
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
      sums = foldl1 (A.zipWith (+)) (map A.use sumss)
      counts = foldl1 (A.zipWith (+)) (map A.use countss)
      som =
        somArray . LL.run
          $ somAggregate nsom dim sums counts (A.use topo) (aggregateSigma opts)
  J.encodeFile (aggregateSomOut opts) som
run _ = putStrLn "Not implemented yet."

runGen ::
     GenOpts
  -> (A.Array (Z :. Int :. Int) Float, A.Array (Z :. Int :. Int) Float)
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
