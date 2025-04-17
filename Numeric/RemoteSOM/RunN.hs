module Numeric.RemoteSOM.RunN where

import Numeric.RemoteSOM

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as LL

arraySumIntVecLL :: A.Vector Int -> A.Vector Int -> A.Vector Int
arraySumIntVecLL = LL.runN arraySum

arraySumFloatMtxLL :: A.Matrix Float -> A.Matrix Float -> A.Matrix Float
arraySumFloatMtxLL = LL.runN arraySum

arraySumIntMtxLL :: A.Matrix Int -> A.Matrix Int -> A.Matrix Int
arraySumIntMtxLL = LL.runN arraySum

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

somCountsLL :: A.Scalar Int -> A.Vector Int -> A.Vector Int
somCountsLL = LL.runN somCounts

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
