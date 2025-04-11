{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Numeric.RemoteSOM.IO where

import Control.Exception
import qualified Data.Aeson as J
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import qualified Data.Array.Accelerate.Representation.Shape as RS
import qualified Data.Array.Accelerate.Sugar.Elt as AE
import qualified Data.Array.Accelerate.Sugar.Shape as AS
import Data.List (intercalate)
import qualified Data.StorableVector as SV
import Foreign.Storable (Storable)
import GHC.Generics
import System.IO

shapeSize' ::
     forall sh. A.Shape sh
  => sh
  -> Int
shapeSize' = RS.size (AS.shapeR @sh) . AE.fromElt

readArrayStorable ::
     (A.Shape sh, Storable a, A.Elt a) => sh -> FilePath -> IO (A.Array sh a)
readArrayStorable sh fp =
  bracket (openFile fp ReadMode) hClose $ \h ->
    A.fromList sh . SV.unpack <$> SV.hGet h (shapeSize' sh)

writeArrayStorable ::
     (A.Shape sh, Storable a, A.Elt a) => A.Array sh a -> FilePath -> IO ()
writeArrayStorable a fp = do
  bracket (openFile fp WriteMode) hClose $ \h ->
    SV.hPut h . SV.pack $ A.toList a

readArrayTSV ::
     (A.Shape sh, Read a, A.Elt a) => sh -> FilePath -> IO (A.Array sh a)
readArrayTSV sh fp = A.fromList sh . map read . words <$> readFile fp

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, yss) = splitAt n xs
   in ys : chunks n yss

writeArrayTSV ::
     (A.Shape sh, Show a, A.Elt a) => A.Array (sh :. Int) a -> FilePath -> IO ()
writeArrayTSV a fp =
  let (_ :. rowsize) = A.arrayShape a
   in writeFile fp
        . unlines
        . map (intercalate "\t")
        . chunks rowsize
        . map show
        $ A.toList a

somArray :: A.Elt f => A.Array (Z :. Int :. Int) f -> [[f]]
somArray c = chunks dim $ A.toList c
  where
    (Z :. _ :. dim) = A.arrayShape c

arraySOM :: A.Elt f => [[f]] -> A.Array ((Z :. Int) :. Int) f
arraySOM c = A.fromList (Z :. nsom :. dim) $ concat c
  where
    nsom = length c
    dim =
      case c of
        (c0:_) -> length c0
        _ -> error "empty SOM"

topoArray :: A.Elt f => A.Array (Z :. Int :. Int) f -> [[f]]
topoArray t = chunks nsom $ A.toList t
  where
    (Z :. _ :. nsom) = A.arrayShape t

arrayTopo :: A.Elt f => [[f]] -> A.Array ((Z :. Int) :. Int) f
arrayTopo t = A.fromList (Z :. nsom :. nsom) $ concat t
  where
    nsom = length t

data DataSummary f = DataSummary
  { sums :: [[f]]
  , counts :: [Int]
  } deriving (Show, Generic, J.FromJSON, J.ToJSON)

summaryArray ::
     A.Elt f
  => A.Array (Z :. Int :. Int) f
  -> A.Array (Z :. Int) Int
  -> DataSummary f
summaryArray s c =
  DataSummary {sums = chunks dim $ A.toList s, counts = A.toList c}
  where
    (Z :. _ :. dim) = A.arrayShape s

arraySummary ::
     A.Elt f
  => DataSummary f
  -> (A.Array (Z :. Int :. Int) f, A.Array (Z :. Int) Int)
arraySummary DataSummary {sums = s, counts = c} =
  (A.fromList (Z :. nsom :. dim) $ concat s, A.fromList (Z :. nsom) c)
  where
    nsom = length s
    dim =
      case s of
        (s0:_) -> length s0
        _ -> error "empty sum"
