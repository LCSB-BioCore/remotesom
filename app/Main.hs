{-# LANGUAGE TypeOperators #-}

module Main where

import StorableArray

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import qualified Data.Array.Accelerate.Interpreter as A
import Data.Array.Accelerate.LLVM.Native as L
import Data.Foldable (foldlM)
import Data.Function ((&))
import System.Environment

floatArray :: A.Array sh Float -> A.Array sh Float
floatArray = id

somIter points gsqdists som eps =
  let pts, somn :: Int
      (Z :. pts :. _) = A.arrayShape $ floatArray points
      (Z :. somn :. dim) = A.arrayShape $ floatArray som
      expts =
        A.replicate (A.constant $ Z :. A.All :. somn :. A.All) $ A.use points
      exsom = A.replicate (A.constant $ Z :. pts :. A.All :. A.All) $ A.use som
      toArg :: A.Exp (Z :. Int :. Int) -> A.Exp Float -> A.Exp (Float, Int)
      toArg eix ev = A.lift (ev, A.indexHead eix)
      closest =
        A.zipWith (-) expts exsom
          & A.map (\x -> x * x)
          & A.sum
          & A.imap toArg
          & A.minimum
          & A.map A.snd
      counts =
        A.permute
          (+)
          (A.constant (Z :. somn) `A.fill` A.constant (0 :: Int))
          (\ix -> A.Just_ . A.I1 $ closest A.! ix)
          (A.constant (Z :. pts) `A.fill` A.constant (1 :: Int))
      sums =
        A.permute
          (+)
          (A.constant (Z :. somn :. dim) `A.fill` A.constant (0 :: Float))
          (\(A.I2 pi dimi) -> A.Just_ $ A.I2 (closest A.! A.I1 pi) dimi)
          (A.use points)
   in A.zipWith A.max (A.shape counts `A.fill` A.constant 1) counts
        & A.replicate (A.constant $ Z :. A.All :. dim)
        & A.map A.fromIntegral
        & A.zipWith (/) sums
        & L.run

main :: IO ()
main = do
  (inputFile:restArgs) <- getArgs
  let [dims, cells, somx, somy] = map read (take 4 restArgs) :: [Int]
      iterEps = map read (drop 4 restArgs) :: [Float]
      somn = somx * somy
      coords = (`divMod` somx)
      somsqdist i j =
        let (a, b) = coords i
            (c, d) = coords j
         in (a - c) * (a - c) + (b - d) * (b - d)
  points <- floatArray <$> readArrayStorable (Z :. cells :. dims) inputFile
  let som0 =
        floatArray
          $ A.fromFunction
              (Z :. somn :. dims)
              (\(Z :. somi :. dimi) -> fromIntegral $ somi - dimi)
  let gridsqdist =
        floatArray
          $ A.fromFunction
              (Z :. somn :. somn)
              (\(Z :. i :. j) -> fromIntegral $ somsqdist i j)
  let finalSom = foldl (somIter points gridsqdist) som0 iterEps
  --T.writeMatrixToTextFile "/dev/stdout" finalSom
  --print gridsqdist
  --print $ runExp $ A.use points A.! A.constant (Z :. 0 :. 0)
  --print $ A.run $ A.slice (A.use points) $ A.constant (Z :. (0::Int) :. A.All)
  print finalSom
