
module Opts where

import Options.Applicative
import System.Environment

data GenOpts = GenOpts
  { genX :: Int
  , genY :: Int
  , genDim :: Int
  , genSeed :: (Maybe Int)
  , genOutFile :: FilePath
  } deriving (Show)

data InputOpts = InputOpts
  { inputData :: FilePath
  , inputPoints :: Int
  } deriving (Show)

data TrainOpts = TrainOpts
  { trainSomIn :: Either GenOpts FilePath
  , trainSomOut :: FilePath
  , trainSigmas :: [Float]
  } deriving (Show)

data StatsOpts = StatsOpts
  { statsSomIn :: FilePath
  , statsOut :: FilePath
  } deriving (Show)

data SummaryOpts = SummaryOpts
  { summarySomIn :: FilePath
  , summaryOut :: FilePath
  } deriving (Show)

data AggregateOpts = AggregateOpts
  { aggregateSummaryIn :: [FilePath]
  , aggregateSomOut :: FilePath
  , aggregateSigma :: Float
  } deriving (Show)

data ServerOpts = ServerOpts
  { serverData :: InputOpts
  , bindAddress :: String
  , bindPort :: Int
  } deriving (Show)

data ClientOpts = ClientOpts
  { clientServerUrl :: String
  , clientSomIn :: Either GenOpts FilePath
  } deriving (Show)

data TrainClientOpts = TrainClientOpts
  { trainClientSomOut :: FilePath
  , trainClientSigmas :: [Float]
  } deriving (Show)

data StatsClientOpts = StatsClientOpts
  { statsClientOut :: FilePath
  } deriving (Show)

data Cmd
  = GenCmd GenOpts
  | TrainCmd TrainOpts InputOpts
  | StatsCmd StatsOpts InputOpts
  | SummaryCmd SummaryOpts InputOpts
  | AggregateCmd AggregateOpts
  | ServerCmd ServerOpts InputOpts
  | RemoteTrainCmd ClientOpts TrainClientOpts
  | RemoteStatsCmd ClientOpts StatsClientOpts
  deriving (Show)

opts :: Parser Cmd
opts = undefined

parseOpts :: IO Cmd
parseOpts = getArgs >>= execParser opts
