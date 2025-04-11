{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Opts where

import Control.Applicative (optional)
import Data.Version (showVersion)
import Options.Applicative
import Paths_remotesom (version)

somin =
  strOption
    $ long "som-in"
        <> short 'i'
        <> metavar "INFILE"
        <> help "read the SOM from this file"

somout =
  strOption
    $ long "som-out"
        <> short 'o'
        <> metavar "OUTFILE"
        <> help "write the output SOM into this file"

topoin =
  strOption
    $ long "topology-in"
        <> short 't'
        <> metavar "TOPOLOGY"
        <> help "read the SOM topology from this file"

topoout =
  strOption
    $ long "topology-out"
        <> short 'T'
        <> metavar "TOPOLOGY"
        <> help "write the SOM topology into this file"

data GenOpts = GenOpts
  { genX :: Int
  , genY :: Int
  , genDim :: Int
  , genSeed :: (Maybe Int)
  } deriving (Show)

genopts = do
  genX <-
    option auto
      $ long "som-x"
          <> short 'x'
          <> metavar "X"
          <> help "grid dimension 1"
          <> value 10
          <> showDefault
  genY <-
    option auto
      $ long "som-y"
          <> short 'y'
          <> metavar "Y"
          <> help "grid dimension 2"
          <> value 10
          <> showDefault
  genDim <-
    option auto
      $ long "dimension" <> short 'd' <> metavar "DIM" <> help "data dimension"
  genSeed <-
    optional . option auto
      $ long "seed"
          <> short 'S'
          <> metavar "SEED"
          <> help "fixed random generator seed"
  pure GenOpts {..}

data InputOpts = InputOpts
  { inputData :: FilePath
  , inputPoints :: Int
  } deriving (Show)

inopts = do
  inputData <-
    strOption
      $ long "in-data"
          <> short 'D'
          <> metavar "DATA"
          <> help "binary file with input data"
  inputPoints <-
    option auto
      $ long "in-points"
          <> short 'n'
          <> metavar "N"
          <> help "number of datapoints in the input file"
  pure InputOpts {..}

data TrainOpts = TrainOpts
  { trainSomIn :: Either (GenOpts, FilePath) (FilePath, FilePath)
  , trainSomOut :: FilePath
  , trainSigmas :: [Float]
  } deriving (Show)

sigma x =
  long "sigma"
    <> short 's'
    <> metavar "SIGMA"
    <> help ("neighborhood smoothing radius for training" ++ x)

trainopts = do
  trainSomIn <-
    Left <$> ((,) <$> genopts <*> topoout)
      <|> Right <$> ((,) <$> somin <*> topoin)
  trainSomOut <- somout
  trainSigmas <- many . option auto $ sigma " (may be specified multiple times)"
  pure TrainOpts {..}

data StatsOpts = StatsOpts
  { statsSomIn :: FilePath
  , statsOut :: FilePath
  } deriving (Show)

statsopts = do
  statsSomIn <- somin
  statsSomOut <- somout
  pure StatsOpts {..}

data SummaryOpts = SummaryOpts
  { summarySomIn :: FilePath
  , summaryOut :: FilePath
  } deriving (Show)

summaryopts = do
  summarySomIn <- somin
  summaryOut <-
    strOption
      $ long "summary-out"
          <> short 'O'
          <> metavar "OUTFILE"
          <> help "write the summary to this file"
  pure SummaryOpts {..}

data AggregateOpts = AggregateOpts
  { aggregateSummaryIn :: [FilePath]
  , aggregateTopoIn :: FilePath
  , aggregateSomOut :: FilePath
  , aggregateSigma :: Float
  } deriving (Show)

aggregateopts = do
  aggregateSummaryIn <-
    many . strOption
      $ long "summary-in"
          <> short 'I'
          <> metavar "INFILE"
          <> help
               "read the summary from this file (may be specified multiple times)"
  aggregateTopoIn <- topoin
  aggregateSomOut <- somout
  aggregateSigma <- option auto $ sigma ""
  pure AggregateOpts {..}

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
  = GenCmd GenOpts FilePath FilePath
  | TrainCmd TrainOpts InputOpts
  | StatsCmd StatsOpts InputOpts
  | SummaryCmd SummaryOpts InputOpts
  | AggregateCmd AggregateOpts
  | ServerCmd ServerOpts InputOpts
  | RemoteTrainCmd ClientOpts TrainClientOpts
  | RemoteStatsCmd ClientOpts StatsClientOpts
  deriving (Show)

cmds =
  [ ( "generate"
    , "Generate a new SOM"
    , GenCmd <$> genopts <*> somout <*> topoout)
  , ("train", "Train a SOM on data locally", TrainCmd <$> trainopts <*> inopts)
  , ( "stats"
    , "Generate stats from local data"
    , StatsCmd <$> statsopts <*> inopts)
  , ( "summarize"
    , "Summarize local data into a SOM training commitment"
    , SummaryCmd <$> summaryopts <*> inopts)
  , ( "aggregate"
    , "Aggregate multiple summarized commitments into a new SOM"
    , AggregateCmd <$> aggregateopts)
  ]

cmd :: Parser Cmd
cmd = hsubparser $ foldMap (\(c, d, p) -> command c (info p (progDesc d))) cmds

opts :: ParserInfo Cmd
opts =
  info
    (cmd <**> helper <**> simpleVersioner (showVersion version))
    (fullDesc
       <> header "remotesom -- federated training of self-organizing-maps"
       <> const mempty (footer "TODO license and contact"))

parseOpts :: IO Cmd
parseOpts = execParser opts
