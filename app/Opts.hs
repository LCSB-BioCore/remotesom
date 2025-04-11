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
          <> short 'R'
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
  statsOut <-
    strOption
      $ long "out-stats"
          <> short 'S'
          <> metavar "OUTFILE"
          <> help "write the statistics to this file"
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

data TrustOpts = TrustOpts
  { trustSystemCerts :: Bool
  , trustCerts :: [FilePath]
  } deriving (Show)

trustopts = do
  trustSystemCerts <-
    flag'
      False
      (long "no-trust-system-ca"
         <> help "avoid validation against system certificates (default)")
      <|> flag
            False
            True
            (long "trust-system-ca"
               <> help "enable using system CA certificates for validation")
  trustCerts <-
    many . strOption
      $ long "accept-certificate"
          <> short 'a'
          <> metavar "PEM"
          <> help "trust peers who validate with a given certificate"
  pure TrustOpts {..}

data ClientTrustOpts = ClientTrustOpts
  { ctrustOpts :: TrustOpts
  , ctrustValidateServerHostname :: Bool
  } deriving (Show)

ctrustopts = do
  ctrustOpts <- trustopts
  ctrustValidateServerHostname <-
    flag'
      True
      (long "validate-hostname"
         <> short 'v'
         <> help
              "force validation of server hostname against certificate CommonName (default)")
      <|> flag
            True
            False
            (long "no-validate-hostname"
               <> help "do not validate server hostname")
  return ClientTrustOpts {..}

data ServerOpts = ServerOpts
  { serverBindHost :: Maybe String
  , serverBindService :: String
  , serverCert :: FilePath
  , serverCertChain :: [FilePath]
  , serverKey :: FilePath
  , serverTrust :: TrustOpts
  } deriving (Show)

serveropts = do
  serverBindHost <-
    optional . strOption
      $ long "bind"
          <> short 'B'
          <> metavar "HOST"
          <> help "optional host name to bind to"
  serverBindService <-
    strOption
      $ long "service"
          <> short 'p'
          <> metavar "PORT"
          <> value "21012"
          <> showDefault
          <> help "service identifier to serve (typically a port number)"
  serverCert <-
    strOption
      $ long "cert"
          <> short 'c'
          <> metavar "PEM"
          <> help "TLS server certificate"
  serverCertChain <-
    many . strOption
      $ long "cert-chain"
          <> short 'C'
          <> metavar "PEM"
          <> help
               "TLS server certificate chain (may be specified multiple times)"
  serverKey <-
    strOption
      $ long "key"
          <> short 'k'
          <> metavar "PEM"
          <> help "TLS server private key"
  serverTrust <- trustopts
  pure ServerOpts {..}

data ClientOpts = ClientOpts
  { clientServerHost :: String
  , clientServerService :: String
  , clientCert :: FilePath
  , clientCertChain :: [FilePath]
  , clientKey :: FilePath
  , clientTrust :: ClientTrustOpts
  } deriving (Show)

clientopts = do
  clientServerHost <-
    strArgument $ metavar "HOST" <> help "host name to connect to"
  clientServerService <-
    strOption
      $ long "service"
          <> short 'p'
          <> metavar "PORT"
          <> value "21012"
          <> showDefault
          <> help "service identifier to connect to (typically a port number)"
  clientCert <-
    strOption
      $ long "cert"
          <> short 'c'
          <> metavar "PEM"
          <> help "TLS client certificate"
  clientCertChain <-
    many . strOption
      $ long "cert-chain"
          <> short 'C'
          <> metavar "PEM"
          <> help
               "TLS client certificate chain (may be specified multiple times)"
  clientKey <-
    strOption
      $ long "key"
          <> short 'k'
          <> metavar "PEM"
          <> help "TLS client private key"
  clientTrust <- ctrustopts
  pure ClientOpts {..}

data Cmd
  = GenCmd GenOpts FilePath FilePath
  | TrainCmd TrainOpts InputOpts
  | StatsCmd StatsOpts InputOpts
  | SummaryCmd SummaryOpts InputOpts
  | AggregateCmd AggregateOpts
  | ServerCmd ServerOpts InputOpts
  | ClientTrainCmd ClientOpts TrainOpts
  | ClientStatsCmd ClientOpts StatsOpts
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
  , ( "server"
    , "Run a server that computes SOM updates from local data for clients who train their own SOMs"
    , ServerCmd <$> serveropts <*> inopts)
  , ( "train-client"
    , "Train a SOM using data on remote servers"
    , ClientTrainCmd <$> clientopts <*> trainopts)
  , ( "stats-client"
    , "Generate stats from data on remote servers"
    , ClientStatsCmd <$> clientopts <*> statsopts)
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
