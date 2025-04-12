{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Opts where

import Data.Version (showVersion)
import Options.Applicative
import Paths_remotesom (version)

somin :: Parser FilePath
somin =
  strOption
    $ long "som-in"
        <> short 'i'
        <> metavar "INFILE"
        <> help "read the SOM from this file"

somout :: Parser FilePath
somout =
  strOption
    $ long "som-out"
        <> short 'o'
        <> metavar "OUTFILE"
        <> help "write the output SOM into this file"

topoin :: Parser FilePath
topoin =
  strOption
    $ long "topology-in"
        <> short 't'
        <> metavar "TOPOLOGY"
        <> help "read the SOM topology from this file"

topoout :: Parser FilePath
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

dimopt :: Parser Int
dimopt =
  option auto
    $ long "dimension" <> short 'd' <> metavar "DIM" <> help "data dimension"

genopts :: Parser GenOpts
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
  genDim <- dimopt
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

inopts :: Parser InputOpts
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

sigmaSpec :: String -> Mod OptionFields Float
sigmaSpec x =
  long "sigma"
    <> short 's'
    <> metavar "SIGMA"
    <> help ("neighborhood smoothing radius for training" ++ x)

trainopts :: Parser TrainOpts
trainopts = do
  trainSomIn <-
    Left <$> ((,) <$> genopts <*> topoout)
      <|> Right <$> ((,) <$> somin <*> topoin)
  trainSomOut <- somout
  trainSigmas <-
    many . option auto . sigmaSpec
      $ " (should be specified multiple times --"
          ++ " each occurence implies a training epoch)"
  pure TrainOpts {..}

data StatsOpts = StatsOpts
  { statsSomIn :: FilePath
  , statsOut :: FilePath
  } deriving (Show)

statsopts :: Parser StatsOpts
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

summaryopts :: Parser SummaryOpts
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

aggregateopts :: Parser AggregateOpts
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
  aggregateSigma <- option auto $ sigmaSpec ""
  pure AggregateOpts {..}

data TrustOpts = TrustOpts
  { trustSystemCerts :: Bool
  , trustCerts :: [FilePath]
  , trustSkipAllValidation :: Bool
  } deriving (Show)

trustopts :: Parser TrustOpts
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
          <> metavar "STORE"
          <> help
               "trust peers who validate with a certificate found in the given store (a PEM file with several certificates, or a PEM store directory)"
  trustSkipAllValidation <-
    flag False True
      $ long "DEBUG-INSECURE-ALLOW-EVERYONE-IN"
          <> help
               ("skip all TLS validation of the remote,"
                  ++ " making debugging easier but potentially exposing data"
                  ++ " to random passer-by (and attackers)")
  pure TrustOpts {..}

data ClientTrustOpts = ClientTrustOpts
  { ctrustOpts :: TrustOpts
  , ctrustValidateServerHostname :: Bool
  , ctrustCommonName :: Maybe String
  } deriving (Show)

ctrustopts :: Parser ClientTrustOpts
ctrustopts = do
  ctrustOpts <- trustopts
  ctrustValidateServerHostname <-
    flag'
      True
      (long "validate-hostname"
         <> short 'v'
         <> help
              ("force validation of server hostname"
                 ++ " against certificate CommonName (default)"))
      <|> flag
            True
            False
            (long "no-validate-hostname"
               <> help "do not validate server hostname")
  ctrustCommonName <-
    optional . strOption
      $ long "server-common-name"
          <> short 'N'
          <> metavar "CN"
          <> help
               ("the common name to appear on a valid server certificate"
                  ++ " (defaults to server hostname)")
  return ClientTrustOpts {..}

data ServerOpts = ServerOpts
  { serverBindHost :: Maybe String
  , serverBindService :: String
  , serverCert :: FilePath
  , serverCertChain :: [FilePath]
  , serverKey :: FilePath
  , serverTrust :: TrustOpts
  } deriving (Show)

serveropts :: Parser ServerOpts
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

type ClientServers = [(String, String)]

data ClientOpts = ClientOpts
  { clientCert :: FilePath
  , clientCertChain :: [FilePath]
  , clientKey :: FilePath
  , clientTrust :: ClientTrustOpts
  } deriving (Show)

parseHostService :: String -> (String, String)
parseHostService hs =
  case words hs of
    [host] -> (host, "21012")
    [host, service] -> (host, service)
    _ -> error $ "could not parse server address: " ++ hs

clientservers :: Parser ClientServers
clientservers =
  many . fmap parseHostService . strArgument
    $ metavar "SERVER"
        <> help
             ("server(s) to connect to, in format HOSTNAME"
                ++ " (defaulting to service port 21012),"
                ++ " or space-separated \"HOSTNAME SERVICE\""
                ++ " (e.g. \"www.example.com 21012\")")

clientopts :: Parser ClientOpts
clientopts = do
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
  | ServerCmd ServerOpts InputOpts Int
  | ClientTrainCmd ClientServers ClientOpts TrainOpts
  | ClientStatsCmd ClientServers ClientOpts StatsOpts
  deriving (Show)

cmds :: [(String, String, Parser Cmd)]
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
    , "Run a server that computes SOM updates from local data"
        ++ " for clients who train their own SOMs"
    , ServerCmd <$> serveropts <*> inopts <*> dimopt)
  , ( "train-client"
    , "Train a SOM using data on remote servers"
    , ClientTrainCmd <$> clientservers <*> clientopts <*> trainopts)
  , ( "stats-client"
    , "Generate stats from data on remote servers"
    , ClientStatsCmd <$> clientservers <*> clientopts <*> statsopts)
  ]

cmd :: Parser Cmd
cmd = hsubparser $ foldMap (\(c, d, p) -> command c (info p (progDesc d))) cmds

parseOpts :: IO Cmd
parseOpts =
  execParser
    $ info
        (cmd <**> helper <**> simpleVersioner (showVersion version))
        (fullDesc
           <> header "remotesom -- federated training of self-organizing-maps"
           <> const mempty (footer "TODO license and contact"))
