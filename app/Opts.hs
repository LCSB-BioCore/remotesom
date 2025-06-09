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

data SomShape
  = SomRectangle Int Int
  | SomHex Int Int Int
  | SomTorus Int Int
  | SomCircle Int Int
  deriving (Show)

somshape :: Parser SomShape
somshape =
  asum
    [ SomRectangle
        <$> (option auto
               $ long "grid-x"
                   <> short 'x'
                   <> metavar "X"
                   <> help "rectangular grid dimension 1"
                   <> value 10
                   <> showDefault)
        <*> (option auto
               $ long "grid-y"
                   <> short 'y'
                   <> metavar "Y"
                   <> help "rectangular grid dimension 2"
                   <> value 10
                   <> showDefault)
    , SomHex
        <$> (option auto
               $ long "hex-x"
                   <> metavar "X"
                   <> help "hexagonal grid dimension 1")
        <*> (option auto
               $ long "hex-y"
                   <> metavar "Y"
                   <> help "hexagonal grid dimension 2")
        <*> (option auto
               $ long "hex-z"
                   <> metavar "Z"
                   <> help "hexagonal grid dimension 3"
                   <> value 1
                   <> showDefault)
    , SomTorus
        <$> (option auto
               $ long "torus-x"
                   <> metavar "X"
                   <> help "toroidal grid dimension 1")
        <*> (option auto
               $ long "torus-y"
                   <> metavar "Y"
                   <> help "toroidal grid dimension 2"
                   <> value 1
                   <> showDefault)
    , SomCircle
        <$> (option auto
               $ long "circle-length"
                   <> metavar "X"
                   <> help "circular grid circumference")
        <*> (option auto
               $ long "circle-width"
                   <> metavar "Y"
                   <> help "circular grid strip width"
                   <> value 1
                   <> showDefault)
    ]

data GenOpts = GenOpts
  { genShape :: SomShape
  , genDim :: Int
  , genSeed :: (Maybe Int)
  } deriving (Show)

dimopt :: Parser Int
dimopt =
  option auto
    $ long "dimension" <> short 'd' <> metavar "DIM" <> help "data dimension"

genopts :: Parser GenOpts
genopts = do
  genShape <- somshape
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

data MedianOpts = MedianOpts
  { mediansOut :: FilePath
  , mediansBounds :: (Float, Float)
  , mediansIters :: Int
  } deriving (Show)

medianopts :: Parser MedianOpts
medianopts = do
  mediansOut <-
    strOption
      $ long "out-medians"
          <> metavar "OUTFILE"
          <> help "write per-cluster medians to this file"
  mediansBounds <-
    (,)
      <$> option
            auto
            (long "median-min"
               <> metavar "LB"
               <> help "lower bound for the median approximation")
      <*> option
            auto
            (long "median-max"
               <> metavar "UB"
               <> help "upper bound for the median approximation")
  mediansIters <-
    option auto
      $ long "median-iters"
          <> metavar "N"
          <> help "nummber of iterations of the median approximation"
  pure MedianOpts {..}

data StatsOpts = StatsOpts
  { statsSomIn :: FilePath
  , statsMeansOut :: Maybe FilePath
  , statsCountsOut :: Maybe FilePath
  , statsVariancesOut :: Maybe FilePath
  , statsMedians :: Maybe MedianOpts
  } deriving (Show)

statsopts :: Parser StatsOpts
statsopts = do
  statsSomIn <- somin
  statsMeansOut <-
    optional . strOption
      $ long "out-means"
          <> metavar "OUTFILE"
          <> help "write the cluster means to this file"
  statsCountsOut <-
    optional . strOption
      $ long "out-counts"
          <> metavar "OUTFILE"
          <> help "write the cluster occupancy counts to this file"
  statsVariancesOut <-
    optional . strOption
      $ long "out-variances"
          <> metavar "OUTFILE"
          <> help "write the intra-cluster variances to this file"
  statsMedians <- optional medianopts
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

type ClientServers = [ClientConnect]

clientservers :: Parser ClientServers
clientservers =
  many . hsubparser . command "connect" . info clientconnect
    $ progDesc "specify a connection to server"

data ClientConnect = ClientConnect
  { connHost :: String
  , connService :: String
  , connTrust :: ClientTrustOpts
  } deriving (Show)

clientconnect :: Parser ClientConnect
clientconnect = do
  connHost <-
    strArgument $ metavar "HOST" <> help "server hostname to connect to"
  connService <-
    strOption
      $ long "service"
          <> short 'p'
          <> metavar "PORT"
          <> help
               "numeric port or string service identifier to connect to on HOST"
          <> value "21012"
          <> showDefault
  connTrust <- ctrustopts
  pure ClientConnect {..}

data ClientOpts = ClientOpts
  { clientCert :: FilePath
  , clientCertChain :: [FilePath]
  , clientKey :: FilePath
  } deriving (Show)

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
  pure ClientOpts {..}

data SubsetSpec
  = SubsetFile FilePath
  | SubsetIndex Int
  deriving (Show)

subsetspec :: Parser SubsetSpec
subsetspec =
  asum
    [ fmap SubsetFile . strOption
        $ long "subset-file"
            <> short 'S'
            <> metavar "JSON"
            <> help "load the neighborhood indexes from a JSON array in a file"
    , fmap SubsetIndex . option auto
        $ short 's'
            <> long "subset"
            <> metavar "INDEX"
            <> help
                 "include this neigborhood index in the subset (can be specified repeatedly)"
    ]

data SubsetOpts = SubsetOpts
  { soSpecs :: [SubsetSpec]
  , soCustomData :: Maybe (FilePath, Int)
  , soMemberOutput :: Maybe FilePath
  , soOutput :: Maybe FilePath
  } deriving (Show)

subsetopts :: Parser SubsetOpts
subsetopts = do
  soSpecs <- many subsetspec
  soCustomData <-
    optional
      $ (,)
          <$> strOption
                (short 'I'
                   <> long "in-set"
                   <> metavar "SETFILE"
                   <> help
                        ("instead of subsetting the input data points,"
                           ++ " subset the entries in this file"
                           ++ " (useful for subsetting various metadata attached to points)"))
          <*> option
                auto
                (short 'b'
                   <> long "entry-size"
                   <> metavar "BYTES"
                   <> help "byte size of each entry in the SETFILE")
  soOutput <-
    optional . strOption
      $ short 'O'
          <> long "out-subset"
          <> metavar "OUTFILE"
          <> help
               ("write the data subset to this file"
                  ++ " and print out the amount of entries written")
  soMemberOutput <-
    optional . strOption
      $ short 'M'
          <> long "out-neighborhoods"
          <> metavar "OUTFILE"
          <> help
               ("also write the neighborhood indexes for each of the files into this file"
                  ++ " (the file is formatted as an array of 4-byte integers in host byte order)")
  pure SubsetOpts {..}

data Cmd
  = GenCmd GenOpts FilePath FilePath
  | TrainCmd TrainOpts InputOpts
  | StatsCmd StatsOpts InputOpts
  | SummaryCmd SummaryOpts InputOpts
  | AggregateCmd AggregateOpts
  | ServerCmd ServerOpts InputOpts Int
  | ClientTrainCmd ClientServers ClientOpts TrainOpts
  | ClientStatsCmd ClientServers ClientOpts StatsOpts
  | SubsetCmd SubsetOpts InputOpts String
  deriving (Show)

-- TODO: it might be viable to also have "manual" commands for the medians
-- approximator, if anyone would ever like to run that by hand.
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
  , ( "subset"
    , "Reduce datasets to the selected SOM neighborhoods and print the size of reduced dataset"
    , SubsetCmd <$> subsetopts <*> inopts <*> somin)
  ]

cmd :: Parser Cmd
cmd = hsubparser $ foldMap (\(c, d, p) -> command c (info p (progDesc d))) cmds

parseOpts :: IO Cmd
parseOpts =
  customExecParser (prefs $ showHelpOnEmpty <> helpShowGlobals)
    $ info
        (cmd <**> helper <**> simpleVersioner (showVersion version))
        (fullDesc
           <> header "remotesom -- federated training of self-organizing-maps"
           <> (footer
                 $ "Copyright (c) University of Luxembourg."
                     ++ " remotesom is developed at Luxembourg Centre for Systems Biomedicine,"
                     ++ " and distributed under the terms of Apache-2.0 license."
                     ++ " See https://github.com/LCSB-BioCore/remotesom for details and source."))
