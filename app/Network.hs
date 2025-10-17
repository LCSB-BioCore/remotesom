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
module Network where

import Opts

import Control.Exception (SomeException, catch, throwIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.String (fromString)
import qualified Data.X509 as X509
import qualified Data.X509.CertificateStore as X509
import qualified Data.X509.Validation as V
import qualified Network.Run.TCP as TCP
import qualified Network.Socket as S
import qualified Network.TLS as TLS
import System.IO (hPutStrLn, stderr)
import qualified System.X509 as X509

readTillNewline :: TLS.Context -> IO LB.ByteString
readTillNewline ctx = go []
  where
    newlineChar = 10
    go buf = TLS.recvData ctx >>= cont buf
    cont buf x
      | B.null x =
        error
          $ "EOF received before newline was read, so far had chunks: "
              ++ show (map B.length buf)
      | Just ix <- B.elemIndex newlineChar x =
        pure . LB.fromChunks . reverse $ B.take ix x : buf
      | otherwise = go (x : buf)

readCACerts :: TrustOpts -> IO X509.CertificateStore
readCACerts topts = do
  let readCertStore s = do
        c <- X509.readCertificateStore s
        case c of
          Just r -> pure r
          Nothing -> error $ "failed to read certificate store: " ++ s
  sys <-
    if trustSystemCerts topts
      then X509.getSystemCertificateStore
      else mempty
  local <- mconcat <$> traverse readCertStore (trustCerts topts)
  pure $ sys <> local

bracketShowError :: IO a -> IO a
bracketShowError =
  flip catch $ \e -> do
    hPutStrLn stderr $ show (e :: SomeException)
    throwIO e

interactServer :: ServerOpts -> (LB.ByteString -> IO LB.ByteString) -> IO ()
interactServer opts oracle = do
  cred <-
    either (error "failed to load server TLS credentials") id
      <$> TLS.credentialLoadX509Chain
            (serverCert opts)
            (serverCertChain opts)
            (serverKey opts)
  cacerts <- readCACerts (serverTrust opts)
  TCP.runTCPServer (serverBindHost opts) (serverBindService opts) $ \sock ->
    bracketShowError $ do
      let p = TLS.defaultParamsServer
          h = TLS.serverHooks p
          sh = TLS.serverShared p
          validate
            | trustSkipAllValidation $ serverTrust opts =
              \_ -> do
                putStrLn
                  "WARNING: TLS validation skipped, letting a random person in!"
                pure TLS.CertificateUsageAccept
            | otherwise =
              TLS.validateClientCertificate cacerts TLS.defaultValidationCache
      ctx <-
        TLS.contextNew
          sock
          p
            { TLS.serverHooks = h {TLS.onClientCertificate = validate}
            , TLS.serverWantClientCert = True
            , TLS.serverShared =
                sh {TLS.sharedCredentials = TLS.Credentials [cred]}
            }
      TLS.handshake ctx
      q <- readTillNewline ctx
      r <- oracle q
      TLS.sendData ctx r
      TLS.sendData ctx $ fromString "\n"
      TLS.bye ctx
      S.close sock

runClientQuery ::
     ClientConnect -> ClientOpts -> LB.ByteString -> IO LB.ByteString
runClientQuery conn opts query = do
  cred <-
    either (error "failed to load client TLS credentials") id
      <$> TLS.credentialLoadX509Chain
            (clientCert opts)
            (clientCertChain opts)
            (clientKey opts)
  let ctopts = connTrust conn
      topts = ctrustOpts ctopts
  cacerts <- readCACerts topts
  TCP.runTCPClient (connHost conn) (connService conn) $ \sock -> do
    let p =
          TLS.defaultParamsClient
            (maybe (connHost conn) id $ ctrustCommonName ctopts)
            mempty
        h = TLS.clientHooks p
        sh = TLS.clientShared p
        validate
          | trustSkipAllValidation topts =
            \_ _ _ _ -> do
              putStrLn
                "WARNING: TLS validation skipped, talking to a random person!"
              pure []
          | otherwise =
            V.validate
              X509.HashSHA256
              V.defaultHooks
              V.defaultChecks
                {V.checkFQHN = ctrustValidateServerHostname ctopts}
    ctx <-
      TLS.contextNew sock
        $ p
            { TLS.clientHooks =
                h
                  { TLS.onServerCertificate = validate
                  , TLS.onCertificateRequest = const . pure $ Just cred
                  }
            , TLS.clientShared = sh {TLS.sharedCAStore = cacerts}
            }
    TLS.handshake ctx
    TLS.sendData ctx query
    TLS.sendData ctx $ fromString "\n"
    res <- readTillNewline ctx
    TLS.bye ctx
    S.close sock
    return res
