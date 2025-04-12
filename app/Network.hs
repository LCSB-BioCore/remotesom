module Network where

import Opts

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.String (fromString)
import qualified Network.Run.TCP as TCP
import qualified Network.Socket as S

import qualified Data.X509 as X509
import qualified Data.X509.CertificateStore as X509
import qualified Data.X509.Validation as V
import qualified Network.TLS as TLS
import qualified System.X509 as X509

readTillNewline :: TLS.Context -> IO LB.ByteString
readTillNewline ctx = go []
  where
    newlineChar = 10
    go buf = do
      x <- TLS.recvData ctx
      case () of
        _
          | B.null x -> error "EOF received before newline was read"
          | Just ix <- B.elemIndex newlineChar x ->
            pure . LB.fromChunks . reverse $ B.take ix x : buf
          | otherwise -> go (x : buf)

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

interactServer :: ServerOpts -> (LB.ByteString -> IO LB.ByteString) -> IO ()
interactServer opts oracle = do
  cred <-
    either (error "failed to load server TLS credentials") id
      <$> TLS.credentialLoadX509Chain
            (serverCert opts)
            (serverCertChain opts)
            (serverKey opts)
  cacerts <- readCACerts (serverTrust opts)
  TCP.runTCPServer (serverBindHost opts) (serverBindService opts) $ \sock -> do
    -- TODO this is bracketed to discard all errors, print them out instead
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
     (String, String) -> ClientOpts -> LB.ByteString -> IO LB.ByteString
runClientQuery (host, service) opts query = do
  cred <-
    either (error "failed to load client TLS credentials") id
      <$> TLS.credentialLoadX509Chain
            (clientCert opts)
            (clientCertChain opts)
            (clientKey opts)
  let ctopts = clientTrust opts
      topts = ctrustOpts ctopts
  cacerts <- readCACerts topts
  TCP.runTCPClient host service $ \sock -> do
    let p =
          TLS.defaultParamsClient
            (maybe host id $ ctrustCommonName ctopts)
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
