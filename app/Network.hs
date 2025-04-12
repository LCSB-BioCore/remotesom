module Network where

import Opts

import qualified Data.X509.Validation as V
import qualified Network.Run.TCP as TCP
import qualified Network.Socket as S
import qualified Network.TLS as TLS

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.String (fromString)
import Data.Word (Word8)

newlineChar :: Word8
newlineChar = 10

readTillNewline :: TLS.Context -> IO LB.ByteString
readTillNewline ctx = go []
  where
    go buf = do
      x <- TLS.recvData ctx
      case () of
        _
          | B.null x -> error "EOF received before newline was read"
          | Just ix <- B.elemIndex newlineChar x ->
            pure . LB.fromChunks . reverse $ B.take ix x : buf
          | otherwise -> go (x : buf)

interactServer :: ServerOpts -> (LB.ByteString -> IO LB.ByteString) -> IO ()
interactServer opts oracle = do
  cred <-
    either (error "failed to load server TLS credentials") id
      <$> TLS.credentialLoadX509Chain
            (serverCert opts)
            (serverCertChain opts)
            (serverKey opts)
  TCP.runTCPServer (serverBindHost opts) (serverBindService opts) $ \sock -> do
    -- TODO this discards all errors, print them out instead
    let p = TLS.defaultParamsServer
        h = TLS.serverHooks p
        sh = TLS.serverShared p
        validate
          | trustSkipAllValidation $ serverTrust opts =
            \_ -> do
              putStrLn
                "warning: TLS validation skipped, letting a random person in!"
              pure TLS.CertificateUsageAccept
          | otherwise = error "TODO validate!"
    ctx <-
      TLS.contextNew
        sock
        p
          { TLS.serverHooks = h {TLS.onClientCertificate = validate}
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
  TCP.runTCPClient host service $ \sock -> do
    let p = TLS.defaultParamsClient host mempty
        h = TLS.clientHooks p
        validate
          | trustSkipAllValidation . ctrustOpts $ clientTrust opts =
            \_ _ _ _ -> do
              putStrLn
                "warning: TLS validation skipped, talking to a random person!"
              pure []
          | otherwise = V.validateDefault
    ctx <-
      TLS.contextNew sock
        $ p
            { TLS.clientHooks =
                h
                  { TLS.onServerCertificate = validate
                  , TLS.onCertificateRequest = \_ -> pure (Just cred)
                  }
            }
    TLS.handshake ctx
    TLS.sendData ctx query
    TLS.sendData ctx $ fromString "\n"
    res <- readTillNewline ctx
    TLS.bye ctx
    S.close sock
    return res
