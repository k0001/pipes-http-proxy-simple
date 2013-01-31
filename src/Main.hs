{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.Trans.Class        (lift)
import           Control.Proxy                    ((<-<), (>->))
import           Data.Monoid
import qualified Control.Exception as E
import qualified Control.Proxy                    as P
import qualified Control.Proxy.Safe               as P
import qualified Control.Proxy.Attoparsec         as PA
import qualified Control.Proxy.Trans.Attoparsec   as PA
import qualified Control.Proxy.Network.TCP        as PN
import qualified Control.Proxy.Network.TCP.Simple as PN
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Char8            as B
import           Data.Char                        (toLower)
import qualified RFC2616                          as R
import           Network.URI



data ReqHead = ReqHead
  { reqhHost          :: B.ByteString
  , reqhPort          :: Int
  , reqhReqLine       :: R.Request
  , reqhHeaders       :: [R.Header]
  , reqhContentLength :: Maybe Int
  } deriving (Show, Eq)


reqHeadC
 :: (Monad m, P.Proxy p)
 => PA.ParseP PA.ParserError (Maybe B.ByteString) p () B.ByteString () P.C m (Maybe ReqHead)
reqHeadC = runMaybeT $ do
   reqLine      <- lift . PA.parsePC $ R.requestLine
   reqHeaders   <- lift . PA.parsePC $ many R.messageHeader <* AB.endOfLine
   (host, port) <- hoistMaybe $ hvHost reqHeaders
   let contentLength = hvContentLength reqHeaders
   return $ ReqHead host port reqLine reqHeaders contentLength
 where
   hvHost = mayParse R.hostHeaderValue . R.headerLookup1 "Host"


data RespHead = RespHead
  { resphRespLine      :: R.Response
  , resphHeaders       :: [R.Header]
  , resphContentLength :: Maybe Int
  } deriving (Show, Eq)


respHeadC
 :: (Monad m, P.Proxy p)
 => PA.ParseP PA.ParserError (Maybe B.ByteString) p () B.ByteString () P.C m (Maybe RespHead)
respHeadC = runMaybeT $ do
   respLine      <- lift . PA.parsePC $ R.responseLine
   respHeaders   <- lift . PA.parsePC $ many R.messageHeader <* AB.endOfLine
   let contentLength = hvContentLength respHeaders
   return $ RespHead respLine respHeaders contentLength

proxyReqD
  :: P.CheckP p
  => () -> P.Pipe (P.ExceptionP (PA.AttoparsecP B.ByteString p)) B.ByteString B.ByteString P.SafeIO r
proxyReqD () = go where
  go = do
     mreqHead <- P.liftP $ (P.unitU <-< const reqHeadC) ()
     case mreqHead of
       Nothing -> go -- ^ Figure out what to do with leftovers
       Just rh -> do
         let outgoing () = do
               P.respond $ R.renderRequest (toFullPathRequest $ reqhReqLine rh)
                        <> R.renderHeaders (proxiedReqHeaders $ reqhHeaders rh)
               PA.takeInputWithLeftoversD (maybe 0 id $ reqhContentLength rh) ()
             (host, port) = (B.unpack $ reqhHost rh, reqhPort rh)
         PN.withClient (PN.ClientSettings host port) $ \(sock,_addr) -> do
           let src = (P.raisePK . P.tryK) (PN.socketProducer 4096 sock)
               dst = (P.raisePK . P.tryK) (PN.socketConsumer sock)
               send = P.mapP outgoing >-> dst >-> P.unitU
               recv = P.unitD >-> src >-> proxyRespD
           send () >> recv ()
         go


proxyRespD
  :: P.CheckP p
  => () -> P.Pipe (P.ExceptionP (PA.AttoparsecP B.ByteString p)) B.ByteString B.ByteString P.SafeIO ()
proxyRespD () = go where
  go = do
     mrespHead <- P.liftP $ (P.unitU <-< const respHeadC) ()
     case mrespHead of
       Nothing -> return () -- ^ Figure out what to do with leftovers
       Just rh -> do
         P.respond $ R.renderResponse (resphRespLine rh)
                  <> R.renderHeaders  (resphHeaders rh)
         case resphContentLength rh of
           Nothing  -> forever $ P.request () >>= P.respond
           Just len -> P.liftP (PA.takeInputWithLeftoversD len ())


main :: IO ()
main = do
  let settings = PN.ServerSettings Nothing 8080
  putStrLn $ "Server settings: " <> show settings
  PN.runServer settings $ \(addr, src, dst) -> do
    let info m = putStrLn $ "Client " <> show addr <> ": " <> m
    info "Opened"

    let src' = P.raisePK . P.tryK $ src
        dst' = P.raisePK . P.tryK $ dst
        p0   = src' >-> proxyReqD >-> dst'
        runp = P.runProxy . PA.runParseK Nothing . P.runEitherK

    -- eerr ::  (Either PA.ParserError (Either P.SomeException ()), Maybe B.ByteString)
    (eerr, mleftovers) <- P.trySafeIO $ runp p0
    info "Done"
    case mleftovers of
      Nothing -> return ()
      Just lo -> info $ "Leftovers: " <> B.unpack lo
    case eerr of
      Left pa -> info $ show pa
      Right (Right ()) -> return ()
      Right (Left ex) -> info (show ex) >> E.throwIO ex



--------------------------------------------------------------------------------
-- Boring stuff

mayParse :: AB.Parser a -> Maybe B.ByteString -> Maybe a
mayParse _ Nothing  = Nothing
mayParse p (Just x) = hush $ AB.parseOnly p x

uriFullPath :: URI -> String
uriFullPath u = uriPath u <> uriQuery u <> uriFragment u

toFullPathRequest :: R.Request -> R.Request
toFullPathRequest req =
    case parseURIReference (B.unpack $ R.requestUri req) of
            Nothing -> req
            Just u  -> req { R.requestUri = B.pack $ uriFullPath u }

skippableReqHeader :: B.ByteString -> Bool
skippableReqHeader n = f1 || f2 where
    n' = B.map toLower n
    f1 = n' == "connection"
    f2 = "proxy-" `B.isPrefixOf` n'

proxiedReqHeaders :: [R.Header] -> [R.Header]
proxiedReqHeaders = R.headerFilter (not . skippableReqHeader)

hvContentLength :: Integral a => [R.Header] -> Maybe a
hvContentLength = mayParse AB.decimal . R.headerLookup1 "Content-Length"
