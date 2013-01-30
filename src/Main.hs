{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Control.Applicative
import           Control.Error
import           qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.Trans.Class        (lift)
import           Control.Proxy                    ((<-<), (>->))
import qualified Control.Proxy                    as P
import qualified Control.Proxy.Attoparsec         as PA
import qualified Control.Proxy.Network.TCP        as PN
import qualified Control.Proxy.Network.TCP.Simple as PN
import qualified Control.Proxy.Safe               as P
import qualified Control.Proxy.Trans.Attoparsec   as PA
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Char8            as B
import           Data.Monoid
import qualified RFC2616                          as R


hvContentLength :: Integral a => [R.Header] -> Maybe a
hvContentLength = mayParse AB.decimal . R.headerLookup1 "Content-Length"

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
  => () -> P.Pipe (PA.AttoparsecP B.ByteString (P.ExceptionP p)) B.ByteString B.ByteString P.SafeIO r
proxyReqD = forever . go where
  go () = do
     mreqHead <- (P.unitU <-< const reqHeadC) ()
     case mreqHead of
       Nothing -> go () -- ^ Figure out what to do with leftovers
       Just rh -> do
         io . putStrLn $ "Got request: " <> show rh
         let hs = R.Header "X-Proxy" ["Happy123"] : reqhHeaders rh
             source () = do
                P.respond $ R.renderRequest (reqhReqLine rh) <> R.renderHeaders hs
                PA.takeInputWithLeftoversD (maybe 0 id $ reqhContentLength rh) ()
             (host, port) = (B.unpack $ reqhHost rh, reqhPort rh)
         econn <- io . E.try $ PN.connect host port
         case econn of
           Left (ex :: E.SomeException) -> do
             io . putStrLn $ "Can't connect " <> show (host,port) <> ": " <> show ex
           Right (sock, addr) -> do
             io . putStrLn $ "Connected to " <> show host <> " (" <> show addr <> ")"
             (source >-> (P.mapP . P.tryK) (PN.socketConsumer sock) >-> P.unitU) ()
             (P.unitD >-> (P.mapP . P.tryK) (PN.socketProducer 4096 sock) >-> proxyRespD) ()
         go ()
  io = P.liftP . P.tryIO

proxyRespD
  :: P.CheckP p
  => () -> P.Pipe (PA.AttoparsecP B.ByteString (P.ExceptionP p)) B.ByteString B.ByteString P.SafeIO ()
proxyRespD () = go where
  go = do
     mrespHead <- (P.unitU <-< const respHeadC) ()
     case mrespHead of
       Nothing -> return () -- ^ Figure out what to do with leftovers
       Just rh -> do
         io . putStrLn $ "Got response: " <> show rh
         let hs = R.Header "X-Proxy" ["Hello234"] : resphHeaders rh
         P.respond $ R.renderResponse (resphRespLine rh)
                  <> R.renderHeaders hs
         case resphContentLength rh of
           Nothing  -> P.idT ()
           Just len -> PA.takeInputWithLeftoversD len ()
  io = P.liftP . P.tryIO

main :: IO ()
main = do
  let settings = PN.ServerSettings Nothing 8001
  putStrLn $ "Server settings: " <> show settings
  PN.runServer settings $ \(addr, src, dst) -> do
    putStrLn $ "Got a connection from " <> show addr
    let foo = (P.mapP . P.tryK) src >-> proxyReqD >-> (P.mapP . P.tryK) dst
        t = P.runProxy . P.runEitherK . PA.runParseK Nothing $ foo
    P.runSafeIO t
    return ()


--------------------------------------------------------------------------------
-- Boring stuff

mayParse :: AB.Parser a -> Maybe B.ByteString -> Maybe a
mayParse _ Nothing  = Nothing
mayParse p (Just x) = hush $ AB.parseOnly p x
