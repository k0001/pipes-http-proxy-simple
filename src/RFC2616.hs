-- | Simple tools for parsing HTTP using Attoparsec.


-- Most of the this code is from RFC2616.hs, distributed as an example
-- along with the Attoparsec source code, by Bryan O'Sullivan.
-- BSD3 license. Copyright (c) Lennart Kolmodin.

{-# LANGUAGE OverloadedStrings #-}

module RFC2616
    (
      Header(..)
    , Request(..)
    , Response(..)
    , isToken
    , messageHeader
    , request
    , requestLine
    , response
    , responseLine
    , lowerHeader
    , headerFind
    , headerLookup
    , headerLookup1
    , renderRequest
    , renderResponse
    , renderHeader
    , renderHeaders
    , hostHeaderValue
    ) where

import           Control.Applicative
import           Data.Attoparsec as P
import           Data.Attoparsec.Char8 (char8, endOfLine, isDigit_w8)
import qualified Data.Attoparsec.Char8 as P8
import qualified Data.ByteString as B (map)
import qualified Data.ByteString.Char8 as B hiding (map)
import           Data.Monoid
import           Data.Word (Word8)

isToken :: Word8 -> Bool
isToken w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

skipSpaces :: Parser ()
skipSpaces = satisfy P8.isHorizontalSpace *> skipWhile P8.isHorizontalSpace

data Request = Request {
      requestMethod  :: !B.ByteString
    , requestUri     :: !B.ByteString
    , requestVersion :: !B.ByteString
    } deriving (Eq, Ord, Show)

httpVersion :: Parser B.ByteString
httpVersion = "HTTP/" *> P.takeWhile (\c -> isDigit_w8 c || c == 46)

requestLine :: Parser Request
requestLine = do
  method <- P.takeWhile1 isToken <* char8 ' '
  uri <- P.takeWhile1 (/=32) <* char8 ' '
  version <- httpVersion <* endOfLine
  return $! Request method uri version

data Header = Header {
      headerName  :: !B.ByteString
    , headerValue :: [B.ByteString]
    } deriving (Eq, Ord, Show)


messageHeader :: Parser Header
messageHeader = do
  header <- P.takeWhile isToken <* char8 ':' <* skipWhile P8.isHorizontalSpace
  body <- takeTill P8.isEndOfLine <* endOfLine
  bodies <- many $ skipSpaces *> takeTill P8.isEndOfLine <* endOfLine
  return $! Header header (body:bodies)

request :: Parser (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine


data Response = Response {
      responseVersion :: !B.ByteString
    , responseCode    :: !B.ByteString
    , responseMsg     :: !B.ByteString
    } deriving (Eq, Ord, Show)

responseLine :: Parser Response
responseLine = do
  version <- httpVersion <* char8 ' '
  code <- P.takeWhile isDigit_w8 <* char8 ' '
  msg <- P.takeTill P8.isEndOfLine <* endOfLine
  return $! Response version code msg

response :: Parser (Response, [Header])
response = (,) <$> responseLine <*> many messageHeader <* endOfLine

lowerHeader :: Header -> Header
lowerHeader (Header n vs) = Header (B.map toLower n) (map (B.map toLower) vs)
  where toLower w | w >= 65 && w <= 90 = w + 32
                  | otherwise          = w

headerFind :: (B.ByteString -> Bool) -> [Header] -> Maybe [B.ByteString]
headerFind _ []     = Nothing
headerFind p ((Header n vs):hs)
  | p n       = Just vs
  | otherwise = headerFind p hs

headerLookup :: B.ByteString -> [Header] -> Maybe [B.ByteString]
headerLookup x = headerFind (==x)

headerLookup1 :: B.ByteString -> [Header] -> Maybe B.ByteString
headerLookup1 x headers = case headerFind (==x) headers of
  Nothing    -> Nothing
  Just []    -> Nothing
  Just (y:_) -> Just y


renderRequest :: Request -> B.ByteString
renderRequest r = mconcat [ requestMethod  r, " "
                          , requestUri     r, " HTTP/"
                          , requestVersion r, "\r\n" ]

renderResponse :: Response -> B.ByteString
renderResponse r = mconcat [ "HTTP/", responseVersion r
                           , " "    , responseCode    r
                           , " "    , responseMsg     r
                           , "\r\n" ]

renderHeader :: Header -> B.ByteString
renderHeader (Header n vs) = n <> ": " <> mconcat (renderValue <$> vs)
  where renderValue v = " " <> v <> "\r\n"

renderHeaders :: [Header] -> B.ByteString
renderHeaders hs = B.concat (renderHeader <$> hs) <> "\r\n"


hostHeaderValue :: Parser (B.ByteString, Int)
hostHeaderValue = (,) <$> host <*> port
  where host = P8.takeTill (==':')
        port = P8.char ':' *> P8.decimal <|> pure 80
