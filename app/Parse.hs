{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse (ParseError, parseHttpReq) where

import Control.Monad (void)
import Data.Attoparsec.ByteString (
    Parser,
    parseOnly,
    string,
    takeWhile1,
 )
import Data.Attoparsec.ByteString.Char8 (space)
import Data.ByteString qualified as BS
import Data.Functor (($>))
import Data.Word8 (isSpace)
import Types (HttpMethod (..), HttpRequest (..))

type ParseError = String

httpRequest :: Parser HttpRequest
httpRequest = do
    method <- httpMethod
    _ <- space
    path <- httpPath
    _ <- space
    version <- httpVersion
    crlf
    return $ HttpRequest{..}

httpMethod :: Parser HttpMethod
httpMethod = string "GET" $> GET

httpPath :: Parser BS.ByteString
httpPath = takeWhile1 (not . isSpace)

httpVersion :: Parser BS.ByteString
httpVersion = string "HTTP/1.1"

crlf :: Parser ()
crlf = void (string "\r\n")

parseHttpReq :: BS.ByteString -> Either String HttpRequest
parseHttpReq = parseOnly httpRequest
