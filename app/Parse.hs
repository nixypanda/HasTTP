{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse (ParseError, parseHttpReq) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.ByteString (
    Parser,
    option,
    parseOnly,
    sepBy,
    string,
    takeByteString,
    takeWhile1,
 )
import Data.Attoparsec.ByteString.Char8 (space)
import Data.ByteString qualified as BS
import Data.Functor (($>))
import Data.Word8 (isSpace, _colon, _cr)
import Types (HttpHeader (..), HttpMethod (..), HttpRequest (..))

type ParseError = String

httpRequest :: Parser HttpRequest
httpRequest = do
    method <- httpMethod
    _ <- space
    path <- httpPath
    _ <- space
    version <- httpVersion
    crlf
    reqHeaders <- option [] $ sepBy httpHeader crlf
    crlf
    crlf
    reqContent <- option "" takeByteString
    return $ HttpRequest{..}

httpMethod :: Parser HttpMethod
httpMethod = string "GET" $> GET <|> string "POST" $> POST

httpPath :: Parser BS.ByteString
httpPath = takeWhile1 (not . isSpace)

httpVersion :: Parser BS.ByteString
httpVersion = string "HTTP/1.1"

httpHeader :: Parser HttpHeader
httpHeader = do
    name <- takeWhile1 (/= _colon)
    _ <- string ":"
    _ <- space
    value <- takeWhile1 (/= _cr)
    return $ HttpHeader name value

crlf :: Parser ()
crlf = void (string "\r\n")

parseHttpReq :: BS.ByteString -> Either String HttpRequest
parseHttpReq = parseOnly httpRequest
