{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (
    HttpHeader (..),
    StatusCode (..),
    HttpResponse (..),
    HttpRequest (..),
    HttpMethod (..),
    responseToStr,
    emptyResWithStatus,
    mkOkRes,
    mkOkFileRes,
    getHeader,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.List (find)

data HttpHeader = HttpHeader
    { name :: BS.ByteString
    , value :: BS.ByteString
    }
    deriving (Show, Eq)

httpHeaderStr :: HttpHeader -> BS.ByteString
httpHeaderStr (HttpHeader n v) = BS.concat [n, ": ", v]

data StatusCode = Ok | Created | NotFound deriving (Show)

statusCodeStr :: StatusCode -> BS.ByteString
statusCodeStr Ok = "200 OK"
statusCodeStr Created = "201 Created"
statusCodeStr NotFound = "404 Not Found"

data HttpResponse = HttpResponse
    { statusCode :: StatusCode
    , resHeaders :: [HttpHeader]
    , resContent :: BS.ByteString
    }

responseToStr :: HttpResponse -> BS.ByteString
responseToStr response =
    let protocol = "HTTP/1.1"
        statusLine = protocol <> " " <> statusCodeStr (statusCode response)
        headersStr = BS.intercalate "\r\n" (map httpHeaderStr $ resHeaders response)
        contentStr = resContent response
     in BSC.unlines [statusLine, headersStr, "", contentStr]

data HttpMethod = GET | POST deriving (Eq)

data HttpRequest = HttpRequest
    { method :: HttpMethod
    , path :: BS.ByteString
    , version :: BS.ByteString
    , reqHeaders :: [HttpHeader]
    , reqContent :: BS.ByteString
    }

emptyResWithStatus :: StatusCode -> HttpResponse
emptyResWithStatus s = HttpResponse s [] ""

mkOkRes :: BS.ByteString -> HttpResponse
mkOkRes content = HttpResponse Ok [contentType, contentLength] content
  where
    contentLength = HttpHeader "Content-Length" (BSC.pack . show $ BS.length content)
    contentType = HttpHeader "Content-Type" "text/plain"

getHeader :: HttpRequest -> BS.ByteString -> Maybe BS.ByteString
getHeader req header = value <$> find ((== header) . name) (reqHeaders req)

mkOkFileRes :: BS.ByteString -> HttpResponse
mkOkFileRes content = HttpResponse Ok [contentType, contentLength] content
  where
    contentLength = HttpHeader "Content-Length" (BSC.pack . show $ BS.length content)
    contentType = HttpHeader "Content-Type" "application/octet-stream"
