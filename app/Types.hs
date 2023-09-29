{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (
    HttpHeader (..),
    StatusCode (..),
    HttpResponse (..),
    responseToStr,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

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