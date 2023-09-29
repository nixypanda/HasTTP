{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as BLC
import Network.Simple.TCP (HostPreference (Host), serve)

main :: IO ()
main = do
    let host = "127.0.0.1"
        port = "4221"

    BLC.putStrLn $ "Listening on " <> BLC.pack host <> ":" <> BLC.pack port

    serve (Host host) port $ \(sock, addr) ->
        BLC.putStrLn $ "Accepted connection from " <> BLC.pack (show addr) <> "."
