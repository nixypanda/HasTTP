{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.Except (
    ExceptT,
    MonadError,
    MonadIO,
    liftEither,
    liftIO,
    runExceptT,
    throwError,
 )
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as BSC
import Data.Maybe (fromJust, fromMaybe)
import Network.Simple.TCP (HostPreference (Host), Socket, recv, send, serve)
import Options.Applicative (
    Parser,
    ParserInfo,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    optional,
    progDesc,
    strOption,
    (<**>),
 )
import Parse (ParseError, parseHttpReq)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Types (
    HttpRequest (..),
    HttpResponse (..),
    StatusCode (..),
    emptyResWithStatus,
    getHeader,
    mkOkFileRes,
    mkOkRes,
    responseToStr,
 )

bufferSize :: Int
bufferSize = 4096

data HttpServerError = EmptyRequest | MalformedReq ParseError
    deriving (Show)

newtype Env = Env {directory :: FilePath}

newtype HttpServer a = HttpServer
    { runHttpServer :: ReaderT Env (ExceptT HttpServerError IO) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError HttpServerError
        , MonadIO
        , MonadReader Env
        )

handleClient :: Socket -> HttpServer ()
handleClient socket = do
    maybeRawReq <- recv socket bufferSize
    rawReq <- maybe (throwError EmptyRequest) pure maybeRawReq
    parsedReq@HttpRequest{..} <- liftEither $ first MalformedReq $ parseHttpReq rawReq
    response <-
        if
                | path == "/" -> pure ok200
                | "/echo/" `BSC.isPrefixOf` path -> pure $ extractPath parsedReq
                | path == "/user-agent" -> pure $ extractHeader parsedReq
                | "/files/" `BSC.isPrefixOf` path -> getFile parsedReq
                | otherwise -> pure notFound404
    _ <- send socket (responseToStr response)
    pure ()

-- Handlers

ok200 :: HttpResponse
ok200 = emptyResWithStatus Ok

notFound404 :: HttpResponse
notFound404 = emptyResWithStatus NotFound

extractPath :: HttpRequest -> HttpResponse
extractPath HttpRequest{..} =
    mkOkRes (fromJust $ BSC.stripPrefix "/echo/" path)

extractHeader :: HttpRequest -> HttpResponse
extractHeader req =
    mkOkRes (fromMaybe "" $ getHeader req "User-Agent")

getFile :: HttpRequest -> HttpServer HttpResponse
getFile HttpRequest{..} = do
    let filename = fromJust $ BSC.stripPrefix "/files/" path
    dir <- asks directory
    let filePath = dir </> BSC.unpack filename
    isFilePresent <- liftIO $ doesFileExist filePath
    if isFilePresent
        then do
            content <- liftIO $ BSC.readFile filePath
            pure $ mkOkFileRes content
        else pure notFound404

-- CLI options parsing

newtype CliOpts = CliOpts
    { maybeDir :: Maybe String
    }

environment :: Parser CliOpts
environment = CliOpts <$> optional opt
  where
    opt =
        strOption
            ( long "directory"
                <> metavar "TARGET"
                <> help "Location of the directory to serve/store files to/from"
            )

cliOpts :: ParserInfo CliOpts
cliOpts = info (environment <**> helper) desc
  where
    desc =
        fullDesc
            <> progDesc "A basic HTTP Server"
            <> header "hs-http-server-clone - a barebones HTTP server written for fun"

-- Main

main :: IO ()
main = do
    let host = "127.0.0.1"
        port = "4221"

    BSC.putStrLn $ "Listening on " <> BSC.pack host <> ":" <> BSC.pack port
    CliOpts{..} <- execParser cliOpts
    let env = Env $ fromMaybe "." maybeDir

    serve (Host host) port $ \(sock, addr) -> do
        BSC.putStrLn $ "Accepted connection from " <> BSC.pack (show addr) <> "."
        errOrRes <- runExceptT $ runReaderT (runHttpServer $ handleClient sock) env
        case errOrRes of
            Left err -> BSC.putStrLn $ "Error: " <> BSC.pack (show err)
            Right _ -> pure ()
