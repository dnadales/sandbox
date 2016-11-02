{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CloudFiles where

import Prelude hiding (log)
import Control.Monad.Free
import Data.Functor.Sum

-- | Definition of the CloudFile DSL

type Path = String
type Bytes = String

data CloudFilesF a
  = SaveFile Path Bytes a
  | ListFiles Path ([Path] -> a) deriving Functor

type CloudFilesM = Free CloudFilesF

saveFile :: Path -> Bytes -> CloudFilesM ()
saveFile path bytes = liftF $ SaveFile path bytes ()

listFiles :: Path -> CloudFilesM [Path]
listFiles path = liftF $ ListFiles path id

-- | Definition of the Logging DSL

data Level = Debug | Info | Warning | Error deriving Show

data LogF a = Log Level String a deriving (Show, Functor)

type LogM = Free LogF

log :: Level -> String -> LogM ()
log level msg = liftF $ Log level msg ()

-- | Definition of the REST DSL

data RestF a
  = Get Path (Bytes -> a)
  | Put Path Bytes (Bytes -> a) deriving Functor

type RestM = Free RestF

get :: Path -> RestM Bytes
get path = liftF $ Get path id

put :: Path -> Bytes -> RestM Bytes
put path bytes = liftF $ Put path bytes id

-- | Interpreters

-- If we only had named type classes (like Scala)
-- class Interpreter f m where
--   interpretTerm :: f a -> m a
--   interpret :: (Monad m) => (Free f) a -> m a
--   interpret = foldFree interpretTerm

restCloudFilesTerm :: CloudFilesF a -> RestM a
restCloudFilesTerm (SaveFile path bytes next) = do
  _ <- put path bytes
  return next
restCloudFilesTerm (ListFiles path withFiles) = do
  content <- get path
  let files = words content
  return (withFiles files)

ioRestTerm :: RestF a -> IO a
ioRestTerm (Get path withBytes) = do
  putStrLn ("Get " ++ path)
  return (withBytes path)
ioRestTerm (Put path bytes withBytes) = do
  putStrLn ("Post " ++ path ++ " " ++ bytes)
  return (withBytes path)

ioRest :: RestM a -> IO a
ioRest = foldFree ioRestTerm

ioLogTerm :: LogF a -> IO a
ioLogTerm (Log level msg next) = do
  putStrLn (show level ++ " " ++ msg)
  return next

ioLog :: LogM a -> IO a
ioLog = foldFree ioLogTerm


logCloudFilesTerm :: CloudFilesF a -> LogM ()
logCloudFilesTerm (SaveFile path bytes _) = do
  liftF $ Log Debug ("SaveFile " ++ path ++ " " ++ bytes) ()
  return ()
logCloudFilesTerm (ListFiles path _) = do
  liftF $ Log Debug ("ListFiles " ++ path) ()
  return ()

-- | Sample programs

cloudFilesProgram :: CloudFilesM [Path]
cloudFilesProgram = do
  saveFile "/data/foo" "foo-contents"
  saveFile "/data/bar" "bar-contents"
  listing <- listFiles "/data/baz"
  saveFile (head listing) "baz-contents"
  return listing

logProgram:: LogM ()
logProgram = do
  log Info "first message"
  log Debug "second message"

-- | We create an interpreter into the sum
(|+>) :: (Functor g, Functor h)
     => (forall a. f a -> g a) -> (f b -> h ())
     -> f b -> (Free (Sum g h)) b
(|+>) interpret effect term = do
  next <- liftF $ InL (interpret term)
  liftF $ InR (effect term)
  return next

collapseWith :: (f a -> b) -> (g a -> b) -> Sum f g a -> b
collapseWith r s combined =
  case combined of
    InL x -> r x
    InR y -> s y

ioCloudFiles :: CloudFilesM a -> IO a
ioCloudFiles = foldFree $
  restCloudFilesTerm |+> logCloudFilesTerm |>>
  collapseWith ioRest ioLog

(|>>) :: (Monad m) => (forall a. f a -> Free g a)
      -> (forall b. g b -> m b) -> f c -> m c
(|>>) r s = foldFree s . r
