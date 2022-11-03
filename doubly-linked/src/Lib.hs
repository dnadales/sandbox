{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import           Prelude    hiding (last)

import           Data.IORef

someFunc :: IO ()
someFunc = test

data Cell a
  = Cell { value :: a
         , next  :: IORef (Cell a)
         , prev  :: IORef (Cell a)
         }
  | Void

data DLList a =
  DLList { first :: IORef (Cell a)
         , last  :: IORef (Cell a)
         }

nil :: IO (DLList a)
nil = do
  firstRef <- newIORef Void
  lastRef  <- newIORef Void
  pure $ DLList { first = firstRef, last = lastRef }

push :: a -> DLList a -> IO ()
push a DLList { first, last } = do
  firstCell   <- readIORef first
  newCellNext <- newIORef firstCell
  newCellPrev <- newIORef Void
  let newCell = Cell { value = a
                     , next  = newCellNext
                     , prev  = newCellPrev
                     }
  case firstCell of
    Void          ->
      writeIORef last newCell
    Cell { prev } -> writeIORef prev newCell
  writeIORef first newCell

unshift :: DLList a -> a -> IO ()
unshift DLList { first, last } a = do
  lastCell    <- readIORef last
  newCellNext <- newIORef Void
  newCellPrev <- newIORef lastCell
  let newCell = Cell { value = a
                     , next  = newCellNext
                     , prev  = newCellPrev
                     }
  case lastCell of
    Void          ->
      writeIORef first newCell
    Cell { next } -> writeIORef next newCell
  writeIORef last newCell

pop :: DLList a -> IO (Maybe a)
pop DLList { first } = do
  firstCell <- readIORef first
  case firstCell of
    Void           -> pure Nothing
    Cell { value, next } -> do
      nextCell <- readIORef next
      writeIORef first nextCell
      pure $ Just value

test :: IO ()
test = do
  xs <- nil
  push 3 xs
  push 2 xs
  push 1 xs
  unshift xs 4
  unshift xs 5
  printDList xs
  where
    printDList xs = do
      mVal <- pop xs
      case mVal of
        Nothing  -> pure ()
        Just val -> print val >> printDList xs
