{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit ((.|), runConduit, sinkList)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Log.Store.Base

main :: IO ()
main = do
  appendedEntries <-
    -- will auto init and release resource
    withLogStore
      --(Config {rootDbPath = "/path/to/dir/in/which/you/want/to/store/data"})
      (Config {rootDbPath = "/Users/wangbin/tmp/log-store-test"})
      ( do
          -- open log1
          lh1 <-
            open
              "log1"
              defaultOpenOptions {writeMode = True, createIfMissing = True}
          -- append entries to log1
          log1EntryId1 <- appendEntry lh1 "log1-entry1"
          log1EntryId2 <- appendEntry lh1 "log1-entry2"
          log1EntryId3 <- appendEntry lh1 "log1-entry3"
          -- open log2
          lh2 <-
            open
              "log2"
              defaultOpenOptions {writeMode = True, createIfMissing = True}
          -- append entries to log1
          log2EntryId1 <- appendEntry lh2 "log2-entry1"
          log2EntryId2 <- appendEntry lh2 "log2-entry2"
          log2EntryId3 <- appendEntry lh2 "log2-entry3"
          -- open log3
          lh3 <-
            open
              "log3"
              defaultOpenOptions {writeMode = True, createIfMissing = True}
          -- append entries to log3
          log3EntryId1 <- appendEntry lh3 "log3-entry1"
          log3EntryId2 <- appendEntry lh3 "log3-entry2"
          log3EntryId3 <- appendEntry lh3 "log3-entry3"
          -- read entries from log1 
          source1 <- readEntries lh1 log1EntryId1 log1EntryId3
          r1 <- liftIO $ runConduit $ source1 .| sinkList
          -- read entries from log2 
          source2 <- readEntries lh2 log2EntryId1 log2EntryId3
          r2 <- liftIO $ runConduit $ source2 .| sinkList
          -- read entries from log3 
          source3 <- readEntries lh3 log3EntryId1 log3EntryId3
          r3 <- liftIO $ runConduit $ source3 .| sinkList
          return
            [ r1,
              r2,
              r3
            ]
      )
  print appendedEntries 
