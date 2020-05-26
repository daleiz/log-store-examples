{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit ((.|), runConduit, sinkList)
import Control.Monad.IO.Class (liftIO)
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
          log1EntryId1 <- liftIM1 lh1 (`appendEntry` "log1-entry1")
          log1EntryId2 <- liftIM1 lh1 (`appendEntry` "log1-entry2")
          log1EntryId3 <- liftIM1 lh1 (`appendEntry` "log1-entry3")
          -- open log2
          lh2 <-
            open
              "log2"
              defaultOpenOptions {writeMode = True, createIfMissing = True}
          -- append entries to log2
          log2EntryId1 <- liftIM1 lh2 (`appendEntry` "log2-entry1")
          log2EntryId2 <- liftIM1 lh2 (`appendEntry` "log2-entry2")
          log2EntryId3 <- liftIM1 lh2 (`appendEntry` "log2-entry3")
          -- open log3
          lh3 <-
            open
              "log3"
              defaultOpenOptions {writeMode = True, createIfMissing = True}
          -- append entries to log3
          log3EntryId1 <- liftIM1 lh3 (`appendEntry` "log3-entry1")
          log3EntryId2 <- liftIM1 lh3 (`appendEntry` "log3-entry2")
          log3EntryId3 <- liftIM1 lh3 (`appendEntry` "log3-entry3")
          -- read entries from log1
          source1 <- liftIM3 lh1 log1EntryId1 log1EntryId3 readEntries
          r1 <-
            liftIM1
              source1
              ( \s -> liftIO $ do
                  r <- runConduit $ s .| sinkList
                  return $ Just r
              )
          -- read entries from log2
          source2 <- liftIM3 lh2 log2EntryId1 log2EntryId3 readEntries
          r2 <-
            liftIM1
              source2
              ( \s -> liftIO $ do
                  r <- runConduit $ s .| sinkList
                  return $ Just r
              )
          -- read entries from log3
          source3 <- liftIM3 lh3 log3EntryId1 log3EntryId3 readEntries
          r3 <-
            liftIM1
              source3
              ( \s -> liftIO $ do
                  r <- runConduit $ s .| sinkList
                  return $ Just r
              )
          return
            [ r1,
              r2,
              r3
            ]
      )
  print appendedEntries
