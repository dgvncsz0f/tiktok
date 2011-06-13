{-# LANGUAGE OverloadedStrings #-}
-- Copyright (c) 2011, Diego Souza
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--   * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright notice,
--     this list of conditions and the following disclaimer in the documentation
--     and/or other materials provided with the distribution.
--   * Neither the name of the <ORGANIZATION> nor the names of its contributors
--     may be used to endorse or promote products derived from this software
--     without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module TikTok.Plugins.Logger
       ( new
       ) where

import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Maybe
import Network.SimpleIRC.Core
import Network.SimpleIRC.Messages
import System.Time
import System.FilePath
import System.Directory
import TikTok.Bot
import TikTok.Plugins.ByteStringHelpers
import TikTok.Plugins.CalendarTimeHelpers

type Whitelist = [String]

data Logger = Logger { basedir   :: FilePath
                     , whitelist :: Whitelist
                     }

new :: FilePath -> [String] -> Plugin
new f w = Plugin (eventHandler $ Logger f w) "logging"

eventHandler :: Logger -> Event -> Bot ()
eventHandler l (EvtPrivmsg m)
  | wantsLast m             = do { irc  <- asks ircConn
                                 ; dest <- liftIO $ getDest irc m
                                 ; if (hasChannel m)
                                   then (liftIO $ getLog l (fromJust (mChan m))) >>= mapM_ (sayPrivmsg dest)
                                   else return ()
                                 }
  | not (shouldSkip m)      = liftIO $ nowTime >>= \now -> gPutLog (fromJust . mChan) (formatPrivmsg now) l m
eventHandler l (EvtJoin m)  = liftIO $ nowTime >>= \now -> gPutLog mMsg (formatJoin now) l m
eventHandler l (EvtPart m)  = liftIO $ nowTime >>= \now -> gPutLog mMsg (formatPart now) l m
eventHandler l (EvtTopic m) = liftIO $ nowTime >>= \now -> gPutLog (fromJust . mChan) (formatTopic now) l m
eventHandler _ _            = return ()

wantsLast :: IrcMessage -> Bool
wantsLast m = "!logger last" `B.isPrefixOf` mMsg m

shouldSkip :: IrcMessage -> Bool
shouldSkip m = skipPrefix `B.isPrefixOf` mMsg m || isNothing (mChan m)
    
skipPrefix :: B.ByteString
skipPrefix = "-- "

parseLog :: B.ByteString -> B.ByteString
parseLog e = case (bwords e)
             of (time:who:msg) -> bunwords ([ "--", time4humans time, who] ++ msg)
                _              -> B.empty
                                  
  where time4humans t = case (parseTimestamp (frombs t))
                        of Nothing -> "??:??"
                           Just t' -> tobs $ humanTime t'

gPutLog :: (IrcMessage -> B.ByteString) -> (IrcMessage -> B.ByteString) -> Logger -> IrcMessage -> IO ()
gPutLog fc fm l m = putLog l (fc m) (fm m)

putLog :: Logger -> B.ByteString -> B.ByteString -> IO ()
putLog l c m = do { now <- nowTime
                  ; let nowDate  = date now
                        filename = basedir l </> frombs c </> nowDate
                        dirname  = dropFileName filename
                    in when whitelisted (createDirectoryIfMissing True dirname >> B.appendFile filename m)
                  }
  where whitelisted = (frombs c) `elem` (whitelist l)

getLog :: Logger -> B.ByteString -> IO [B.ByteString]
getLog l c = do { now <- nowTime
                ; hasFile <- doesFileExist (filename now)
                ; if (hasFile)
                  then fmap ((logHeader now :) . map parseLog . limit) (B.readFile $ filename now)
                  else return []
                }
  where limit m = m `seq` take 15 (reverse (blines m))
        filename now  = basedir l </> frombs c </> date now
        logHeader now = tobs $ "-- printing last 15 messages [date: " ++ humanDate now ++ ", TZ=UTC]"
        
formatPrivmsg :: CalendarTime -> IrcMessage -> B.ByteString
formatPrivmsg c m
  | meCommand = bunwords [ bTimestamp, "*", nick, B.init (B.drop 8 rawLogMsg), "\n" ]
  | otherwise = bunwords [ bTimestamp, nick, rawLogMsg, "\n" ]
    where rawLogMsg  = mMsg m
          Just nick  = mNick m
          bTimestamp = tobs (timestamp c)
          meCommand  = "\1ACTION " `B.isPrefixOf` rawLogMsg && "\1" `B.isSuffixOf` rawLogMsg

formatJoin :: CalendarTime -> IrcMessage -> B.ByteString
formatJoin c m = bunwords [ bTimestamp, who, "has joined", "\n" ]
  where who = case (mNick m)
              of Nothing -> "someone"
                 Just x  -> x
        bTimestamp = tobs (timestamp c)

formatPart :: CalendarTime -> IrcMessage -> B.ByteString
formatPart c m = bunwords [ bTimestamp, who, "has left", "\n" ]
  where who = case (mNick m)
              of Nothing -> "someone"
                 Just x  -> x
        bTimestamp = tobs (timestamp c)

formatTopic :: CalendarTime -> IrcMessage -> B.ByteString
formatTopic c m = bunwords [ bTimestamp, who, mMsg m, "\n" ]
  where who = case (mNick m)
              of Nothing -> "topic changed:"
                 Just x  -> x `B.append` " has changed the topic to:"
        bTimestamp = tobs (timestamp c)

hasChannel :: IrcMessage -> Bool
hasChannel m = isJust (mChan m)
