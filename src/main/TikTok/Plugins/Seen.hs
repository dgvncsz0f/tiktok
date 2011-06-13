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

module TikTok.Plugins.Seen
       ( new
       , withStringDBM
       ) where

import Control.Monad.Reader
import qualified Data.ByteString as B
import Network.SimpleIRC.Messages
import Network.SimpleIRC.Core
import Database.AnyDBM
import Database.AnyDBM.StringDBM
import TikTok.Bot
import TikTok.Plugins.ByteStringHelpers
import TikTok.Plugins.CalendarTimeHelpers

type WithDBM a = (a -> Bot ()) -> Bot ()

withStringDBM :: FilePath -> WithDBM StringDBM
withStringDBM file = \fm -> do { dbm <- liftIO $ openStringDBM file ReadWriteMode
                               ; fm dbm
                               ; liftIO $ closeA dbm
                               }

new :: (AnyDBM a) => WithDBM a -> Plugin
new wdbm = Plugin (eventHandler wdbm) "invite"

wantsRead :: IrcMessage -> Bool
wantsRead m = "!seen " `B.isPrefixOf` (mMsg m)

who :: IrcMessage -> B.ByteString
who m = B.drop 6 (mMsg m)

eventHandler :: (AnyDBM a) => WithDBM a -> Event -> Bot ()
eventHandler wdbm (EvtPrivmsg m)
  | wantsRead m = do { irc <- asks ircConn 
                     ; dest  <- liftIO $ getDest irc m
                     ; wdbm $ \db -> do { mseen <- liftIO $ lookupA db (frombs $ who m)
                                        ; case mseen
                                          of Nothing   -> sayPrivmsg dest ("I haven't seen " `B.append` who m)
                                             Just seen -> sayPrivmsg dest (formatSeen (who m) (read seen))
                                        }
                     }
  | otherwise   = case (mNick m, mChan m)
                  of (Just nick, Just chan)
                       -> wdbm $ \db -> liftIO $ do { time <- fmap timestamp nowTime
                                                    ; insertA db (frombs nick) (show (frombs chan, time))
                                                    }
                     _
                       -> return ()
eventHandler _ _ 
  = return ()

formatSeen :: B.ByteString -> (String, String) -> B.ByteString
formatSeen nick (chan, rawtime) = let Just cal = parseTimestamp rawtime
                                  in B.concat [ nick
                                              , " was last heard speaking in "
                                              , tobs chan 
                                              , " "
                                              , tobs $ humanTimestamp cal
                                              , " TODO:make_this_more_human_friendly"
                                              ]
