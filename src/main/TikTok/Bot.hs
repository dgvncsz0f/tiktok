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

module TikTok.Bot
       ( -- * Types
         Event(..)
       , Plugin(..)
       , BotConfig(..)
       , TikTok(config, say, ircConn)
       , Bot()
         -- * EventHandler related functions
       , discardMessage
         -- * Input/Output
       , sayPrivmsg
         -- * Bot management
       , runBot
       ) where

import qualified Data.ByteString as B
import Data.Char
import Control.Concurrent
import Control.Monad.Reader
import Network.SimpleIRC.Core
import Network.SimpleIRC.Messages

data Event = EvtPrivmsg IrcMessage
           | EvtPing IrcMessage
           | EvtJoin IrcMessage
           | EvtPart IrcMessage
           | EvtTopic IrcMessage
           | EvtInvite IrcMessage
           | EvtQuit IrcMessage
           | EvtNotice IrcMessage
           | EvtDisconnect
           | EvtTikOfInfinity

data Plugin = Plugin { evhandler :: Event -> Bot ()
                     , name      :: String
                     }

data BotConfig = BotConfig { ircServer  :: String
                           , ircPort    :: Int
                           , myNick     :: String
                           , myPasswd   :: String
                           , myUserName :: String
                           , myRealName :: String
                           , plugins    :: [Plugin]
                           }

data TikTok = TikTok { config      :: BotConfig
                     , ircConn     :: MIrc
                     , say         :: Command -> Bot ()
                     }

type Bot = ReaderT TikTok IO

discardMessage :: Event -> IrcMessage -> Bot ()
discardMessage _ _ = return ()

sayPrivmsg :: B.ByteString -> B.ByteString -> Bot ()
sayPrivmsg to m = do { sayf <- asks say
                     ; sayf (MPrivmsg to m)
                     }

runPlugin :: Plugin -> Bot ()
runPlugin p = do { s <- asks ircConn
                 ; c <- liftIO $ newChan
                 ; liftIO $ register s c
                 ; execPlugin c
                 }
  where register s c = do { _ <- addEvent s (Privmsg $ \_ m  -> writeChan c (EvtPrivmsg m))
                          ; _ <- addEvent s (Ping $ \_ m     -> writeChan c (EvtPing m))
                          ; _ <- addEvent s (Join $ \_ m     -> writeChan c (EvtJoin m))
                          ; _ <- addEvent s (Part $ \_ m     -> writeChan c (EvtPart m))
                          ; _ <- addEvent s (Topic $ \_ m    -> writeChan c (EvtTopic m))
                          ; _ <- addEvent s (Invite $ \_ m   -> writeChan c (EvtInvite m))
                          ; _ <- addEvent s (Quit $ \_ m     -> writeChan c (EvtQuit m))
                          ; _ <- addEvent s (Notice $ \_ m   -> writeChan c (EvtNotice m))
                          ; _ <- addEvent s (Disconnect $ \_ -> writeChan c EvtDisconnect)
                          ; return ()
                          }
        execPlugin c = fix $ \loop -> do { e <- liftIO $ readChan c
                                         ; evhandler p e
                                         ; case e
                                           of EvtDisconnect -> return ()
                                              _             -> loop
                                         }

waitDisconnect :: Bot ()
waitDisconnect = do { s     <- asks ircConn
                    ; liftIO $ do { mutex <- newEmptyMVar
                                  ; _     <- register s mutex
                                  ; takeMVar mutex
                                  }
                    }
  where register s mutex = addEvent s (Disconnect $ \_ -> putMVar mutex ())

sendTikOfInfinity :: Bot ()
sendTikOfInfinity = do { myPlugins <- asks (plugins . config)
                       ; mapM_ (\p -> evhandler p EvtTikOfInfinity) myPlugins
                       }

runTikOfInfinity :: TikTok -> IO ()
runTikOfInfinity bot = forkIO (fix $ \loop -> do { threadDelay 60000000
                                                 ; runReaderT sendTikOfInfinity bot
                                                 ; loop
                                                 }) 
                       >> return ()

runBot :: BotConfig -> IO ()
runBot cfg = do { rc <- connect ircCfg True True
                ; case rc
                  of Left e  -> fail (show e)
                     Right s -> let bot = TikTok cfg s sendCmdWithFloodCtrl
                                in do { sendCmd s (MPrivmsg "nickserv" ("identify " `B.append` (tobs $ myPasswd cfg)))
                                      ; mapM_ (\p -> forkIO $ runReaderT (runPlugin p) bot) (plugins cfg)
                                      ; runTikOfInfinity bot
                                      ; runReaderT waitDisconnect bot
                                      }
                }
  where ircCfg = defaultConfig { cAddr     = (ircServer cfg)
                               , cPort     = (ircPort cfg)
                               , cNick     = (myNick cfg)
                               , cUsername = (myUserName cfg)
                               , cRealname = (myRealName cfg)
                               , cEvents   = []
                               , cChannels = ["##bitforest"]
                               }

sendCmdWithFloodCtrl :: Command -> Bot ()
sendCmdWithFloodCtrl c = do { s <- asks ircConn
                            ; liftIO $ sendCmd s c
                            ; liftIO $ threadDelay 1000000
                            } 

tobs :: String -> B.ByteString
tobs = B.pack . map (fromIntegral . ord)
