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

-- This is simply a copy with mutation of:
-- http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot

module Main where

import System
import System.Time
import System.IO
import TikTok.Bot
import TikTok.Config
import TikTok.Plugins.Logger as Logger
import TikTok.Plugins.Bitly as Bitly
import TikTok.Plugins.Hudson as Hudson
import TikTok.Plugins.Invite as Invite
import TikTok.Plugins.Seen as Seen
import TikTok.Plugins.Uptime as Uptime

readBotConfig :: IO BotConfig
readBotConfig = do { args <- getArgs
                   ; prog <- getProgName
                   ; now  <- getClockTime
                   ; case args
                     of [cfgFile] -> fmap (botConfig now . read) (readFile cfgFile)
                        _         -> error $ "usage: " ++ prog ++ " cfgfile"
                   }
  where pluginsList core cfg = let hasLogger  = getWithDefault cfg "logger_enabled" True
                                   hasBitly   = getWithDefault cfg "bitly_enabled" False
                                   hasHudson  = getWithDefault cfg "hudson_enabled" False
                                   hasInvite  = getWithDefault cfg "invite_enabled" True
                                   hasSeen    = getWithDefault cfg "seen_enabled" False
                                   inviteWlst = concatMap unpackString (getWithDefault cfg "invite_whitelist" [])
                                   loggerWlst = concatMap unpackString (getWithDefault cfg "logger_whitelist" [])
                          in core ++ (map snd $ filter fst [ (hasLogger, Logger.new (getWithDefault cfg "logger_basedir" "/tmp/irclogs") loggerWlst)
                                                           , (hasBitly,  Bitly.new  (getWithDefault cfg "bitly_user" "tiktok") (getWithDefault cfg "bitly_apikey" "unknown"))
                                                           , (hasHudson, Hudson.new (getWithDefault cfg "hudson_endpoint" "http://localhost/hudson"))
                                                           , (hasInvite, Invite.new inviteWlst)
                                                           , (hasSeen,   Seen.new (withStringDBM $ getWithDefault cfg "seen_dbm" "/tmp/seen.dbm"))
                                                           ])
        
        botConfig now cfg = BotConfig (getWithDefault cfg "irc_host" "irc.freenode.net")
                                      (fromInteger $ getWithDefault cfg "irc_port" 6667)
                                      (getWithDefault cfg "nick" "tiktok")
                                      (getWithDefault cfg "passwd" "tiktok")
                                      (getWithDefault cfg "name" "tiktok")
                                      (getWithDefault cfg "name" "tiktok")
                                      (pluginsList [Uptime.new now] cfg)

main :: IO ()
main = do { hSetBinaryMode stderr True
          ; hSetBinaryMode stdin True
          ; cfg <- readBotConfig
          ; runBot cfg
          }
