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

module TikTok.Plugins.Hudson
       ( Endpoint(..)
       , new
       ) where

import Control.Monad.Reader
import Control.Applicative
import qualified Data.ByteString as B
import Data.Maybe
import Data.Aeson
import Network.SimpleIRC.Core
import Network.SimpleIRC.Messages
import Network.HTTP
import TikTok.Bot
import TikTok.Plugins.ByteStringHelpers
import TikTok.Plugins.JsonHelpers

newtype Endpoint = Endpoint { endpoint :: String }

type JobName = String

data Status = Bad
            | Good

newtype LHudsonStatus = LHudsonStatus { allStatus :: [HudsonStatus] }

data HudsonStatus = HudsonStatus { jobName   :: JobName
                                 , jobStatus :: Status
                                 , jobUrl    :: String
                                 }

new :: Endpoint -> Plugin
new e = Plugin (eventHandler e) "hudson"

eventHandler :: Endpoint -> Event -> Bot ()
eventHandler e (EvtPrivmsg m) 
  = do { irc  <- asks ircConn
       ; dest <- liftIO $ getDest irc m
       ; case (mMsg m)
         of "!hudson url"    -> 
              sayPrivmsg dest (tobs $ endpoint e)
            "!hudson status" -> 
              do { hstatus <- liftIO $ getStatus e
                 ; mapM_ (sayPrivmsg dest . tobs . show) hstatus
                 ; sayPrivmsg dest "-- *** --"
                 }
            mjob             ->
              withJob mjob $ \j -> do { mstatus <- liftIO $ getJobStatus e (frombs j)
                                      ; case mstatus
                                        of Nothing -> return ()
                                           Just st -> sayPrivmsg dest (tobs $ show st)
                                      }
       }
eventHandler _ _
  = return ()

getJobStatus :: Endpoint -> JobName -> IO (Maybe HudsonStatus)
getJobStatus e j = do { rsp <- simpleHTTP (getRequest apiUrl)
                      ; fmap readJSON (getResponseBody rsp)
                      }
  where apiUrl = endpoint e ++ "/job/" ++ j ++ "/api/json"

getStatus :: Endpoint -> IO [HudsonStatus]
getStatus e = do { rsp     <- simpleHTTP (getRequest apiUrl)
                 ; jsonVal <- fmap readJSON (getResponseBody rsp)
                 ; return $ head (maybeToList (fmap allStatus jsonVal))
                 }
  where apiUrl = endpoint e ++ "/api/json"
        
withJob :: B.ByteString -> (B.ByteString -> Bot ()) -> Bot ()
withJob raw f
  | "!hudson status " `B.isPrefixOf` raw = f (B.drop 15 raw)
  | otherwise                            = return ()

instance FromJSON Status where
  parseJSON (String "blue") = return Good
  parseJSON (String _)      = return Bad
  parseJSON _               = mzero

instance FromJSON HudsonStatus where
  parseJSON (Object v) = HudsonStatus <$> v .: "name" <*> v .: "color" <*> v .: "url"
  parseJSON _          = mzero

instance FromJSON LHudsonStatus where
  parseJSON (Object v) = LHudsonStatus <$> v .: "jobs"
  parseJSON _          = mzero

instance Show HudsonStatus where
  show h = "-- " ++ jobName h ++ ": " ++ show (jobStatus h)
  
instance Show Status where
  show Bad  = "*bad*"
  show Good = "_good_"
