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

module TikTok.Plugins.Bitly
       ( Login
       , ApiKey
       , new
       ) where

import Control.Monad.Reader
import Control.Applicative
import Data.Char
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Maybe
import Data.Aeson
import Data.Text (unpack)
import Network.URI
import Network.SimpleIRC.Core
import Network.SimpleIRC.Messages
import Network.HTTP
import TikTok.Bot
import TikTok.Plugins.JsonHelpers
import TikTok.Plugins.ByteStringHelpers

type Login = String

type ApiKey = String

type Url = String

type Title = String

type Service = String

data Bitly = Bitly { login  :: String
                   , apiKey :: String
                   }

newtype ExpandResponse = ExpandResponse { longUrl :: Url }

newtype ShortenResponse = ShortenResponse { shortUrl :: Url }

newtype InfoResponse = InfoResponse { urlTitle :: Title }

new :: Login -> ApiKey -> Plugin
new l k = Plugin (eventHandler (Bitly l k)) "bitly"

parseUrls :: B.ByteString -> [URI]
parseUrls = map fromJust . filter ((Just True ==) . fmap properUrl) . map toURL . bwords
  where toURL = parseURI . frombs
        properUrl url = (uriScheme url == "http:") && (uriAuthority url /= Nothing)

eventHandler :: Bitly -> Event -> Bot ()
eventHandler b (EvtPrivmsg m) = let myUrls = parseUrls (mMsg m)
                                in do { irc      <- asks ircConn
                                      ; dest     <- liftIO $ getDest irc m
                                      ; btlyUrls <- liftIO $ concatMapM (doBitly b) myUrls
                                      ; mapM_ (sayPrivmsg dest . tobs) btlyUrls
                                      }
eventHandler _ _              = return ()

doBitly :: Bitly -> URI -> IO [String]
doBitly btly url
  | domain `elem` btlyDomains = fmap (map showPair . maybeToList) (expandRequest btly url)
  | otherwise                 = fmap (map showPair . maybeToList) (shortenRequest btly url)
    where Just domain = fmap (map toLower . uriRegName) (uriAuthority url)
          btlyDomains = ["bit.ly", "j.mp", "yhoo.it"]

showPair :: (Url,Title) -> String
showPair (u,t) = u ++ " - " ++ t

expandRequest :: Bitly -> URI -> IO (Maybe (Url,Title))
expandRequest btly url = do { mu <- fmap (fmap longUrl) (bitlyRequest btly ("/v3/expand", [("shortUrl", show url)]))
                            ; mt <- infoRequest btly (show url)
                            ; return ((,) <$> mu <*> mt)
                            }

shortenRequest :: Bitly -> URI -> IO (Maybe (Url,Title))
shortenRequest btly url = do { mu <- fmap (fmap shortUrl) (bitlyRequest btly ("/v3/shorten", [("longUrl", show url)]))
                             ; mt <- mFromJust $ liftM (infoRequest btly) mu
                             ; return ((,) <$> mu <*> mt)
                             }
  where mFromJust Nothing   = return Nothing
        mFromJust (Just mx) = mx

infoRequest :: Bitly -> Url -> IO (Maybe Title)
infoRequest btly url = fmap (fmap urlTitle) (bitlyRequest btly ("/v3/info", [("shortUrl", url)]))

bitlyRequest :: (FromJSON a) => Bitly -> (Service,[(String,String)]) -> IO (Maybe a)
bitlyRequest btly (srv,params) = do { rsp  <- simpleHTTP (getRequest apiUrl)
                                    ; body <- getResponseBody rsp
                                    ; return (readJSON body)
                                    }
  where authParams = [("login", login btly), ("apiKey", apiKey btly), ("format", "json")]
        apiUrl = "http://api.bitly.com" ++ srv ++ "?" ++ urlEncodeVars (authParams ++ params)
          
instance Show ExpandResponse where
  show (ExpandResponse x) = x

instance Show ShortenResponse where
  show (ShortenResponse x) = x

instance FromJSON ShortenResponse where
  parseJSON (Object v) = do { Object d <- v .: "data"
                            ; ShortenResponse <$> (liftA unpack (d .: "url"))
                            }
  parseJSON _          = mzero

instance FromJSON ExpandResponse where
  parseJSON (Object v) = do { Object a <- v .: "data"
                            ; Object b <- liftA V.head (a .: "expand")
                            ; ExpandResponse <$> (liftA unpack (b .: "long_url"))
                            }
  parseJSON _          = mzero

instance FromJSON InfoResponse where
  parseJSON (Object v) = do { Object a <- v .: "data"
                            ; Object b <- liftA V.head (a .: "info")
                            ; InfoResponse <$> (liftA unpack (b .: "title"))
                            }
  parseJSON _           = mzero

concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f
