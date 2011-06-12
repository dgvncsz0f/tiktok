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

module TikTok.Plugins.ByteStringHelpers where

import Data.Char
import qualified Data.ByteString as B

tobs :: String -> B.ByteString
tobs = B.pack . map (fromIntegral . ord)
  
frombs :: B.ByteString -> String
frombs = map (chr . fromIntegral) . B.unpack

blines :: B.ByteString -> [B.ByteString]
blines b 
  | B.null b  = []
  | otherwise = let (x,xs) = B.break (==10) b
                in x : blines (B.drop 1 xs)

bwords :: B.ByteString -> [B.ByteString]
bwords b
  | B.null b  = []
  | otherwise = let (x,xs) = B.break (`elem` [32,9]) b
                in x : bwords (B.drop 1 xs)

bunwords :: [B.ByteString] -> B.ByteString
bunwords = B.intercalate (B.singleton 32)
