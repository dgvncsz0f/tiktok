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

module TikTok.Plugins.CalendarTimeHelpers where

import Data.List (foldl')
import Control.Arrow
import System.Time
import System.Time.Parse
import System.Locale

nowTime :: IO CalendarTime
nowTime = fmap toUTCTime getClockTime

date :: CalendarTime -> String
date = formatCalendarTime defaultTimeLocale "%Y%m%d"

timestamp :: CalendarTime -> String
timestamp = formatCalendarTime defaultTimeLocale "%Y%m%d%H%M%S"

humanDate :: CalendarTime -> String
humanDate = formatCalendarTime defaultTimeLocale "%Y-%m-%d"

humanTime :: CalendarTime -> String
humanTime = formatCalendarTime defaultTimeLocale "%H:%M"

humanTimestamp :: CalendarTime -> String
humanTimestamp c = humanDate c ++ " " ++ humanTime c

parseTimestamp :: String -> Maybe CalendarTime
parseTimestamp = parseCalendarTime defaultTimeLocale "%Y%m%d%H%M%S"

-- | Credits: http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot
pretty :: TimeDiff -> String
pretty td =
  unwords $ map (uncurry (++) . first show) $
  if null diffs then [(0,"s")] else diffs
  where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                    in (tot',(sec',typ):acc)
        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
        diffs = filter ((/= 0) . fst) $ reverse $ snd $
                foldl' merge (tdSec td,[]) metrics
