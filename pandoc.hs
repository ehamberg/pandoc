{-# LANGUAGE CPP, TupleSections, ScopedTypeVariables #-}
{-
Copyright (C) 2006-2016 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Main
   Copyright   : Copyright (C) 2006-2016 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Parses command-line options and calls the appropriate readers and
writers.
-}
module Main where
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8

main :: IO ()
main = do
  let readerOpts = def { readerSmart = True
                       , readerStandalone = True
                       }

  s <- UTF8.getContents
  let Right doc = readMarkdown readerOpts s

  let writerOptions = def { writerHtml5 = True }
  UTF8.putStr (writeHtmlString writerOptions doc)
