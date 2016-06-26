{-# Language ForeignFunctionInterface #-}
{-# Language OverloadedStrings #-}

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
-}
module Main where

import Text.Pandoc

import GHCJS.Marshal
import GHCJS.Foreign.Callback
import JavaScript.Object
import JavaScript.Object.Internal (Object(..))

convert :: String -> String
convert s = do
  let readerOpts = def { readerSmart = True }

  let Right doc = readMarkdown readerOpts s

  let writerOptions = def { writerHtml5 = True }
  writeHtmlString writerOptions doc

foreign import javascript unsafe "convert_ = $1"
    js_set_convert :: Callback a -> IO ()

main :: IO ()
main = do
    putStrLn "Haskell convert core starting..."

    callback <- syncCallback1 ThrowWouldBlock $ \jsval -> do
        let o = Object jsval
        getProp "in" o
          >>= fromJSValUnchecked
          >>= pure . convert
          >>= toJSVal
          >>= \v -> setProp "out" v o

    js_set_convert callback

    putStr "Haskell convert callback initialized."
