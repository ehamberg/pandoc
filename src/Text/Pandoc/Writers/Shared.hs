{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2013-2015 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Shared
   Copyright   : Copyright (C) 2013-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Shared utility functions for pandoc writers.
-}
module Text.Pandoc.Writers.Shared (
                       tagWithAttrs
                     , fixDisplayMath
                     )
where
import Text.Pandoc.Definition
import Text.Pandoc.Pretty
import Text.Pandoc.XML (escapeStringForXML)
import Control.Monad (liftM)
import Text.Pandoc.Options (WriterOptions(..))
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.UTF8 (toStringLazy)
import qualified Data.Traversable as Traversable
import Data.List ( groupBy )

-- Produce an HTML tag with the given pandoc attributes.
tagWithAttrs :: String -> Attr -> Doc
tagWithAttrs tag (ident,classes,kvs) = hsep
  ["<" <> text tag
  ,if null ident
      then empty
      else "id=" <> doubleQuotes (text ident)
  ,if null classes
      then empty
      else "class=" <> doubleQuotes (text (unwords classes))
  ,hsep (map (\(k,v) -> text k <> "=" <>
                doubleQuotes (text (escapeStringForXML v))) kvs)
  ] <> ">"

isDisplayMath :: Inline -> Bool
isDisplayMath (Math DisplayMath _) = True
isDisplayMath _                    = False

stripLeadingTrailingSpace :: [Inline] -> [Inline]
stripLeadingTrailingSpace = go . reverse . go . reverse
  where go (Space:xs) = xs
        go xs         = xs

-- Put display math in its own block (for ODT/DOCX).
fixDisplayMath :: Block -> Block
fixDisplayMath (Plain lst)
  | any isDisplayMath lst && not (all isDisplayMath lst) =
    -- chop into several paragraphs so each displaymath is its own
    Div ("",["math"],[]) $ map (Plain . stripLeadingTrailingSpace) $
       groupBy (\x y -> (isDisplayMath x && isDisplayMath y) ||
                         not (isDisplayMath x || isDisplayMath y)) lst
fixDisplayMath (Para lst)
  | any isDisplayMath lst && not (all isDisplayMath lst) =
    -- chop into several paragraphs so each displaymath is its own
    Div ("",["math"],[]) $ map (Para . stripLeadingTrailingSpace) $
       groupBy (\x y -> (isDisplayMath x && isDisplayMath y) ||
                         not (isDisplayMath x || isDisplayMath y)) lst
fixDisplayMath x = x
