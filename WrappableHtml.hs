{-#LANGUAGE OverloadedStrings #-}

module WrappableHtml where

import Lucid
import Data.Monoid

class ToHtml h => WrappableHtml h where
  wrapHtml :: h -> Html () -> Html ()
  wrapHtml _ x = x 
  blockHeader :: h -> Html ()
  blockHeader _ = mempty 





