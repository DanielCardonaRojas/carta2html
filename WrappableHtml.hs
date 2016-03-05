{-#LANGUAGE OverloadedStrings #-}

module WrappableHtml where

import Lucid
import Data.Monoid
import CartaTypes

class ToHtml h => WrappableHtml h where
  wrapHtml :: h -> Html () -> Html ()
  wrapHtml _ x = x 
  blockHeader :: h -> Html ()
  blockHeader _ = mempty 


class Wrappable h where
	innerWrap :: ToHtml a => h a -> Html () -> Html ()
	sectionHeader :: ToHtml a => h a -> Html ()
	sectionHeader _ = mempty


instance Wrappable CartaSection where
 	innerWrap sect = div_ [class_ "row"]
 	sectionHeader sect = h2_ [class_ "title"] (toHtml $ sectionName sect)


instance ToHtml ItemCarta where
	toHtml m = case m of 
		          D1PItem i -> mempty
		          D2PItem i -> mempty
		          D3PItem i -> mempty

	toHtmlRaw = undefined

newtype MasFincaItemCarta = MasFincaItemCarta ItemCarta deriving (Show,Eq)


instance ToHtml MasFincaItemCarta where
	toHtml (MasFincaItemCarta m) = case m of 
		          D1PItem i -> mempty
		          D2PItem i -> mempty
		          D3PItem i -> mempty

	toHtmlRaw = undefined



