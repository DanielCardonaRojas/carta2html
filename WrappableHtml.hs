{-#LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module WrappableHtml where

import Lucid
import Data.Monoid
import CartaTypes
import Customizable
import Helpers
import CartaStyles

--class ToHtml h => WrappableHtml h where
--  wrapHtml :: h -> Html () -> Html ()
--  wrapHtml _ x = x 
--  blockHeader :: h -> Html ()
--  blockHeader _ = mempty 


class ToHtml a => Wrappable h a | a -> h where
	innerWrap :: h a -> Html () -> Html ()
	outerWrap :: h a -> Html () -> Html ()
	outerWrap _ = id
	sectionHeader :: h a -> Html ()
	sectionHeader _ = mempty
	sectionFooter :: h a -> Html ()
	sectionFooter _ = mempty

styleSection ::(Wrappable CartaSection a) => GridSys -> Int -> CartaSection a -> Html ()
styleSection sys n carta = 
    let      
      makeGridCol' = makeGridCol sys n
      ls = items carta   
      innerWrap' = innerWrap carta
      outerWrap' = outerWrap carta
      sectionHeader' = sectionHeader carta 
      sectionFooter' = sectionFooter carta 
      wrappedColumns wrapper = map toHtml ls |> divideReduce n |> map (wrapper .> makeGridCol')
    in  outerWrap' (sectionHeader' <> div_ [class_ "row"] (mconcat (wrappedColumns innerWrap')) <> sectionFooter')


---------------------------------- Instances ---------------------------------------
{-
If you enable the -w flag from command line than your type must be an instance
-}

instance Wrappable CartaSection ItemCarta where
 	innerWrap sect = div_ [class_ "row"]
 	sectionHeader sect = h2_ [class_ "title-section"] (toHtml $ sectionName sect)

instance Wrappable CartaSection BrasasItemCarta

instance Wrappable CartaSection MoltoItemCarta where
	outerWrap sect html = 
          div_ [class_ "container text-carta text-center"] $ html          	 
 	
 	sectionHeader sect = span_ [class_ "line-title-carta"] (toHtml $ sectionName sect)
	
	innerWrap sect html = 
			table_ [class_ "table carta-molto"] $ do 
					tbody_ (html)
	



