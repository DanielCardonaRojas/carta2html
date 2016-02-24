{- |
Este es el modulo que se expone al usuario para personalizar el comportamiento de el programa.
En esencia los pasos para crear un estilo de carta nueva es crear un tipo nuevo. 

> newtype AnkItemCarta = AnkItemCarta { ankItemCarta :: ItemCarta}

luego hacer este tipo de dato una instancia de la clase ToHtml.

GeneralizedNewtypeDeriving
-}
{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Customizable where


import Lucid
import Data.Monoid
import Data.Text (pack,append) 
import CartaTypes
import ListModifiers
import WrappableHtml

-------------------- Helpers ------------------

priceOne' :: (Monad m, PricedOnce a) => a -> HtmlT m ()
priceOne' = toHtml . formatPrice . priceOne

priceTwo' :: (Monad m, PricedTwice a) => a -> HtmlT m ()
priceTwo' = toHtml . formatPrice . priceTwo

priceThree' :: (Monad m, PricedThree a) => a -> HtmlT m ()
priceThree' = toHtml . formatPrice . priceThree

itemsDescription' :: (Monad m, NamedItem a) => a -> HtmlT m ()
itemsDescription' = toHtml . itemsDescription

itemsName' ::  (Monad m, NamedItem a) => a -> HtmlT m ()
itemsName' = toHtml . itemsName

priceFour' :: Monad m => Item4 -> HtmlT m ()
priceFour' = toHtml . formatPrice . fourthPrice
--------------------------- Format -------------------------- 
formatPrice :: String -> String
formatPrice l | null l = []
formatPrice l | length l < 4 = "$" ++ l
formatPrice l = ("$" ++) . addDot . removeDollar  $ l
          where 
            removeDollar = filter (not . flip elem ("$. " :: String))
            addDot = reverse . insertAt 2 '.' . reverse


formatItem :: Item -> Item 
formatItem it = it {firstPrice = formatPrice $ firstPrice it} 

--------------------------- Item Styles ------------------------
itemStyle :: Monad m => Item -> HtmlT m ()
itemStyle it = let i = formatItem it in do
	div_ [class_ "menu-c"] $ do 
	   h3_ [class_ "carta-name"] (itemsName' i)
	   div_ [class_ "content-info-price"] $ do 
	   	(itemsDescription' i) <> (priceOne' i)

-- item2Style :: Item2 -> Html ()
item2Style :: Monad m => Item2 -> HtmlT m ()
item2Style i2 = do
	div_ [class_ "row"] $ do 
	  div_ [class_ "large-4 columns"] $ h5_ [class_ "carta-titulo"] (itemsName' i2)
	  div_ [class_ "large-4 columns"] $ span_ [class_ "carta-descripcion"] (itemsDescription' i2)
	  div_ [class_ "large-2 columns"] $ span_ [class_ "carta-precio"] (priceOne' i2)
	  div_ [class_ "large-2 columns"] $ span_ [class_ "carta-precio"] (priceTwo' i2) 

-- item3Style :: Item3 -> Html ()
item3Style :: Monad m => Item3 -> HtmlT m ()
item3Style i3 = do
	div_ [class_ "row"] $ do 
	  div_ [class_ "large-3 columns"] $ h5_ [class_ "carta-titulo"] (itemsName' i3)
	  div_ [class_ "large-3 columns"] $ span_ [class_ "carta-descripcion"] (itemsDescription' i3)
	  div_ [class_ "large-2 columns"] $ span_ [class_ "carta-precio"] (priceOne' i3)
	  div_ [class_ "large-2 columns"] $ span_ [class_ "carta-precio"] (priceTwo' i3) 
	  div_ [class_ "large-2 columns"] $ span_ [class_ "carta-precio"] (priceThree' i3) 


----------------------- Convert to Html ---------------------

--How to transform a Maybe type into Html
instance ToHtml a => ToHtml (Maybe a) where
 	toHtml Nothing = mempty :: Monad m => HtmlT m () 
 	toHtml (Just x) = toHtml x :: Monad m => HtmlT m () 

 	toHtmlRaw Nothing = mempty :: Monad m => HtmlT m () 
 	toHtmlRaw (Just x) = toHtmlRaw x :: Monad m => HtmlT m ()  

--------------------- Deafult conversion from ItemCarta to html ------------

instance ToHtml ItemCarta where
	toHtml m = case m of 
		          D1PItem i -> itemStyle i 
		          D2PItem i -> item2Style i
		          D3PItem i -> item3Style i 

	toHtmlRaw = undefined
 
instance WrappableHtml ItemCarta
--------------------------------------- STYLE RESTAURANTE ANK 
newtype AnkItemCarta = AnkItemCarta { ankItemCarta :: ItemCarta} deriving (Eq, Show)
instance ToHtml AnkItemCarta where
	toHtml (AnkItemCarta m) = case m of 
		          D1PItem i -> do
		               dl_ $ do 
		               	  dt_ (itemsName' i)
		               	  dd_ [class_ "price"] (priceOne' i)
		               div_ [class_ "carta-txt"] $ do 
		               	  span_ [class_ "carta-span"] (itemsDescription' i)    

		          D2PItem i -> undefined
		          D3PItem i -> undefined
		          D4PItem i -> undefined
	toHtmlRaw _ = undefined 
instance WrappableHtml AnkItemCarta
--------------------------------------- STYLE MOLTO 
newtype MoltoItemCarta = MoltoItemCarta { moltoItemCarta :: ItemCarta} deriving (Eq, Show)
instance ToHtml MoltoItemCarta where
	toHtml (MoltoItemCarta m) = case m of 
		          D1PItem i -> 
		          	tr_ $ do 
		          		td_ [class_ "title-carta"] $ do
		          		  itemsName' i 
		          		  p_ $ itemsDescription' i
		          		td_ [class_ "precio-carta"] $ priceOne' i

		          D2PItem i -> undefined
		          D3PItem i -> 
		             tr_ $ do 
		          		td_ [class_ "title-carta"] $ do
		          		  itemsName' i 
		          		  p_ $ itemsDescription' i
		          		td_ [class_ "precio-carta"] $ priceOne' i
		          		td_ [class_ "precio-carta"] $ priceTwo' i
		          		td_ [class_ "precio-carta"] $ priceThree' i

		          D4PItem i -> 
		             tr_ $ do 
		          		td_ [class_ "title-carta"] $ do
		          		  itemsName' i 
		          		  p_ $ itemsDescription' i
		          		td_ [class_ "precio-carta"] $ priceOne' i
		          		td_ [class_ "precio-carta"] $ priceTwo' i
		          		td_ [class_ "precio-carta"] $ priceThree' i
		          		td_ [class_ "precio-carta"] $ priceFour' i

	toHtmlRaw _ = undefined

instance WrappableHtml MoltoItemCarta where
	wrapHtml h html = 
		div_ [class_ "container text-carta text-center"] $ do 
			span_ [class_ "line-title-carta"] $ do 
				span_ (blockHeader h) 
			table_ [class_ "table carta-molto"] $ do 
					tbody_ (html)
	blockHeader _ = "ENTRADAS" 

--------------------------------------- STYLE BRASAS 
newtype BrasasItemCarta = BrasasItemCarta { brasasItemCarta :: ItemCarta} deriving (Eq, Show)
instance ToHtml BrasasItemCarta where
	toHtml (BrasasItemCarta m) = case m of 
		          D1PItem i -> undefined
		          D2PItem i -> undefined
		          D3PItem i -> undefined
		          D4PItem i -> undefined
	toHtmlRaw _ = undefined 
instance WrappableHtml BrasasItemCarta
--------------------------------------- STYLE SUSHI7
newtype Sushi7ItemCarta = Sushi7ItemCarta { sushi7ItemCarta :: ItemCarta} deriving (Eq, Show) 
instance ToHtml Sushi7ItemCarta where
	toHtml (Sushi7ItemCarta m) = case m of 
		          D1PItem i -> undefined
		          D2PItem i -> undefined
		          D3PItem i -> undefined
		          D4PItem i -> undefined
	toHtmlRaw _ = undefined 
instance WrappableHtml Sushi7ItemCarta
--------------------------------------- STYLE PICOTEO
newtype PicoteoItemCarta = PicoteoItemCarta { picoteoItemCarta :: ItemCarta} deriving (Eq, Show) 
instance ToHtml PicoteoItemCarta where
	toHtml (PicoteoItemCarta m) = case m of 
		          D1PItem i -> do 
		          	li_ $ itemsName' i 
		          	small_ $ itemsDescription' i
		          	
		          D2PItem i -> undefined
		          D3PItem i -> undefined
		          D4PItem i -> undefined
	toHtmlRaw _ = undefined 	
instance WrappableHtml PicoteoItemCarta
--------------------------------------- STYLE WAJACA
newtype WajacaItemCarta = WajacaItemCarta { wajacaItemCarta :: ItemCarta} deriving (Eq, Show) 
instance ToHtml WajacaItemCarta where
	toHtml (WajacaItemCarta m) = case m of 
		          D1PItem i -> undefined
		          D2PItem i -> undefined
		          D3PItem i -> undefined
		          D4PItem i -> undefined
	toHtmlRaw _ = undefined 
instance WrappableHtml WajacaItemCarta
--------------------------------------- STYLE WAJACA
newtype VillageItemCarta = VillageItemCarta { villageItemCarta :: ItemCarta} deriving (Eq, Show) 
instance ToHtml VillageItemCarta where
	toHtml (VillageItemCarta m) = case m of 
		          D1PItem i -> undefined
		          D2PItem i -> undefined
		          D3PItem i -> undefined
		          D4PItem i -> undefined
	toHtmlRaw _ = undefined 

instance WrappableHtml VillageItemCarta
--------------------------------------- STYLE FEDERAL RIBS
newtype FederalRibsItemCarta = FederalRibsItemCarta { federalRibsItemCarta :: ItemCarta} deriving (Eq, Show) 
instance ToHtml FederalRibsItemCarta where
	toHtml (FederalRibsItemCarta m) = case m of 
		          D1PItem i -> do 
		          	div_ [class_ "col-md-3"] $ do
		          	    img_ [src_ "/images/logos/federal-logo-carta.jpg", style_ "text-align: justify;"]
		          	div_ $ do 
		          		h3_ (itemsName' i)
		          		span_ (itemsDescription' i)

		          D2PItem i -> undefined
		          D3PItem i -> undefined
		          D4PItem i -> undefined
	toHtmlRaw _ = undefined 

instance WrappableHtml FederalRibsItemCarta
