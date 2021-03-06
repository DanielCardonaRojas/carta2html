{- | 

En este modulo se describen los tipos bases para describir un elemento de una carta de 
restaurante. Por simplicidad se ha definido que un elemento de carta tiene nombre, posiblemente 
descripcion y al menos un precio. Para otro tipo de elementos con mas precios como para 
describir un trago en la seccion de licores se ha dispuesto dos tipos mas que pueden tener dos 
o tres precios.

-}
{-# LANGUAGE OverloadedStrings #-}
module CartaTypes ( ItemCarta (..)
                  , Item (..)
                  , Item2 (..)
                  , Item3 (..)
                  , Item4 (..)
                  , PricedOnce (..)
                  , PricedTwice (..)
                  , PricedThree (..)
                  , NamedItem (..)
                  , CartaSection (..)
                  , module InputOptions 
                  , testItemCarta
                  , testCartaSection
                  , changeSectionItems

                  ) where



import Data.Either
import CsvUtils
import Data.Csv
import Control.Applicative 
import Data.ByteString.Char8 hiding (map)
import Data.ByteString.Lazy.Char8 hiding (map)
import InputOptions

testItemCarta = D1PItem (Item "Hamburguesa" (Just "Carne de Buffalo") "$10.000" )
testCartaSection = CartaSection (Prelude.replicate 4 testItemCarta) "Comida Rapida"
-- | Un item de carta es un item descrito con 3,2 o 1 precio. 
data ItemCarta = D4PItem Item4 | D3PItem Item3 | D2PItem Item2 | D1PItem Item deriving (Eq, Show)

data CartaSection a = CartaSection {items :: [a], sectionName :: String} deriving (Eq,Show)

changeSectionItems f (CartaSection is name) = CartaSection (map f is) name 

data Item = Item
    { itemName :: String
    , description :: Maybe String
    , firstPrice :: String 
    } deriving (Eq, Show)

data Item2 = Item2
    { itemInfo :: Item
    , secondPrice :: String 
    } deriving (Eq, Show)

data Item3 = Item3
    { item2Info :: Item2
    , thirdPrice :: String 
    } deriving (Eq, Show)

data Item4 = Item4
    { item3Info :: Item3
    , fourthPrice :: String 
    } deriving (Eq, Show)


-- INSTANCES for parsing CSV data
instance FromNamedRecord Item  where
    parseNamedRecord r = Item <$> (r .: "nombre") <*> (r .: "descripcion") <*> (r .: "precio")

instance FromRecord Item where
    parseRecord r = Item <$> (r .! 0) <*> (r .! 1) <*> (r .! 2)

instance FromRecord Item2 where
    parseRecord r = Item2 <$> (parseRecord r) <*> (r .! 3)

instance FromNamedRecord Item2 where
     parseNamedRecord r = Item2 <$> (parseNamedRecord r) <*> (r .: "precio_2")

instance FromRecord Item3 where
    parseRecord r = Item3 <$> (parseRecord r) <*> (r .! 4)

instance FromNamedRecord Item3 where
     parseNamedRecord r = Item3 <$> (parseNamedRecord r) <*> (r .: "precio_3")

instance FromRecord Item4 where
    parseRecord r = Item4 <$> (parseRecord r) <*> (r .! 4)

instance FromNamedRecord Item4 where
     parseNamedRecord r = Item4 <$> (parseNamedRecord r) <*> (r .: "precio_4")

--Does Does Parser instance of Alternative rewind string if failure ? <|>
instance FromRecord ItemCarta where
    parseRecord r = (D3PItem <$> (parseRecord r)) <|> (D2PItem <$> (parseRecord r)) 
                                  <|> (D1PItem <$> (parseRecord r)) 

instance FromNamedRecord ItemCarta where
    parseNamedRecord r = 
            (D4PItem <$> (parseNamedRecord r)) 
               <|> (D3PItem <$> (parseNamedRecord r))
               <|> (D2PItem <$> (parseNamedRecord r)) 
               <|> (D1PItem <$> (parseNamedRecord r))            



-- Utility Classes
-- | Un item o elemento de la carta/menu que tiene al menos un primer precio
class PricedOnce a where 
    priceOne :: a -> String

-- | Un item o elemento de la carta/menu que tiene al menos un primer precio
class PricedTwice a where
    priceTwo :: a -> String

class PricedThree a where
    priceThree :: a -> String    


class NamedItem a where
    itemsName :: a -> String
    itemsDescription :: a -> Maybe String


instance PricedOnce Item where priceOne = firstPrice

instance PricedOnce Item2 where priceOne = firstPrice . itemInfo 
instance PricedOnce Item3 where priceOne = priceOne . item2Info
instance PricedOnce Item4 where priceOne = priceOne . item3Info

instance PricedTwice Item2 where priceTwo = secondPrice
instance PricedTwice Item3 where priceTwo = secondPrice . item2Info
instance PricedTwice Item4 where priceTwo = priceTwo . item3Info

instance PricedThree Item3 where priceThree = thirdPrice
instance PricedThree Item4 where priceThree = priceThree . item3Info


instance NamedItem Item where
    itemsName = itemName
    itemsDescription = description

instance NamedItem Item2 where
    itemsName = itemName . itemInfo
    itemsDescription = description . itemInfo

instance NamedItem Item3 where
    itemsName = itemsName . item2Info
    itemsDescription = itemsDescription . item2Info 

instance NamedItem Item4 where
    itemsName = itemsName . item3Info
    itemsDescription = itemsDescription . item3Info 





