{-#LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import CsvUtils
import CartaTypes
import CartaStyles
import System.Environment (getArgs)
import System.Directory
import Data.Either
import Lucid
import Customizable
import InputOptions

{- |
To load this in GHCI first do :set -XOverloadedStrings

This Program depends on cassava for csv and lucid for html templating

install running: cabal install 

Compile with:

ghc --make Main.hs -XOverloadedStrings -o cartaHtml

Generate haddock Documentation runing this in terminal: 
-h is for Generating Html
-w turnoff warnings

haddock <fileName>.hs -o <folderNameForDocumentation> -h -w --optghc=-XOverloadedStrings

Use example: 
cartaHtml "entradas.csv" "0 m"
Use 0 for no wrapping in columns
-}

-- Todo tidy up this function handle different args passing (e.g default outfileName)
main = processOptionsWith availableStyles processCartaOptions

availableStyles =
	["brasas"
    ,"sushi7"
    ,"wajaca"
    ,"village"
    ,"ank"
    ,"molto"
    ]

processCartaOptions :: CartaOptions -> IO ()
processCartaOptions (CartaOptions file (StyleSelector s) wraps cols sys title) = 
	let 
	    renderHtml h = renderToFile (getFileName file ++ ".html") (styleCartaItems sys cols h)
	    correctParse = fmap rights $ readNamedRecords' file :: IO [ItemCarta]
	in case s of 
	       "" -> correctParse >>= renderHtml 
	       "brasas" -> correctParse >>= renderHtml . map (BrasasItemCarta) 
	       "sushi7" -> correctParse >>= renderHtml . map (Sushi7ItemCarta)
	       "wajaca" -> correctParse >>= renderHtml . map (WajacaItemCarta)
	       "village" -> correctParse >>= renderHtml . map (VillageItemCarta)
	       "ank" -> correctParse >>= renderHtml . map (AnkItemCarta)
	       "molto" -> correctParse >>= renderHtml . map (MoltoItemCarta)

parseOpt :: String -> (Int, String)
parseOpt (c:cs) = (read [c], safeTail cs) where safeTail s = if null s then [] else tail s 

--------------- Utils -------------------
nameAndExt = swap . toBoth reverse . break' (== '.') . reverse 
    where
    	toBoth f (x,y) = (f x, f y)
    	swap (x,y) = (y,x)
    	break' p = fmap tail . break p

getFileName = fst . nameAndExt
getExt = snd . nameAndExt
isCSV = (== "csv") . getExt

getCSVFiles = filter (isCSV) <$> (getCurrentDirectory >>= getDirectoryContents)


getStyleOption :: ToHtml b => [(String,b)] -> String -> Maybe b    	
getStyleOption = flip lookup 	


 
	  