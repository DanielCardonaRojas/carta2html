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
main = processOptionsWith' processCartaOptions

processCartaOptions :: CartaOptions -> IO ()
processCartaOptions (CartaOptions file (StyleSelector s) wraps cols sys title) = 
	let 
	    -- Main html rendering function
	    renderHtml h = renderToFile (getFileName file ++ ".html") (styleCartaItems sys cols h)
	    -- Reads all well formed csv records
	    correctParsedRecords = fmap rights $ readNamedRecords' file :: IO [ItemCarta]
	    parseAndRenderWithStyle f = correctParsedRecords >>= renderHtml . map f
	    -- ADD NEW STYLES HERE
	    styleAction = 
	    	[ ("brasas", parseAndRenderWithStyle BrasasItemCarta)
	    	, ("sushi7", parseAndRenderWithStyle Sushi7ItemCarta)
	    	, ("wajaca", parseAndRenderWithStyle WajacaItemCarta)
	    	, ("village", parseAndRenderWithStyle VillageItemCarta)
	    	, ("ank", parseAndRenderWithStyle AnkItemCarta)
	    	, ("molto", parseAndRenderWithStyle MoltoItemCarta)
	    	, ("todofresa", parseAndRenderWithStyle TodoFresaItemCarta)
	    	]
	    lookupStyle someStyle = lookup someStyle styleAction
	    styles = fst $ unzip styleAction
	in maybe (printAvailableOption styles >> parseAndRenderWithStyle id) id (lookupStyle s)


printAvailableOption ls = putStrLn "Estos son estilos disponibles: " >> mapM_ print ls
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


 
	  
