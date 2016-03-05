{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import CsvUtils
import CartaTypes
import CartaStyles
import System.Environment (getArgs)
import System.Directory
import Data.Either
import Lucid
import Customizable
import WrappableHtml
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


styleSelectors = 
	[ "molto"
	, "brasas"
	]
             
processCartaOptions :: CartaOptions -> IO ()
processCartaOptions (CartaOptions file (StyleSelector s) wraps cols sys title) = 
	let 	
	    parseItemAndRenderWithStyle' :: ToHtml a => (ItemCarta -> a) -> IO ()	    
	    parseItemAndRenderWithStyle' = parseItemAndRenderWithStyle file sys cols
	    parseCartaAndRenderWithStyle' :: Wrappable CartaSection a => (ItemCarta -> a) -> IO ()
	    parseCartaAndRenderWithStyle' = parseCartaAndRenderWithStyle file sys cols title
	    -- ADD NEW STYLES HERE
	    itemRenderStyles = zip styleSelectors
	    	[ parseItemAndRenderWithStyle' MoltoItemCarta
	    	, parseItemAndRenderWithStyle' BrasasItemCarta
	    	]
	    cartaRenderStyles = zip styleSelectors
	    	[ parseCartaAndRenderWithStyle' MoltoItemCarta
	    	, parseCartaAndRenderWithStyle' BrasasItemCarta
	    	]	    	  
	in case wraps of 
		Wrapped -> do 		
			maybe (printAvailableOption styleSelectors) id (lookup s cartaRenderStyles)
		NotWrapped -> do
			maybe (printAvailableOption styleSelectors) id (lookup s itemRenderStyles)

------------------------------ Rendering Function -----------------------------
renderCarta :: Wrappable CartaSection a => FilePath -> GridSys -> Int -> CartaSection a -> IO ()
renderCarta f sys n c = renderToFile (getFileName f ++ ".html") $ styleSection sys n c

renderItems :: ToHtml a => FilePath -> GridSys -> Int -> [a] -> IO ()
renderItems file sys n h = renderToFile (getFileName file ++ ".html") (styleCartaItems sys n h)

parseItemAndRenderWithStyle :: ToHtml a => FilePath -> GridSys -> Int -> (ItemCarta -> a) -> IO ()
parseItemAndRenderWithStyle file sys n f = 
	   parsedRecords file >>= (renderItems file sys n . map f)

parseCartaAndRenderWithStyle :: Wrappable CartaSection a => FilePath -> GridSys -> Int -> String -> (ItemCarta -> a) -> IO ()
parseCartaAndRenderWithStyle file sys n title f = 
	changeSectionItems f <$> (parsedCarta file title) >>= (renderCarta file sys n)	

printAvailableOption ls = putStrLn "Estos son estilos disponibles: " >> mapM_ print ls


--------------------------------- Parse CSV Functions -------------------------------
parsedRecords :: FilePath -> IO [ItemCarta]
parsedRecords file = fmap rights $ readNamedRecords' file

parsedCarta :: FilePath -> String -> IO (CartaSection ItemCarta)
parsedCarta file title = CartaSection <$> parsedRecords file <*> pure title

-------------------------------------- Utils -----------------------------------------
nameAndExt = swap . toBoth reverse . break' (== '.') . reverse 
    where
    	toBoth f (x,y) = (f x, f y)
    	swap (x,y) = (y,x)
    	break' p = fmap tail . break p

getFileName = fst . nameAndExt
getExt = snd . nameAndExt
isCSV = (== "csv") . getExt

getCSVFiles = filter (isCSV) <$> (getCurrentDirectory >>= getDirectoryContents)


 
	  
