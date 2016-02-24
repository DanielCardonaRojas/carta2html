{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module CartaStyles 
      ( styleCartaItems
      
	   ) where


import Lucid
import Data.Monoid
import Data.Text (pack,append) 
import qualified Data.Text.Lazy.IO as B
-- Custom Imports
import CartaTypes
import CsvUtils
import ListModifiers
import Customizable
import Data.List 
import Control.Applicative
import Control.Arrow
import WrappableHtml


{-
There is clearly something anoying about working with nested records and needing to repeat
code for these....

Something came to mind since HtmlT m () is a monad transformer. What about trying to use a Continuation monad.


Just to review the templating process. A list of instances of ToHtml are converted to HTMl (map toHtml).
Then a grouping of these html blocks (use a typeclass) is made (some more html might be interleaved between these blocks)
these blocks might be wrapped by some extra html and finally 
-}

-- TODO: Create a prewrapping of html and GridSystem

--Helpers

(<?>) :: Monoid b => (a -> b) -> Maybe a -> b
(<?>) = maybe mempty 

----------------------------- Column options ------------------------------
itemCartaStyle :: ToHtml m => m -> Html ()
itemCartaStyle = toHtml   

-- | Comment separator. Just to make html more readable 

comment s = toHtmlRaw ("\n<!--" <> s <> "-->\n")
commentBlock s = comment (padSep <> s <> padSep) where padSep = replicate 15 '*' 

nItemRow :: (ToHtml m) => GridSys -> Int -> [m] -> Html ()
nItemRow _ _ [] = mempty
nItemRow _ 0 is = mapM_ itemCartaStyle is 
nItemRow sys x is = 
	let 
	 padding m l = nextMultiple m l - l
	 numPadding = padding x (length is)
	 elems = (padAppending numPadding (last is) is)
	in div_ [class_ "row"] $ do 
         mapM_ ((commentBlock "FILA" <>) . div_ [class_ $ makeNRowClass sys x] . itemCartaStyle) elems


joinHtml pos pre n = pos . mconcat . map (map pre) . splitEvery n
      

makeNRowClass syst = pack . defaultColumnedRow syst 

defaultColumnedRow _ n | n < 0 = error "No se puede construir un layout con este numero de columnas"
defaultColumnedRow Foundation n = "large-" ++ (show n) ++ " medium-" ++ (show n) ++ " small-12 columns"
defaultColumnedRow Bootstrap n = "col-md-" ++ (show n) ++ " col-sm-" ++ (show n) ++ " col-xs-12"

nextMultiple n l = if mod l n == 0 then l else (div l n + 1) * n

------------------------------------ Exports ---------------------------------- 
-- | 'styleCartaItems' __n is__ produces Html using the ToHtml instances of __is__ elements and  
-- gathering them in rows of n columns (Bootstrap or Zurb Foundation).
-- 
-- > Note: calling styleCartaItems 0 is will do no extra wrapping around the HTML produced by each item.
styleCartaItems :: (ToHtml m, WrappableHtml m) => GridSys -> Int -> [m] -> Html () 
styleCartaItems g n is = mapM_  (\x -> wrapHtml (head x) $ nItemRow g n x) (splitEvery n is)


