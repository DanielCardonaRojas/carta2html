{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module CartaStyles 
      ( styleCartaItems
      , joinHtml	
      , module WrappableHtml
      , module ListModifiers

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
(.>) = flip (.)
(|>) = flip ($)

----------------------------- Column options ------------------------------
itemCartaStyle :: ToHtml m => m -> Html ()
itemCartaStyle = toHtml   

-- | Comment separator. Just to make html more readable 

comment s = toHtmlRaw ("\n<!--" <> s <> "-->\n")
commentBlock s = comment (padSep <> s <> padSep) where padSep = replicate 15 '*' 


makeGridCol :: GridSys -> Int -> Html () -> Html ()
makeGridCol sys x = (commentBlock "FILA" <>) . div_ [class_ $ makeNRowClass sys x] 

joinHtml pre pos n = mconcat .  map pos . map mconcat . splitEvery n . map pre


collectReduce n = splitEvery n .> map mconcat 
divideReduce n = divide n .> map mconcat
divide n ls = let l = length ls in splitEvery (div (l + 1) n) ls
collectReduceWrap n g  = collectReduce n .> map g    
joinHtml' n f g ls = ls |> map f |> collectReduce n |> map g 


styleCartaItems ::  WrappableHtml a => GridSys -> Int -> [a] -> Html ()
styleCartaItems sys 0 ls = joinHtml toHtml (wrapHtml (head ls)) 0 ls
styleCartaItems sys n ls = 
    let 
      wrapHtml' = wrapHtml $ head ls 
      makeGridCol' = makeGridCol sys n  
    in ls |> map toHtml |> divideReduce n |> map (wrapHtml' .> makeGridCol') |> map (div_ [class_ "row"]) |> mconcat


makeNRowClass syst n = pack $ defaultColumnedRow syst (div 12 n)

defaultColumnedRow _ n | n < 0 || n > 12 = error "No se puede construir un layout con este numero de columnas"
defaultColumnedRow Foundation n = "large-" ++ (show n) ++ " medium-" ++ (show n) ++ " small-12 columns"
defaultColumnedRow Bootstrap n = "col-md-" ++ (show n) ++ " col-sm-" ++ (show n) ++ " col-xs-12"

nextMultiple n l = if mod l n == 0 then l else (div l n + 1) * n
