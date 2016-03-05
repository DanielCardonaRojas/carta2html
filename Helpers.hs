module Helpers where 

import ListModifiers
--Helpers

(<?>) :: Monoid b => (a -> b) -> Maybe a -> b
(<?>) = maybe mempty 
(.>) = flip (.)
(|>) = flip ($)

joinHtml pre pos n = mconcat .  map pos . map mconcat . splitEvery n . map pre


collectReduce n = splitEvery n .> map mconcat 
divideReduce n = divide n .> map mconcat
divide n ls = let l = length ls in splitEvery (div (l + 1) n) ls
collectReduceWrap n g  = collectReduce n .> map g    
joinHtml' n f g ls = ls |> map f |> collectReduce n |> map g 