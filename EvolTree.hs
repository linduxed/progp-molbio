-- Imports, types and data {{{
module EvolTree where

import Distance

data Tree a = EmptyTree
            | Leaf a
            | Branch [Tree a]
-- }}}
