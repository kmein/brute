-- | the module from which the brute-forcing function is imported
module BruteForce (bruteForce, bruteForce') where

import Control.Monad (replicateM)

-- | lists every possible combination
-- from an alphabet given it and a combination length
bruteForce' :: [a] -- ^ an input alphabet
            -> Int -- ^ the list length
            -> [[a]] -- ^ the combination list
bruteForce' = flip replicateM

-- | works much like `bruteForce'`, it just is not limited on a certain length
bruteForce :: [a] -- ^ an 'alphabet' (list to choose elements from)
           -> [[a]] -- ^ stream of all possible lists
bruteForce alphabet = concatMap (bruteForce' alphabet) [1..]
