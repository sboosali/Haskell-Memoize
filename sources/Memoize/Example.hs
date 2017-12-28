{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Memoize.Example where
-- import Memoize
import qualified Memoize.Map as MapMemo

import System.Environment

{-|
@
stack build && stack exec -- example-memo
@
-}
main :: IO ()
main = do
 arguments <- getArgs >>= \case
  [s] -> return (s)
  _ -> return ("")
 mainWith arguments

increment :: Integer -> Integer
increment = (+1) 

increment_memo :: Integer -> Integer
increment_memo = MapMemo.memo increment 

mainWith s = do
 putStrLn s
 putStrLn "(Memoize.Example...)"

 let domain = [1..10]
 putStrLn "[`increment` coincides with `increment_memo`]"
 print $ fmap increment domain == fmap increment_memo domain

