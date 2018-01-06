{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Memoize.Example where
-- import Memoize
import qualified Memoize.Map as MapMemo
import qualified Memoize.Table as Memo  
import Memoize.Types as Memo  

import "base" System.Environment

import qualified "dejafu" Test.DejaFu as Dejafu
-- import qualified "concurrency" Control.Concurrent.Classy as 
import           "concurrency" Control.Concurrent.Classy (MonadConc)

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

increment_memoViaMVar :: Integer -> Integer
increment_memoViaMVar = Memo.memoViaMVar increment

-- increment_Dejafu :: Integer -> MonadConc Integer
-- increment_Dejafu = f
--  where 
--  f :: (Integer -> Integer) -> Dejafu.MonadConc (Integer -> Dejafu.MonadConc Integer)
--  f = Memo.memo_WithMap_ViaDejafu increment

increments_Dejafu :: forall m. (MonadConc m) => [Integer] -> m [Integer] 
increments_Dejafu xs = do
  -- :: (Integer -> Integer) -> (Integer -> m Integer)
  f <- Memo.memo_WithMap_ViaDejafu increment
  traverse f xs 

mainWith s = do
 putStrLn s
 putStrLn "(Memoize.Example...)"

 let inputs = [1..10]
 let outputs = fmap increment inputs
 
 putStrLn "\n[memo]"
-- putStrLn "[`increment` coincides with `increment_memo`]"
 print $ fmap increment inputs == fmap increment_memo inputs

 putStrLn "\n[memo ST]"
 print $ outputs == Memo.memoOn_MapST increment inputs 

 putStrLn "\n[dejafu]"
 print =<< Dejafu.autocheck (increments_Dejafu inputs)
 
 putStrLn "\n[memoOn MVar HashMap]"
 ys <- Memo.memoOn (Memo.Memoizer Memo.memo_WithHashMap_ViaMVar) increment inputs 
 print $ fmap increment inputs == ys 
 
 putStrLn "\n[memo MVar]"
 print $ fmap increment inputs == fmap increment_memoViaMVar inputs

 return () 
