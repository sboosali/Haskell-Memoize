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

import System.Console.GetOpt
--import Data.Maybe ( fromMaybe )

----------------------------------------

{-|
@
stack build && stack exec -- example-memo
@
-}
main :: IO ()
main = do
 flag <- getArguments myCommand >>= \case
  [Input s] -> return (s)
  _       -> return ("")
 mainWith flag

type CommandDescription a = (String, OptionDescriptions a)
type OptionDescriptions a = [OptDescr a]

data Flag
  = Input String
  deriving (Show)
    -- = Verbose  | Version 
    --- | Input String | Output String | LibDir String

myCommand :: CommandDescription Flag
myCommand =
  ( ""
  , [
    ]
  )

-- getOpt returns a triple consisting of the option arguments, a list of non-options, and a list of error messages.

-- myOptions :: OptionDescriptions Flag
-- myOptions =
--   [
--   ]

getArguments
  :: CommandDescription a
  -> IO [a]
--  -> IO ([a], [String])
getArguments (name,options) = do
  argv <- getArgs
  let result = getOpt Permute options argv 
  case result of
   (o,_n,[]  ) -> return o -- (o,n)
   (_,_,errs) -> failure errs
  where
  failure errors =
    ioError (userError (concat errors ++ usageInfo header options))
  header =
    "Usage: "++ name ++" [...] ..."

----------------------------------------

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
 print $ outputs == Memo.memoSTMap increment inputs 

 putStrLn "\n[dejafu]"
 print =<< Dejafu.autocheck (increments_Dejafu inputs)
 
 putStrLn "\n[memoOn MVar HashMap]"
 ys <- Memo.memoOn (Memo.Memoizer Memo.memo_WithHashMap_ViaMVar) increment inputs 
 print $ fmap increment inputs == ys 
 
 putStrLn "\n[memo MVar]"
 print $ fmap increment inputs == fmap increment_memoViaMVar inputs

 return () 
