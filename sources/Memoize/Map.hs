-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|

from the @uglymemo@ package. 

-}
module Memoize.Map(memoIO, memo) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Map (Map)
import System.IO.Unsafe (unsafePerformIO)

{-|

Memoize the given function by allocating a memo table,
and then updating the memo table on each function call.

The internal memo table is an 'MVar',
and thus should be thread-safe.

-}
memoIO :: forall a b. (Ord a)
       => (a -> b)           -- ^Function to memoize
       -> IO (a -> IO b)
memoIO f = do
    v <- newMVar M.empty
    let h = g v
    return h
  where
  g :: MVar (Map a b) -> a -> IO b
  g table x = do
    m <- readMVar table
    let y' = M.lookup x m 
    case y' of
      -- already computed 
      Just y  -> return y
      -- compute then insert 
      Nothing -> do
        let y = f x
        modifyMVar_ table (return . M.insert x y)
        return y

{-# INLINE memoIO #-}

{-|

The pure version of 'memoIO' (preserves referential transparency)

-}
memo :: (Ord a)
     => (a -> b)           -- ^Function to memoize
     -> (a -> b)
memo f = h
  where
  g = unsafePerformIO (memoIO f)
  h x = unsafePerformIO (g x)
{-# INLINE memo #-}
