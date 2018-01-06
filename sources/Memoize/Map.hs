-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|

equivalent to the @uglymemo@ package. 

@
import qualified Memoize.Map as Map

f = Map.memo f'
@

-}
module Memoize.Map(memoIO, memo) where
import Memoize.Table

-- import Control.Concurrent.MVar
-- import qualified Data.Map as M
-- import Data.Map (Map)

--------------------------------------------------------------------------------

{-|

@
= 'memoWithMap' 
@ 


-}
memo
  :: (Ord a)
  => (a -> b)  
  -> (a -> b)
memo = memoWithMap 

{-|

@
= 'memoWithMapInIO' 
@ 

-}
memoIO :: (Ord a) => (a -> b) -> IO (a -> IO b)
memoIO = memoWithMapInIO

-- -- |The 'MonadRef' type class abstracts over the details of manipulating
-- -- references, allowing one to write code that uses references and can operate
-- -- in any monad that supports reference operations.

-- class (Monad m) => MonadRef r m | m -> r where
--     -- |Create a new reference
--     newRef    :: a -> m (r a)
--     -- |Read the value of a reference
--     readRef   :: r a -> m a
--     -- |Write a new value to a reference
--     writeRef  :: r a -> a -> m ()
--     -- |Mutate the contents of a reference
--     modifyRef :: r a -> (a -> a) -> m ()
--     modifyRef r f = readRef r >>= writeRef r . f
--     -- |Strict version of 'modifyRef'
--     modifyRef' :: r a -> (a -> a) -> m ()
--     modifyRef' r f = readRef r >>= \x -> let x' = f x in x' `seq` writeRef r x'

-- class (MonadRef r m) => MonadAtomicRef r m | m -> r where
--     -- |Atomically mutate the contents of a reference
--     atomicModifyRef :: r a -> (a -> (a, b)) -> m b
--     -- |Strict version of atomicModifyRef. This forces both the value stored in
--     -- the reference as well as the value returned.
--     atomicModifyRef' :: r a -> (a -> (a, b)) -> m b
--     atomicModifyRef' r f = do
--         b <- atomicModifyRef r
--                 (\x -> let (a, b) = f x
--                         in (a, a `seq` b))
--         b `seq` return b

-- instance MonadRef (STRef s) (ST s) where
--     newRef     = newSTRef
--     readRef    = readSTRef
--     writeRef   = writeSTRef
--     modifyRef  = modifySTRef
--     modifyRef' = modifySTRef'

-- instance MonadRef IORef IO where
--     newRef     = newIORef
--     readRef    = readIORef
--     writeRef   = writeIORef
--     modifyRef  = modifyIORef
--     modifyRef' = modifyIORef'

-- instance MonadRef TVar STM where
--     newRef    = newTVar
--     readRef   = readTVar
--     writeRef  = writeTVar

-- instance MonadAtomicRef IORef IO where
--     atomicModifyRef = atomicModifyIORef
--     atomicModifyRef' = atomicModifyIORef'

-- instance MonadAtomicRef TVar STM where
--     atomicModifyRef r f = do x <- readRef r
--                              let (x', y) = f x
--                              writeRef r x'
--                              return y

--------------------------------------------------------------------------------

{-

module Data.MemoUgly(memoIO, memo) where
import Control.Concurrent.MVar
import qualified Data.Map as M
import System.IO.Unsafe(unsafePerformIO)

-- | Memoize the given function by allocating a memo table,
-- and then updating the memo table on each function call.
memoIO :: (Ord a)
       => (a -> b)           -- ^Function to memoize
       -> IO (a -> IO b)
memoIO f = do
    v <- newMVar M.empty
    let f' x = do
            m <- readMVar v
            case M.lookup x m of
                Nothing -> do let { r = f x }; modifyMVar_ v (return . M.insert x r); return r
                Just r  -> return r
    return f'

-- | The pure version of 'memoIO'.
memo :: (Ord a)
     => (a -> b)           -- ^Function to memoize
     -> (a -> b)
memo f = let f' = unsafePerformIO (memoIO f) in \ x -> unsafePerformIO (f' x)

-}
