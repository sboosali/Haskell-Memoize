-- {-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

{-|

-}
module Memoize.Table where

-- import "StateVar" Data.StateVar

import qualified "containers" Data.Map as Map
import           "containers" Data.Map        (Map)

import qualified "containers" Data.IntMap as IntMap
import           "containers" Data.IntMap     (IntMap)

-- import qualified "containers" Data.Set as Set
-- import           "containers" Data.Set        (Set)

import qualified "base" Control.Concurrent.MVar as MVar
import           "base" Control.Concurrent.MVar (MVar)

import qualified "base" Data.IORef as IORef
import           "base" Data.IORef (IORef)

import "base" System.IO.Unsafe (unsafePerformIO)
import "base" Control.Arrow

-- import "stm" Control.Concurrent.STM (STM)
-- import "stm" Control.Concurrent.STM.TVar (TVar,
--                                     newTVar,
--                                     readTVar,
--                                     writeTVar)


-- import Control.Concurrent.STM (STM)
-- import Control.Concurrent.STM.TVar (TVar,
--                                     newTVar,
--                                     readTVar,
--                                     writeTVar)

-- import Control.Monad.ST (ST)
-- import Data.IORef (IORef,
--                    atomicModifyIORef',
--                    modifyIORef',
--                    atomicModifyIORef,
--                    modifyIORef,
--                    newIORef,
--                    readIORef,
--                    writeIORef)
-- import Data.STRef (STRef,
--                    modifySTRef',
--                    modifySTRef,
--                    newSTRef,
--                    readSTRef,
--                    writeSTRef)

import "spiros" Prelude.Spiros

--------------------------------------------------------------------------------

{-| a mapping @t@ between @a@ and @b@  

-}
data Table t a b = Table
 { emptyTable :: !t 
 , readTable  :: !(t -> a -> Maybe b)
 , writeTable :: !(a -> b -> t -> t)
 } 

{-| a reference @r@ holding @a@s within a monad @m@ 

-}
data Reference r m a = Reference
 { newReference    :: !(a -> m (r a))
 , readReference   :: !(r a -> m a)
 , writeReference  :: !(r a -> a        -> m ())
 , modifyReference :: !(r a -> (a -> a) -> m ())
 }


{-


data Table t a b = Table
 { emptyTable :: t 
 , readTable  :: t -> a -> Maybe b
 , writeTable :: a -> b -> t -> t  
 } 

data Reference r m a = Reference
 { newReference    :: a -> m (r a) 
 , readReference   :: r a -> m a
 , writeReference  :: r a -> a -> m ()
 , modifyReference :: r a -> (a -> a) -> m () 
 }


data Table t a b = Table
 { emptyTable :: !t 
 , readTable  :: !(t -> a -> Maybe b)
 , writeTable :: !(a -> b -> t -> t)
 } 

data Reference r m a = Reference
 { newReference    :: !(a -> m (r a))
 , readReference   :: !(r a -> m a)
 , writeReference  :: !(r a -> a -> m ())
 , modifyReference :: !(r a -> (a -> a) -> m ())
 }

-}


{-

table :: Table (t) a b 
table = Table{..}
  where
  emptyTable = 
  readTable  = 
  writeTable =

reference :: Reference r m a 
reference = Reference{..} 
  where 
  newReference    =  -- :: a -> m (r a) 
  readReference   =  -- :: r a -> m a
  writeReference  =  -- :: r a -> a -> m ()
  modifyReference =  -- :: r a -> (a -> a) -> m ()

-}

--------------------------------------------------------------------------------

type TableMap a b = Table (Map a b) a b

tableMap :: (Ord a) => TableMap a b
-- tableMap :: forall a b. (Ord a) => Table (Map a b) a b
tableMap = Table{..}
  where
  emptyTable = Map.empty       -- :: Map a b
  readTable  = flip Map.lookup -- :: Map a b -> a -> Maybe b 
  writeTable = Map.insert      -- :: a -> b -> Map a b -> Map a b

type TableIntMap b = Table (IntMap b) Int b

tableIntMap :: Table (IntMap b) Int b
tableIntMap = Table{..}
  where
  emptyTable = IntMap.empty      
  readTable  = flip IntMap.lookup 
  writeTable = IntMap.insert      

-- tableSet :: (Ord a) => Table (Set a) a Bool 
-- tableSet = Table{..}
--   where
--   emptyTable = Set.empty      
--   readTable  = flip Set.member
--   writeTable = \a b -> if b then Set.insert a else Set.delete a -- TODO this makes sense right? 
--   -- boolean2maybe False = Nothing
--   -- boolean2maybe True  = Just ()
--   -- maybe2boolean = maybe False (const True)  

--------------------------------------------------------------------------------

{-| 

-}
referenceMVar :: Reference MVar IO a 
referenceMVar = Reference{..} 
  where 
  newReference    = MVar.newMVar  
  readReference   = MVar.takeMVar 
  writeReference  = MVar.putMVar 
  modifyReference r f = MVar.modifyMVar_ r (f >>> return) 


{-| 

-}
referenceAtomicIORef :: Reference IORef IO a 
referenceAtomicIORef = Reference{..} 
  where 
  newReference    = IORef.newIORef  
  readReference   = IORef.readIORef 
  writeReference  = IORef.atomicWriteIORef
  modifyReference r f = IORef.atomicModifyIORef' r (f &&& const ())

{-| 

-}
referenceIORef :: Reference IORef IO a 
referenceIORef = Reference{..} 
  where 
  newReference    = IORef.newIORef  
  readReference   = IORef.readIORef 
  writeReference  = IORef.writeIORef
  modifyReference = IORef.modifyIORef' 

-- -XExistentialQuantification

-- data Table a b = forall table. Table
--  { readTable  :: table -> a -> Maybe b
--  , writeTable :: table -> a -> b -> table 
--  } 

-- data Reference m a = forall reference. Reference
--  { newReference    :: m (reference a) 
--  , readReference   :: reference      -> m a
--  , writeReference  :: reference -> a -> m ()
--  }

{- TODO déjà fu

import "dejafu" Control.Concurrent.Classy as Dejafu  

myFunction :: MonadConc m => m String

referenceClassyMVar :: MonadConc m => Reference MVar m a 
referenceClassyMVar = Reference{..} 
  where 
  newReference    = Dejafu.newMVar  
  readReference   = Dejafu.takeMVar 
  writeReference  = Dejafu.putMVar 
  modifyReference r f = Dejafu.modifyMVar_ r (f >>> return) 

-}


--------------------------------------------------------------------------------

{-|

The pure version of 'memoIO' (preserving referential transparency)

-}
memoWithMap
  :: (Ord a)
  => (a -> b)           -- ^ Function to memoize
  -> (a -> b)
memoWithMap f = h
  where
  g = unsafePerformIO (memoWithMapInIO f)
  h x = unsafePerformIO (g x)
{-# INLINE memoWithMap #-}

{-|

Memoize the given function by allocating a memo table,
and then updating the memo table on each function call.

The internal memo table is an 'MVar',
and thus should be thread-safe.

@
= 'memoWithM' 'tableMap' 'referenceMVar'
@ 

-}
memoWithMapInIO :: (Ord a) => (a -> b) -> IO (a -> IO b)
memoWithMapInIO = memoWithM tableMap referenceAtomicIORef -- TODO referenceMVar causes "blocked indefinitely" exception
-- referenceMVar "blocked indefinitely" / referenceAtomicIORef worked / referenceIORef worked 
{-# INLINE memoWithMapInIO #-}

--------------------------------------------------------------------------------

memoWithM
  :: forall t r m a b. (Monad m) 
  => (Table t) a b
  -> Reference r m t
  -> (a -> b)
  -> m (a -> m b)
memoWithM Table{..} Reference{..} = \f -> do    -- the INLINE pragmatic uses the "lexical left-hand side" 
    r <- newReference emptyTable      -- we close over the table, and thus it's 
                                      -- shared by all calls to the `h` closure 
    let g = cached r f
    return g

  where
  cached :: r t -> (a -> b) -> (a -> m b)
  cached reference f x = do
    table <- readReference reference 
    let y' = readTable table x 
    case y' of
      -- do nothing if already computed 
      Just y  -> return y
      -- otherwise compute, then insert 
      Nothing -> do
        let y = f x
        -- compute once 
        reference `modifyReference` (writeTable x y)
        return y

{-# INLINEABLE memoWithM #-}
-- {-# SPECIALIZE memoWithM :: #-}
-- {-# SPECIALIZE memoWithM :: #-}

-- --------------------------------------------------------------------------------

-- {-|

-- Memoize the given function by allocating a memo table,
-- and then updating the memo table on each function call.

-- The internal memo table is an 'MVar',
-- and thus should be thread-safe.

-- -}
-- memoIO
--   :: forall a b. (Ord a)
--   => (a -> b)           -- ^Function to memoize
--   -> IO (a -> IO b)
-- memoIO f = do
--     v <- newMVar M.empty
--     let h = g v
--     return h
--   where
--   g :: MVar (Map a b) -> a -> IO b
--   g table x = do
--     m <- readMVar table
--     let y' = M.lookup x m 
--     case y' of
--       -- already computed 
--       Just y  -> return y
--       -- compute then insert 
--       Nothing -> do
--         let y = f x
--         modifyMVar_ table (return . M.insert x y)
--         return y

-- {-# INLINE memoIO #-}

