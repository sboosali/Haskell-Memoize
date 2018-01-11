{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

{-|

-}
module Memoize.Table where

import Memoize.Types

-- import "StateVar" Data.StateVar

import qualified "concurrency" Control.Concurrent.Classy as Dejafu  
import           "concurrency" Control.Concurrent.Classy (MonadConc)  

import qualified "unordered-containers" Data.HashMap.Strict as HashMap
import           "unordered-containers" Data.HashMap.Strict (HashMap) 

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

import qualified "base" Data.STRef as STRef
import "base" Data.STRef (STRef) 
import           "base" Control.Monad.ST (ST, runST)

import qualified "stm" Control.Concurrent.STM as TVar
import           "stm" Control.Concurrent.STM (STM, TVar)

import "base" System.IO.Unsafe (unsafePerformIO)
import "base" Control.Arrow

import "spiros" Prelude.Spiros

----------------------------------------

type TableMap a b = Table (Map a b) a b

type TableIntMap b = Table (IntMap b) Int b

----------------------------------------

tableMap :: (Ord a) => TableMap a b
-- tableMap :: forall a b. (Ord a) => Table (Map a b) a b
tableMap = Table{..}
  where
  emptyTable = Map.empty       -- :: Map a b
  readTable  = flip Map.lookup -- :: Map a b -> a -> Maybe b 
  writeTable = Map.insert      -- :: a -> b -> Map a b -> Map a b

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

type TableHashMap a b = Table (HashMap a b) a b

tableHashMap :: (Eq a, Hashable a) => TableHashMap a b
-- tableHashMap :: forall a b. (Ord a) => Table (HashMap a b) a b
tableHashMap = Table{..}
  where
  emptyTable = HashMap.empty     
  readTable  = flip HashMap.lookup 
  writeTable = HashMap.insert      


----------------------------------------

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
  modifyReference r f = IORef.atomicModifyIORef r (f &&& const ())

{-| 

-}
referenceIORef :: Reference IORef IO a 
referenceIORef = Reference{..} 
  where 
  newReference    = IORef.newIORef  
  readReference   = IORef.readIORef 
  writeReference  = IORef.writeIORef
  modifyReference = IORef.modifyIORef

{-| 

-}
referenceSTRef :: Reference (STRef s) (ST s) a 
referenceSTRef = Reference{..} 
  where 
  newReference    = STRef.newSTRef  
  readReference   = STRef.readSTRef 
  writeReference  = STRef.writeSTRef
  modifyReference = STRef.modifySTRef

{-| 

-}
referenceTVar :: Reference TVar STM a 
referenceTVar = Reference{..} 
  where 
  newReference    = TVar.newTVar  
  readReference   = TVar.readTVar 
  writeReference  = TVar.writeTVar
  modifyReference = TVar.modifyTVar

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

-- déjà fu
referenceClassyMVar :: MonadConc m => Reference (Dejafu.MVar m) m a 
referenceClassyMVar = Reference{..} 
  where 
  newReference    = Dejafu.newMVar  
  readReference   = Dejafu.takeMVar 
  writeReference  = Dejafu.putMVar 
  modifyReference r f = Dejafu.modifyMVar_ r (f >>> return) 

----------------------------------------

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
-- memoWithMapInIO = memo_WithMap_ViaMVar
memoWithMapInIO = memo_WithMap_ViaAtomicIORef 
-- TODO referenceMVar causes "blocked indefinitely" exception
-- referenceMVar "blocked indefinitely" / referenceAtomicIORef worked / referenceIORef worked 
{-# INLINE memoWithMapInIO #-}

{-| memoize the function @f@ on the domain @xs@

@memoMapST f xs@ 

via 'ST'. the function is memoized only for the given domain.
so distinct calls have distinct tables. 

i.e. two calls

@
memoMapST f xs
memoMapST f xs
@ 

won't share anything, the computations will be "duplicated". 

-}
memoSTMap :: (Traversable t, Ord a) => (a -> b) -> t a -> t b
memoSTMap f xs = runST $ do
  memoOn (Memoizer memo_WithMap_ViaSTRef) f xs

  -- fmapMemo runST (Memoizer memo_WithMap_ViaSTRef)
  --
  -- where
  -- memo_WithMap_ViaSTRef :: (a -> b) -> ST s (a -> ST s b)
  -- g :: (a -> ST s b)
  -- traverse g :: t a -> ST s (t b)
{-# INLINE memoSTMap #-}

----------------------------------------

{-| purifies the impure memoizer, whose impurity is referentially transparent . 

-}
memoFromIO  
  :: Memoizer IO a 
  -> (a -> b)  
  -> (a -> b)
memoFromIO (Memoizer memoize) f = h
  where
  g = unsafePerformIO (memoize f)
  h x = unsafePerformIO (g x)
{-# INLINE memoFromIO #-}

memoViaMVar :: Ord a => (a -> b) -> a -> b
memoViaMVar = memoFromIO (Memoizer memo_WithMap_ViaMVar) 

memoViaIORef :: Ord a => (a -> b) -> a -> b
memoViaIORef = memoFromIO (Memoizer memo_WithMap_ViaAtomicIORef)

----------------------------------------

memo_WithMap_ViaMVar :: (Ord a) => (a -> b) -> IO (a -> IO b)
memo_WithMap_ViaMVar = memoWithM tableMap referenceMVar
{-# INLINE memo_WithMap_ViaMVar #-}

memo_WithMap_ViaAtomicIORef :: (Ord a) => (a -> b) -> IO (a -> IO b)
memo_WithMap_ViaAtomicIORef = memoWithM tableMap referenceAtomicIORef 
{-# INLINE memo_WithMap_ViaAtomicIORef #-}

memo_WithMap_ViaTVar :: (Ord a) => (a -> b) -> STM (a -> STM b)
memo_WithMap_ViaTVar = memoWithM tableMap referenceTVar
{-# INLINE memo_WithMap_ViaTVar #-}

memo_WithMap_ViaSTRef :: (Ord a) => (a -> b) -> ST s (a -> ST s b)
memo_WithMap_ViaSTRef = memoWithM tableMap referenceSTRef
{-# INLINE memo_WithMap_ViaSTRef #-}

memo_WithMap_ViaDejafu
  :: (MonadConc m, Ord a) => (a -> b) -> m (a -> m b)
memo_WithMap_ViaDejafu = memoWithM tableMap referenceClassyMVar
{-# INLINE memo_WithMap_ViaDejafu #-}

----------------------------------------

memo_WithHashMap_ViaMVar :: (Eq a, Hashable a) => (a -> b) -> IO (a -> IO b)
memo_WithHashMap_ViaMVar = memoWithM tableHashMap referenceMVar
{-# INLINE memo_WithHashMap_ViaMVar #-}

memo_WithHashMap_ViaAtomicIORef :: (Eq a, Hashable a) => (a -> b) -> IO (a -> IO b)
memo_WithHashMap_ViaAtomicIORef = memoWithM tableHashMap referenceAtomicIORef 
{-# INLINE memo_WithHashMap_ViaAtomicIORef #-}

memo_WithHashMap_ViaTVar :: (Eq a, Hashable a) => (a -> b) -> STM (a -> STM b)
memo_WithHashMap_ViaTVar = memoWithM tableHashMap referenceTVar
{-# INLINE memo_WithHashMap_ViaTVar #-}

memo_WithHashMap_ViaSTRef :: (Eq a, Hashable a) => (a -> b) -> ST s (a -> ST s b)
memo_WithHashMap_ViaSTRef = memoWithM tableHashMap referenceSTRef
{-# INLINE memo_WithHashMap_ViaSTRef #-}

memo_WithHashMap_ViaDejafu
  :: (MonadConc m, Eq a, Hashable a) => (a -> b) -> m (a -> m b)
memo_WithHashMap_ViaDejafu = memoWithM tableHashMap referenceClassyMVar
{-# INLINE memo_WithHashMap_ViaDejafu #-}

----------------------------------------

{-|

assuming @extract@ and @memoize@ are "proper",
e.g. that @extract@ is safe or that @memoize@ preserves the function,
then:

@
fmapMemo extract memoize f xs
=== 
fmap                     f xs 
@ 

-}
fmapMemo 
  :: (Monad m, Traversable t, Ord a)
  => (forall x. m x -> x)
  -> Memoizer m a
  -> (a -> b)
  -> t a
  -> t b
fmapMemo extract u f xs = extract $ memoOn u f xs

{-| memo the function only on a given domain.

@
-- memoize the function @f@ on the domain @xs@ with the memoizer @u@
memoOn u f xs
@

same shape as 'memoWithM'

-}
memoOn
  :: (Monad m, Traversable t, Ord a)
  => Memoizer m a
  -> (a -> b)
 --  -> (a -> m b)
  -> t a
  -> m (t b)
memoOn (Memoizer memoize) f xs = do
  g <- memoize f
  -- 
  traverse g xs
  -- memo_WithMap_ViaSTRef :: (a -> b) -> ST s (a -> ST s b)
  -- g :: (a -> ST s b)
  -- traverse g :: t a -> ST s (t b)
{-# INLINE memoOn #-}

{-|

When contested, for 'References' like 'MVar':

* there won't be corruption, 
if the 'Table' is immutable, and the function is pure.
* there should be no deadlock, 
but the thread that runs second will block 
until the thread that ran first finishes.

-}
memoWithM
  :: forall t r m a b. (Monad m) 
  => (Table t) a b
  -> (Reference r) m t
  -> (a -> b)
  -> m (a -> m b)
memoWithM Table{..} Reference{..} = \f -> do    -- the INLINE pragma uses the "lexical left-hand side" 
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
      Just y  -> do
        writeReference reference table -- put it back, e.g. for MVar 
        return y
      -- otherwise compute, then insert 
      Nothing -> do
        let y = f x
        -- compute once
        writeReference reference $ writeTable x y table 
        -- reference `modifyReference` (writeTable x y)
        return y

-- (before)
-- in a race condition, there's no deadlock or corruption, 
-- but the one that finishes last will update the cache, while
-- the one that finishes first won't affect the cache.  

{-# INLINEABLE memoWithM #-}
-- {-# SPECIALIZE memoWithM :: #-}
-- {-# SPECIALIZE memoWithM :: #-}

-- ----------------------------------------

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

