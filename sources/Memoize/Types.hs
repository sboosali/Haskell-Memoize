{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes, BangPatterns #-}

module Memoize.Types where
-- import Memoize.Extra

import "spiros" Prelude.Spiros

----------------------------------------

{-|

-}
newtype Memoizer m a = Memoizer { getMemoizer ::
  forall x. (a -> x) -> Memoized m a x 
  }

{-| @Memoized m a b@ is a function @a -> b@ 
memoized in the context of some @Monad m@,
which stores the memo table and updates it. 

-}
type Memoized m a b = m (a -> m b)
                       
{-| a mapping @t@ between @a@ and @b@  

'emptyTable' can be nonempty,
if you want to initialize it with something.

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
