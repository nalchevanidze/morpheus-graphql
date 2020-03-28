{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module Data.Morpheus.Types.Internal.Operation
    ( Empty(..)        
    , Selectable(..)
    , Singleton(..)
    , Listable(..)
    , Join(..)
    , Failure(..)
    , KeyOf(..)
    , toPair
    , selectBy
    , member
    , keys
    )
    where 

import           Data.Text                              ( Text )
import           Instances.TH.Lift                      ( )
import           Data.HashMap.Lazy                      ( HashMap )
import qualified Data.HashMap.Lazy                   as HM 
import           Data.Morpheus.Error.NameCollision      (NameCollision(..))
import           Data.Morpheus.Types.Internal.AST.Base  ( Name
                                                        , Named
                                                        , GQLErrors
                                                        )
import           Text.Megaparsec.Internal               ( ParsecT(..) )
import           Text.Megaparsec.Stream                 ( Stream )


class Empty a where 
  empty :: a

instance Empty (HashMap k v) where
  empty = HM.empty

class Selectable c a | c -> a where 
  selectOr :: d -> (a -> d) -> Name -> c -> d

instance Selectable [(Name, a)] a where 
  selectOr fb f key lib = maybe fb f (lookup key lib)

instance Selectable (HashMap Text a) a where 
  selectOr fb f key lib = maybe fb f (HM.lookup key lib)

selectBy :: (Failure e m, Selectable c a, Monad m) => e -> Name -> c -> m a
selectBy err = selectOr (failure err) pure

member :: forall a c. Selectable c a => Name -> c -> Bool
member = selectOr False toTrue
  where 
    toTrue :: a -> Bool
    toTrue _ = True


class Singleton c a where
  singleton  :: Name -> a -> c

class KeyOf a where 
  keyOf :: a -> Name

instance KeyOf (Name,a) where
  keyOf = fst

toPair :: KeyOf a => a -> (Name,a)
toPair x = (keyOf x, x)

class Listable c a | c -> a where
  size :: c -> Int
  size = length . toList 
  fromAssoc   :: (Monad m, Failure GQLErrors m, NameCollision a) => [Named a] ->  m c
  toAssoc     ::  c  -> [Named a]
  fromList :: (KeyOf a, Monad m, Failure GQLErrors m, NameCollision a) => [a] ->  m c
  -- TODO: fromValues
  toList = map snd . toAssoc 
  fromList = fromAssoc . map toPair  
  -- TODO: toValues    
  toList :: c -> [a] 

keys :: Listable c a  => c -> [Name]
keys = map fst . toAssoc

class Join a where 
  join :: (Monad m, Failure GQLErrors m) => a -> a -> m a

class Applicative f => Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure error (Either error) where
  failure = Left

instance (Stream s, Ord e, Failure [a] m) => Failure [a] (ParsecT e s m) where
  failure x = ParsecT $ \_ _ _ _ _ -> failure x