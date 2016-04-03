module Rad.Typeable
  ( class Typeable
  , TypeRep(..)
  , typeof
  , typename

  , TypeableExists
  , mkTypeableExists
  , runTypeableExists
  ) where

import Prelude (Unit, class Show)
import Data.List (List)

-- TypeRep, a UNIQUE definition of a type

newtype TypeRep a = TypeRep String

instance showTypeRep :: Show (TypeRep a) where
  show = toString

toString :: forall a. TypeRep a -> String
toString (TypeRep s) = s

-- Typeable, says that a type contains a unique type representation

class (Show a) <= Typeable a where
  typeof :: TypeRep a

instance typeofUnit :: Typeable Unit where
  typeof = TypeRep "Unit"

instance typeofInt :: Typeable Int where
  typeof = TypeRep "Int"

instance typeofString :: Typeable String where
  typeof = TypeRep "String"

instance typeofList :: (Typeable a) => Typeable (List a) where
  typeof = TypeRep (toString basetype)
    where basetype :: TypeRep a
          basetype = typeof


typename :: forall a. (Typeable a) => a -> String
typename x = toString typerep
  where typerep :: TypeRep a
        typerep = typeof


-- Existential Typeable Kind
-- Given a kind parameterized over a Typeable type, we create an existential type

foreign import data TypeableExists :: (* -> *) -> *
foreign import mkTypeableExists :: forall f a. (Typeable a) => f a -> TypeableExists f
foreign import runTypeableExists :: forall f r. (forall a. (Typeable a) => f a -> r) -> TypeableExists f -> r
