module Database.MongodbF.Types
  ( Collection(..)
  , Field(..)
  , Document(..)
  , ObjectId(..)
  , Value(..)
  , Val, val
  , (->>)
  ) where

import Data.Tuple

type Collection = String

type Field = Tuple String Value

type Document = [Field]

data ObjectId = ObjectId String

data Value
  = VString String
  | VNumber Number
  | VDocument [Tuple String Value]
  | VArray [Value]
  | VObjectId ObjectId

class Val a where
  val :: a -> Value

instance stringVal :: Val String where
  val = VString

instance numberVal :: Val Number where
  val = VNumber

instance documentVal :: Val [Tuple String Value] where
  val = VDocument

instance arrayVal :: Val [Value] where
  val = VArray

instance objectIdVal :: Val ObjectId where
  val = VObjectId

infix 0 ->>

(->>) :: forall a. (Val a) => String -> a -> Tuple String Value
(->>) f v = Tuple f (val v)
