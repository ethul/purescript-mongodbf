module Database.MongodbF
  ( MongodbF(..)
  , Mongodb(..)
  , Cursor(..)
  , Collection(..)
  , Document(..)
  , FMDocument(..)
  , UpdateOptions(..)
  , collect
  , count
  , find
  , findAndModify
  , findOne
  , insert
  , remove
  , save
  , update
  ) where

import Prelude
import Control.Monad.Free
import Data.Maybe
import Data.Foreign

foreign import data Cursor :: *

type Collection = String

type Document a = { | a }

type UpdateOptions = { upsert :: Boolean, multi :: Boolean }

type FMForeign
  = { query :: Foreign
    , sort :: Foreign
    , remove :: Foreign
    , update :: Foreign
    , new :: Boolean
    , fields :: Foreign
    , upsert :: Boolean
    }

type FMDocument a b c d e
  = { query :: Document a
    , sort :: Document b
    , remove :: Document c
    , update :: Document d
    , new :: Boolean
    , fields :: Document e
    , upsert :: Boolean
    }

data MongodbF a
  = Collect Cursor ([Foreign] -> a)
  | Count Collection Foreign (Number -> a)
  | Find Collection Foreign Foreign (Cursor -> a)
  | FindAndModify Collection FMForeign (Maybe Foreign -> a)
  | FindOne Collection Foreign Foreign (Maybe Foreign -> a)
  | Insert Collection [Foreign] a
  | Remove Collection Foreign Boolean a
  | Save Collection Foreign a
  | Update Collection Foreign Foreign UpdateOptions a

instance mongodbFFunctor :: Functor MongodbF where
  (<$>) f (Collect c k) = Collect c (\x -> f $ k x)
  (<$>) f (Count c q k) = Count c q (\x -> f $ k x)
  (<$>) f (Find c r p k) = Find c r p (\x -> f $ k x)
  (<$>) f (FindAndModify c d k) = FindAndModify c d (\x -> f $ k x)
  (<$>) f (FindOne c r p k) = FindOne c r p (\x -> f $ k x)
  (<$>) f (Insert c d a) = Insert c d (f a)
  (<$>) f (Remove c r o a) = Remove c r o (f a)
  (<$>) f (Save c d a) = Save c d (f a)
  (<$>) f (Update c q u o a) = Update c q u o (f a)

type Mongodb = Free MongodbF

foreign import toForeign "function toForeign(o){return o;}" :: forall a. Document a -> Foreign

collect :: Cursor -> Mongodb [Foreign]
collect c = liftF $ Collect c id

count :: forall a. Collection -> Document a -> Mongodb Number
count c q = liftF $ Count c (toForeign q) id

find :: forall a b. Collection -> Document a -> Document b -> Mongodb Cursor
find c r p = liftF $ Find c (toForeign r) (toForeign p) id

findAndModify :: forall a b c d e. Collection -> FMDocument a b c d e -> Mongodb (Maybe Foreign)
findAndModify c d =
  let
    doc =
      { query: toForeign d.query
      , sort: toForeign d.sort
      , remove: toForeign d.remove
      , update: toForeign d.update
      , new: d.new
      , fields: toForeign d.fields
      , upsert: d.upsert
      }
  in liftF $ FindAndModify c doc id

findOne :: forall a b. Collection -> Document a -> Document b -> Mongodb (Maybe Foreign)
findOne c r p = liftF $ FindOne c (toForeign r) (toForeign p) id

insert :: forall a. Collection -> [Document a] -> Mongodb {}
insert c ds = liftF $ Insert c (toForeign <$> ds) {}

remove :: forall a. Collection -> Document a -> Boolean -> Mongodb {}
remove c r o = liftF $ Remove c (toForeign r) o {}

save :: forall a. Collection -> Document a -> Mongodb {}
save c d = liftF $ Save c (toForeign d) {}

update :: forall a b. Collection -> Document a -> Document b -> UpdateOptions -> Mongodb {}
update c q u o = liftF $ Update c (toForeign q) (toForeign u) o {}
