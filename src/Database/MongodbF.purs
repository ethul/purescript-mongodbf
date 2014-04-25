module Database.MongodbF
  ( MongodbF(..)
  , Mongodb(..)
  , Cursor(..)
  , FindAndModifyOptions(..)
  , UpdateOptions(..)
  , aggregate
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
import Database.MongodbF.Types

foreign import data Cursor :: *

type UpdateOptions = { upsert :: Boolean, multi :: Boolean }

type FindAndModifyOptions
  = { query :: Document
    , sort :: Document
    , remove :: Document
    , update :: Document
    , new :: Boolean
    , fields :: Document
    , upsert :: Boolean
    }

data MongodbF a
  = Aggregate Collection [Document] (Foreign -> a)
  | Collect Cursor ([Foreign] -> a)
  | Count Collection Document (Number -> a)
  | Find Collection Document Document (Cursor -> a)
  | FindAndModify Collection FindAndModifyOptions (Maybe Foreign -> a)
  | FindOne Collection Document Document (Maybe Foreign -> a)
  | Insert Collection [Document] a
  | Remove Collection Document Boolean a
  | Save Collection Document a
  | Update Collection Document Document UpdateOptions a

instance mongodbFFunctor :: Functor MongodbF where
  (<$>) f (Aggregate c p k) = Aggregate c p (\x -> f $ k x)
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

aggregate :: Collection -> [Document] -> Mongodb Foreign
aggregate c p = liftF $ Aggregate c p id

collect :: Cursor -> Mongodb [Foreign]
collect c = liftF $ Collect c id

count :: Collection -> Document -> Mongodb Number
count c q = liftF $ Count c q id

find :: Collection -> Document -> Document -> Mongodb Cursor
find c r p = liftF $ Find c r p id

findAndModify :: Collection -> FindAndModifyOptions -> Mongodb (Maybe Foreign)
findAndModify c o = liftF $ FindAndModify c o id

findOne :: Collection -> Document -> Document -> Mongodb (Maybe Foreign)
findOne c r p = liftF $ FindOne c r p id

insert :: Collection -> [Document] -> Mongodb {}
insert c ds = liftF $ Insert c ds {}

remove :: Collection -> Document -> Boolean -> Mongodb {}
remove c r o = liftF $ Remove c r o {}

save :: Collection -> Document -> Mongodb {}
save c d = liftF $ Save c d {}

update :: Collection -> Document -> Document -> UpdateOptions -> Mongodb {}
update c q u o = liftF $ Update c q u o {}
