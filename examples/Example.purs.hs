module Example.Interpreter where

import Control.Monad.Cont.Trans
import Control.Monad.Eff
import Control.Monad.Free
import Data.Foreign
import Database.MongodbF

runMongodbF :: forall e r a. Db -> MongodbF (ContT r (Eff (mdb :: MDB | e)) a) -> ContT r (Eff (mdb :: MDB | e)) a
runMongodbF db (Find c r p f) = ContT (\k -> findf db c r p >>= k) >>= f
runMongodbF _ (Collect c f) = ContT (\k -> collectf c k) >>= f

runMongodb :: forall e r a. Db -> Mongodb a -> ContT r (Eff (mdb :: MDB | e)) a
runMongodb db = iterM $ runMongodbF db

foreign import data MDB :: !

foreign import data Db :: *

foreign import findf
  "function findf(db){ \
  \  return function(col){ \
  \    return function(crit){ \
  \      return function(proj){ \
  \        return function(){ \
  \          return db.collection(col).find(crit, proj); \
  \        }; \
  \      }; \
  \    }; \
  \  }; \
  \};" :: forall e r a. Db -> String -> Foreign -> Foreign -> Eff (mdb :: MDB | e) Cursor

foreign import collectf
  "function collectf(cursor){ \
  \  return function(k){ \
  \    return function(){ \
  \      cursor.toArray(function(e, res){ \
  \        k(res)(); \
  \      }); \
  \    }; \
  \  }; \
  \};" :: forall e r a. Cursor -> ([Foreign] -> Eff (mdb :: MDB | e) a) -> Eff (mdb :: MDB | e) r

foreign import connectf
  "function connectf(uri){ \
  \  return function(k){ \
  \    return function(){ \
  \      var client = require('mongodb').MongoClient; \
  \      client.connect(uri, function(e, db){ \
  \        k(db)(); \
  \      }); \
  \    }; \
  \  }; \
  \};" :: forall e r a. String -> (Db -> Eff (mdb :: MDB | e) a) -> Eff (mdb :: MDB | e) r

foreign import closef
  "function closef(db){ \
  \  return function(){ \
  \    return db.close(); \
  \  }; \
  \};" :: forall e. Db -> Eff (mdb :: MDB | e) {}
