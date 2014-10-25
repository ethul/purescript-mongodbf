module Example.User where

import Database.MongodbF
import Database.MongodbF.Types

program = do
  rs <- find "users" ["name" ->> "eric"] ["name" ->> 1, "age" ->> 1]
  as <- collect rs
  return as
