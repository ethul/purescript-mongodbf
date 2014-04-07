module Example.User where

import Database.MongodbF

program = do
  rs <- find "users" {age: {"$gt": 10}} {name: 1, age: 1}
  as <- collect rs
  return as
