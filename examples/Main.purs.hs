module Example.Main where

import Control.Monad.Cont.Trans
import Control.Monad.Trans
import Data.Array
import Data.Either
import Data.Foreign
import Debug.Trace
import Example.Interpreter
import Example.User (program)

data User = User { name :: String, age  :: Number }

instance readUser :: ReadForeign User where
  read = do
    name <- prop "name"
    age <- prop "age"
    return $ User { name: name, age: age }

instance showUser :: Show User where
  show (User a) = "User { name: " ++ a.name ++ ", " ++ "age: " ++ show a.age ++ " }"

uri = "mongodb://127.0.0.1:27017/purescript_test"

run = do
  db <- ContT $ connectf uri
  rs <- runMongodb db program
  lift (closef db)
  return rs

main = runContT run $ (\r ->
  trace $ joinWith ((\a -> case parseForeign read a of
                             Left e -> e
                             Right a@(User _) -> show a) <$> r) "\n")
