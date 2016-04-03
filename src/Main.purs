module Main where

import Prelude (Unit, ($), bind, show, (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Text.Parsing.StringParser (runParser)
import Text.Parsing.Parser (runParser) as Parser

import Rad.GraphQL.Parser (document)
import Rad.GraphQL.Parserer (document) as P

foreign import timerStart :: forall e. String -> Eff (console :: CONSOLE | e) Unit
foreign import timerEnd   :: forall e. String -> Eff (console :: CONSOLE | e) Unit

query :: String
query = "{"
     <> "  field {"
     <> "    myFoo: foo"
     <> "    bar"
     <> "  }"
     <> "}"

tq :: String 
tq = ",, query _nA1_Me { a: f } ,,,,,,,          \n   mutation query { b: f } query a { c: f }"

kitchenSink :: String
kitchenSink =
     "query myQuery($foo: Test) {"
  <> "  foo\n\n"
  <> "  ... on myFragment @skip (if: true) { test }"
  <> "  ... test"
  <> "  testField {"
  <> "    ... on MyType {"
  <> "      myField(withArg: \"string literal \\\"with escaping \\\" \\\\ \")" -- TODO: fix this
  <> "    }"
  <> "  }"
  <> "}"
  <> "query otherQuery($foo: Test) {"
  <> "  myFun(int: 1234)"
  <> "  myFun(int: -1234)"
  <> "}"
  <> "fragment myFragment on User {"
  <> "  my, fields, with(args: $test, arg2: MYENUM, arg3: [ MYENUM, MYENUM, MYENUM ])"
  <> "}"

p :: String -> String
p s = show $ Parser.runParser s P.document

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

  let x' = p tq
  timerStart "parser"
  let x' = p kitchenSink
  timerEnd "parser"
  timerStart "parser"
  log $ p query
  log $ p tq
  log $ p kitchenSink
  timerEnd "parser"
  timerStart "parser"
  log $ p query
  log $ p tq
  log $ p kitchenSink
  log $ p kitchenSink
  timerEnd "parser"

  let x = runParser document tq
  timerStart "stringparser"
  let x = runParser document kitchenSink
  timerEnd "stringparser"
  timerStart "stringparser"
  log $ show $ runParser document query
  log $ show $ runParser document tq
  log $ show $ runParser document kitchenSink
  timerEnd "stringparser"
  timerStart "stringparser"
  log $ show $ runParser document query
  log $ show $ runParser document tq
  log $ show $ runParser document kitchenSink
  log $ show $ runParser document kitchenSink
  timerEnd "stringparser"

  let x' = p tq
  timerStart "parser"
  let x' = p kitchenSink
  timerEnd "parser"
  timerStart "parser"
  log $ p query
  log $ p tq
  log $ p kitchenSink
  timerEnd "parser"
  timerStart "parser"
  log $ p query
  log $ p tq
  log $ p kitchenSink
  log $ p kitchenSink
  timerEnd "parser"


