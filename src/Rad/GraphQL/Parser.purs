module Rad.GraphQL.Parser
  ( document
  ) where

import Prelude
  ( Unit
  , unit
  , pure
  , bind
  , const
  , negate
  , ($)
  , (>)
  , (*)
  , (+)
  , (<$>)
  , (<*>)
  , (<<<)
  , (==)
  , (/=)
  , (&&)
  , (||)
  , (<>)
  )

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))

import Data.Foldable (foldr, foldl)
import Data.Functor ((<$), ($>))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.List (List(..), (:), toUnfoldable)
import Data.String (fromCharArray)

import Text.Parsing.StringParser (Parser, try, fail)
import Text.Parsing.StringParser.Combinators ((<?>), between, option, fix, many, many1, optionMaybe)
import Text.Parsing.StringParser.String (satisfy, string, oneOf, char, anyDigit)

import Rad.GraphQL.Query

  ( Name
  , Alias

  , Document(..)
  , Definition(..)

  , OperationDefinition(..)
  , Node(..)

  , SelectionSet
  , Selection(..)
  , Field(..)

  , Arguments
  , Argument(..)

  , FragmentSpread(..)
  , FragmentDefinition(..)
  , TypeCondition
  , InlineFragment(..)

  , Value(..)
  , ListValue(..)
  , ObjectValue(..)
  , ObjectField(..)
  , DefaultValue

  , Variable(..)
  , VariableDefinitions
  , VariableDefinition(..)

  , Type(..)
  , NamedType(..)
  , ListType(..)
  , NonNullType(..)

  , Directives
  , Directive(..)

  )

-- * Name

cAZ :: Array Char
cAZ = [ 'A', 'B', 'C', 'D', 'E', 'F', 'G'
      , 'H', 'I', 'J', 'K', 'L', 'M', 'N'
      , 'O', 'P', 'Q', 'R', 'S', 'T', 'U'
      , 'V', 'W', 'X', 'Y', 'Z']

caz :: Array Char
caz = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g'
      , 'h', 'i', 'j', 'k', 'l', 'm', 'n'
      , 'o', 'p', 'q', 'r', 's', 't', 'u'
      , 'v', 'w', 'x', 'y', 'z']

cdigits :: Array Char
cdigits = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

c_azAZ :: Array Char
c_azAZ = ['_'] <> cAZ <> caz

c_azAZ09 :: Array Char
c_azAZ09 = c_azAZ <> cdigits

name :: Parser Name
name = fromCharArray <<< toUnfoldable <$> do
  c  <- oneOf c_azAZ
  cs <- many $ oneOf c_azAZ09
  _  <- whitespace
  pure $ c : cs

-- * Document Parser

document :: Parser Document
document = whitespace
   *> (Document <$> many1 definition)
  <|> (Document <<< pure
                <<< DefinitionOperation
                <<< Query
                <<< Node mempty mempty mempty
                <$> selectionSet)
  <?> "error !!"

definition :: Parser Definition
definition = DefinitionOperation <$> operationDefinition
         <|> DefinitionFragment  <$> fragmentDefinition
         <?> "definition error!"

operationDefinition :: Parser OperationDefinition
operationDefinition = Query    <$ tok "query"    <*> node
                  <|> Mutation <$ tok "mutation" <*> node
                  <?> "definition error!"

node :: Parser Node
node = Node <$> name
            <*> optempty variableDefinitions
            <*> pure mempty
            <*> selectionSet

variableDefinitions :: Parser VariableDefinitions
variableDefinitions = parens $ many1 variableDefinition

variableDefinition :: Parser VariableDefinition
variableDefinition = VariableDefinition <$> variable
                                        <*  tok ":"
                                        <*> type_
                                        <*> optionMaybe defaultValue

defaultValue :: Parser DefaultValue
defaultValue = tok "=" *> value

variable :: Parser Variable
variable = Variable <$ tok "$" <*> name

_selectionSet :: Parser Selection -> Parser SelectionSet
_selectionSet selection' = braces $ many1 $ selection'

_selection :: Parser SelectionSet -> Parser Selection
_selection selectionSet'
  = SelectionField <$> _field selectionSet'
                   <|> try ( SelectionInlineFragment <$> _inlineFragment selectionSet' )
                   <|> SelectionFragmentSpread <$> fragmentSpread
                   <?> "selection error!"

_field :: Parser SelectionSet -> Parser Field
_field selectionSet' = Field <$> optempty (try alias)
                             <*> name
                             <*> optempty arguments
                             <*> optempty directives
                             <*> optempty selectionSet'

selectionSet :: Parser SelectionSet
selectionSet = fix \s ->
                 braces $ many1 $ _selection s

alias :: Parser Alias
alias = name <* tok ":"

arguments :: Parser Arguments
arguments = parens $ many1 argument

argument :: Parser Argument
argument = Argument <$> name
                    <*  tok ":"
                    <*> value

-- * Fragments

fragmentSpread :: Parser FragmentSpread
fragmentSpread = FragmentSpread <$ tok "..."
                                <*> name
                                <*> optempty directives

_inlineFragment :: Parser SelectionSet -> Parser InlineFragment
_inlineFragment selectionSet' =
  InlineFragment <$  tok "..."
                 <*  tok "on"
                 <*> optionMaybe typeCondition
                 <*> optempty directives
                 <*> selectionSet'

fragmentDefinition :: Parser FragmentDefinition
fragmentDefinition =
  FragmentDefinition <$  tok "fragment"
                     <*> name
                     <*  tok "on"
                     <*> typeCondition
                     <*> optempty directives
                     <*> selectionSet

typeCondition :: Parser TypeCondition
typeCondition = namedType

-- * Values
value :: Parser Value
value = fix \v -> ValueVariable <$> variable
              <|> ValueInt      <$> intValue
              <|> ValueFloat    <$> floatValue
              <|> ValueBoolean  <$> booleanValue
              <|> ValueString   <$> stringValue
              <|> ValueEnum     <$> name
              <|> ValueList     <$> _listValue v
              <|> ValueObject   <$> _objectValue v
              <?> "value error!"

intValue :: Parser Int
intValue = do
  sign <- option '+' (char '-')
  n    <- number
  case sign of
       '-' -> pure $ negate n
       _   -> pure n

floatValue :: Parser Number
floatValue = try $ fail "dont know how to do this yet"

number :: Parser Int
number = do
  digits <- many1 anyDigit
  pure $ foldl folder 0 digits
  where folder :: Int -> Char -> Int
        folder x d = 10 * x + digitToInt d

digitToInt :: Char -> Int
digitToInt '1' = 1
digitToInt '2' = 2
digitToInt '3' = 3
digitToInt '4' = 4
digitToInt '5' = 5
digitToInt '6' = 6
digitToInt '7' = 7
digitToInt '8' = 8
digitToInt '9' = 9
digitToInt _ = 0

-- STRING LITERAL PARSING
stringValue :: Parser String
stringValue = do
  maybeCharArray <- quotes $ many stringChar
  _              <- whitespace
  pure $ fromCharArray $ toUnfoldable $ foldr folder Nil maybeCharArray
 where folder :: Maybe Char -> List Char -> List Char
       folder Nothing cs  = cs
       folder (Just c) cs = c : cs
stringChar :: Parser (Maybe Char)
stringChar = (Just <$> stringLetter)
          <|> stringEscape
          <?> "string character"
stringLetter :: Parser Char
stringLetter = satisfy \c ->
                 c /= '"' && c /= '\\' && (c > '\026')
stringEscape :: Parser (Maybe Char)
stringEscape = do
  char '\\'
  (escapeGap $> Nothing) <|> (escapeEmpty $> Nothing) <|> (Just <$> escapeCode)
escapeEmpty :: Parser Char
escapeEmpty = char '&'
escapeGap :: Parser Char
escapeGap = whitespace *> char '\\' <?> "end of string gap"
escapeCode :: Parser Char
escapeCode = char '"' <|> char '\\' <|> char '\''

booleanValue :: Parser Boolean
booleanValue = try (tok "true")  *> pure true
           <|> try (tok "false") *> pure false

_listValue :: Parser Value -> Parser ListValue
_listValue value' = ListValue <$> brackets (many value')

_objectValue :: Parser Value -> Parser ObjectValue
_objectValue value' = ObjectValue <$> braces (many $ _objectField value')

_objectField :: Parser Value -> Parser ObjectField
_objectField value'= ObjectField <$> name <* tok ":" <*> value

-- * Directives

directives :: Parser Directives
directives = many1 directive

directive :: Parser Directive
directive = Directive <$ tok "@"
                      <*> name
                      <*> optempty arguments

-- * Type Reference
_type_ :: Parser ListType -> Parser NonNullType -> Parser Type
_type_ listType' nonNullType' =      TypeList    <$> listType'
                            <|> try (TypeNonNull <$> nonNullType')
                            <|>      TypeNamed   <$> namedType
                            <?> "type_ error!"

namedType :: Parser NamedType
namedType = NamedType <$> name

_nonNullType :: Parser ListType -> Parser NonNullType
_nonNullType listType' = NonNullTypeNamed <$> namedType <* tok "!"
                     <|> NonNullTypeList  <$> listType' <* tok "!"
                     <?> "nonNullType error!"

listType :: Parser ListType
listType = fix $ \p ->
             ListType <$> brackets (_type_ p (_nonNullType p))

type_ :: Parser Type
type_ = _type_ listType (_nonNullType listType)

-- * Token Parsers

tok :: String -> Parser String
tok p = string p <* whitespace

parens :: forall a. Parser a -> Parser a
parens = between (tok "(") (tok ")")

brackets :: forall a. Parser a -> Parser a
brackets = between (tok "[") (tok "]")

braces :: forall a. Parser a -> Parser a
braces = between (tok "{") (tok "}")

quotes :: forall a. Parser a -> Parser a
quotes = between (char '"') (char '"')

optempty :: forall a. (Monoid a) => Parser a -> Parser a
optempty = option mempty

whitespace :: Parser Unit
whitespace = do
  cs <- many $ satisfy
    \c -> c == '\n'
       || c == '\r'
       || c == ' '
       || c == '\t'
       || c == ','
  pure unit
