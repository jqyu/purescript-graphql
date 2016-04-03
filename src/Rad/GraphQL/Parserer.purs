module Rad.GraphQL.Parserer
  ( document
  ) where

import Prelude
  ( Unit
  , class Monad
  , unit
  , pure
  , ($)
  , (<$>)
  , (<*>)
  , (<<<)
  )

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Lazy (fix)

import Data.Functor ((<$))
import Data.Monoid (class Monoid, mempty)
import Data.List (many, some)

import Text.Parsing.Parser (Parser) as Parser
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators ((<?>), option, optionMaybe, try)
import Text.Parsing.Parser.String (char, oneOf)
import Text.Parsing.Parser.Token
  ( GenLanguageDef(LanguageDef)
  , LanguageDef
  , TokenParser
  , makeTokenParser
  , letter
  , alphaNum
  )

type Parser a = Parser.Parser String a

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

-- * Document

document :: Parser Document
document =  skip
        *> (Document <$> some definition)
       <|> (Document <<< pure
                     <<< DefinitionOperation
                     <<< Query
                     <<< Node mempty mempty mempty
                     <$> selectionSet)
       <?> "document error!"

definition :: Parser Definition
definition = DefinitionOperation <$> operationDefinition
         <|> DefinitionFragment  <$> fragmentDefinition
         <?> "definition error!"

operationDefinition :: Parser OperationDefinition
operationDefinition = Query    <$ tok "query"    <*> node
                  <|> Mutation <$ tok "mutation" <*> node
                  <?> "operationDefinition error!"

node :: Parser Node
node = Node <$> name
            <*> optempty variableDefinitions
            <*> optempty directives
            <*> selectionSet

variableDefinitions :: Parser VariableDefinitions
variableDefinitions = parens (some variableDefinition)

variableDefinition :: Parser VariableDefinition
variableDefinition = VariableDefinition
  <$> variable
  <*  op ":"
  <*> type_
  <*> optionMaybe defaultValue

defaultValue :: Parser DefaultValue
defaultValue = op "=" *> value

variable :: Parser Variable
variable = Variable <$ op "$" <*> name

selectionSet :: Parser SelectionSet
selectionSet = fix \s -> braces $ some (selection s)

selection :: Parser SelectionSet -> Parser Selection
selection s = SelectionField <$> field s
          <|> SelectionInlineFragment <$> inlineFragment s
          <|> SelectionFragmentSpread <$> fragmentSpread
          <?> "selection error!"

field :: Parser SelectionSet -> Parser Field
field s = Field <$> optempty (try alias)
                <*> name
                <*> optempty arguments
                <*> optempty directives
                <*> optempty s

alias :: Parser Alias
alias = name <* op ":"

arguments :: Parser Arguments
arguments = parens $ some argument

argument :: Parser Argument
argument = Argument <$> name
                    <*  op ":"
                    <*> value

-- * Fragments

fragmentSpread :: Parser FragmentSpread
fragmentSpread = FragmentSpread
  <$  op "..."
  <*> name
  <*> optempty directives

inlineFragment :: Parser SelectionSet -> Parser InlineFragment
inlineFragment s = InlineFragment
  <$  op "..."
  <*  tok "on"
  <*> optionMaybe typeCondition
  <*> optempty directives
  <*> s

fragmentDefinition :: Parser FragmentDefinition
fragmentDefinition = FragmentDefinition
  <$  tok "fragment"
  <*> name
  <*  tok "on"
  <*> typeCondition
  <*> optempty directives
  <*> selectionSet

typeCondition :: Parser TypeCondition
typeCondition = namedType

-- * Values

-- TODO: implement parse-time assertions about value types

value :: Parser Value
value = fix \v ->
        ValueVariable <$> variable
    <|> ValueInt      <$> int 
    <|> ValueFloat    <$> float
    <|> ValueBoolean  <$> bool
    <|> ValueString   <$> stringLiteral
    <|> ValueEnum     <$> name
    <|> ValueList     <$> listValue v
    <|> ValueObject   <$> objectValue v

listValue :: Parser Value -> Parser ListValue
listValue v = ListValue <$> brackets (many v)

objectValue :: Parser Value -> Parser ObjectValue
objectValue v = ObjectValue <$> braces (many $ objectField v)

objectField :: Parser Value -> Parser ObjectField
objectField v = ObjectField <$> name
                            <*  op ":"
                            <*> v

-- * Directives

directives :: Parser Directives
directives = some directive

directive :: Parser Directive
directive = Directive <$  op "@"
                      <*> name
                      <*> optempty arguments

-- * Type Reference

type_ :: Parser Type
type_ = fix \t -> TypeList    <$> listType t
        <|> try ( TypeNonNull <$> nonNullType t )
        <|>       TypeNamed   <$> namedType
        <?> "type_ error!"

namedType :: Parser NamedType
namedType = NamedType <$> name

listType :: Parser Type -> Parser ListType
listType t = ListType <$> brackets type_

nonNullType :: Parser Type -> Parser NonNullType
nonNullType t = NonNullTypeNamed <$> namedType  <* op "!"
            <|> NonNullTypeList  <$> listType t <* op "!"
            <?> "nonNullType error!"

-- INTERNAL HELPERS

graphQLStyle :: LanguageDef
graphQLStyle = LanguageDef
                { commentStart    : ""
                , commentEnd      : ""
                , commentLine     : "#"
                , nestedComments  : false
                , identStart      : letter <|> char '_'
                , identLetter     : alphaNum <|> char '_'
                , opStart         : op'
                , opLetter        : op''
                , reservedOpNames : []
                , reservedNames   : []
                , caseSensitive   : true
                }
    where op'  :: forall m . (Monad m) => ParserT String m Char
          op'  = oneOf ['!', '$', '.', ':', '=', '@']
          op'' :: forall m . (Monad m) => ParserT String m Char
          op'' = char '.'

gql :: TokenParser
gql = makeTokenParser graphQLStyle

-- Wrap tokenizing functions to handle comma insignificance
-- TODO: make a PR allowing custom whitespace characters

skip :: Parser Unit
skip = many gql.comma *> pure unit

name :: Parser Name
name = gql.identifier <* skip

tok :: String -> Parser String
tok s = gql.symbol s <* skip

op :: String -> Parser String
op s = gql.symbol s <* skip

parens :: forall a. Parser a -> Parser a
parens p = gql.parens p <* skip

braces :: forall a. Parser a -> Parser a
braces p = gql.braces p <* skip

brackets :: forall a. Parser a -> Parser a
brackets p = gql.brackets p <* skip

optempty :: forall a. (Monoid a) => Parser a -> Parser a
optempty = option mempty

int :: Parser Int
int = gql.integer <* skip

float :: Parser Number
float = gql.float <* skip

bool :: Parser Boolean
bool = tok "true"  *> pure true
   <|> tok "false" *> pure false
   <?> "bool error!"

stringLiteral :: Parser String
stringLiteral = gql.stringLiteral <* skip
