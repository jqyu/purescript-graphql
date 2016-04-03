-- mostly a translation of the graphql-haskell package
-- https://facebook.github.io/graphql/
-- https://github.com/jdnavarro/graphql-haskell/blob/master/Data/GraphQL/AST.hs

module Rad.GraphQL.Query

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

  ) where

import Prelude (class Show, show, class Eq, eq, (<>), (&&))

import Data.List (List(..))
import Data.Maybe (Maybe)

type Name  = String
type Alias = String

-- * Document
-- https://facebook.github.io/graphql/#sec-Language.Query-Document

newtype Document = Document (List Definition)

data Definition = DefinitionOperation OperationDefinition
                | DefinitionFragment  FragmentDefinition

-- * Operation
-- https://facebook.github.io/graphql/#sec-Language.Operations

data OperationDefinition = Query    Node
                         | Mutation Node

data Node = Node Name VariableDefinitions Directives SelectionSet

-- * Selection Set
-- https://facebook.github.io/graphql/#sec-Selection-Sets

type SelectionSet = List Selection

data Selection = SelectionField          Field
               | SelectionFragmentSpread FragmentSpread
               | SelectionInlineFragment InlineFragment

-- * Field
-- https://facebook.github.io/graphql/#sec-Language.Fields

data Field = Field Alias Name Arguments Directives SelectionSet

-- * Argument
-- https://facebook.github.io/graphql/#sec-Language.Arguments

type Arguments = List Argument
data Argument = Argument Name Value

-- * Fragment
-- https://facebook.github.io/graphql/#sec-Language.Fragments

data FragmentSpread = FragmentSpread Name Directives

data FragmentDefinition = FragmentDefinition Name TypeCondition Directives SelectionSet

-- * Type Conditions
-- https://facebook.github.io/graphql/#sec-Type-Conditions

type TypeCondition = NamedType

-- * Inline Fragments
-- https://facebook.github.io/graphql/#sec-Inline-Fragments

data InlineFragment = InlineFragment (Maybe TypeCondition) Directives SelectionSet

-- * Input Values
-- https://facebook.github.io/graphql/#sec-Input-Values

data Value = ValueVariable Variable
           | ValueInt      Int
           | ValueFloat    Number
           | ValueBoolean  Boolean
           | ValueString   String
           | ValueEnum     Name
           | ValueList     ListValue
           | ValueObject   ObjectValue

newtype ListValue = ListValue (List Value)

newtype ObjectValue = ObjectValue (List ObjectField)
data ObjectField = ObjectField Name Value

type DefaultValue = Value

-- * Variables
-- https://facebook.github.io/graphql/#sec-Language.Variables

newtype Variable = Variable Name

type VariableDefinitions = List VariableDefinition

data VariableDefinition = VariableDefinition Variable Type (Maybe DefaultValue)

-- * Input Types
-- https://facebook.github.io/graphql/#sec-Input-Types

data Type = TypeNamed   NamedType
          | TypeList    ListType
          | TypeNonNull NonNullType

newtype NamedType = NamedType Name
newtype ListType = ListType Type
data NonNullType = NonNullTypeNamed NamedType
                 | NonNullTypeList  ListType

-- * Directives
-- https://facebook.github.io/graphql/#sec-Language.Directives

type Directives = List Directive

data Directive = Directive Name Arguments

-------------------------------------------------------------
-- Instances ------------------------------------------------
-------------------------------------------------------------
   -- because purescript doesn't have deriving (Eq, Show)

commaJoin :: forall a. (Show a) => List a -> String
commaJoin Nil = ""
commaJoin (Cons x Nil) = show x
commaJoin (Cons x xs) = show x <> "," <> commaJoin xs

showList :: forall a. (Show a) => List a -> String
showList xs = "[" <> commaJoin xs <> "]"

-- Document
instance eqDocument :: Eq Document where
  eq (Document xs) (Document ys) = eq xs ys
instance showDocument :: Show Document where
  show (Document defs) = "(Document " <> showList defs <> ")"
-- Definition
instance eqDefinition :: Eq Definition where
  eq (DefinitionOperation x) (DefinitionOperation y) = eq x y
  eq (DefinitionFragment  x) (DefinitionFragment  y) = eq x y
  eq _ _ = false
instance showDefinition :: Show Definition where
  show (DefinitionOperation od) = "(DefinitionOperation " <> show od <> ")"
  show (DefinitionFragment  fd) = "(DefinitionFragment "  <> show fd <> ")"
-- Operation
instance eqOperationDefinition :: Eq OperationDefinition where
  eq (Query    x) (Query    y) = eq x y
  eq (Mutation x) (Mutation y) = eq x y
  eq _ _ = false
instance showOperationDefinition :: Show OperationDefinition where
  show (Query    x) = "(Query "    <> show x <> ")"
  show (Mutation x) = "(Mutation " <> show x <> ")"
-- Node
instance eqNode :: Eq Node where
  eq (Node n vs ds ss) (Node n' vs' ds' ss') = eq n n' && eq vs vs' && eq ds ds' && eq ss ss'
instance showNode :: Show Node where
  show (Node n vs ds ss) = "(Node " <> show n <> " " <> showList vs <> " " <> showList ds <> " " <> showList ss <> ")"
-- Selection
instance eqSelection :: Eq Selection where
  eq (SelectionField f) (SelectionField f') = eq f f'
  eq (SelectionFragmentSpread s) (SelectionFragmentSpread s') = eq s s'
  eq (SelectionInlineFragment i) (SelectionInlineFragment i') = eq i i'
  eq _ _ = false
instance showSelection :: Show Selection where
  show (SelectionField f) = "(Selection " <> show f <> ")"
  show (SelectionFragmentSpread s) = "(SelectionFragmentSpread " <> show s <> ")"
  show (SelectionInlineFragment i) = "(SelectionInlineFragment " <> show i <> ")"
-- Field
instance eqField :: Eq Field where
  eq (Field a n as ds ss) (Field a' n' as' ds' ss') = eq a a' && eq n n' && eq as as' && eq ds ds' && eq ss ss'
instance showField :: Show Field where
  show (Field a n as ds ss) = "(Field " <> show a <> " " <> show n <> " " <> showList as <> " " <> showList ds <> " " <> showList ss <>  ")"
-- Argument
instance eqArgument :: Eq Argument where
  eq (Argument n v) (Argument n' v') = eq n n' && eq v v'
instance showArgument :: Show Argument where
  show (Argument n v) = "(Argument " <> show n <> " " <> show v <> ")"
-- FragmentSpread
instance eqFragmentSpread :: Eq FragmentSpread where
  eq (FragmentSpread n ds) (FragmentSpread n' ds') = eq n n' && eq ds ds'
instance showFragmentSpread :: Show FragmentSpread where
  show (FragmentSpread n ds) = "(FragmentSpread " <> show n <> " " <> showList ds <> ")"
-- FragmentDefinition
instance eqFragmentDefinition :: Eq FragmentDefinition where
  eq (FragmentDefinition n tc ds ss) (FragmentDefinition n' tc' ds' ss') = eq n n' && eq tc tc' && eq ds ds' && eq ss ss'
instance showFragmentDefinition :: Show FragmentDefinition where
  show (FragmentDefinition n tc ds ss) = "(FragmentDefinition " <> show n <> " " <> show tc <> " " <> showList ds <> " " <> showList ss <> ")"
-- InlineFragment
instance eqInlineFragment :: Eq InlineFragment where
  eq (InlineFragment tc ds ss) (InlineFragment tc' ds' ss') = eq tc tc' && eq ds ds' && eq ss ss'
instance showInlineFragment :: Show InlineFragment where
  show (InlineFragment tc ds ss) = "(InlineFragment " <> show tc <> " " <> showList ds <> " " <> showList ss <> ")"
-- Value
instance eqValue :: Eq Value where
  eq (ValueVariable v) (ValueVariable v') = eq v v'
  eq (ValueInt      i) (ValueInt      i') = eq i i'
  eq (ValueFloat    f) (ValueFloat    f') = eq f f'
  eq (ValueBoolean  b) (ValueBoolean  b') = eq b b'
  eq (ValueString   s) (ValueString   s') = eq s s'
  eq (ValueEnum     e) (ValueEnum     e') = eq e e'
  eq (ValueList     l) (ValueList     l') = eq l l'
  eq (ValueObject   o) (ValueObject   o') = eq o o'
  eq _ _ = false
instance showValue :: Show Value where
  show (ValueVariable v) = "(ValueVariable " <> show v <> ")"
  show (ValueInt      i) = "(ValueInt "      <> show i <> ")"
  show (ValueFloat    f) = "(ValueFloat "    <> show f <> ")"
  show (ValueBoolean  b) = "(ValueBoolean "  <> show b <> ")"
  show (ValueString   s) = "(ValueString "   <> show s <> ")"
  show (ValueEnum     e) = "(ValueEnum "     <> show e <> ")"
  show (ValueList     l) = "(ValueList "     <> show l <> ")"
  show (ValueObject   o) = "(ValueObject "   <> show o <> ")"
instance eqListValue :: Eq ListValue where
  eq (ListValue xs) (ListValue ys) = eq xs ys
instance showListValue :: Show ListValue where
  show (ListValue xs) = "(ListValue " <> showList xs <> ")"
instance eqObjectValue :: Eq ObjectValue where
  eq (ObjectValue xs) (ObjectValue ys) = eq xs ys
instance showObjectValue :: Show ObjectValue where
  show (ObjectValue xs) = "(ObjectValue " <> showList xs <> ")"
instance eqObjectField :: Eq ObjectField where
  eq (ObjectField n v) (ObjectField n' v') = eq n n' && eq v v'
instance showObjectField :: Show ObjectField where
  show (ObjectField n v) = "(ObjectField " <> show n <> " " <> show v <> ")"
-- Variable
instance eqVariable :: Eq Variable where
  eq (Variable x) (Variable y) = eq x y
instance showVariable :: Show Variable where
  show (Variable v) = "(Variable " <> v <> ")"
-- VariableDefinition
instance eqVariableDefinition :: Eq VariableDefinition where
  eq (VariableDefinition v t d) (VariableDefinition v' t' d') = eq v v' && eq t t' && eq d d'
instance showVariableDefinition :: Show VariableDefinition where
  show (VariableDefinition v t d) = "(VariableDefinition " <> show v <> " " <> show t <> " " <> show d <> ")"
-- Type
instance eqType :: Eq Type where
  eq (TypeNamed x) (TypeNamed y) = eq x y
  eq (TypeList xs) (TypeList ys) = eq xs ys
  eq (TypeNonNull x) (TypeNonNull y) = eq x y
  eq _ _ = false
instance showType :: Show Type where
  show (TypeNamed x) = "(TypeNamed " <> show x <> ")"
  show (TypeList xs) = "(TypeList " <> show xs <> ")"
  show (TypeNonNull x) = "(TypeNonNull " <> show x <> ")"
instance eqNamedType :: Eq NamedType where
  eq (NamedType x) (NamedType y) = eq x y
instance showNamedType :: Show NamedType where
  show (NamedType x) = "(NamedType " <> show x <> ")"
instance eqListType :: Eq ListType where
  eq (ListType xs) (ListType ys) = eq xs ys
instance showListType :: Show ListType where
  show (ListType xs) = "(ListType " <> show xs <> ")"
instance eqNonNullType :: Eq NonNullType where
  eq (NonNullTypeNamed t) (NonNullTypeNamed t') = eq t t'
  eq (NonNullTypeList ts) (NonNullTypeList ts') = eq ts ts'
  eq _ _ = false
instance showNonNullType :: Show NonNullType where
  show (NonNullTypeNamed t) = "(NonNullTypeNamed " <> show t <> ")"
  show (NonNullTypeList ts) = "(NonNullTypeList " <> show ts <> ")"
-- Directive
instance eqDirective :: Eq Directive where
  eq (Directive n as) (Directive n' as') = eq n n' && eq as as'
instance showDirective :: Show Directive where
  show (Directive n as) = "(Directive " <> show n <> " " <> showList as <> ")"
