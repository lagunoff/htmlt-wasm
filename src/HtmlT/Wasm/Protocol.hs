module HtmlT.Wasm.Protocol where

import Data.Binary (Binary)
import Data.Int
import Data.Text ()
import Data.Word
import GHC.Generics

import "this" HtmlT.Wasm.Protocol.JNumber (JNumber)
import "this" HtmlT.Wasm.Protocol.Utf8 (Utf8)

data UpCmd
  = EvalExpr { expr :: Expr }
  | HotReload -- ^ Used under dev server
  | Exit
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data DownCmd
  = Start
  | Return JValue
  | ExecCallbackCommand JValue CallbackId
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

-- | Strict Lambda calculus with arbitrary side-effects, meant to be
-- used as commands executed in the JavaScript side, optimized for
-- non-blocking execution and minimizing round-trips.
data Expr
  = Null
  -- ^ Represents null or undefined values
  | Boolean Bool
  -- ^ JavaScript boolean value
  | Num JNumber
  -- ^ JavaScript integer number
  | Str Utf8
  -- ^ JavaScript string
  | Arr [Expr]
  -- ^ JavaScript array
  | Obj [(Utf8, Expr)]
  -- ^ JavaScript object

  | Dot Expr Utf8
  -- ^ Read string property of an object. @(Dot (Id "document")
  -- "body")@ is equivalent to @document.body@ JavaScript expression
  | AssignProp Expr Utf8 Expr
  -- ^ Assign a value to a string property of an object @(AssignProp
  -- (Id "foo") "bar" (Str "baz"))@ is equivalent to @foo['bar'] =
  -- baz;@ JavaScript expression
  | Ix Expr Int64
  -- ^ Read value from an integer index of an object. @(Ix (Id
  -- "foo") 0)@ is equivalent to @foo[0]@ JavaScript expression

  | Add Expr Expr
  -- ^ Binary addition @(Add 256 5647)@ is equivalent to @256 + 5647@
  | Subtract Expr Expr
  -- ^ Binary substraction @(Subtract 256 5647)@ is equivalent to @256 - 5647@
  | Multiply Expr Expr
  -- ^ Binary multiplication @(Multiply 256 5647)@ is equivalent to @256 * 5647@
  | Divide Expr Expr
  -- ^ Binary division @(Divide 256 5647)@ is equivalent to @256 / 5647@

  | Id Utf8
  | Lam Expr
  | Arg Word8 Word8
  | Apply Expr [Expr]
  | Call Expr Utf8 [Expr]

  | AssignVar VarId Expr
  | FreeVar VarId
  | Var VarId

  | InsertNode Expr Expr
  | WithBuilder Expr Expr
  | CreateElement Utf8
  | CreateText Utf8
  | ElementProp Expr Utf8 Expr
  | ElementAttr Expr Utf8 Utf8
  | AddEventListener Expr Utf8 Expr
  | ToggleClass Expr Utf8 Bool
  | AssignText Expr Utf8
  | InsertBoundary Expr
  | ClearBoundary Expr Bool

  | RevSeq [Expr]
  -- ^ Sequence of the expressions is in reverse order! It starts
  -- evaluating from the end of the list to the beggining. Returns
  -- whatever the last expression evaluetes into (last being the
  -- expression from the tip of the list)
  | Eval Utf8
  | ExecCallback CallbackId Expr
  | UncaughtException Utf8
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

-- | Represents a fully-evaluated form of 'Expr' with no lambdas (a
-- JSON basically). This is the result we get from JavaScript after
-- evaluating an 'Expr'.
data JValue
  = JNull
  | JBool Bool
  | JNum JNumber
  | JStr Utf8
  | JArr [JValue]
  | JObj [(Utf8, JValue)]
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

fromJValue :: JValue -> Expr
fromJValue = \case
  JNull -> Null
  JBool a -> Boolean a
  JNum a -> Num a
  JStr a -> Str a
  JArr xs -> Arr $ fmap fromJValue xs
  JObj kv -> Obj $ fmap (\(k, v) -> (k, fromJValue v)) kv

newtype VarId = VarId { unVarId :: Int64 }
  deriving newtype (Show, Num, Binary, Enum, Ord, Eq)

newtype CallbackId = CallbackId { unCallbackId :: Int64 }
  deriving newtype (Show, Num, Binary, Ord, Eq)
