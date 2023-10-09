module HtmlT.Wasm.Protocol where

import Data.Binary (Binary)
import Data.ByteString as BS
import Data.Int
import GHC.Generics

data UpCmd
  = Eval { expr :: Expr }
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

data Expr
  = Null
  | Boolean Bool
  | Num Int64
  | Str ByteString
  | Arr [Expr]
  | Obj [(ByteString, Expr)]

  | Dot Expr ByteString
  | AssignProp Expr ByteString Expr
  | Ix Expr Int64

  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr

  | Id ByteString
  | Lam [ByteString] Expr
  | Apply Expr [Expr]
  | Call Expr ByteString [Expr]

  | AssignVar VarId Expr
  | FreeVar VarId
  | Var VarId

  -- TODO: Explain DomBuilder and DomBoundaries
  | ElInitBuilder DomBuilder Expr
  | ElDestroyBuilder DomBuilder
  | ElPush DomBuilder ByteString
  | ElNoPush DomBuilder ByteString
  | ElProp DomBuilder ByteString Expr
  | ElAttr DomBuilder ByteString ByteString
  | ElEvent DomBuilder ByteString Expr
  | ElText DomBuilder ByteString
  | ElAssignTextContent VarId ByteString
  | ElPop DomBuilder
  | ElInsertBoundary DomBuilder
  | ElClearBoundary DomBuilder
  | ElToggleClass DomBuilder ByteString Bool

  | RevSeq [Expr]
  -- ^ Sequence of the expressions is in reverse order! It starts
  -- evaluating from the end of the list to the beggining. Returns
  -- whatever the last expression evaluetes into (last being the
  -- expression from the tip of the list)
  | ExecCallback CallbackId Expr
  | UncaughtException ByteString
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

-- | Represents a fully-evaluated form of 'Expr' with no lambdas (a
-- JSON basically). This is the result we get from JavaScript after
-- evaluating an 'Expr'.
data JValue
  = JNull
  | JBool Bool
  | JNum Int64
  | JStr ByteString
  | JArr [JValue]
  | JObj [(ByteString, JValue)]
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

newtype DomBuilder = DomBuilder { unDomBuilder :: VarId }
  deriving newtype (Show, Binary)
