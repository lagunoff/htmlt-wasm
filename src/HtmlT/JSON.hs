{-| The goal here is to have code that is partially compatible with the
aeson library so that you can use it as a replacement in common
modules with derive FromJSON/ToJSON declarations and avoid compiling
aeson and its dependencies under WebAssembly. If you want full
compatibility or you rely on the aeson stuff not present here, you
should use the real thing
-}
{-# LANGUAGE NoPolyKinds #-}
module HtmlT.JSON where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Fix
import Data.Binary
import Data.Char
import Data.Kind
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import GHC.Generics as G
import GHC.Int

import HtmlT.Protocol.JNumber (JNumber(..))
import HtmlT.Protocol.JNumber qualified as JNumber

data Value
  = Object Object
  | Array Array
  | String Text
  | Number JNumber
  | Bool Bool
  | Null
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

type Key = Text

type Object = [(Key, Value)]

type Array = [Value]
--------------------------------------------------------------------------------

class ToJSON a where
  toJSON :: a -> Value
  default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
  toJSON = gToJSON . G.from

instance ToJSON Value where toJSON = Prelude.id

instance ToJSON Bool where toJSON = Bool

instance ToJSON Int64 where toJSON i = Number (JNumber i 0)

instance ToJSON Int where toJSON i = Number (JNumber (fromIntegral i) 0)

instance ToJSON Rational where
  toJSON r = Number (JNumber.jsNumberFromRational r)

instance {-# OVERLAPPABLE #-} Real a => ToJSON a where
  toJSON = toJSON . toRational

instance ToJSON Text where toJSON = String

instance ToJSON a => ToJSON [a] where toJSON = Array . fmap toJSON

instance ToJSON a => ToJSON (Maybe a) where toJSON = maybe Null toJSON

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
  toJSON (a, b) = toJSON [toJSON a, toJSON b]

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a, b, c) where
  toJSON (a, b, c) = toJSON [toJSON a, toJSON b, toJSON c]
--------------------------------------------------------------------------------

class FromJSON a where
  parseJSON :: Value -> Parser a
  default parseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
  parseJSON = fmap G.to . gParseJSON

instance FromJSON Value where parseJSON = pure

instance FromJSON Bool where
  parseJSON = withBool "Bool" pure

instance FromJSON Int64 where
  parseJSON = withScientific "Int64" \(JNumber c e) ->
    if e >= 0
      then pure (c * (10 ^ e))
      -- Ignoring the remainder after decimal point
      else pure (fst (quotRem c (10 ^ (-e))))

instance FromJSON Int where
  parseJSON = fmap fromIntegral . parseJSON @Int64

instance FromJSON Rational where
  parseJSON = withScientific "Rational" $ pure . JNumber.jsNumberToRational

instance {-# OVERLAPPABLE #-} Fractional a => FromJSON a where
  parseJSON = fmap fromRational . parseJSON

instance FromJSON Text where
  parseJSON = withText "Text" pure

instance FromJSON a => FromJSON [a] where
  parseJSON = withArray "[a]" $ mapM parseJSON

instance FromJSON a => FromJSON (Maybe a) where
  parseJSON = fmap Just . parseJSON @a

instance (FromJSON a, FromJSON b) => FromJSON (a, b) where
  parseJSON = withArray "(a, b)" \case
    (a:b:_) -> (,) <$> parseJSON a <*> parseJSON b
    _ -> fail "Not enough elements in the tuple"

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a, b, c) where
  parseJSON = withArray "(a, b, c)" \case
    (a:b:c:_) -> (,,) <$> parseJSON a <*> parseJSON b <*> parseJSON c
    _ -> fail "Not enough elements in the tuple"
--------------------------------------------------------------------------------

class GFromJSON (f :: Type -> Type) where
  gParseJSON :: Value -> Parser (f a)

instance GFromJSON f => GFromJSON (M1 m c f) where
  gParseJSON = fmap M1 . gParseJSON @f

instance (GFromJSObject x, GFromJSObject y) => GFromJSON (x :*: y) where
  gParseJSON = withObject "Object" gFromJSObject

instance {-# OVERLAPPING #-} FromJSON a => GFromJSON (S1 s (Rec0 a)) where
  gParseJSON = fmap (M1 . K1) . parseJSON @a
--------------------------------------------------------------------------------

class GToJSON (f :: Type -> Type) where
  gToJSON :: f x -> Value

instance GToJSON f => GToJSON (M1 m c f) where
  gToJSON (M1 f) = gToJSON f

instance GToJSObject (x :*: y) => GToJSON (x :*: y) where
  gToJSON (x :*: y) = Object $ gToJSObject (x :*: y)

instance {-# OVERLAPPING #-} (ToJSON a) => GToJSON (S1 s (Rec0 a)) where
  gToJSON (M1 (K1 a)) = toJSON a
--------------------------------------------------------------------------------

class GToJSObject (f :: Type -> Type) where
  gToJSObject :: f x -> [(Text, Value)]

instance (GToJSObject x, GToJSObject y) => GToJSObject (x :*: y) where
  gToJSObject (x :*: y) = gToJSObject x <> gToJSObject y

instance (GToJSObject f) => GToJSObject (M1 m c f) where
  gToJSObject (M1 a) = gToJSObject a

instance {-# OVERLAPPING #-} (ToJSON a, Selector s) => GToJSObject (S1 s (Rec0 a)) where
  gToJSObject (M1 (K1 a)) = [(key, toJSON a)]
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
--------------------------------------------------------------------------------

class GFromJSObject (f :: Type -> Type) where
  gFromJSObject :: [(Text, Value)] -> Parser (f x)

instance (GFromJSObject x, GFromJSObject y) => GFromJSObject (x :*: y) where
  gFromJSObject kvs = liftA2 (:*:) (gFromJSObject kvs) (gFromJSObject kvs)

instance (GFromJSObject f) => GFromJSObject (M1 m c f) where
  gFromJSObject = fmap M1 . gFromJSObject

instance {-# OVERLAPPING #-} (FromJSON a, Selector s) => GFromJSObject (S1 s (Rec0 a)) where
  gFromJSObject kvs = do
    a <- explicitParseField parseJSON kvs key
    return $ M1 . K1 $ a
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)

-- AESON COPY-PASTE BEGINS ----------------------------------------------------
-- | Elements of a JSON path used to describe the location of an
-- error.
data JSONPathElement = Key Text
                       -- ^ JSON path element of a key into an object,
                       -- \"object.key\".
                     | Index {-# UNPACK #-} !Int
                       -- ^ JSON path element of an index into an
                       -- array, \"array[index]\".
                       deriving (Eq, Show, Typeable, Ord)

type JSONPath = [JSONPathElement]

-- | The internal result of running a 'Parser'.
data IResult a = IError JSONPath String
               | ISuccess a
               deriving (Eq, Show, Typeable)

-- | The result of running a 'Parser'.
data Result a = Error String
              | Success a
                deriving (Eq, Show, Typeable)

instance NFData JSONPathElement where
  rnf (Key t)   = rnf t
  rnf (Index i) = rnf i

instance (NFData a) => NFData (IResult a) where
    rnf (ISuccess a)      = rnf a
    rnf (IError path err) = rnf path `seq` rnf err

instance (NFData a) => NFData (Result a) where
    rnf (Success a) = rnf a
    rnf (Error err) = rnf err

instance Functor IResult where
    fmap f (ISuccess a)      = ISuccess (f a)
    fmap _ (IError path err) = IError path err
    {-# INLINE fmap #-}

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error err) = Error err
    {-# INLINE fmap #-}

instance Monad IResult where
    return = pure
    {-# INLINE return #-}

    ISuccess a      >>= k = k a
    IError path err >>= _ = IError path err
    {-# INLINE (>>=) #-}

instance MonadFail IResult where
    fail err = IError [] err
    {-# INLINE fail #-}

instance Monad Result where
    return = pure
    {-# INLINE return #-}

    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

instance MonadFail Result where
    fail err = Error err
    {-# INLINE fail #-}

instance Applicative IResult where
    pure  = ISuccess
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Applicative Result where
    pure  = Success
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus IResult where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(ISuccess _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Alternative IResult where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Alternative Result where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Semigroup (IResult a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Monoid (IResult a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Semigroup (Result a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Foldable IResult where
    foldMap _ (IError _ _) = mempty
    foldMap f (ISuccess y) = f y
    {-# INLINE foldMap #-}

    foldr _ z (IError _ _) = z
    foldr f z (ISuccess y) = f y z
    {-# INLINE foldr #-}

instance Foldable Result where
    foldMap _ (Error _)   = mempty
    foldMap f (Success y) = f y
    {-# INLINE foldMap #-}

    foldr _ z (Error _)   = z
    foldr f z (Success y) = f y z
    {-# INLINE foldr #-}

instance Traversable IResult where
    traverse _ (IError path err) = pure (IError path err)
    traverse f (ISuccess a)      = ISuccess <$> f a
    {-# INLINE traverse #-}

instance Traversable Result where
    traverse _ (Error err) = pure (Error err)
    traverse f (Success a) = Success <$> f a
    {-# INLINE traverse #-}

-- | Failure continuation.
type Failure f r   = JSONPath -> String -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | A JSON parser.  N.B. This might not fit your usual understanding of
--  "parser".  Instead you might like to think of 'Parser' as a "parse result",
-- i.e. a parser to which the input has already been applied.
newtype Parser a = Parser {
      runParser :: forall f r.
                   JSONPath
                -> Failure f r
                -> Success a f r
                -> f r
    }

instance Monad Parser where
    m >>= g = Parser $ \path kf ks -> let ks' a = runParser (g a) path kf ks
                                       in runParser m path kf ks'
    {-# INLINE (>>=) #-}
    return = pure
    {-# INLINE return #-}

-- |
--
-- @since 2.1.0.0
instance MonadFix Parser where
    mfix f = Parser $ \path kf ks -> let x = runParser (f (fromISuccess x)) path IError ISuccess in
        case x of
            IError p e -> kf p e
            ISuccess y -> ks y
      where
        fromISuccess :: IResult a -> a
        fromISuccess (ISuccess x)      = x
        fromISuccess (IError path msg) = error $ "mfix @Aeson.Parser: " ++ formatPath path ++ ": " ++ msg

instance MonadFail Parser where
    fail msg = Parser $ \path kf _ks -> kf (reverse path) msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \path kf ks -> let ks' a = ks (f a)
                                        in runParser m path kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure a = Parser $ \_path _kf ks -> ks a
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \path kf ks -> let kf' _ _ = runParser b path kf ks
                                         in runParser a path kf' ks
    {-# INLINE mplus #-}

instance Semigroup (Parser a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

-- | Annotate an error message with a
-- <http://goessner.net/articles/JsonPath/ JSONPath> error location.
formatError :: JSONPath -> String -> String
formatError path msg = "Error in " ++ formatPath path ++ ": " ++ msg

-- | Format a <http://goessner.net/articles/JsonPath/ JSONPath> as a 'String',
-- representing the root object as @$@.
formatPath :: JSONPath -> String
formatPath path = "$" ++ formatRelativePath path

-- | Format a <http://goessner.net/articles/JsonPath/ JSONPath> as a 'String'
-- which represents the path relative to some root object.
formatRelativePath :: JSONPath -> String
formatRelativePath path = format "" path
  where
    format :: String -> JSONPath -> String
    format pfx []                = pfx
    format pfx (Index idx:parts) = format (pfx ++ "[" ++ show idx ++ "]") parts
    format pfx (Key key:parts)   = format (pfx ++ formatKey key) parts

    formatKey :: Text -> String
    formatKey key
       | isIdentifierKey strKey = "." ++ strKey
       | otherwise              = "['" ++ escapeKey strKey ++ "']"
      where strKey = Text.unpack key

    isIdentifierKey :: String -> Bool
    isIdentifierKey []     = False
    isIdentifierKey (x:xs) = isAlpha x && all isAlphaNum xs

    escapeKey :: String -> String
    escapeKey = concatMap escapeChar

    escapeChar :: Char -> String
    escapeChar '\'' = "\\'"
    escapeChar '\\' = "\\\\"
    escapeChar c    = [c]

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  b <$> e
{-# INLINE apP #-}

-- | Fail parsing due to a type mismatch, with a descriptive message.
--
-- The following wrappers should generally be preferred:
-- 'withObject', 'withArray', 'withText', 'withBool'.
--
-- ==== Error message example
--
-- > typeMismatch "Object" (String "oops")
-- > -- Error: "expected Object, but encountered String"
typeMismatch :: String -- ^ The name of the JSON type being parsed
                       -- (@\"Object\"@, @\"Array\"@, @\"String\"@, @\"Number\"@,
                       -- @\"Boolean\"@, or @\"Null\"@).
             -> Value  -- ^ The actual value encountered.
             -> Parser a
typeMismatch expected actual =
    fail $ "expected " ++ expected ++ ", but encountered " ++ typeOf actual

-- | Fail parsing due to a type mismatch, when the expected types are implicit.
--
-- ==== Error message example
--
-- > unexpected (String "oops")
-- > -- Error: "unexpected String"
unexpected :: Value -> Parser a
unexpected actual = fail $ "unexpected " ++ typeOf actual

-- | JSON type of a value, name of the head constructor.
typeOf :: Value -> String
typeOf v = case v of
    Object _ -> "Object"
    Array _  -> "Array"
    String _ -> "String"
    Number _ -> "Number"
    Bool _   -> "Boolean"
    Null     -> "Null"

-- | If the inner @Parser@ failed, modify the failure message using the
-- provided function. This allows you to create more descriptive error messages.
-- For example:
--
-- > parseJSON (Object o) = modifyFailure
-- >     ("Parsing of the Foo value failed: " ++)
-- >     (Foo <$> o .: "someField")
--
-- Since 0.6.2.0
modifyFailure :: (String -> String) -> Parser a -> Parser a
modifyFailure f (Parser p) = Parser $ \path kf ks ->
    p path (\p' m -> kf p' (f m)) ks

-- | If the inner 'Parser' failed, prepend the given string to the failure
-- message.
--
-- @
-- 'prependFailure' s = 'modifyFailure' (s '++')
-- @
prependFailure :: String -> Parser a -> Parser a
prependFailure = modifyFailure . (++)

-- | Add context to a failure message, indicating the name of the structure
-- being parsed.
--
-- > prependContext "MyType" (fail "[error message]")
-- > -- Error: "parsing MyType failed, [error message]"
prependContext :: String -> Parser a -> Parser a
prependContext name = prependFailure ("parsing " ++ name ++ " failed, ")

-- | @'withObject' name f value@ applies @f@ to the 'Object' when @value@
-- is an 'Data.Aeson.Object' and fails otherwise.
--
-- ==== Error message example
--
-- > withObject "MyType" f (String "oops")
-- > -- Error: "parsing MyType failed, expected Object, but encountered String"
withObject :: String -> (Object -> Parser a) -> Value -> Parser a
withObject _    f (Object obj) = f obj
withObject name _ v            = prependContext name (typeMismatch "Object" v)

-- | @'withText' name f value@ applies @f@ to the 'Text' when @value@ is a
-- 'Data.Aeson.String' and fails otherwise.
--
-- ==== Error message example
--
-- > withText "MyType" f Null
-- > -- Error: "parsing MyType failed, expected String, but encountered Null"
withText :: String -> (Text -> Parser a) -> Value -> Parser a
withText _    f (String txt) = f txt
withText name _ v            = prependContext name (typeMismatch "String" v)

-- | @'withArray' expected f value@ applies @f@ to the 'Array' when @value@ is
-- an 'Data.Aeson.Array' and fails otherwise.
--
-- ==== Error message example
--
-- > withArray "MyType" f (String "oops")
-- > -- Error: "parsing MyType failed, expected Array, but encountered String"
withArray :: String -> (Array -> Parser a) -> Value -> Parser a
withArray _    f (Array arr) = f arr
withArray name _ v           = prependContext name (typeMismatch "Array" v)

-- | @'withScientific' name f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Data.Aeson.Number' and fails using 'typeMismatch'
-- otherwise.
--
-- /Warning/: If you are converting from a scientific to an unbounded
-- type such as 'Integer' you may want to add a restriction on the
-- size of the exponent (see 'withBoundedScientific') to prevent
-- malicious input from filling up the memory of the target system.
--
-- ==== Error message example
--
-- > withScientific "MyType" f (String "oops")
-- > -- Error: "parsing MyType failed, expected Number, but encountered String"
withScientific :: String -> (JNumber -> Parser a) -> Value -> Parser a
withScientific _ f (Number scientific) = f scientific
withScientific name _ v = prependContext name (typeMismatch "Number" v)

-- | A variant of 'withScientific' which doesn't use 'prependContext', so that
-- such context can be added separately in a way that also applies when the
-- continuation @f :: Scientific -> Parser a@ fails.
--
-- /Warning/: If you are converting from a scientific to an unbounded
-- type such as 'Integer' you may want to add a restriction on the
-- size of the exponent (see 'withBoundedScientific') to prevent
-- malicious input from filling up the memory of the target system.
--
-- ==== Error message examples
--
-- > withScientific' f (String "oops")
-- > -- Error: "unexpected String"
-- >
-- > prependContext "MyType" (withScientific' f (String "oops"))
-- > -- Error: "parsing MyType failed, unexpected String"
withScientific' :: (JNumber -> Parser a) -> Value -> Parser a
withScientific' f v = case v of
    Number n -> f n
    _ -> typeMismatch "Number" v

-- | @'withBoundedScientific' name f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Number' with exponent less than or equal to 1024.
withBoundedScientific :: String -> (JNumber -> Parser a) -> Value -> Parser a
withBoundedScientific name f v = withBoundedScientific_ (prependContext name) f v

-- | A variant of 'withBoundedScientific' which doesn't use 'prependContext',
-- so that such context can be added separately in a way that also applies
-- when the continuation @f :: Scientific -> Parser a@ fails.
withBoundedScientific' :: (JNumber -> Parser a) -> Value -> Parser a
withBoundedScientific' f v = withBoundedScientific_ id f v

-- | A variant of 'withBoundedScientific_' parameterized by a function to apply
-- to the 'Parser' in case of failure.
withBoundedScientific_ :: (Parser a -> Parser a) -> (JNumber -> Parser a) -> Value -> Parser a
withBoundedScientific_ whenFail f (Number scientific) =
    if False {-exp10 > 1024-}
    then whenFail (fail msg)
    else f scientific
  where
    exp10 = base10Exponent scientific
    msg = "found a number with exponent " ++ show exp10 ++ ", but it must not be greater than 1024"
withBoundedScientific_ whenFail _ v =
    whenFail (typeMismatch "Number" v)

-- | @'withBool' expected f value@ applies @f@ to the 'Bool' when @value@ is a
-- 'Boolean' and fails otherwise.
--
-- ==== Error message example
--
-- > withBool "MyType" f (String "oops")
-- > -- Error: "parsing MyType failed, expected Boolean, but encountered String"
withBool :: String -> (Bool -> Parser a) -> Value -> Parser a
withBool _    f (Bool arr) = f arr
withBool name _ v          = prependContext name (typeMismatch "Boolean" v)

-- | Variant of '.:' with explicit parser function.
--
-- E.g. @'explicitParseField' 'parseJSON1' :: ('FromJSON1' f, 'FromJSON' a) -> 'Object' -> 'Text' -> 'Parser' (f a)@
explicitParseField :: (Value -> Parser a) -> Object -> Key -> Parser a
explicitParseField p obj key = case List.lookup key obj of
    Nothing -> fail $ "key " ++ show key ++ " not found"
    Just v  -> p v <?> Key key

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
(.:) :: (FromJSON a) => Object -> Key -> Parser a
(.:) = explicitParseField parseJSON

-- | Add JSON Path context to a parser
--
-- When parsing a complex structure, it helps to annotate (sub)parsers
-- with context, so that if an error occurs, you can find its location.
--
-- > withObject "Person" $ \o ->
-- >   Person
-- >     <$> o .: "name" <?> Key "name"
-- >     <*> o .: "age"  <?> Key "age"
--
-- (Standard methods like '(.:)' already do this.)
--
-- With such annotations, if an error occurs, you will get a JSON Path
-- location of that error.
--
-- Since 0.10
(<?>) :: Parser a -> JSONPathElement -> Parser a
p <?> pathElem = Parser $ \path kf ks -> runParser p (pathElem:path) kf ks

type Pair = (Key, Value)

-- | Create a 'Value' from a list of name\/value 'Pair's.  If duplicate
-- keys arise, later keys and their associated values win.
object :: [Pair] -> Value
object = Object
{-# INLINE object #-}

-- | Run a 'Parser' with a 'Maybe' result type.
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v = runParser (m v) [] (\_ _ -> Nothing) Just
{-# INLINE parseMaybe #-}

-- | Run a 'Parser' with an 'Either' result type.  If the parse fails,
-- the 'Left' payload will contain an error message.
parseEither :: (a -> Parser b) -> a -> Either String b
parseEither m v = runParser (m v) [] onError Right
  where onError path msg = Left (formatError path msg)
{-# INLINE parseEither #-}

-- AESON COPY-PASTE ENDS -------------------------------------------------------
