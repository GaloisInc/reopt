module Reopt.Utils.Printf (
  Conversion (..),
  Flag (..),
  IntSpecifier (..),
  Length (..),
  Specifier (..),
  UnpackedRep (..),
  UnpackError (..),
  ppLength,
  unpackFormat,
) where

import Control.Monad (when)
import Control.Monad.Except (
  ExceptT,
  MonadError (throwError),
  runExceptT,
 )
import Control.Monad.Identity (Identity (Identity))
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Control.Monad.State (
  MonadState (get),
  State,
  StateT (StateT),
  runState,
 )
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isDigit)
import Data.Functor (($>))
import GHC.Natural (Natural)

-----------------------------------------------------------------------
-- Utilities

slice :: ByteString -> Int -> Int -> ByteString
slice s l h = BSC.take (h - l) (BSC.drop l s)

-----------------------------------------------------------------------
-- Declarations

-- | A flag in the printf representation
data Flag
  = LeftAlign
  | PrependPlus
  | PrependSpace
  | PrependZero
  | ThousandsGrouping
  | AlternateForm
  | AlternateDigits
  deriving (Show)

-- | Match a flag character
matchFlag :: Char -> Maybe Flag
matchFlag '-' = Just LeftAlign
matchFlag '+' = Just PrependPlus
matchFlag ' ' = Just PrependSpace
matchFlag '0' = Just PrependZero
matchFlag '\'' = Just ThousandsGrouping
matchFlag '#' = Just AlternateForm
matchFlag '|' = Just AlternateDigits
matchFlag _ = Nothing

-- | An optional integer specifier for the width or precision fields.
data IntSpecifier
  = -- | Indicates no argument was given
    UnspecifiedInt
  | -- ! @IntLit x@ is a natural literal of @x@.
    IntLit !Integer
  | -- ! @NextArg@ indicates specifier of '*' which indicates
    -- next argument has the value.
    NextArg
  | -- | @SpecificArgWidth n@ indicates width specifier with form '*n$'.
    --
    -- This indicates argument at position @n@ gives the width.
    -- The argument should be positive.
    SpecificArg !Natural
  deriving (Show)

-- | A printf length specifier.
data Length
  = -- | @hh@.
    HH
  | -- | @h@
    H
  | -- | @l@
    LongInt
  | -- | @ll@
    LLongInt
  | LongDouble -- @L@
  | Q -- @q@ -- synonym for @ll@ that should not be used.
  | J -- @j@
  | Z -- @z@
  | T -- @t@s
  | NoLength
  deriving (Show)

ppLength :: Length -> String
ppLength len =
  case len of
    HH -> "hh"
    H -> "h"
    LongInt -> "l"
    LLongInt -> "ll"
    LongDouble -> "L"
    Q -> "q"
    J -> "j"
    Z -> "z"
    T -> "t"
    NoLength -> ""

-- | A printf conversion specifier.
data Conversion
  = -- | @d@ or @i@.
    IntDecimal
  | -- | @o@
    UnsignedOctal
  | -- | @u@.
    UnsignedDecimal
  | -- | @x@ and @X@
    UnsignedHexadecimal
  | -- | @e@ or @E@
    FloatExponent
  | FloatDecimal -- @f@ or @F@
  | FloatGeneral -- @g@ or @G@.
  | FloatHexadecimal -- @a@ or @A@
  | -- | @c@
    Char
  | -- | @s@
    String
  | -- | @p@
    Pointer
  | -- | @n@
    CharCount
  | PrintStderr -- @m@ Glibc extension saying to use stderr
  deriving (Show)

-- | Match a character to a conversion
matchConversion :: Char -> Maybe Conversion
matchConversion c =
  case c of
    'd' -> Just IntDecimal
    'i' -> Just IntDecimal
    'o' -> Just UnsignedOctal
    'u' -> Just UnsignedDecimal
    'x' -> Just UnsignedHexadecimal
    'X' -> Just UnsignedHexadecimal
    'e' -> Just FloatExponent
    'E' -> Just FloatExponent
    'f' -> Just FloatDecimal
    'F' -> Just FloatDecimal
    'g' -> Just FloatGeneral
    'G' -> Just FloatGeneral
    'a' -> Just FloatHexadecimal
    'A' -> Just FloatHexadecimal
    'c' -> Just Char
    's' -> Just String
    'p' -> Just Pointer
    'n' -> Just CharCount
    'm' -> Just PrintStderr
    _ -> Nothing

-- | An argument specifier.
data Specifier = Specifier
  { specifierParameter :: Natural --- ^ 1-based index of argument or zero if parameters not used.
  , specifierFlags :: ![Flag]
  -- ^ Flags read so far.
  , specifierWidth :: !IntSpecifier
  , specifierPrecision :: !IntSpecifier
  , specifierLength :: !Length
  , specifierConv :: !Conversion
  }
  deriving (Show)

type StringIndex = Int

data UnpackError
  = -- | String terminated while parsing a specifier that started at given index.
    UnexpectedEnd !StringIndex
  | -- | @UnexpectedConversion i c@ an unexpected conversion character @c@ at index @i@.
    UnexpectedConversion !StringIndex !Char
  | -- | @MissingArgTerminator i@ indicates we expected a '$' at index i to
    -- terminate an argugment index in a width or length.
    MissingArgTerminator !StringIndex
  | -- | @InvalidFieldWidth i@ could not parse field width at index i.
    InvalidFieldWidth !StringIndex
  deriving (Show)

-- | The decoded representation of a printf format string.
data UnpackedRep
  = UnpackedTerm !ByteString
  | -- A string that had an escape sequence we needed to escape
    UnpackedLiteral !ByteString !UnpackedRep
  | UnpackedSpecifier !Specifier !UnpackedRep
  | UnpackedError !UnpackError
  deriving (Show)

-- | Add a literal string to the front of the rep.
unpackPrefix ::
  -- | Bytestring we are extractng from
  BSC.ByteString ->
  -- | Starting index to extract from
  Int ->
  -- | One past the last index included in bytestring
  Int ->
  -- | Unpacked representation to include fixed bits.
  UnpackedRep ->
  UnpackedRep
unpackPrefix bs s i r =
  if s == i
    then r
    else UnpackedLiteral (slice bs s i) r

-----------------------------------------------------------------------
-- Unpacking function

-- | A parser that reads a bytestring.
type Parser = ReaderT ByteString (State StringIndex)

-- | Run a parser
runParser :: ByteString -> StringIndex -> Parser a -> (a, StringIndex)
runParser bs idx p = runState (runReaderT p bs) idx

getIndex :: Parser StringIndex
getIndex = get

atEnd :: Parser Bool
atEnd = ReaderT $ \bs -> StateT $ \i -> Identity (i >= BSC.length bs, i)

-- | Get next character in parser or `0` if empty.
peekChar :: Parser Char
peekChar = ReaderT $ \bs -> StateT $ \i ->
  let c = if i < BSC.length bs then BSC.index bs i else toEnum 0
   in seq c $ Identity (c, i)

-- | Skip next character in parser
skipChar :: Parser ()
skipChar = ReaderT $ \bs -> StateT $ \i ->
  let j = if i < BSC.length bs then i + 1 else i
   in seq j $ Identity ((), j)

asDigit :: Char -> Int
asDigit c = fromEnum c - fromEnum '0'

-- | @parseNatDigits c@ consumes digits in the string and
-- adds them to @c@.
parseNatDigits :: Natural -> Parser Natural
parseNatDigits prev = do
  c <- peekChar
  if isDigit c
    then do
      skipChar
      parseNatDigits $! 10 * prev + fromIntegral (asDigit c)
    else pure prev

-- | Parse a number that starts with 1..9 or return 0
parseNzNat :: Parser Natural
parseNzNat = do
  c <- peekChar
  if '1' <= c && c <= '9'
    then do
      skipChar
      parseNatDigits (fromIntegral (asDigit c))
    else pure 0

-- | Read the length from the character stream.
parseLength :: Parser Length
parseLength = do
  c <- peekChar
  case c of
    'h' -> do
      skipChar
      d <- peekChar
      case d of
        'h' -> skipChar $> HH
        _ -> pure H
    'l' -> do
      skipChar
      d <- peekChar
      case d of
        'l' -> skipChar $> LLongInt
        _ -> pure LongInt
    'L' -> skipChar $> LongDouble
    'q' -> skipChar $> Q
    'j' -> skipChar $> J
    'z' -> skipChar $> Z
    't' -> skipChar $> T
    _ -> pure NoLength

-- | Parse the flags
parseFlags' :: [Flag] -> Parser [Flag]
parseFlags' prev = do
  c <- peekChar
  case matchFlag c of
    Just f -> skipChar *> parseFlags' (f : prev)
    Nothing -> pure (reverse prev)

-- | Parse the flags in a specifier.
parseFlags :: Parser [Flag]
parseFlags = parseFlags' []

-- | Parse a argument index (string of 'k$' where k is a non-zero nat)
-- or a empty string.
parseArgIndex :: ExceptT UnpackError Parser IntSpecifier
parseArgIndex = do
  argIndex <- lift parseNzNat
  if argIndex == 0
    then pure NextArg
    else do
      d <- lift peekChar
      when (d /= '$') $ do
        idx <- lift getIndex
        throwError $ MissingArgTerminator idx
      lift skipChar
      pure (SpecificArg argIndex)

parseWidth :: ExceptT UnpackError Parser IntSpecifier
parseWidth = do
  i <- lift getIndex
  c <- lift peekChar
  case c of
    '*' -> do
      lift skipChar
      parseArgIndex
    '-' -> do
      lift skipChar
      d <- lift parseNzNat
      when (d == 0) $ do
        throwError $ InvalidFieldWidth i
      pure $! IntLit (negate (toInteger d))
    _
      | '1' <= c
      , c <= '9' -> lift $ do
          skipChar
          val <- parseNatDigits (fromIntegral (asDigit c))
          pure $! IntLit (toInteger val)
      | otherwise -> do
          pure UnspecifiedInt

-- | Peek the precision setting
parsePrecision :: ExceptT UnpackError Parser IntSpecifier
parsePrecision = do
  d <- lift peekChar
  if d == '.'
    then do
      c <- lift peekChar
      case c of
        '*' -> do
          lift skipChar
          parseArgIndex
        _ -> lift $ do
          skipChar
          val <- parseNatDigits 0
          pure $! IntLit (toInteger val)
    else pure UnspecifiedInt

-- | Parse a specifier -- returning either an error or the specifier.
parseSpecifier :: ExceptT UnpackError Parser Specifier
parseSpecifier = do
  specStart <- lift getIndex
  lift skipChar
  paramOrWidth <- lift parseNzNat
  (param, flags, width) <-
    if paramOrWidth == 0
      then do
        flags <- lift parseFlags
        width <- parseWidth
        pure (0, flags, width)
      else do
        c <- lift peekChar
        if c == '$'
          then do
            lift skipChar
            flags <- lift parseFlags
            width <- parseWidth
            pure (paramOrWidth, flags, width)
          else pure (0, [], IntLit (toInteger paramOrWidth))
  prec <- parsePrecision
  len <- lift parseLength
  e <- lift atEnd
  if e
    then throwError $ UnexpectedEnd specStart
    else do
      c <- lift peekChar
      case matchConversion c of
        Nothing -> do
          idx <- lift getIndex
          throwError $ UnexpectedConversion idx c
        Just co -> do
          lift skipChar
          pure $!
            Specifier
              { specifierParameter = param
              , specifierFlags = flags
              , specifierWidth = width
              , specifierPrecision = prec
              , specifierLength = len
              , specifierConv = co
              }

-- | Unpack after a percent
unpackSpecifier ::
  -- | String to parse
  BSC.ByteString ->
  -- | Start of specifier
  Int ->
  UnpackedRep
unpackSpecifier bs i =
  case runParser bs i (runExceptT parseSpecifier) of
    (Left e, _) -> UnpackedError e
    (Right s, j) -> UnpackedSpecifier s (unpackFormat' bs j j)

unpackFormat' :: BSC.ByteString -> Int -> Int -> UnpackedRep
unpackFormat' bs s i
  | i >= BSC.length bs = UnpackedTerm (BSC.drop s bs)
  | otherwise =
      if BSC.index bs i == '%'
        then
          if i + 1 >= BSC.length bs
            then UnpackedError (UnexpectedEnd i)
            else
              if BSC.index bs (i + 1) == '%'
                then unpackPrefix bs s (i + 1) (unpackFormat' bs (i + 2) (i + 2))
                else unpackPrefix bs s i (unpackSpecifier bs i)
        else unpackFormat' bs s (i + 1)

unpackFormat :: BSC.ByteString -> UnpackedRep
unpackFormat bs = unpackFormat' bs 0 0
