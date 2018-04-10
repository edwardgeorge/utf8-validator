{-|

Haskell implementation of http://bjoern.hoehrmann.de/utf-8/decoder/charClasses/

Validate a UTF8 'ByteString' in constant-space without building a 'Text' value.
-}
module Text.Utf8Validator2
  ( State
  , initialState
  , isAccepting
  , isRejected
  , feed
  , validateBS
  , validateBS'
  ) where
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as V
import           Data.Word

data CharClass
  = C0
  | C1
  | C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9
  | C10
  | C11
  deriving (Eq, Ord, Enum, Bounded, Show)

data S
  = S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  deriving (Eq, Ord, Enum, Bounded, Show)

charClass :: Word8 -> CharClass
charClass i
  | i <= 0x7f = C0
  | i <= 0x8f = C1
  | i <= 0x9f = C9
  | i <= 0xbf = C7
  | i <= 0xc1 = C8
  | i <= 0xdf = C2
  | i <= 0xe0 = C10
  | i <= 0xec = C3
  | i <= 0xed = C4
  | i <= 0xef = C3
  | i <= 0xf0 = C11
  | i <= 0xf3 = C6
  | i <= 0xf4 = C5
  | otherwise = C8

ccMask :: CharClass -> Int
ccMask c = shift 0xff (-(fromEnum c))

transitions :: CharClass -> S -> S
transitions cc st =
  case (st, cc) of
    (S0, C0) -> S0
    (S0, C2) -> S2
    (S0, C3) -> S3
    (S0, C4) -> S5
    (S0, C5) -> S8
    (S0, C6) -> S7
    (S0, C10) -> S4
    (S0, C11) -> S6
    (S2, C1) -> S0
    (S2, C7) -> S0
    (S2, C9) -> S0
    (S3, C1) -> S2
    (S3, C7) -> S2
    (S3, C9) -> S2
    (S4, C7) -> S2
    (S5, C1) -> S2
    (S5, C9) -> S2
    (S6, C7) -> S3
    (S6, C9) -> S3
    (S7, C1) -> S3
    (S7, C7) -> S3
    (S7, C9) -> S3
    (S8, C1) -> S3
    _ -> S1

data State = State
  { codepoint :: !Int
  , state :: !S
  } deriving (Eq, Ord, Show)

initialState :: State
initialState = State 0 S0

isAccepting :: State -> Bool
isAccepting (State _ S0) = True
isAccepting (State _ _) = False

isRejected :: State -> Bool
isRejected (State _ S1) = True
isRejected (State _ _) = False

feed :: Word8 -> State -> State
feed inp (State cp st) = State cp' st'
  where
    typ = charClass inp
    chr = fromIntegral inp
    cp' =
      case st of
        S0 -> ccMask typ .&. chr
        _ -> chr .&. 0x3f .|. shift cp 6
    st' = transitions typ st

validateBS' :: BS.ByteString -> State -> State
validateBS' bs st = BS.foldl' (flip feed) st bs

validateBS :: BS.ByteString -> Bool
validateBS bs = isAccepting $ validateBS' bs initialState
