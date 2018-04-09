{-|

Haskell implementation of http://bjoern.hoehrmann.de/utf-8/decoder/charClasses/

Validate a UTF8 'ByteString' in constant-space without building a 'Text' value.
-}
module Text.Utf8Validator
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

charClasses :: V.Vector Int
charClasses = V.generate 0x100 chrclass
  where
    chrclass i
      | i <= 0x7f = 0
      | i <= 0x8f = 1
      | i <= 0x9f = 9
      | i <= 0xbf = 7
      | i <= 0xc1 = 8
      | i <= 0xdf = 2
      | i <= 0xe0 = 10
      | i <= 0xec = 3
      | i <= 0xed = 4
      | i <= 0xef = 3
      | i <= 0xf0 = 11
      | i <= 0xf3 = 6
      | i <= 0xf4 = 5
      | otherwise = 8

transitions :: V.Vector Int
transitions = V.fromList
  [ 0, 1, 2, 3, 5, 8, 7, 1, 1, 1, 4, 6, 1, 1, 1, 1
  , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  , 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1
  , 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1
  , 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1
  , 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1
  , 1, 1, 1, 1, 1, 1, 1, 3, 1, 3, 1, 1, 1, 1, 1, 1
  , 1, 3, 1, 1, 1, 1, 1, 3, 1, 3, 1, 1, 1, 1, 1, 1
  , 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  ]

data State = State
  { codepoint :: !Int
  , state :: !Int
  } deriving (Eq, Ord, Show)

initialState :: State
initialState = State 0 0

isAccepting :: State -> Bool
isAccepting (State _ 0) = True
isAccepting (State _ _) = False

isRejected :: State -> Bool
isRejected (State _ 1) = True
isRejected (State _ _) = False

feed :: Word8 -> State -> State
feed inp (State cp st) = State cp' st'
  where
    typ = charClasses V.! fromIntegral inp
    chr = fromIntegral inp
    cp' =
      case st of
        0 -> shift 0xff (-typ) .&. chr
        _ -> chr .&. 0x3f .|. shift cp 6
    st' = transitions V.! ((st * 16) + typ)

validateBS' :: BS.ByteString -> State -> State
validateBS' bs st = BS.foldl' (flip feed) st bs

validateBS :: BS.ByteString -> Bool
validateBS bs = state (validateBS' bs initialState) == 0
