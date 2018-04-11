{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString (ByteString, pack)
import Data.Foldable
import Data.Text ()
import Data.Text.Encoding
import Test.Tasty
import Test.Tasty.HUnit

import Text.Utf8Validator

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ testGroup
        "simple tests"
        [ testCase
            "valid utf8 1"
            (assertBool "validation returned False" . validateBS $
             encodeUtf8 "Hello-µ@ßöäüàá-UTF-8!!")
        , testCase
            "valid utf8 2"
            (assertBool "validation returned False" $
             validateBS "\xce\xba\xe1\xbd\xb9\xcf\x83\xce\xbc\xce\xb5")
        , testCase
            "invalid utf8 1"
            (assertBool "validation returned False" . not $
             validateBS
               "\xce\xba\xe1\xbd\xb9\xcf\x83\xce\xbc\xce\xb5\xed\xa0\x80\x65\x64\x69\x74\x65\x64")
        , testCase
            "invalid utf8 2"
            (assertBool "validation returned False" . not $
             validateBS
               "\xce\xba\xe1\xbd\xb9\xcf\x83\xce\xbc\xce\xb5\xf4\x90\x80\x80\x65\x64\x69\x74\x65\x64")
        ]
    , testGroup "test sequences" $ map gentest utf8TestSequences
    ]
  where
    gentest (s, d) = testCaseSteps s . fmap snd $ foldl' indtest init d
    indtest x (e, d) step =
      let (i, act) = x step
      in ( i + 1
         , do act
              step ("sequence " ++ show i)
              validateBS d @=? e)
    init _ = (1, return ())

-- Create test sequences for UTF-8 decoder tests from
-- http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
-- implementation translated from case6_x_x.py in autobahntestcase
utf8TestSequences :: [(String, [(Bool, ByteString)])]
utf8TestSequences =
  [ ( "Some valid UTF-8 sequences"
    , [ (True, "hello\x24world")
      , (True, "hello\xC2\xA2world")
      , (True, "hello\xE2\x82\xACworld")
      , (True, "hello\xF0\xA4\xAD\xA2world")
      , (True, "\xce\xba\xe1\xbd\xb9\xcf\x83\xce\xbc\xce\xb5")
      ])
  -- missing: All prefixes of correct UTF-8 text
  , ( "First possible sequence of a certain length"
    , [ (True, "\x00")
      , (True, "\xc2\x80")
      , (True, "\xe0\xa0\x80")
      , (True, "\xf0\x90\x80\x80")
      ])
  , ( "First possible sequence length 5/6 (invalid codepoints)"
    , [(False, "\xf8\x88\x80\x80\x80"), (False, "\xfc\x84\x80\x80\x80\x80")])
  , ( "Last possible sequence of a certain length"
    , [ (True, "\x7f")
      , (True, "\xdf\xbf")
      , (True, "\xef\xbf\xbf")
      , (True, "\xf4\x8f\xbf\xbf")
      ])
  , ( "Last possible sequence length 4/5/6 (invalid codepoints)"
    , [ (False, "\xf7\xbf\xbf\xbf")
      , (False, "\xfb\xbf\xbf\xbf\xbf")
      , (False, "\xfd\xbf\xbf\xbf\xbf\xbf")
      ])
  , ( "Other boundary conditions"
    , [ (True, "\xed\x9f\xbf")
      , (True, "\xee\x80\x80")
      , (True, "\xef\xbf\xbd")
      , (True, "\xf4\x8f\xbf\xbf")
      , (False, "\xf4\x90\x80\x80")
      ])
  , ( "Unexpected continuation bytes"
    , [ (False, "\x80")
      , (False, "\xbf")
      , (False, "\x80\xbf")
      , (False, "\x80\xbf\x80")
      , (False, "\x80\xbf\x80\xbf")
      , (False, "\x80\xbf\x80\xbf\x80")
      , (False, "\x80\xbf\x80\xbf\x80\xbf")
      , (False, pack [0x80 .. 0xbe])
      ])
  , ( "Lonely start characters"
    , [ (False, pack [a .. b])
      | (a, b) <-
          [(0xc0, 0xde), (0xe0, 0xee), (0xf0, 0xf6), (0xf8, 0xfb), (0xfc, 0xfc)]
      ])
  , ( "Sequences with last continuation byte missing"
    , [ (False, "\xc0")
      , (False, "\xe0\x80")
      , (False, "\xf0\x80\x80")
      , (False, "\xf8\x80\x80\x80")
      , (False, "\xfc\x80\x80\x80\x80")
      , (False, "\xdf")
      , (False, "\xef\xbf")
      , (False, "\xf7\xbf\xbf")
      , (False, "\xfb\xbf\xbf\xbf")
      , (False, "\xfd\xbf\xbf\xbf\xbf")
      ])
  -- missing: Concatenation of incomplete sequences
  , ( "Impossible bytes"
    , [(False, "\xfe"), (False, "\xff"), (False, "\xfe\xfe\xff\xff")])
  , ( "Examples of an overlong ASCII character"
    , [ (False, "\xc0\xaf")
      , (False, "\xe0\x80\xaf")
      , (False, "\xf0\x80\x80\xaf")
      , (False, "\xf8\x80\x80\x80\xaf")
      , (False, "\xfc\x80\x80\x80\x80\xaf")
      ])
  , ( "Maximum overlong sequences"
    , [ (False, "\xc1\xbf")
      , (False, "\xe0\x9f\xbf")
      , (False, "\xf0\x8f\xbf\xbf")
      , (False, "\xf8\x87\xbf\xbf\xbf")
      , (False, "\xfc\x83\xbf\xbf\xbf\xbf")
      ])
  , ( "Overlong representation of the NUL character"
    , [ (False, "\xc0\x80")
      , (False, "\xe0\x80\x80")
      , (False, "\xf0\x80\x80\x80")
      , (False, "\xf8\x80\x80\x80\x80")
      , (False, "\xfc\x80\x80\x80\x80\x80")
      ])
  , ( "Single UTF-16 surrogates"
    , [ (False, "\xed\xa0\x80")
      , (False, "\xed\xad\xbf")
      , (False, "\xed\xae\x80")
      , (False, "\xed\xaf\xbf")
      , (False, "\xed\xb0\x80")
      , (False, "\xed\xbe\x80")
      , (False, "\xed\xbf\xbf")
      ])
  , ( "Paired UTF-16 surrogates"
    , [ (False, "\xed\xa0\x80\xed\xb0\x80")
      , (False, "\xed\xa0\x80\xed\xbf\xbf")
      , (False, "\xed\xad\xbf\xed\xb0\x80")
      , (False, "\xed\xad\xbf\xed\xbf\xbf")
      , (False, "\xed\xae\x80\xed\xb0\x80")
      , (False, "\xed\xae\x80\xed\xbf\xbf")
      , (False, "\xed\xaf\xbf\xed\xb0\x80")
      , (False, "\xed\xaf\xbf\xed\xbf\xbf")
      ])
  , ( "Non-character code points (valid UTF-8)"
    , [(True, "\xef\xbf\xbe"), (True, "\xef\xbf\xbf")] ++
      [ (True, pack [z1, z2, 0xbf, z3])
      | z1 <- [0xf0, 0xf1, 0xf2, 0xf3, 0xf4]
      , z2 <- [0x8f, 0x9f, 0xaf, 0xbf]
      , z1 /= 0xf4 && z2 /= 0x8f
      , z1 /= 0xf0 && z2 /= 0x8f
      , z3 <- [0xbe, 0xbf]
      ])
  , ( "Unicode specials (i.e. replacement char)"
    , [ (True, "\xef\xbf\xb9")
      , (True, "\xef\xbf\xba")
      , (True, "\xef\xbf\xbb")
      , (True, "\xef\xbf\xbc")
      , (True, "\xef\xbf\xbd")
      , (True, "\xef\xbf\xbe")
      , (True, "\xef\xbf\xbf")
      ])
  ]
