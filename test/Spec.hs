{-# LANGUAGE OverloadedStrings #-}
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
