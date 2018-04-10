{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main

import qualified Text.Utf8Validator as V
import qualified Text.Utf8Validator2 as V2

main :: IO ()
main =
  defaultMain
    [ bench "1" $ whnf V.validateBS "Hello-µ@ßöäüàá-UTF-8!!"
    , bench "2" $ whnf V2.validateBS "Hello-µ@ßöäüàá-UTF-8!!"
    ]
