{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main

import Text.Utf8Validator

main :: IO ()
main = defaultMain [bench "1" $ whnf validateBS "Hello-µ@ßöäüàá-UTF-8!!"]
