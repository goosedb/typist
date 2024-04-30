{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit
import TextShow (TextShow(..))
import Data.String (IsString(..))
import Typist (fmt)
import Typist.TextShow


main :: IO ()
main = defaultMain do
  testGroup
    "Interpolation"
    [ testGroup
        "No template"
        [ testCase "No arguments" do
            fmt @"Hello" id `shouldBe` "Hello"
        , testCase "One argument" do
            fmt @"Hello, #{name}!" (#name #= Unquoted @String "Kitty") `shouldBe` "Hello, Kitty!"
        , testCase "Two arguments" do
            let formatted = fmt @"Hello, #{name}, do you like #{dish}?" $
                  (#name #= Unquoted @String "Mike") .
                  (#dish #= Unquoted @String "pasta")
            formatted `shouldBe` "Hello, Mike, do you like pasta?"
        , testCase "Many arguments" do
            let formatted = fmt @"One: #{one}, two: #{two}, three: #{three}, four: #{four}, five: #{five}" $
                  (#one #= (1 :: Int)) .
                  (#two #= True) .
                  (#three #= 'a') .
                  (#four #= [5 :: Int]) .
                  (#five #= ('a', False))
            formatted `shouldBe` "One: 1, two: True, three: 'a', four: [5], five: ('a',False)"
        ]
    , testGroup
        "Escaping"
        [ testCase "Should not count escaped sequence as placeholer" do
            fmt @"Hello, \\#{huh} #{name}" (#name #= Unquoted @String "Kitty")
              `shouldBe` "Hello, \\#{huh} Kitty"

            fmt @"Hello, #{name} \\#{huh}" (#name #= Unquoted @String "Kitty")
              `shouldBe` "Hello, Kitty \\#{huh}"
        ]
    ]

shouldBe :: (Show a, Eq a) => a -> a -> Assertion
shouldBe a b = assertBool "" (a == b)
