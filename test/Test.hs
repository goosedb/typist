{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Typist.Logged (Unquoted (..), WithTemplate (WithTemplate), fmt, fmtt)

main :: IO ()
main = defaultMain do
  testGroup
    "Interpolation"
    [ testGroup
        "No template"
        [ testCase "No arguments" do
            fmt @"Hello" `shouldBe` "Hello"
        , testCase "One argument" do
            fmt @"Hello, #{name}!" (#name $ Unquoted @String "Kitty") `shouldBe` "Hello, Kitty!"
        , testCase "Two arguments" do
            fmt @"Hello, #{name}, do you like #{dish}?"
              (#name $ Unquoted @String "Mike")
              (#dish $ Unquoted @String "pasta")
              `shouldBe` "Hello, Mike, do you like pasta?"
        , testCase "Many agruments" do
            fmt @"One: #{one}, two: #{two}, three: #{three}, four: #{four}, five: #{five}"
              (#one (1 :: Int))
              (#two True)
              (#three 'a')
              (#four [5 :: Int])
              (#five ('a', False))
              `shouldBe` "One: 1, two: True, three: 'a', four: [5], five: ('a',False)"
        ]
    , testGroup
        "With template"
        [ testCase "No arguments" do
            fmtt @"Hello" `shouldBe` WithTemplate "Hello" "Hello"
        , testCase "One argument" do
            fmtt @"Hello, #{name}!" (#name $ Unquoted @String "Kitty")
              `shouldBe` WithTemplate "Hello, Kitty!" "Hello, #{name}!"
        , testCase "Two arguments" do
            fmtt @"Hello, #{name}, do you like #{dish}?"
              (#name $ Unquoted @String "Mike")
              (#dish $ Unquoted @String "pasta")
              `shouldBe` WithTemplate "Hello, Mike, do you like pasta?" "Hello, #{name}, do you like #{dish}?"
        , testCase "Many agruments" do
            fmtt @"One: #{one}, two: #{two}, three: #{three}, four: #{four}, five: #{five}"
              (#one (1 :: Int))
              (#two True)
              (#three 'a')
              (#four [5 :: Int])
              (#five ('a', False))
              `shouldBe` WithTemplate
                "One: 1, two: True, three: 'a', four: [5], five: ('a',False)"
                "One: #{one}, two: #{two}, three: #{three}, four: #{four}, five: #{five}"
        ]
    ]

shouldBe :: (Show a, Eq a) => a -> a -> Assertion
shouldBe a b = assertBool "" (a == b)
