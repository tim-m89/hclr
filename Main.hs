{-# LANGUAGE QuasiQuotes  #-}

module Main where

import Foreign.HCLR

main :: IO ()
main = [runClr|
  System.Console.WriteLine "Hello World"
|]


