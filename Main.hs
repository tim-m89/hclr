{-# LANGUAGE QuasiQuotes  #-}

module Main where

import Foreign.HCLR

main :: IO ()
main = [runClr|
  System.Console.WriteLine "Hello World"
  System.Windows.Forms.Application.EnableVisualStyles
  f <- new System.Windows.Forms.Form 
  System.Windows.Forms.Application.Run f
|]


