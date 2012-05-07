{-# LANGUAGE QuasiQuotes #-}

module Main where

import Foreign.HCLR

main :: IO ()
main = [runClr|
h <-  new System.Windows.Forms.Form
|]


