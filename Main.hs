{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Main where

import Foreign.HCLR
import Foreign.HCLR.Ast
import Foreign.HCLR.Binding
import Data.Text

corlib = Assembly "mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
formslib = Assembly "System.Windows.Forms, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"

nullObject = box ("S"::Text)

main :: IO ()
main = withRuntime $ do
  invokeMethod corlib "System.Console" "WriteLine" NullObject ("Hello World" ::Text)
  invokeMethod formslib "System.Windows.Forms.Application" "EnableVisualStyles" NullObject ()
  f <- objectNew formslib "System.Windows.Forms.Form" ()
  invokeMethod formslib "System.Windows.Forms.Application" "Run" NullObject f
  return ()



