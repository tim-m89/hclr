{-# LANGUAGE QuasiQuotes  #-}

module Main where

import Foreign.HCLR
import Foreign.HCLR.Ast
import Foreign.HCLR.Binding
import Data.Text

corlibs = "mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
corlib = Assembly corlibs
formslibs = "System.Windows.Forms, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
formslib = Assembly formslibs

--nullObject = box ("S"::Text)

{-main :: IO ()
main = withRuntime $ do
  putStrLn "a"
  invokeMethodCorlib "System.Console" "WriteLine(string)" NullObject ("Hello World" ::Text)
  invokeMethod formslib "System.Windows.Forms.Application" "EnableVisualStyles()" NullObject ()
  f <- objectNew formslib "System.Windows.Forms.Form" ()
  invokeMethod formslib "System.Windows.Forms.Application" "Run(System.Windows.Forms.Form)" NullObject f
  monoGetClass formslibs "System.Windows.Forms" "Form" >>= monoClassAllSuper >>= \x-> mapM (\y-> monoClassName y >>= putStrLn) x
  c1 <- monoGetClass formslibs "System.Windows.Forms" "Form"
  c2 <- monoGetClass corlibs "System" "Object"
  isTy <- c1 `isType` c2
  putStrLn $ show isTy
  return ()

-}

main :: IO ()
main = [runClr|
  System.Console.WriteLine "Hello World"
|]


