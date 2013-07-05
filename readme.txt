Pragmatic use of the Common Language Runtime within a quasi
quoted dsl for Haskell. Very early stages at the moment. Hello world
on Mono. MS dotNet bindings need a lot of work.



Foreign.HCLR   // libray entry point. quasi quoter
Foreign.HCLR.Binding // binding to runtime
Foreign.HCLR.Binding.Common
Foreign.HCLR.Binding.Mono
Foreign.HCLR.Binding.Win
Foreign.HCLR.Ast // abstract syntax tree
Foreign.HCLR.CodeGen // generate code
Foreign.HCLR.Parser // parser for dsl

windows uses a driver dll due to the lack of capabilities in the hosting api

marshall.c contains some glue




