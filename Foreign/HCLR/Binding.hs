{-# LANGUAGE CPP #-}

module Foreign.HCLR.Binding (
  withRuntime,
  assemHasType ) where

#if (mingw32_HOST_OS)
import Foreign.HCLR.Win
#else
import Foreign.HCLR.Mono
#endif





  



