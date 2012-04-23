{-# LANGUAGE CPP #-}

module Foreign.HCLR.Binding (
  withRuntime,
  assemHasType ) where

#if (mingw32_HOST_OS)
import Foreign.HCLR.Binding.Win
#else
import Foreign.HCLR.Binding.Mono
#endif





  



