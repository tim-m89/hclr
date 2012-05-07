{-# LANGUAGE CPP #-}

module Foreign.HCLR.Binding (
#if (mingw32_HOST_OS)
  module Foreign.HCLR.Binding.Win
) where
import Foreign.HCLR.Binding.Win
#else
  module Foreign.HCLR.Binding.Mono
) where
import Foreign.HCLR.Binding.Mono
#endif







  



