{-# LANGUAGE CPP #-}

module Foreign.HCLR.Binding (
#if (MONO)
  module Foreign.HCLR.Binding.Mono
) where
import Foreign.HCLR.Binding.Mono
#else
  module Foreign.HCLR.Binding.Win
) where
import Foreign.HCLR.Binding.Win
#endif







  



