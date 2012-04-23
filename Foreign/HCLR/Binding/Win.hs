module Foreign.HCLR.Binding.Win

import Foreign
import Foreign.HCLR.Binding.Common
import Data.Text
import Data.Text.Foreign

runtimeVersion = "v4.0.30319" 
runtimeFlavor = "wks"
mscorlibString = case runtimeVersion of
  "v2.0.50727" -> "mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
  "v4.0.30319" -> "mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"


stringLength (Object x) = do
  Object s1 <- box ("System.String, " `append` mscorlibString :: Text)
  Object s2 <- box ("Length" :: Text)
  getValue s1 s2 x >>= unBoxInt32


instance Box Text where
  box x = useAsPtr x $ \p-> \l->
    boxString (fromIntegral $ ptrToIntPtr p) (fromIntegral l) >>= return . Object
  unBox o@(Object x) = do
    len <- stringLength o
    allocaBytes (fromIntegral $ len * 2) $ \p2-> do
      unBoxString x (fromIntegral $ ptrToIntPtr p2)
      fromPtr p2 (fromIntegral len)


