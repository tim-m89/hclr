module Foreign.HCLR.Binding.Common where

import Foreign

newtype Object = Object {oid :: Word32}

class Box a where
  box :: a -> IO Object
  unBox :: Object -> IO a
  arg :: a -> (Int32 -> Int32 -> IO b) -> IO b
  arg x f = do
    Object b <- box x
    with b $ \p->
      f 1 (fromIntegral $ ptrToWordPtr p)


