module Foreign.HCLR.Binding.Common where

import Foreign

newtype Object = Object {oid :: ForeignPtr Word32}

class Box a where
  box :: a -> IO Object
  unBox :: Object -> IO a
  arg :: a -> (Int32 -> IO b) -> IO b
  arg x f = do
    Object fb <- box x
    withForeignPtr fb $ \b->
      with b $ \p->
        f (fromIntegral $ ptrToWordPtr p)


