{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse, UndecidableInstances, IncoherentInstances #-}

module Foreign.HCLR.Binding.Mono (
  module Foreign.HCLR.Binding.Mono
) where

import Control.Exception
import Data.List
import Foreign
import Foreign.C
import Foreign.HCLR.Ast
import System.Environment
import qualified Data.Text as T
import Data.Text.Foreign
import qualified Foreign.Concurrent as FC

type GBool = CInt
gboolTrue = 1
gboolFalse = 0

data MonoAssembly = MonoAssembly
type MonoAssemblyPtr = Ptr MonoAssembly
data MonoAssemblyName = MonoAssemblyName
type MonoAssemblyNamePtr = Ptr MonoAssemblyName
data MonoDomain = MonoDomain
type MonoDomainPtr = Ptr MonoDomain
data MonoImage = MonoImage
type MonoImagePtr = Ptr MonoImage
type MonoString = MonoObject
type MonoStringPtr = Ptr MonoString
data MonoMethodDesc = MonoMethodDesc
type MonoMethodDescPtr = Ptr MonoMethodDesc
data MonoMethod = MonoMethod
type MonoMethodPtr = Ptr MonoMethod
data MonoObject = MonoObject
type MonoObjectPtr = Ptr MonoObject
type MonoHandle = Word32
data MonoClass = MonoClass
type MonoClassPtr = Ptr MonoClass
data MonoImageOpenStatus = MonoImageOpenStatus
type MonoImageOpenStatusPtr = Ptr MonoImageOpenStatus
data MonoMethodSignature = MonoMethodSignature
type MonoMethodSignaturePtr = Ptr MonoMethodSignature

foreign import ccall mono_jit_init :: CString -> IO MonoDomainPtr
foreign import ccall mono_jit_cleanup :: MonoDomainPtr -> IO ()
foreign import ccall mono_get_corlib :: IO MonoImagePtr
foreign import ccall mono_string_new :: MonoDomainPtr -> CString -> IO MonoStringPtr
foreign import ccall mono_string_new_utf16 :: MonoDomainPtr -> Ptr Word16 -> Int32 -> IO MonoStringPtr
foreign import ccall mono_string_new_wrapper :: CString -> IO MonoStringPtr
foreign import ccall mono_method_desc_new :: CString -> GBool -> IO MonoMethodDescPtr
foreign import ccall mono_method_desc_search_in_image :: MonoMethodDescPtr -> MonoImagePtr -> IO MonoMethodPtr
foreign import ccall mono_method_desc_free :: MonoMethodDescPtr -> IO ()
foreign import ccall mono_runtime_invoke :: MonoMethodPtr -> MonoObjectPtr -> Ptr MonoObjectPtr -> Ptr () -> IO MonoObjectPtr
foreign import ccall mono_domain_get :: IO MonoDomainPtr
foreign import ccall mono_gchandle_new  :: MonoObjectPtr -> GBool -> IO MonoHandle
foreign import ccall mono_gchandle_get_target :: MonoHandle -> IO MonoObjectPtr
foreign import ccall mono_gchandle_free :: MonoHandle -> IO ()
foreign import ccall mono_get_int16_class :: IO MonoClassPtr
foreign import ccall mono_value_box :: MonoDomainPtr -> MonoClassPtr -> Ptr Int -> IO MonoObjectPtr
foreign import ccall mono_object_unbox :: MonoObjectPtr -> IO (Ptr Int)
foreign import ccall mono_assembly_get_image :: MonoAssemblyPtr -> IO MonoImagePtr
foreign import ccall mono_assembly_name_new :: CString -> IO MonoAssemblyNamePtr
foreign import ccall mono_assembly_load :: MonoAssemblyNamePtr -> CString -> MonoImageOpenStatusPtr -> IO MonoAssemblyPtr
foreign import ccall mono_class_from_name :: MonoImagePtr -> CString -> CString -> IO MonoClassPtr
foreign import ccall mono_object_new :: MonoDomainPtr -> MonoClassPtr -> IO MonoObjectPtr
foreign import ccall mono_config_parse :: CString -> IO ()
foreign import ccall mono_runtime_object_init :: MonoObjectPtr -> IO ()
foreign import ccall mono_image_get_name :: MonoImagePtr -> IO CString
foreign import ccall mono_class_num_methods :: MonoClassPtr -> IO CInt
foreign import ccall mono_class_get_methods :: MonoClassPtr -> Ptr CIntPtr -> IO MonoMethodPtr
foreign import ccall mono_method_get_name :: MonoMethodPtr -> IO CString
foreign import ccall mono_method_signature :: MonoMethodPtr -> IO MonoMethodSignaturePtr

foreign import ccall "marshal.c boxString" boxString :: Word32 -> Int32 -> IO MonoHandle
foreign import ccall "marshal.c getString" getString :: MonoHandle -> IO (Ptr Word16)
foreign import ccall "marshal.c stringLength" stringLength :: MonoHandle -> IO Int32

monoLoadAssembly :: String -> IO MonoAssemblyPtr
monoLoadAssembly s = withCString s (\c-> mono_assembly_name_new c >>= \n-> mono_assembly_load n nullPtr nullPtr)

monoInit :: IO MonoDomainPtr
monoInit = do
  mono_config_parse nullPtr
  getProgName >>= flip withCString mono_jit_init


withRuntime :: IO b -> IO b
withRuntime x = bracket monoInit mono_jit_cleanup (\z-> x)

assemHasType :: Assembly -> CLRType -> IO Bool
assemHasType (Assembly a) (CLRType t) = do
  let ns = concat $ intersperse "." $ init t
      typ = last t
  assem <- monoLoadAssembly a
  image <- mono_assembly_get_image assem
  cls <- withCString ns (\nsc-> withCString typ (\typc-> mono_class_from_name image nsc typc))
  print a
  print ns
  print typ
  return (cls /= nullPtr)

withObject :: Object -> (Word32 -> IO a) -> IO a
withObject (Object fp) f = withForeignPtr fp $ \p-> do
  x <- peek p
  f x

data Object = NullObject | Object {oid :: ForeignPtr MonoHandle}

objectFromPtr :: MonoObjectPtr -> IO Object
objectFromPtr oP = do
    handle <- mono_gchandle_new oP gboolTrue
    fp <- mallocForeignPtr
    withForeignPtr fp (\p-> poke p handle)
    FC.addForeignPtrFinalizer fp (mono_gchandle_free handle)
    return (Object fp)

  

objectGetHandle :: Object -> IO MonoHandle
objectGetHandle (Object fp) =  withForeignPtr fp peek

objectGetTarget :: Object -> IO MonoObjectPtr
objectGetTarget obj = case obj of
  (Object fp) -> objectGetHandle obj >>= mono_gchandle_get_target
  NullObject -> return nullPtr

class Box a where
  box :: a -> IO Object
  unBox :: Object -> IO a

class Marshal a where
  arg :: a -> (Ptr MonoObjectPtr -> IO b) -> IO b


instance Box T.Text where
  box x = do
    domain <- mono_domain_get
    string <- useAsPtr x (\t-> \l-> mono_string_new_utf16 domain t (fromIntegral l) )
    handle <- mono_gchandle_new string gboolTrue
    fp <- mallocForeignPtr
    withForeignPtr fp (\p-> poke p handle)
    FC.addForeignPtrFinalizer fp (mono_gchandle_free handle)
    return (Object fp)
  unBox ob = withObject ob $ \x-> do
    len <- stringLength x
    s <- getString x
    fromPtr s (fromIntegral len)

instance Box String where
  box x = box $ T.pack x
  unBox ob = unBox ob >>= \x-> return $ T.unpack x

instance (Box a) => Marshal a where
  arg x f = do
      obj <- box x
      t <- objectGetTarget obj
      withArray [t] f

instance Box () where
  box () = return NullObject
  unBox obj = return ()

instance Marshal () where
  arg () f = withArray [] f

instance Box Object where
  box x = return x
  unBox x = return x

instance (Box a, Box b) => Marshal (a, b) where
  arg (x,y) f = do
    x' <- box x >>= objectGetTarget
    y' <- box y >>= objectGetTarget
    withArray [x', y'] f

instance (Box a, Box b, Box c) => Marshal (a, b, c) where
  arg (x, y, z) f = do
    x' <- box x >>= objectGetTarget
    y' <- box y >>= objectGetTarget
    z' <- box z >>= objectGetTarget
    withArray [x', y', z'] f

instance (Box a, Box b, Box c, Box d) => Marshal (a, b, c, d) where
  arg (a1, a2, a3, a4) f = do
    a1' <- box a1 >>= objectGetTarget
    a2' <- box a2 >>= objectGetTarget
    a3' <- box a3 >>= objectGetTarget
    a4' <- box a4 >>= objectGetTarget
    withArray [a1', a2', a3', a4'] f

instance (Box a, Box b, Box c, Box d, Box e) => Marshal (a, b, c, d, e) where
  arg (a1, a2, a3, a4, a5) f = do
    a1' <- box a1 >>= objectGetTarget
    a2' <- box a2 >>= objectGetTarget
    a3' <- box a3 >>= objectGetTarget
    a4' <- box a4 >>= objectGetTarget
    a5' <- box a5 >>= objectGetTarget
    withArray [a1', a2', a3', a4', a5'] f

instance (Box a, Box b, Box c, Box d, Box e, Box f) => Marshal (a, b, c, d, e, f) where
  arg (a1, a2, a3, a4, a5, a6) f = do
    a1' <- box a1 >>= objectGetTarget
    a2' <- box a2 >>= objectGetTarget
    a3' <- box a3 >>= objectGetTarget
    a4' <- box a4 >>= objectGetTarget
    a5' <- box a5 >>= objectGetTarget
    a6' <- box a6 >>= objectGetTarget
    withArray [a1', a2', a3', a4', a5', a6'] f

instance (Box a, Box b, Box c, Box d, Box e, Box f, Box g) => Marshal (a, b, c, d, e, f, g) where
  arg (a1, a2, a3, a4, a5, a6, a7) f = do
    a1' <- box a1 >>= objectGetTarget
    a2' <- box a2 >>= objectGetTarget
    a3' <- box a3 >>= objectGetTarget
    a4' <- box a4 >>= objectGetTarget
    a5' <- box a5 >>= objectGetTarget
    a6' <- box a6 >>= objectGetTarget
    a7' <- box a7 >>= objectGetTarget
    withArray [a1', a2', a3', a4', a5', a6', a7'] f

instance (Box a, Box b, Box c, Box d, Box e, Box f, Box g, Box h) => Marshal (a, b, c, d, e, f, g, h) where
  arg (a1, a2, a3, a4, a5, a6, a7, a8) f = do
    a1' <- box a1 >>= objectGetTarget
    a2' <- box a2 >>= objectGetTarget
    a3' <- box a3 >>= objectGetTarget
    a4' <- box a4 >>= objectGetTarget
    a5' <- box a5 >>= objectGetTarget
    a6' <- box a6 >>= objectGetTarget
    a7' <- box a7 >>= objectGetTarget
    a8' <- box a7 >>= objectGetTarget
    withArray [a1', a2', a3', a4', a5', a6', a7', a8'] f

instance (Box a, Box b, Box c, Box d, Box e, Box f, Box g, Box h, Box i) => Marshal (a, b, c, d, e, f, g, h, i) where
  arg (a1, a2, a3, a4, a5, a6, a7, a8, a9) f = do
    a1' <- box a1 >>= objectGetTarget
    a2' <- box a2 >>= objectGetTarget
    a3' <- box a3 >>= objectGetTarget
    a4' <- box a4 >>= objectGetTarget
    a5' <- box a5 >>= objectGetTarget
    a6' <- box a6 >>= objectGetTarget
    a7' <- box a7 >>= objectGetTarget
    a8' <- box a7 >>= objectGetTarget
    a9' <- box a9 >>= objectGetTarget
    withArray [a1', a2', a3', a4', a5', a6', a7', a8', a9'] f

instance (Box a, Box b, Box c, Box d, Box e, Box f, Box g, Box h, Box i, Box j) => Marshal (a, b, c, d, e, f, g, h, i, j) where
  arg (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) f = do
    a1' <- box a1 >>= objectGetTarget
    a2' <- box a2 >>= objectGetTarget
    a3' <- box a3 >>= objectGetTarget
    a4' <- box a4 >>= objectGetTarget
    a5' <- box a5 >>= objectGetTarget
    a6' <- box a6 >>= objectGetTarget
    a7' <- box a7 >>= objectGetTarget
    a8' <- box a7 >>= objectGetTarget
    a9' <- box a9 >>= objectGetTarget
    a10' <- box a10 >>= objectGetTarget
    withArray [a1', a2', a3', a4', a5', a6', a7', a8', a9', a10'] f







 
assemblyImage :: String -> IO MonoImagePtr
assemblyImage s = do
  assem <- monoLoadAssembly s
  if assem == nullPtr then do
    putStrLn "null assem"
    return nullPtr
  else
    mono_assembly_get_image assem


invokeMethod :: Marshal a => Assembly -> String -> String -> Object -> a -> IO Object  --method name is parsed with argument types eg WriteLine(String)
invokeMethod (Assembly assem) t mth target args = do
  let funName = (t ++ ":" ++ mth)
  image <- assemblyImage assem
  if image == nullPtr then
    putStrLn "image null" >> return NullObject
  else do
    desc <- withCString funName (\c-> mono_method_desc_new c gboolTrue)
    method <- mono_method_desc_search_in_image desc image
    mono_method_desc_free desc
    if (method == nullPtr) then
      error ("Cannot find method: " ++ funName)
    else do
      arg args $ \argP-> do
        targetP <- objectGetTarget target
        ret <- mono_runtime_invoke method targetP argP nullPtr >>= flip mono_gchandle_new gboolTrue
        fp <- mallocForeignPtr
        withForeignPtr fp (\p-> poke p ret)
        FC.addForeignPtrFinalizer fp (mono_gchandle_free ret)
        return $ Object fp


monoGetClass :: String -> String -> String -> IO MonoClassPtr
monoGetClass assem ns n = assemblyImage assem >>= \image-> withCString ns (\nsC-> withCString n (\nC-> mono_class_from_name image nsC nC) ) 

monoObjectNew :: MonoClassPtr -> IO Object
monoObjectNew cls = do
  domain <- mono_domain_get
  objPtr <- mono_object_new domain cls
  if objPtr==nullPtr then
    error "null object"
  else do
    obj <- mono_gchandle_new objPtr gboolTrue
    fp <- mallocForeignPtr
    withForeignPtr fp (\p-> poke p obj)
    FC.addForeignPtrFinalizer fp (mono_gchandle_free obj)
    return (Object fp)

nsPermute :: [String] -> [(String,String)]
nsPermute (x:xs) = map (\(t1,t2)-> (intercalate "." t1, intercalate "." t2)) $ nsPermute' (length xs) (x:xs)
  where nsPermute' i z = splitAt i z : if i > 1 then nsPermute' (i-1) z else []


monoFindClass :: String -> [String] -> IO MonoClassPtr
monoFindClass assem typ = do
  ptrs <- mapM (uncurry (monoGetClass assem)) $ nsPermute typ
  found <- return $ find (/= nullPtr) ptrs
  case found of
    Just x -> return x
    Nothing -> return nullPtr

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = 
 case break (==x) xs of
   (ys,[])   -> [ys]
   (ys,_:zs) -> ys:splitOn x zs

objectNew :: Marshal a => Assembly -> String -> a -> IO Object
objectNew (Assembly assem) t args = do
  let typ = splitOn '.' t
  cls <- monoFindClass assem typ
  o <- monoObjectNew cls
  invokeMethod (Assembly assem) t ".ctor()" o args
  return o

