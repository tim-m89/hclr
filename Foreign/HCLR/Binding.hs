{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse #-}

module Foreign.HCLR.Binding where

import Control.Concurrent
import Control.Monad
import Data.Int
import Data.Text as TXT
import Data.Text.Foreign
import Data.Word
import Foreign.C
import qualified Foreign.Concurrent as FC
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import qualified Foreign.Marshal.Utils as FU
import Foreign.Ptr
import Foreign.Storable
import System.Environment

type GBool = CInt
gboolTrue = 1
gboolFalse = 0

data MonoAssembly = MonoAssembly
type MonoAssemblyPtr = Ptr MonoAssembly
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
foreign import ccall mono_runtime_invoke :: MonoMethodPtr -> Ptr () -> Ptr MonoObjectPtr -> Ptr () -> IO MonoObjectPtr
foreign import ccall mono_domain_get :: IO MonoDomainPtr
foreign import ccall mono_gchandle_new  :: MonoObjectPtr -> GBool -> IO MonoHandle
foreign import ccall mono_gchandle_get_target :: MonoHandle -> IO MonoObjectPtr
foreign import ccall mono_gchandle_free :: MonoHandle -> IO ()
foreign import ccall mono_get_int16_class :: IO MonoClassPtr
foreign import ccall mono_value_box :: MonoDomainPtr -> MonoClassPtr -> Ptr Int16 -> IO MonoObjectPtr
foreign import ccall mono_assembly_get_image :: MonoAssemblyPtr -> IO MonoImagePtr
foreign import ccall mono_assembly_load_with_partial_name :: CString -> MonoImageOpenStatusPtr -> IO MonoAssemblyPtr
foreign import ccall mono_class_from_name :: MonoImagePtr -> CString -> CString -> IO MonoClassPtr
foreign import ccall mono_object_new :: MonoDomainPtr -> MonoClassPtr -> IO MonoObjectPtr
foreign import ccall mono_config_parse :: Ptr () -> IO ()
foreign import ccall mono_runtime_object_init :: MonoObjectPtr -> IO ()
foreign import ccall mono_image_get_name :: MonoImagePtr -> IO CString
foreign import ccall mono_class_num_methods :: MonoClassPtr -> IO CInt
foreign import ccall mono_class_get_methods :: MonoClassPtr -> Ptr CIntPtr -> IO MonoMethodPtr
foreign import ccall mono_method_get_name :: MonoMethodPtr -> IO CString
foreign import ccall mono_method_signature :: MonoMethodPtr -> IO MonoMethodSignaturePtr

newtype MObject = MObject (ForeignPtr MonoHandle)

getTarget :: MObject -> IO MonoObjectPtr
getTarget (MObject fp) = withForeignPtr fp (\p-> peek p >>= mono_gchandle_get_target)

class Boxable from where
  box :: from -> IO MObject
  evaluateWith :: (MObject -> IO a) -> from -> IO a
  evaluateWith f o = box o >>= f
  --unbox :: MObject -> from

instance Boxable String where
  box s = do
    domain <- mono_domain_get
    string <- useAsPtr (TXT.pack s) (\t-> \l-> mono_string_new_utf16 domain t (fromIntegral l) )
    handle <- mono_gchandle_new string gboolTrue
    fp <- mallocForeignPtr
    withForeignPtr fp (\p-> poke p handle)
    FC.addForeignPtrFinalizer fp (mono_gchandle_free handle)
    return (MObject fp)

instance Boxable Int16 where
  box i = do
    domain <- mono_domain_get
    cls <- mono_get_int16_class
    boxed <- FU.with i (mono_value_box domain cls)
    handle <- mono_gchandle_new boxed gboolTrue
    fp <- mallocForeignPtr
    withForeignPtr fp (\p-> poke p handle)
    FC.addForeignPtrFinalizer fp (mono_gchandle_free handle)
    return (MObject fp)

instance Boxable MObject where
  box = return

monoInvoke :: MonoImagePtr -> String -> [MObject] -> IO MObject
monoInvoke image funName args = do
  desc <- withCString funName (\c-> mono_method_desc_new c gboolTrue)
  method <- mono_method_desc_search_in_image desc image
  if (method == nullPtr) then
    error ("Cannot find method: " ++ funName)
  else do
    mono_method_desc_free desc
    argsp <- mapM getTarget args
    ret <- withArray argsp (\argP-> mono_runtime_invoke method nullPtr argP nullPtr) >>= flip mono_gchandle_new gboolTrue
    fp <- mallocForeignPtr
    withForeignPtr fp (\p-> poke p ret)
    FC.addForeignPtrFinalizer fp (mono_gchandle_free ret)
    return (MObject fp)

getMethod :: String -> String -> IO ([MObject] -> IO MObject)
getMethod assem funName = do
  image <- assemblyImage assem
  desc <- withCString funName (\c-> mono_method_desc_new c gboolTrue)
  method <- mono_method_desc_search_in_image desc image
  mono_method_desc_free desc
  if (method == nullPtr) then
    error ("Cannot find method: " ++ funName)
  else return (\args -> do
    argsp <- mapM getTarget args
    ret <- withArray argsp (\argP-> mono_runtime_invoke method nullPtr argP nullPtr) >>= flip mono_gchandle_new gboolTrue
    fp <- mallocForeignPtr
    withForeignPtr fp (\p-> poke p ret)
    FC.addForeignPtrFinalizer fp (mono_gchandle_free ret)
    return (MObject fp) )

invokeMethod :: String -> String -> [MObject] -> IO MObject
invokeMethod assem funName args = do
  image <- assemblyImage assem
  desc <- withCString funName (\c-> mono_method_desc_new c gboolTrue)
  method <- mono_method_desc_search_in_image desc image
  mono_method_desc_free desc
  if (method == nullPtr) then
    error ("Cannot find method: " ++ funName)
  else do
    argsp <- mapM getTarget args
    ret <- withArray argsp (\argP-> mono_runtime_invoke method nullPtr argP nullPtr) >>= flip mono_gchandle_new gboolTrue
    fp <- mallocForeignPtr
    withForeignPtr fp (\p-> poke p ret)
    FC.addForeignPtrFinalizer fp (mono_gchandle_free ret)
    return (MObject fp)

monoLoadAssembly :: String -> IO MonoAssemblyPtr
monoLoadAssembly s = withCString s (\c->mono_assembly_load_with_partial_name c nullPtr)

monoGetClass :: String -> String -> String -> IO MonoClassPtr
monoGetClass assem ns n = assemblyImage assem >>= \image-> withCString ns (\nsC-> withCString n (\nC-> mono_class_from_name image nsC nC) ) 

monoObjectNew :: MonoClassPtr -> IO MObject
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
    getTarget (MObject fp) >>= mono_runtime_object_init
    return (MObject fp)

assemblyImage :: String -> IO MonoImagePtr
assemblyImage s = monoLoadAssembly s >>= mono_assembly_get_image

monoClassGetMethods :: MonoClassPtr -> IO [MonoMethodPtr]
monoClassGetMethods cls = alloca (\ptr-> do
  methodCount <- mono_class_num_methods cls >>= return . fromEnum
  replicateM methodCount (mono_class_get_methods cls ptr) )
