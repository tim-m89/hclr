{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.HCLR.Mono where

import Control.Exception
import Data.Int
import Data.Word
import Data.List
import Foreign.HCLR.Ast
import Foreign.C
import qualified Foreign.Concurrent as FC
import Foreign.ForeignPtr
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
foreign import ccall mono_runtime_invoke :: MonoMethodPtr -> Ptr () -> Ptr MonoObjectPtr -> Ptr () -> IO MonoObjectPtr
foreign import ccall mono_domain_get :: IO MonoDomainPtr
foreign import ccall mono_gchandle_new  :: MonoObjectPtr -> GBool -> IO MonoHandle
foreign import ccall mono_gchandle_get_target :: MonoHandle -> IO MonoObjectPtr
foreign import ccall mono_gchandle_free :: MonoHandle -> IO ()
foreign import ccall mono_get_int16_class :: IO MonoClassPtr
foreign import ccall mono_value_box :: MonoDomainPtr -> MonoClassPtr -> Ptr Int16 -> IO MonoObjectPtr
foreign import ccall mono_assembly_get_image :: MonoAssemblyPtr -> IO MonoImagePtr
foreign import ccall mono_assembly_name_new :: CString -> IO MonoAssemblyNamePtr
foreign import ccall mono_assembly_load :: MonoAssemblyNamePtr -> CString -> MonoImageOpenStatusPtr -> IO MonoAssemblyPtr
foreign import ccall mono_class_from_name :: MonoImagePtr -> CString -> CString -> IO MonoClassPtr
foreign import ccall mono_object_new :: MonoDomainPtr -> MonoClassPtr -> IO MonoObjectPtr
foreign import ccall mono_config_parse :: Ptr () -> IO ()
foreign import ccall mono_runtime_object_init :: MonoObjectPtr -> IO ()
foreign import ccall mono_image_get_name :: MonoImagePtr -> IO CString
foreign import ccall mono_class_num_methods :: MonoClassPtr -> IO CInt
foreign import ccall mono_class_get_methods :: MonoClassPtr -> Ptr CIntPtr -> IO MonoMethodPtr
foreign import ccall mono_method_get_name :: MonoMethodPtr -> IO CString
foreign import ccall mono_method_signature :: MonoMethodPtr -> IO MonoMethodSignaturePtr


monoLoadAssembly :: String -> IO MonoAssemblyPtr
monoLoadAssembly s = withCString s (\c-> mono_assembly_name_new c >>= \n-> mono_assembly_load n nullPtr nullPtr)

monoInit :: IO MonoDomainPtr
monoInit = do
  mono_config_parse nullPtr
  getProgName >>= flip withCString mono_jit_init


--
-- Shared API
--

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

