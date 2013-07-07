{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse, UndecidableInstances, IncoherentInstances #-}

module Foreign.HCLR.Binding.Mono.Internal (
  module Foreign.HCLR.Binding.Mono.Internal
) where

import Control.Exception
import Data.List
import Foreign
import Foreign.C
import System.Environment
import qualified Data.Text as T
import Data.Text.Foreign
import qualified Foreign.Concurrent as FC

type GBool = CInt
gboolTrue :: GBool
gboolTrue = 1
gboolFalse :: GBool
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
type ObjectHandle = Word32
data MonoClass = MonoClass
type MonoClassPtr = Ptr MonoClass
data MonoImageOpenStatus = MonoImageOpenStatus
type MonoImageOpenStatusPtr = Ptr MonoImageOpenStatus
data MonoMethodSignature = MonoMethodSignature
type MonoMethodSignaturePtr = Ptr MonoMethodSignature
data MonoType = MonoType
type MonoTypePtr = Ptr MonoType
data MonoProperty = MonoProperty
type MonoPropertyPtr = Ptr MonoProperty

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
foreign import ccall mono_gchandle_new  :: MonoObjectPtr -> GBool -> IO ObjectHandle
foreign import ccall mono_gchandle_get_target :: ObjectHandle -> IO MonoObjectPtr
foreign import ccall mono_gchandle_free :: ObjectHandle -> IO ()
foreign import ccall mono_get_int16_class :: IO MonoClassPtr
foreign import ccall mono_value_box :: MonoDomainPtr -> MonoClassPtr -> Ptr Int -> IO MonoObjectPtr
foreign import ccall mono_object_unbox :: MonoObjectPtr -> IO (Ptr Int)
foreign import ccall mono_assembly_get_image :: MonoAssemblyPtr -> IO MonoImagePtr
foreign import ccall mono_assembly_name_new :: CString -> IO MonoAssemblyNamePtr
foreign import ccall mono_assembly_fill_assembly_name :: MonoImagePtr -> MonoAssemblyNamePtr -> IO GBool
foreign import ccall mono_stringify_assembly_name :: MonoAssemblyNamePtr -> IO CString
foreign import ccall mono_assembly_load :: MonoAssemblyNamePtr -> CString -> MonoImageOpenStatusPtr -> IO MonoAssemblyPtr
foreign import ccall mono_class_from_name :: MonoImagePtr -> CString -> CString -> IO MonoClassPtr
foreign import ccall mono_object_new :: MonoDomainPtr -> MonoClassPtr -> IO MonoObjectPtr
foreign import ccall mono_config_parse :: CString -> IO ()
foreign import ccall mono_runtime_object_init :: MonoObjectPtr -> IO ()
foreign import ccall mono_image_get_name :: MonoImagePtr -> IO CString
foreign import ccall mono_class_num_methods :: MonoClassPtr -> IO Int
foreign import ccall mono_class_get_methods :: MonoClassPtr -> Ptr (Ptr Int) -> IO MonoMethodPtr
foreign import ccall mono_signature_get_param_count :: MonoMethodSignaturePtr -> IO Int
foreign import ccall mono_signature_get_params :: MonoMethodSignaturePtr -> Ptr (Ptr Int) -> IO MonoTypePtr
foreign import ccall mono_method_get_name :: MonoMethodPtr -> IO CString
foreign import ccall mono_method_full_name :: MonoMethodPtr -> GBool -> IO CString
foreign import ccall mono_method_signature :: MonoMethodPtr -> IO MonoMethodSignaturePtr
foreign import ccall mono_class_get_name :: MonoClassPtr -> IO CString
foreign import ccall mono_class_get_namespace :: MonoClassPtr -> IO CString
foreign import ccall mono_class_get_parent :: MonoClassPtr -> IO MonoClassPtr
foreign import ccall mono_class_from_mono_type :: MonoTypePtr -> IO MonoClassPtr
foreign import ccall mono_class_get_property_from_name :: MonoClassPtr -> CString -> IO MonoPropertyPtr
foreign import ccall mono_object_get_class :: MonoObjectPtr -> IO MonoClassPtr
foreign import ccall mono_domain_create_appdomain :: CString -> CString -> IO MonoDomainPtr
foreign import ccall mono_domain_set :: MonoDomainPtr -> GBool -> IO GBool
foreign import ccall mono_get_root_domain :: IO MonoDomainPtr
foreign import ccall mono_runtime_init :: MonoDomainPtr -> Ptr () -> Ptr () -> IO ()
foreign import ccall mono_set_dirs :: Ptr () -> Ptr () -> IO ()
foreign import ccall mono_register_config_for_assembly :: CString -> CString -> IO ()
foreign import ccall mono_signature_get_return_type :: MonoMethodSignaturePtr -> IO MonoTypePtr
foreign import ccall mono_get_string_class :: IO MonoClassPtr
foreign import ccall mono_class_get_image :: MonoClassPtr -> IO MonoImagePtr 
foreign import ccall "marshal.c boxString" boxString :: Ptr Word16 -> Int32 -> IO ObjectHandle
foreign import ccall "marshal.c getString" getString :: ObjectHandle -> IO (Ptr Word16)
foreign import ccall "marshal.c stringLength" stringLength :: ObjectHandle -> IO Int32
foreign import ccall "marshal.c setupDomain" setupDomain :: MonoDomainPtr -> CString -> CString -> IO () 

monoLoadAssembly :: String -> IO MonoAssemblyPtr
monoLoadAssembly s = withCString s (\c-> mono_assembly_name_new c >>= \n-> mono_assembly_load n nullPtr nullPtr)

monoInit :: IO MonoDomainPtr
monoInit = do
  prog <- getProgName
  dom <- withCString (prog) mono_jit_init
  mono_config_parse nullPtr
  withCString "./" $ \baseDir->
    withCString "hclr.config" $ \configFile-> do
      setupDomain dom baseDir configFile
      return dom



