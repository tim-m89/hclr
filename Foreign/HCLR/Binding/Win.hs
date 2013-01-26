{-# LANGUAGE ForeignFunctionInterface, DoAndIfThenElse, DeriveDataTypeable, ScopedTypeVariables, FlexibleInstances, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : 
-- Copyright   : (c) 2011-2012 Tim Matthews.
--               May include code from Salsa (http://hackage.haskell.org/package/Salsa)
--               (c) 2007-2008 Andrew Appleyard
-- Licence     : BSD-style (see LICENSE)
-- 
-- 
-----------------------------------------------------------------------------

module Foreign.HCLR.Binding.Win (
  module Foreign.HCLR.Binding.Win
) where

import Driver
import System.Win32.DLL
import System.Win32.Types
import Data.Char
import Data.Word
import Numeric
import Foreign
import Foreign.C
import Control.Exception (bracket)
import Data.Maybe
import Data.ByteString.Unsafe
import Data.Text
import Data.Text.Foreign
import qualified Foreign.Concurrent as FC
import Foreign.HCLR.Ast



--
-- COM Support
--

type ComObject = Ptr (FunPtr ())
type ComObjectPtr = Ptr ComObject

foreign import stdcall "CoInitializeEx" coInitializeEx :: Ptr () -> Int32 -> IO HRESULT
foreign import stdcall "CoUninitialize" coUninitialize :: IO ()

coInit_MultiThreaded     = 0 :: Int32
coInit_ApartmentThreaded = 2 :: Int32

hS_OK :: HRESULT
hS_OK = 0x00000000

getFunction :: Int -> (FunPtr a -> b) -> ComObjectPtr -> IO b
getFunction index makeFun this = do
    funPtr <- peek this >>= flip peekElemOff index
    return $ makeFun $ castFunPtr funPtr

checkHR :: String -> IO HRESULT -> IO ()
checkHR s ihr = do
  hr <- ihr
  if hr == hS_OK then return () else do
    putStr $ s ++ "  "
    error $ "HRESULT value == " ++ (hex $ fromIntegral hr)

hex :: Word32 -> String
hex n = "0x" ++ showHex n ""

showE :: HRESULT -> IO ()
showE h = putStrLn $ hex $ fromIntegral h



--
-- GUID Support
--

data GUID = GUID Word32 Word16 Word16 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 
  deriving (Show, Eq)

instance Storable GUID where
  sizeOf    _ = 16
  alignment _ = 4

  peek guidPtr = do
    a  <- peek $ plusPtr guidPtr 0 
    b  <- peek $ plusPtr guidPtr 4
    c  <- peek $ plusPtr guidPtr 6
    d0 <- peek $ plusPtr guidPtr 8
    d1 <- peek $ plusPtr guidPtr 9
    d2 <- peek $ plusPtr guidPtr 10
    d3 <- peek $ plusPtr guidPtr 11
    d4 <- peek $ plusPtr guidPtr 12
    d5 <- peek $ plusPtr guidPtr 13
    d6 <- peek $ plusPtr guidPtr 14
    d7 <- peek $ plusPtr guidPtr 15
    return $ GUID a b c d0 d1 d2 d3 d4 d5 d6 d7

  poke guidPtr (GUID a b c d0 d1 d2 d3 d4 d5 d6 d7) = do
    poke (plusPtr guidPtr 0)  a
    poke (plusPtr guidPtr 4)  b
    poke (plusPtr guidPtr 6)  c
    poke (plusPtr guidPtr 8)  d0
    poke (plusPtr guidPtr 9)  d1
    poke (plusPtr guidPtr 10) d2
    poke (plusPtr guidPtr 11) d3
    poke (plusPtr guidPtr 12) d4
    poke (plusPtr guidPtr 13) d5
    poke (plusPtr guidPtr 14) d6
    poke (plusPtr guidPtr 15) d7

type IID = GUID
type REFIID = Ptr IID
type CLSID = GUID
type REFCLSID = Ptr CLSID
type LIBID = GUID
type REFLIBID = Ptr LIBID
type CATID = GUID
type REFCATID = Ptr CATID

clsid_CLRStrongName = GUID 0xB79B0ACD 0xF5CD 0x409b 0xB5 0xA5 0xA1 0x62 0x44 0x61 0x0B 0x92
iid_ICLRMetaHost = GUID 0xD332DB9E 0xB9B3 0x4125 0x82 0x07 0xA1 0x48 0x84 0xF5 0x32 0x16
clsid_CLRMetaHost = GUID 0x9280188d 0xe8e 0x4867 0xb3 0xc 0x7f 0xa8 0x38 0x84 0xe8 0xde
iid_ICLRMetaHostPolicy = GUID 0xE2190695 0x77B2 0x492e 0x8E 0x14 0xC4 0xB3 0xA7 0xFD 0xD5 0x93
clsid_CLRMetaHostPolicy = GUID 0x2ebcd49a 0x1b47 0x4a61 0xb1 0x3a 0x4a 0x3 0x70 0x1e 0x59 0x4b
iid_ICLRDebugging = GUID 0xd28f3c5a 0x9634 0x4206 0xa5 0x9 0x47 0x75 0x52 0xee 0xfb 0x10
clsid_CLRDebugging = GUID 0xbacc578d 0xfbdd 0x48a4 0x96 0x9f 0x2 0xd9 0x32 0xb7 0x46 0x34
iid_ICLRRuntimeInfo = GUID 0xBD39D1D2 0xBA2F 0x486a 0x89 0xB0 0xB4 0xB0 0xCB 0x46 0x68 0x91
iid_ICLRStrongName = GUID 0x9FD93CCF 0x3280 0x4391 0xB3 0xA9 0x96 0xE1 0xCD 0xE7 0x7C 0x8D
clsid_CLRDebuggingLegacy = GUID 0xDF8395B5 0xA4BA 0x450b 0xA7 0x7C 0xA9 0xA4 0x77 0x62 0xC5 0x20
clsid_CLRProfiling = GUID 0xbd097ed8 0x733e 0x43fe 0x8e 0xd7 0xa9 0x5f 0xf9 0xa8 0x44 0x8c
iid_ICLRProfiling = GUID 0xb349abe3 0xb56f 0x4689 0xbf 0xcd 0x76 0xbf 0x39 0xd8 0x88 0xea
iid_ICLRDebuggingLibraryProvider = GUID 0x3151c08d 0x4d09 0x4f9b 0x88 0x38 0x28 0x80 0xbf 0x18 0xfe 0x51
clsid_CLRRuntimeHost = GUID 0x90F1A06E 0x7712 0x4762 0x86 0xB5 0x7A 0x5E 0xBA 0x6B 0xDB 0x02
iid_ICLRRuntimeHost = GUID 0x90F1A06C 0x7712 0x4762 0x86 0xB5 0x7A 0x5E 0xBA 0x6B 0xDB 0x02
clsid_CorRuntimeHost = GUID 0xcb2f6723 0xab3a 0x11d2 0x9c 0x40 0x00 0xc0 0x4f 0xa3 0x0a 0x3e
iid_ICorRuntimeHost = GUID 0xcb2f6722 0xab3a 0x11d2 0x9c 0x40 0x00 0xc0 0x4f 0xa3 0x0a 0x3e
iid_I_AppDomain = GUID 0x05F696DC 0x2B29 0x3663 0xAD 0x8B 0xC4 0x38 0x9C 0xF2 0xA7 0x13
iid_I_Type = GUID 0xbca8b44d 0xaad6 0x3a86 0x8a 0xb7 0x03 0x34 0x9f 0x4f 0x2d 0xa2
iid_NULL = GUID 0x00000000 0x0000 0x0000 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
iid_Object = GUID 0x65074f7f 0x63c0 0x304e 0xaf 0x0a 0xd5 0x17 0x41 0xcb 0x4a 0x8d
clsid_IntPtr = GUID 0xa1cb710c 0x8d50 0x3181 0xbb 0x38 0x65 0xce 0x2e 0x98 0xf9 0xa6
iid_IUnknown = GUID 0x00000000 0x0000 0x0000 0xC0 0x00 0x00 0x00 0x00 0x00 0x00 0x46
guid_corlib = GUID 0xBED7F4EA 0x1A96 0x11d2 0x8F 0x08 0x00 0xA0 0xC9 0xA6 0x18 0x6D

--
-- CLS Context
--

clsctx_INPROC_SERVER = 0x1
clsctx_INPROC_HANDLER = 0x2
clsctx_LOCAL_SERVER = 0x4
clsctx_REMOTE_SERVER = 0x10

--clsctx_SERVER = (clsctx_INPROC_SERVER .|. clsctx_LOCAL_SERVER .|. clsctx_REMOTE_SERVER)
--clsctx_ALL = (clsctx_INPROC_HANDLER .|. clsctx_SERVER)


--
-- Variant Support
--
data Variant = Variant VarType Word64 deriving (Show, Eq)
type VarType = Word16

getVarType :: Variant -> VarType
getVarType (Variant vt vd) = vt

vt_EMPTY	= 0 :: VarType
vt_NULL	= 1 :: VarType
vt_I2	= 2 :: VarType
vt_I4	= 3 :: VarType
vt_R4	= 4 :: VarType
vt_R8	= 5 :: VarType
vt_CY	= 6 :: VarType
vt_DATE	= 7 :: VarType
vt_BSTR	= 8 :: VarType
vt_DISPATCH	= 9 :: VarType
vt_ERROR	= 10 :: VarType
vt_BOOL	= 11 :: VarType
vt_VARIANT	= 12 :: VarType
vt_UNKNOWN	= 13 :: VarType
vt_DECIMAL	= 14 :: VarType
vt_I1	= 16 :: VarType
vt_UI1	= 17 :: VarType
vt_UI2	= 18 :: VarType
vt_UI4	= 19 :: VarType
vt_I8	= 20 :: VarType
vt_UI8	= 21 :: VarType
vt_INT	= 22 :: VarType
vt_UINT	= 23 :: VarType
vt_VOID	= 24 :: VarType
vt_HRESULT	= 25 :: VarType
vt_PTR	= 26 :: VarType
vt_SAFEARRAY	= 27 :: VarType
vt_CARRAY	= 28 :: VarType
vt_USERDEFINED	= 29 :: VarType
vt_LPSTR	= 30 :: VarType
vt_LPWSTR	= 31 :: VarType
vt_RECORD	= 36 :: VarType
vt_INT_PTR	= 37 :: VarType
vt_UINT_PTR	= 38 :: VarType
vt_FILETIME	= 64 :: VarType
vt_BLOB	= 65 :: VarType
vt_STREAM	= 66 :: VarType
vt_STORAGE	= 67 :: VarType
vt_STREAMED_OBJECT	= 68 :: VarType
vt_STORED_OBJECT	= 69 :: VarType
vt_BLOB_OBJECT	= 70 :: VarType
vt_CF	= 71 :: VarType
vt_CLSID	= 72 :: VarType
vt_VERSIONED_STREAM	= 73 :: VarType
vt_BSTR_BLOB	= 0xfff :: VarType
vt_VECTOR	= 0x1000 :: VarType
vt_ARRAY	= 0x2000 :: VarType
vt_BYREF	= 0x4000 :: VarType
vt_RESERVED	= 0x8000 :: VarType
vt_ILLEGAL	= 0xffff :: VarType
vt_ILLEGALMASKED	= 0xfff :: VarType
vt_TYPEMASK	= 0xfff :: VarType

instance Storable Variant where
    sizeOf    _ = 16
    alignment _ = 8

    peek ptr = do
        a  <- peek $ plusPtr ptr 0 
        b  <- peek $ plusPtr ptr 8
        return $ Variant a b

    poke ptr (Variant a b) = do
        poke (plusPtr ptr 0) a
        poke (plusPtr ptr 2) (0 :: Word16)
        poke (plusPtr ptr 4) (0 :: Word16)
        poke (plusPtr ptr 6) (0 :: Word16)
        poke (plusPtr ptr 8) b


type VariantBool = Word16
varTrue = 0xFFFF :: VariantBool
varFalse = 0x0000 :: VariantBool

type BindingFlags = Int32

--
-- Safe Array Support
--
--
type SafeArrayPtr = Ptr ()

foreign import stdcall "SafeArrayCreateVector" safeArrayCreateVector :: VarType -> Int32 -> Word32 -> IO SafeArrayPtr
foreign import stdcall "SafeArrayAccessData"   safeArrayAccessData   :: SafeArrayPtr -> Ptr (Ptr a) -> IO HRESULT
foreign import stdcall "SafeArrayUnaccessData" safeArrayUnaccessData :: SafeArrayPtr -> IO HRESULT
foreign import stdcall "SafeArrayDestroy"      safeArrayDestroy      :: SafeArrayPtr -> IO HRESULT

--
-- BStr Support
--
type BStr = Ptr ()

sysAllocString :: String -> IO BStr
sysAllocString s = withCWStringLen s $ \(a,b)-> prim_SysAllocStringLen a $ fromIntegral b
foreign import stdcall "oleauto.h SysAllocStringLen" prim_SysAllocStringLen :: CWString -> Word32 -> IO BStr

sysFreeString :: BStr -> IO ()
sysFreeString = prim_SysFreeString
foreign import stdcall "oleauto.h SysFreeString" prim_SysFreeString :: BStr -> IO ()

withBStr :: String -> (BStr -> IO a) -> IO a
withBStr s = bracket (sysAllocString s) (sysFreeString)


--
-- Other Foreign Imports
-- 
type QueryInterface = ComObjectPtr -> REFIID -> Ptr ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeQueryInterface :: FunPtr QueryInterface -> QueryInterface

type GetRuntime = ComObjectPtr -> LPCWSTR -> REFIID -> Ptr ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeGetRuntime :: FunPtr GetRuntime -> GetRuntime

type IsLoadable = ComObjectPtr -> Ptr Bool -> IO HRESULT
foreign import stdcall "dynamic" makeIsLoadable :: FunPtr IsLoadable -> IsLoadable

type CLRCreateInstance = REFCLSID -> REFIID -> Ptr ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeCLRCreateInstance :: FunPtr CLRCreateInstance -> CLRCreateInstance

type GetInterface = ComObjectPtr -> REFCLSID -> REFIID -> Ptr ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeGetInterface :: FunPtr GetInterface -> GetInterface

type CorBindToRuntimeEx = LPWSTR -> LPWSTR -> DWORD -> REFCLSID -> REFIID -> Ptr ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeCorBindToRuntimeEx :: FunPtr CorBindToRuntimeEx -> CorBindToRuntimeEx

type RuntimeHostStart = ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeRuntimeHostStart :: FunPtr RuntimeHostStart -> RuntimeHostStart

type GetDefaultDomain = ComObjectPtr -> Ptr ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeGetDefaultDomain :: FunPtr GetDefaultDomain -> GetDefaultDomain

type AppDomainLoad = ComObjectPtr -> BStr -> Ptr ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeAppDomainLoad :: FunPtr AppDomainLoad -> AppDomainLoad

type AppDomainLoadBytes = ComObjectPtr -> SafeArrayPtr -> Ptr ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeAppDomainLoadBytes :: FunPtr AppDomainLoadBytes -> AppDomainLoadBytes

type AssemblyGetType = ComObjectPtr -> BStr -> Ptr ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeAssemblyGetType :: FunPtr AssemblyGetType -> AssemblyGetType

type TypeGetFullName = ComObjectPtr -> Ptr BStr -> IO HRESULT
foreign import stdcall "dynamic" makeTypeGetFullName :: FunPtr TypeGetFullName -> TypeGetFullName

type TypeGetAssembly = ComObjectPtr -> Ptr ComObjectPtr -> IO HRESULT
foreign import stdcall "dynamic" makeTypeGetAssembly :: FunPtr TypeGetAssembly -> TypeGetAssembly

type AssemblyCreateInstance = ComObjectPtr -> BStr -> VariantBool -> BindingFlags -> Ptr () -> SafeArrayPtr -> Ptr () -> SafeArrayPtr -> Ptr Variant -> IO HRESULT
foreign import stdcall "dynamic" makeAssemblyCreateInstance :: FunPtr AssemblyCreateInstance -> AssemblyCreateInstance

--type TypeInvokeMember = ComObjectPtr -> BStr -> Int32 ->  Ptr () -> Variant -> SafeArrayPtr -> Ptr Variant -> IO HRESULT
--foreign import stdcall "dynamic" makeTypeInvokeMember :: FunPtr TypeInvokeMember -> TypeInvokeMember

foreign import stdcall "marshal.c myTypeInvokeMember" myTypeInvokeMember :: FunPtr a -> ComObjectPtr -> BStr -> BindingFlags -> Ptr () -> Ptr Variant -> SafeArrayPtr -> Ptr Variant -> IO HRESULT

foreign import stdcall "marshal.c setFunctionPointers" setFunctionPointers :: Ptr a -> IO ()

-- Driver function wrappers --
foreign import stdcall "marshal.c freeHandle" freeHandle :: Int32 -> IO ()
foreign import stdcall "marshal.c invoke" invoke' :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO Int32
foreign import stdcall "marshal.c getValue" getValue :: Int32 -> Int32 -> Int32 -> IO Int32
foreign import stdcall "marshal.c boxInt16" boxInt16 :: Int16 -> IO Int32
foreign import stdcall "marshal.c boxString" boxString :: Int32 -> Int32 -> IO Int32
foreign import stdcall "marshal.c unBoxInt32" unBoxInt32 :: Int32 -> IO Int32
foreign import stdcall "marshal.c unBoxString" unBoxString :: Int32 -> Int32 -> IO ()
foreign import stdcall "marshall.c createInstance" createInstance :: Int32 -> Int32 -> Int32 -> IO Int32

foreign import stdcall "CoCreateInstance" coCreateInstance :: REFCLSID -> Ptr () -> DWORD -> REFIID -> Ptr (Ptr IntPtr) -> IO HRESULT

foreign import stdcall "LoadTypeLibEx" loadTypeLibEx :: LPCWSTR -> Int -> Ptr IntPtr -> IO HRESULT

lcidNeutral :: LCID
lcidNeutral = 0x7F

foreign import stdcall "LoadRegTypeLib" loadRegTypeLib :: REFLIBID -> WORD -> WORD -> LCID ->  Ptr IntPtr -> IO HRESULT

--
-- Internal Functions
--

checkPtr :: Ptr a -> IO b -> IO b
checkPtr p action = if p==nullPtr then error "Null Ptr" else action


bindToRuntimeOld :: HINSTANCE -> IO ComObjectPtr
bindToRuntimeOld mscoree = alloca $ \outHost-> do
  addr <- getProcAddress mscoree "CorBindToRuntimeEx"  
  checkPtr addr $ withCWString runtimeVersion $ \vers-> withCWString runtimeFlavor $ \flav-> do
    checkHR "CorBindToRuntimeEx Error" $ with clsid_CorRuntimeHost $ \clsid-> with iid_ICorRuntimeHost $ \iid-> do
      makeCorBindToRuntimeEx (castPtrToFunPtr addr) vers nullPtr 0 clsid iid outHost
    peek outHost

bindToRuntimeNew :: ComObjectPtr -> IO ComObjectPtr
bindToRuntimeNew metaHost = alloca $ \outRuntimeInfo-> do
  initRuntimeInfo metaHost outRuntimeInfo
  runtimeInfo <- peek outRuntimeInfo
  isLoadable <- runtimeInfoIsLoadable runtimeInfo
  if isLoadable then runtimeInfoGetHost runtimeInfo
  else error "Runtime not loadable"
  where runtimeInfoIsLoadable :: ComObjectPtr -> IO Bool
        runtimeInfoIsLoadable runInfo = alloca $ \pBool-> do
          func <- getFunction 10 makeIsLoadable runInfo
          hResult <- func runInfo pBool
          if (hResult/=hS_OK) then return False
          else peek pBool
        initRuntimeInfo :: ComObjectPtr -> Ptr ComObjectPtr -> IO HRESULT
        initRuntimeInfo metaHost runinfo = do
          func <- getFunction 3 makeGetRuntime metaHost
          withCWString runtimeVersion $ \vers-> do
            with iid_ICLRRuntimeInfo $ \refinfo-> do
              func metaHost vers refinfo runinfo
        runtimeInfoGetHost :: ComObjectPtr -> IO ComObjectPtr
        runtimeInfoGetHost runinfo = alloca $ \outHost-> do
          func <- getFunction 9 makeGetInterface runinfo
          checkHR "Error getting CLRRuntimeInfo" $ with clsid_CorRuntimeHost $ \clsid-> with iid_ICorRuntimeHost $ \iid-> do
            func runinfo clsid iid outHost
          peek outHost

hostStart :: ComObjectPtr -> IO HRESULT
hostStart host = getFunction 10 makeRuntimeHostStart host >>= \func-> func host

getDefaultDomain :: ComObjectPtr -> IO ComObjectPtr
getDefaultDomain host = alloca $ \outDomain-> do
  dom <- callGetDefaultDomain host
  checkPtr dom $ do
    func <- getFunction 0 makeQueryInterface dom
    checkHR "Error querying appdomain interface" $ with iid_I_AppDomain $ \ref-> do
      func dom ref outDomain
    peek outDomain
  where callGetDefaultDomain host = alloca $ \outDomain-> do
        func <- getFunction 13 makeGetDefaultDomain host
        checkHR "Error getting default domain" $ func host outDomain
        peek outDomain


initDriver :: ComObjectPtr -> IO ()
initDriver assem = withBStr "HCLR.Driver" $ \bstr-> alloca $ \outVar-> alloca $ \ptr-> allocaBytes (4*18) $ \bytesPtr-> do
  func <- getFunction 43 makeAssemblyCreateInstance assem
  ar1 <- safeArrayCreateVector vt_VARIANT 0 1
  checkHR "arg" $ safeArrayAccessData ar1 ptr
  ptr2 <- peek ptr
  poke ptr2 (Variant vt_INT $ fromIntegral $ ptrToIntPtr bytesPtr)
  safeArrayUnaccessData ar1
  ar2 <- safeArrayCreateVector vt_EMPTY 0 0
  checkHR "Could not init driver" $ func assem bstr varFalse 512 nullPtr ar1 nullPtr ar2 outVar
  setFunctionPointers bytesPtr
  var <- peek outVar
  return ()


loadDriver :: ComObjectPtr -> SafeArrayPtr -> IO ComObjectPtr
loadDriver dom vect = do
  alloca $ \outAssembly-> do
    func <- getFunction 45 makeAppDomainLoadBytes dom
    checkHR "Could not load driver from bytes" $ func dom vect outAssembly
    peek outAssembly



--
--
--

type WinHandle = Int32

data Object = Object {oid :: ForeignPtr WinHandle} | NullObject

withObject :: Object -> (Int32 -> IO b) -> IO b
withObject o f = case o of
  NullObject -> f 0
  (Object fp) -> withForeignPtr fp $ \p-> do
    p2 <- peek p
    f p2


runtimeVersion = "v4.0.30319" 
runtimeFlavor = "wks"
mscorlibString = case runtimeVersion of
  "v2.0.50727" -> "mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
  "v4.0.30319" -> "mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"
corlib_tlbVers :: (WORD, WORD)
corlib_tlbVers = case runtimeVersion of
  "v2.0.50727" -> (2, 0)
  "v4.0.30319" -> (2, 4)


stringLength x = do
  s1 <- box ("System.String, " `append` mscorlibString :: Text)
  s2 <- box ("Length" :: Text)
  withObject s1 $ \sp1->
    withObject s2 $ \sp2->
      withObject x $ \xp->
        getValue sp1 sp2 xp >>= unBoxInt32

class Box a where
  box :: a -> IO Object
  unBox :: Object -> IO a
  arg :: a -> (Int32 -> Int32 -> IO b) -> IO b
  arg x f = do
    Object fb <- box x
    withForeignPtr fb $ \b-> do
        f 1 (fromIntegral $ ptrToIntPtr b)



instance Box Text where
  box x = do
    p <- mallocBytes (lengthWord16 x * 2) 
    unsafeCopyToPtr x p
    boxed <- boxString (fromIntegral $ ptrToIntPtr p) (fromIntegral (lengthWord16 x ))
    fp <- mallocForeignPtr
    withForeignPtr fp (\p2-> poke p2 boxed)
    FC.addForeignPtrFinalizer fp (freeHandle boxed >> free p)
    return $ Object fp
  unBox o@(Object x) = do
    len <- stringLength o
    allocaBytes (fromIntegral $ len * 2) $ \p2-> do
      withObject o $ \y-> do
        unBoxString y (fromIntegral $ ptrToIntPtr p2)
        fromPtr p2 (fromIntegral len)

instance Box () where
  box () = return NullObject
  unBox obj = return ()
  arg () f = withArray [0::Int] (\p-> f 0 $ fromIntegral $ ptrToIntPtr $ p)

instance Box Object where
  box x = return x
  unBox x = return x



initCLR :: IO ()
initCLR = alloca $ \outMetaHost-> do
  checkHR "Could not initialize COM" $ coInitializeEx nullPtr coInit_MultiThreaded
  mscoree <- loadLibrary "mscoree.dll"
  checkPtr mscoree $ do
    addr <- getProcAddress mscoree "CLRCreateInstance"
    host <- if (addr==nullPtr) then bindToRuntimeOld mscoree
    else do
      hr <- with clsid_CLRMetaHost $ \clsid-> with iid_ICLRMetaHost $ \iid-> do
        makeCLRCreateInstance (castPtrToFunPtr addr) clsid iid outMetaHost
      if (hr/=hS_OK)
      then bindToRuntimeOld mscoree
      else peek outMetaHost >>= bindToRuntimeNew
    checkPtr host $ do
      checkHR "Cannot start host" $ hostStart host
      with guid_corlib $ \refGuidCorLib-> alloca $ \p-> do
        dom <- getDefaultDomain host
        let (major, minor) = corlib_tlbVers
        checkHR "Could not load typelib" $ loadRegTypeLib refGuidCorLib major minor lcidNeutral p
        alloca $ \ptr-> unsafeUseAsCStringLen driverData $ \(driverPtr, driverLength)-> do
          vect <- safeArrayCreateVector vt_UI1 0 (fromIntegral $ driverLength)
          checkHR "Could not access data to load driver" $ safeArrayAccessData vect ptr
          ptr2 <- peek ptr
          copyBytes ptr2 driverPtr driverLength
          checkHR "Could not unaccess data to load driver" $ safeArrayUnaccessData vect
          loadDriver dom vect >>= initDriver
          return ()

withRuntime :: IO a -> IO a
withRuntime x = initCLR >> x

invokeMethod :: Box a => Assembly -> Text -> Text -> Object -> a -> IO Object
invokeMethod (Assembly assem) typ m target args = arg args $ \c-> \p-> do
  typeString <- box $ typ `append` ", " `append` (pack assem)
  methodString <- box m
  withObject typeString $ \typeString'->
    withObject methodString $ \methodString'->
      withObject target $ \target'-> do
        oid <- invoke' typeString' methodString' target' c p
        fp <- mallocForeignPtr
        withForeignPtr fp (\p-> poke p oid)
        FC.addForeignPtrFinalizer fp (freeHandle oid)
        return $ Object fp


assemHasType :: Assembly -> CLRType -> IO Bool
assemHasType (Assembly a) (CLRType t) = return True

objectNew :: Box a => Assembly -> String -> a -> IO Object
objectNew (Assembly assem) typ args = arg args $ \c-> \p-> do
  typeString <- box $ (pack typ) `append` ", " `append` (pack assem)
  withObject typeString $ \typeString'-> do
    oid <- createInstance typeString' c p
    fp <- mallocForeignPtr
    withForeignPtr fp (\p-> poke p oid)
    FC.addForeignPtrFinalizer fp (freeHandle oid)
    return $ Object fp


