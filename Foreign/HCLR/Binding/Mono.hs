{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances, DoAndIfThenElse, UndecidableInstances, IncoherentInstances #-}

module Foreign.HCLR.Binding.Mono (
  module Foreign.HCLR.Binding.Mono
) where

import Foreign.HCLR.Binding.Common
import Foreign.HCLR.Binding.Mono.Internal
import Control.Monad (replicateM, zipWithM, filterM)
import Control.Exception
import Data.List
import Data.Maybe (fromJust)
import Foreign
import Foreign.C
import qualified Foreign.HCLR.Ast as Ast
import System.Environment
import qualified Data.Text as T
import Data.Text.Foreign
import qualified Foreign.Concurrent as FC



withRuntime :: IO b -> IO b
withRuntime x = bracket monoInit mono_jit_cleanup (\z-> x)

imageGetType :: Image -> Ast.CLRType -> IO RuntimeType
imageGetType  image (Ast.CLRType t) = do
  let ns = concat $ intersperse "." $ init t
      typ = last t
  cls <- withCString ns (\nsc-> withCString typ (\typc-> mono_class_from_name image nsc typc))
  return cls



withObject :: Object -> (Word32 -> IO a) -> IO a
withObject (Object fp) f = withForeignPtr fp $ \p-> do
  x <- peek p
  f x

data Object = NullObject | Object {oid :: ForeignPtr ObjectHandle}

objectFromPtr :: MonoObjectPtr -> IO Object
objectFromPtr oP = do
    handle <- mono_gchandle_new oP gboolTrue
    fp <- mallocForeignPtr
    withForeignPtr fp (\p-> poke p handle)
    FC.addForeignPtrFinalizer fp (mono_gchandle_free handle)
    return (Object fp)

  

objectGetHandle :: Object -> IO ObjectHandle
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

instance Box [Char] where
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


invokeMethod :: Marshal a => String -> String -> String -> Object -> a -> IO Object  --method name is parsed with argument types eg WriteLine(String)
invokeMethod assem t mth target args = do
  image <- assemblyImage assem
  if (image == nullPtr) then
    error ("Image")
  else
    invokeMethodImage image t mth target args

invokeMethodImage :: Marshal a => MonoImagePtr -> String -> String -> Object -> a -> IO Object  --method name is parsed with argument types eg WriteLine(String)
invokeMethodImage image t mth target args = do
  let funName = (t ++ ":" ++ mth )
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

invokeMethodCorlib :: Marshal a => String -> String -> Object -> a -> IO Object  --method name is parsed with argument types eg WriteLine(String)
invokeMethodCorlib t mth target args = do
  corlib <- mono_get_corlib
  invokeMethodImage corlib t mth target args


monoGetClass :: String -> String -> String -> IO MonoClassPtr
monoGetClass assem ns n = assemblyImage assem >>= \image-> withCString ns (\nsC-> withCString n (\nC-> mono_class_from_name image nsC nC) ) 

monoImageGetClass :: MonoImagePtr -> String -> String -> IO MonoClassPtr
monoImageGetClass image ns n = withCString ns (\nsC-> withCString n (\nC-> mono_class_from_name image nsC nC) ) 

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

monoFindClass' :: MonoImagePtr -> Ast.CLRType -> IO MonoClassPtr
monoFindClass' image (Ast.CLRType typ) = do
  ptrs <- mapM (uncurry (monoImageGetClass image)) $ nsPermute typ
  found <- return $ find (/= nullPtr) ptrs
  case found of
    Just x -> return x
    Nothing -> return nullPtr



assemblyGetClass :: Ast.Assembly -> Ast.CLRType -> IO MonoClassPtr
assemblyGetClass (Ast.Assembly a) (Ast.CLRType t) = monoFindClass a t

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = 
 case break (==x) xs of
   (ys,[])   -> [ys]
   (ys,_:zs) -> ys:splitOn x zs

objectNewRuntime :: Marshal a => RuntimeType -> a -> IO Object
objectNewRuntime typ args = do
  o <- monoObjectNew typ
  image <- typeGetImage typ
  t <- typeGetName typ >>= \name-> typeGetNS typ >>= \ns-> return $ ns ++ "." ++ name 
  invokeMethodImage image t ".ctor()" o args
  return o

newObject :: Marshal a => String -> String -> a -> IO Object
newObject assem typ args = do
  cls <- monoFindClass assem (splitOn '.' typ)
  objectNewRuntime cls args


monoClassName :: MonoClassPtr -> IO String
monoClassName x = mono_class_get_name x >>= peekCString


monoClassAllSuper :: MonoClassPtr -> IO [MonoClassPtr]
monoClassAllSuper c = monoClassAllSuper' [c]
  where
    monoClassAllSuper' :: [MonoClassPtr] -> IO [MonoClassPtr]
    monoClassAllSuper' (x:xs) = do
      s <- monoClassName x
      if s=="Object" then
        return (x:xs)
      else mono_class_get_parent x >>= \parent-> monoClassAllSuper' (parent:x:xs)

--is t1 equal to or a subtype of t2
isType :: MonoClassPtr -> MonoClassPtr -> IO Bool 
isType t1 t2 = do
  c <- monoClassAllSuper t1
  return $ t2 `elem` c

type Sig = [RuntimeType]

isSigCompat :: Sig -> Sig -> IO Bool
isSigCompat sig1 sig2 = do
  if null sig1 && null sig2 then
    return True
  else
    zipWithM isType sig1 sig2 >>= \l-> return $ foldl1 (&&) l

isSigSame :: Sig -> Sig -> IO Bool
isSigSame sig1 sig2 = zipWithM (\t1-> \t2-> return $ t1 == t2) sig1 sig2 >>= \l-> return $ foldl1 (&&) l

monoClassGetMethods :: MonoClassPtr -> IO [MonoMethodPtr]
monoClassGetMethods cls = with (nullPtr::Ptr Int) $ \iter-> do
  i <- mono_class_num_methods cls
  replicateM i $ mono_class_get_methods cls iter 

monoMethodGetParamClasses :: MonoMethodPtr -> IO [MonoClassPtr]
monoMethodGetParamClasses meth = with (nullPtr :: Ptr Int) $ \iter-> do
  sig <- mono_method_signature meth
  i <- mono_signature_get_param_count sig
  replicateM i $ mono_signature_get_params sig iter >>= mono_class_from_mono_type

monoMethodGetReturnClass :: MonoMethodPtr -> IO MonoClassPtr
monoMethodGetReturnClass meth = do
  sig <- mono_method_signature meth
  ret <- mono_signature_get_return_type sig
  mono_class_from_mono_type ret


type RuntimeType = MonoClassPtr

expGetRuntimeType :: Ast.Exp -> Image -> IO RuntimeType
expGetRuntimeType e image = do
  cls <- monoFindClass' image $ Ast.expGetType e
  return cls


type Image = MonoImagePtr

imageGetName :: Image -> IO String
imageGetName image = withCString " " $ \s-> do
  name <- mono_assembly_name_new s
  mono_assembly_fill_assembly_name image name
  cstring <- mono_stringify_assembly_name name
  peekCString cstring 

stringType :: IO RuntimeType
stringType = mono_get_string_class

type Method = MonoMethodPtr

getMethodName :: Method -> IO String
getMethodName mth = mono_method_get_name mth >>= peekCString

methodGetFullName mth withSig = mono_method_full_name mth withSig >>= peekCString

typeGetMethods :: RuntimeType -> String -> IO [Method]
typeGetMethods typ name = do
  methods <- monoClassGetMethods typ
  filterM (\mth-> getMethodName mth >>= \n-> return $ n == name) methods

methodGetSig :: Method -> IO Sig
methodGetSig mth = do
  name <- getMethodName mth
  monoMethodGetParamClasses mth

methodGetSigString :: Method -> IO String
methodGetSigString mth = do
  fullName <- methodGetFullName mth gboolTrue
  let i = fromJust $ elemIndex '(' fullName
  return $ drop i fullName
  

methodSigIs :: Method -> Sig -> IO Bool
methodSigIs mth sig = do
  sig2 <- methodGetSig mth
  isSigSame sig sig2

typeGetImage :: RuntimeType -> IO Image
typeGetImage = mono_class_get_image

methodGetReturnType :: Method -> IO RuntimeType
methodGetReturnType = monoMethodGetReturnClass

typeGetName typ = mono_class_get_name typ  >>= peekCString

typeGetNS typ = mono_class_get_namespace typ >>= peekCString 
