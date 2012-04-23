#ifdef __cplusplus
extern "C" {
#endif

#if defined _WIN32

#include <windows.h>
#include <stdint.h>


HRESULT WINAPI myTypeInvokeMember(HRESULT(*func)(void*, void*,int,void*,VARIANT,void*,void*), void* obj, void* name, int invokeAttr, void * binder, VARIANT * target , void * args , void * pRetVal)
{
	return (*func)(obj, name, invokeAttr, NULL, *target, args, pRetVal);
}

static void* funcPointers[18];

void WINAPI setFunctionPointers(void** fp)
{
  int i;
  for(i=0; i<18; i++)
  {
    funcPointers[i] = fp[i];
  }
}

typedef void (*freeHandle_t)(int32_t);
void WINAPI freeHandle(int32_t oid)
{
  freeHandle_t func = (freeHandle_t)funcPointers[0];
  func(oid);
}

typedef int32_t (*invoke_t)(int32_t, int32_t, int32_t, int32_t, int32_t);           
int32_t WINAPI invoke(int32_t iTypeName, int32_t iMethodName, int32_t target, int32_t argCount, int32_t argP)
{
  invoke_t func = (invoke_t)funcPointers[1];
  return func(iTypeName, iMethodName, target, argCount, argP);
}

typedef int32_t (*getValue_t)(int32_t, int32_t, int32_t);
int32_t WINAPI getValue(int32_t iTypeName, int32_t iPropertyName, int32_t target)
{
  getValue_t func = (getValue_t)funcPointers[2];
  return func(iTypeName, iPropertyName, target);
}

typedef int32_t (*createInstance_t)(int32_t, int32_t, int32_t);
int32_t WINAPI createInstance(int32_t iTypeName, int32_t argCount, int32_t argP)
{
  createInstance_t func = (createInstance_t)funcPointers[3];
  return func(iTypeName, argCount, argP);
}

typedef int32_t (*boxInt16_t)(int16_t);
int32_t WINAPI boxInt16(int16_t a)
{
  boxInt16_t func = (boxInt16_t)funcPointers[4];
  return func(a);
}

typedef int32_t (*boxInt32_t)(int32_t);
int32_t WINAPI boxInt32(int32_t a)
{
  boxInt32_t func = (boxInt32_t)funcPointers[5];
  return func(a);
}

typedef int32_t (*boxInt64_t)(int64_t);
int32_t WINAPI boxInt64(int64_t a)
{
  boxInt64_t func = (boxInt64_t)funcPointers[6];
  return func(a);
}

typedef int32_t (*boxUInt16_t)(uint16_t);
int32_t WINAPI boxUInt16(uint16_t a)
{
  boxUInt16_t func = (boxUInt16_t)funcPointers[7];
  return func(a);
}

typedef int32_t (*boxUInt32_t)(uint32_t);
int32_t WINAPI boxUInt32(uint32_t a)
{
  boxUInt32_t func = (boxUInt32_t)funcPointers[8];
  return func(a);
}

typedef int32_t (*boxUInt64_t)(uint64_t);
int32_t WINAPI boxUInt64(uint64_t a)
{
  boxUInt64_t func = (boxUInt64_t)funcPointers[9];
  return func(a);
}

typedef int32_t (*boxString_t)(int32_t, int32_t);
int32_t WINAPI boxString(int32_t ptr, int32_t length)
{
  boxString_t func = (boxString_t)funcPointers[10];
  return func(ptr,length);
}

typedef int16_t (*unBoxInt16_t)(int32_t);
int16_t WINAPI unBoxInt16(int32_t a)
{
  unBoxInt16_t func = (unBoxInt16_t)funcPointers[11];
  return func(a);
}

typedef int32_t (*unBoxInt32_t)(int32_t);
int32_t WINAPI unBoxInt32(int32_t a)
{
  unBoxInt32_t func = (unBoxInt32_t)funcPointers[12];
  return func(a);
}

typedef int64_t (*unBoxInt64_t)(int32_t);
int64_t WINAPI unBoxInt64(int32_t a)
{
  unBoxInt64_t func = (unBoxInt64_t)funcPointers[13];
  return func(a);
}

typedef uint16_t (*unBoxUInt16_t)(int32_t);
uint16_t WINAPI unBoxUInt16(int32_t a)
{
  unBoxUInt16_t func = (unBoxUInt16_t)funcPointers[14];
  return func(a);
}

typedef uint32_t (*unBoxUInt32_t)(int32_t);
uint32_t WINAPI unBoxUInt32(int32_t a)
{
  unBoxUInt32_t func = (unBoxUInt32_t)funcPointers[15];
  return func(a);
}

typedef uint64_t (*unBoxUInt64_t)(int32_t);
uint64_t WINAPI unBoxUInt64(int32_t a)
{
  unBoxUInt64_t func = (unBoxUInt64_t)funcPointers[16];
  return func(a);
}

typedef void (*unBoxString_t)(int32_t, int32_t);
void WINAPI unBoxString(int32_t a, int32_t ptr)
{
  unBoxString_t func = (unBoxString_t)funcPointers[17];
  func(a, ptr);
}

#else

#include <stdint.h>
#include <mono/metadata/appdomain.h>

uint32_t boxString(uint32_t ptr, int32_t length)
{
  return mono_gchandle_new(mono_string_new_utf16(mono_domain_get(), (mono_unichar2 *)ptr, length), 0);
}

uint16_t * getString(uint32_t a)
{
  return (uint16_t *)mono_string_chars((MonoString *)mono_gchandle_get_target(a));
}

int32_t stringLength(uint32_t a)
{
  return mono_string_length((MonoString *)mono_gchandle_get_target(a));
}

#endif

#ifdef __cplusplus
}
#endif

