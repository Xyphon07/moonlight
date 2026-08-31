#include "moonlight_triangulation.h"
#include "HsFFI.h"

#if defined(_WIN32)
#include <windows.h>

static INIT_ONCE moonlight_runtime_once = INIT_ONCE_STATIC_INIT;

static BOOL CALLBACK moonlight_initialize_runtime(PINIT_ONCE once, PVOID parameter, PVOID *context) {
  int argc = 1;
  char program_name[] = "moonlight-triangulation";
  char *argv[] = {program_name, NULL};
  char **argv_pointer = argv;
  (void)once;
  (void)parameter;
  (void)context;
  hs_init(&argc, &argv_pointer);
  return TRUE;
}

ML_API ml_status ml_runtime_initialize(void) {
  return InitOnceExecuteOnce(&moonlight_runtime_once, moonlight_initialize_runtime, NULL, NULL)
    ? ML_STATUS_OK
    : ML_STATUS_RUNTIME_FAILURE;
}
#else
#include <pthread.h>

static pthread_once_t moonlight_runtime_once = PTHREAD_ONCE_INIT;

static void moonlight_initialize_runtime(void) {
  int argc = 1;
  char program_name[] = "moonlight-triangulation";
  char *argv[] = {program_name, NULL};
  char **argv_pointer = argv;
  hs_init(&argc, &argv_pointer);
}

ML_API ml_status ml_runtime_initialize(void) {
  return pthread_once(&moonlight_runtime_once, moonlight_initialize_runtime) == 0
    ? ML_STATUS_OK
    : ML_STATUS_RUNTIME_FAILURE;
}
#endif

ML_API uint32_t ml_abi_version(void) {
  return ML_ABI_VERSION;
}
