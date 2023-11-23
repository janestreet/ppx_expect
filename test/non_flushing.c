#include <caml/alloc.h>

CAMLprim value stdout_from_stubs_but_dont_flush(value vunit) {
  printf("hello from c");
  return vunit;
}

CAMLprim value stderr_from_stubs_but_dont_flush(value vunit) {
  fprintf(stderr, "error from c");
  return vunit;
}
