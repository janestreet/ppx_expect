#include <fnmatch.h>

#include <caml/mlvalues.h>
#include <caml/fail.h>

/* We do our own escape handling */
#define FNMATCH_FLAGS FNM_NOESCAPE

CAMLprim value ppx_expect_matches_glob(value v_glob, value v_str)
{
  char *glob = String_val(v_glob);
  char *str = String_val(v_str);
  int ret = fnmatch(glob, str, FNMATCH_FLAGS);
  switch (ret) {
  case 0 : return Val_true;
  case FNM_NOMATCH : return Val_false;
  default : caml_failwith("fnmatch");
  }
}
