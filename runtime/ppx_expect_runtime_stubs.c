#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif

/* #include <caml/io.h> */

/* The definition of channel should be kept in sync with upstream ocaml  */
/* Start of duplicated code from caml/io.h */
#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 65536
#endif

#if defined(_WIN32)
typedef __int64 file_offset;
#elif defined(HAS_OFF_T)
#include <sys/types.h>
typedef off_t file_offset;
#else
typedef long file_offset;
#endif

struct channel {
  int fd;                      /* Unix file descriptor */
  file_offset offset;          /* Absolute position of fd in the file */
  char *end;                   /* Physical end of the buffer */
  char *curr;                  /* Current position in the buffer */
  char *max;                   /* Logical end of the buffer (for input) */
  void *mutex;                 /* Placeholder for mutex (for systhreads) */
  struct channel *next, *prev; /* Double chaining of channels (flush_all) */
  int revealed;                /* For Cash only */
  int old_revealed;            /* For Cash only */
  int refcount;                /* For flush_all and for Cash */
  int flags;                   /* Bitfield */
  char buff[IO_BUFFER_SIZE];   /* The buffer itself */
  char *name;                  /* Optional name (to report fd leaks) */
};

#define Channel(v) (*((struct channel **)(Data_custom_val(v))))

/* End of duplicated code from caml/io.h */

/* Start of duplicated code from caml/sys.h */
#define NO_ARG Val_int(0)
CAMLextern void caml_sys_error(value);
/* End of duplicated code from caml/sys.h */

static int ppx_expect_runtime_saved_stdout;
static int ppx_expect_runtime_saved_stderr;

#define SYSCALL(var, expr)                                                               \
  {                                                                                      \
    var = expr;                                                                          \
    if (var == -1)                                                                       \
      caml_sys_error(NO_ARG);                                                            \
  }

CAMLprim value ppx_expect_runtime_before_test(value voutput, value vstdout,
                                              value vstderr) {
  struct channel *output = Channel(voutput);
  struct channel *cstdout = Channel(vstdout);
  struct channel *cstderr = Channel(vstderr);
  int fd, ret;
  SYSCALL(fd, dup(cstdout->fd))
  ppx_expect_runtime_saved_stdout = fd;
  SYSCALL(fd, dup(cstderr->fd))
  ppx_expect_runtime_saved_stderr = fd;
  SYSCALL(ret, dup2(output->fd, cstdout->fd))
  SYSCALL(ret, dup2(output->fd, cstderr->fd))
  return Val_unit;
}

CAMLprim value ppx_expect_runtime_after_test(value vstdout, value vstderr) {
  struct channel *cstdout = Channel(vstdout);
  struct channel *cstderr = Channel(vstderr);
  int ret;
  SYSCALL(ret, dup2(ppx_expect_runtime_saved_stdout, cstdout->fd))
  SYSCALL(ret, dup2(ppx_expect_runtime_saved_stderr, cstderr->fd))
  SYSCALL(ret, close(ppx_expect_runtime_saved_stdout))
  SYSCALL(ret, close(ppx_expect_runtime_saved_stderr))
  return Val_unit;
}

CAMLprim value ppx_expect_runtime_out_channel_position(value vchan) {
  struct channel *chan = Channel(vchan);
  file_offset ret;
  caml_enter_blocking_section();
  ret = lseek(chan->fd, 0, SEEK_CUR);
  caml_leave_blocking_section();
  if (ret == -1)
    caml_sys_error(NO_ARG);
  if (ret > Max_long)
    caml_failwith("ppx_expect_runtime_out_channel_position: overflow");
  return Val_long(ret);
}

CAMLprim value ppx_expect_runtime_flush_stubs_streams(value vunit) {
  fflush(stdout);
  fflush(stderr);
  return vunit;
}
