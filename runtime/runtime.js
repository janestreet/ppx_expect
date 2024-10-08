//Provides: ppx_expect_runtime_saved_stdout
var ppx_expect_runtime_saved_stdout
//Provides: ppx_expect_runtime_saved_stderr
var ppx_expect_runtime_saved_stderr

//Provides: ppx_expect_runtime_before_test
//Requires: caml_ml_channel_redirect
//Requires: ppx_expect_runtime_saved_stderr, ppx_expect_runtime_saved_stdout
function ppx_expect_runtime_before_test (voutput, vstdout, vstderr){
  ppx_expect_runtime_saved_stderr = caml_ml_channel_redirect(vstderr, voutput);
  ppx_expect_runtime_saved_stdout = caml_ml_channel_redirect(vstdout, voutput);
  return 0;
}

//Provides: ppx_expect_runtime_after_test
//Requires: caml_ml_channel_restore
//Requires: ppx_expect_runtime_saved_stderr, ppx_expect_runtime_saved_stdout
function ppx_expect_runtime_after_test (vstdout, vstderr){
  caml_ml_channel_restore(vstdout,ppx_expect_runtime_saved_stdout);
  caml_ml_channel_restore(vstderr,ppx_expect_runtime_saved_stderr);
  return 0;
}

//Provides: ppx_expect_runtime_out_channel_position
//Requires: caml_ml_channel_get
function ppx_expect_runtime_out_channel_position(chan){
  var info = caml_ml_channel_get(chan);
  return info.offset
}

//Provides: ppx_expect_runtime_flush_stubs_streams
function ppx_expect_runtime_flush_stubs_streams(vunit){
  return 0
}
