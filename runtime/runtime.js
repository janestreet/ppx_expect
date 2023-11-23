//Provides: ppx_expect_runtime_saved_stdout
var ppx_expect_runtime_saved_stdout
//Provides: ppx_expect_runtime_saved_stderr
var ppx_expect_runtime_saved_stderr

//Provides: ppx_expect_runtime_before_test
//Requires: caml_ml_channels
//Requires: ppx_expect_runtime_saved_stderr, ppx_expect_runtime_saved_stdout
function ppx_expect_runtime_before_test (voutput, vstdout, vstderr){
  ppx_expect_runtime_saved_stderr = caml_ml_channels[vstderr];
  ppx_expect_runtime_saved_stdout = caml_ml_channels[vstdout];
  var output = caml_ml_channels[voutput];
  caml_ml_channels[vstdout] = output;
  caml_ml_channels[vstderr] = output;
  return 0;
}

//Provides: ppx_expect_runtime_after_test
//Requires: caml_ml_channels
//Requires: ppx_expect_runtime_saved_stderr, ppx_expect_runtime_saved_stdout
function ppx_expect_runtime_after_test (vstdout, vstderr){
  caml_ml_channels[vstdout] = ppx_expect_runtime_saved_stdout;
  caml_ml_channels[vstderr] = ppx_expect_runtime_saved_stderr;
  return 0;
}

//Provides: ppx_expect_runtime_out_channel_position
//Requires: caml_ml_channels
function ppx_expect_runtime_out_channel_position(chan){
  var info = caml_ml_channels[chan];
  return info.offset
}

//Provides: ppx_expect_runtime_flush_stubs_streams
function ppx_expect_runtime_flush_stubs_streams(vunit){
  return 0
}
