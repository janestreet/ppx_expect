//Provides: expect_test_collector_saved_stdout
var expect_test_collector_saved_stdout 
//Provides: expect_test_collector_saved_stderr
var expect_test_collector_saved_stderr

//Provides: expect_test_collector_before_test
//Requires: caml_global_data, caml_ml_channel_redirect
//Requires: expect_test_collector_saved_stderr, expect_test_collector_saved_stdout
function expect_test_collector_before_test (voutput, vstdout, vstderr){
  expect_test_collector_saved_stderr = caml_ml_channel_redirect(vstderr,voutput);
  expect_test_collector_saved_stdout = caml_ml_channel_redirect(vstdout,voutput);
  return 0;
}

//Provides: expect_test_collector_after_test
//Requires: caml_global_data, caml_ml_channel_restore
//Requires: expect_test_collector_saved_stderr, expect_test_collector_saved_stdout
function expect_test_collector_after_test (vstdout, vstderr){
  caml_ml_channel_restore(vstdout,expect_test_collector_saved_stdout);
  caml_ml_channel_restore(vstderr,expect_test_collector_saved_stderr);
  return 0;
}

//Provides:caml_out_channel_pos_fd
//Requires: caml_global_data, caml_ml_channel_get
function caml_out_channel_pos_fd(chanid){
  var chan = caml_ml_channel_get(chanid);
  return chan.offset
}
