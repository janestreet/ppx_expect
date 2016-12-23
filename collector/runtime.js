//Provides: caml_out_channel_swap_fd
//Requires: caml_global_data, caml_ml_channels
function caml_out_channel_swap_fd (oldchan,newchan){
  var old_ = caml_ml_channels[oldchan];
  var new_ = caml_ml_channels[newchan];
  caml_ml_channels[oldchan] = new_;
  caml_ml_channels[newchan] = old_;
  return 0;
}

//Provides:caml_out_channel_pos_fd
//Requires: caml_global_data, caml_ml_channels
function caml_out_channel_pos_fd(chan){
  var info = caml_ml_channels[chan];
  return info.offset
}
