(module
   (import "env" "caml_ml_get_channel_fd"
      (func $caml_ml_get_channel_fd (param (ref eq)) (result i32)))
   (import "env" "caml_ml_set_channel_fd"
      (func $caml_ml_set_channel_fd (param (ref eq)) (param i32)))
   (import "env" "caml_ml_get_channel_offset"
      (func $caml_ml_get_channel_offset (param (ref eq)) (result i64)))
   (@if $portable-int
   (@then
      (import "env" "val_portable_int"
         (func $val_portable_int (param i64) (result (ref eq))))
   ))

   (global $saved_stdout (mut i32) (i32.const 0))
   (global $saved_stderr (mut i32) (i32.const 0))

   (func (export "ppx_expect_runtime_before_test")
      (param $outputw (ref eq)) (param $outputr_opt (ref eq))
      (param $stdout (ref eq)) (param $stderr (ref eq))
      (result (ref eq))
      (local $fd i32)
      (global.set $saved_stdout
         (call $caml_ml_get_channel_fd (local.get $stdout)))
      (global.set $saved_stderr
         (call $caml_ml_get_channel_fd (local.get $stderr)))
      (local.set $fd (call $caml_ml_get_channel_fd (local.get $outputw)))
      (call $caml_ml_set_channel_fd (local.get $stdout) (local.get $fd))
      (call $caml_ml_set_channel_fd (local.get $stderr) (local.get $fd))
      (ref.i31 (i32.const 0)))

   (func (export "ppx_expect_runtime_after_test")
      (param $stdout (ref eq)) (param $stderr (ref eq)) (result (ref eq))
      (call $caml_ml_set_channel_fd (local.get $stdout)
         (global.get $saved_stdout))
      (call $caml_ml_set_channel_fd (local.get $stderr)
         (global.get $saved_stderr))
      (ref.i31 (i32.const 0)))

   (func (export "ppx_expect_runtime_out_channel_position")
      (param (ref eq)) (result (ref eq))
      (@if $portable-int
      (@then
         (call $val_portable_int
            (call $caml_ml_get_channel_offset (local.get 0))))
      (@else
      (ref.i31
         (i32.wrap_i64
            (call $caml_ml_get_channel_offset (local.get 0))))
      )))

   (func (export "ppx_expect_runtime_flush_stubs_streams")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
