let package_name = "ppx_expect"

let sections =
  [ ("lib",
    [ ("built_lib_expect_test_collector", None)
    ; ("built_lib_expect_test_common", None)
    ; ("built_lib_expect_test_config", None)
    ; ("built_lib_expect_test_matcher", None)
    ; ("built_lib_ppx_expect", None)
    ; ("built_lib_ppx_expect_payload", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_ppx", Some "../lib/ppx_expect/ppx")
    ],
    [])
  ]
