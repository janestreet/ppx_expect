let package_name = "ppx_expect"

let sections =
  [ ("lib",
    [ ("built_lib_expect_test_collector", None)
    ; ("built_lib_expect_test_evaluator_lib", None)
    ; ("built_lib_ppx_expect", None)
    ],
    [ ("META", None)
    ])
  ; ("libexec",
    [ ("built_exec_ppx", Some "ppx")
    ],
    [])
  ]
