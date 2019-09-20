(func $Snippet (export "Snippet") (param $x0 f64)
  i32.const 7
  i32.const 0
  local.get $x0
  f64.store
  call $println
)
;; output:
2.4000000953674316
