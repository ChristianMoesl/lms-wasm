(func $Snippet (export "Snippet") (param $x0 f32)
  i32.const 6
  i32.const 0
  local.get $x0
  f32.store
  call $println
)
;; output:
2.4000000953674316
