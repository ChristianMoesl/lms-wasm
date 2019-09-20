(func $Snippet (export "Snippet") (param $x0 i32)
  i32.const 5
  i32.const 0
  local.get $x0
  i64.extend_s/i32
  i64.store
  call $println
)
;; output:
1
