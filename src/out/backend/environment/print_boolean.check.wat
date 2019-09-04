(func $Snippet (export "Snippet") (param $x0 i32)
  i32.const 1
  i32.const 0
  local.get $x0
  i32.store
  call $println1
)
;; output:
1
0
