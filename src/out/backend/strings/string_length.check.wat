(func $Snippet (export "Snippet") (param $x0 i32)
  i32.const 4
  i32.const 0
  local.get $x0
  call $stringLength
  i32.store
  call $println
)
;; output:
12
