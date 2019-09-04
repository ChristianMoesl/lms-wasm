(func $Snippet (export "Snippet") (param $x0 i32)
  i32.const 3
  i32.const 0
  local.get $x0
  i32.store8
  call $println1
)
;; output:
Hello World!
