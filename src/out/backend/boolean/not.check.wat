(func $Snippet (export "Snippet") (param $x0 i32)
  i32.const 32
  i32.const 1
  i32.const 0
  local.get $x0
  i32.const 1
  i32.gt_s
  i32.eqz
  i32.store
  call $printf
)
;; output:
1
0
