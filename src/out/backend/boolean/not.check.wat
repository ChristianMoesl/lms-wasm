(func $Snippet (export "Snippet") (param $x1 i32)
  local.get $x1
  i32.const 1
  i32.gt_s
  i32.eqz
  call $printf
)
;; output:
1
0
