(func $Snippet (export "Snippet") (param $x0 i32)
  local.get $x0
  i32.const 1
  i32.gt_s
  if (result i32)
    i32.const 10
    call $printf
    local.get $x0
    i32.const 2
    i32.gt_s
  else
    i32.const 0
  end
  call $printf
)
;; output:
0
10
0
10
1
