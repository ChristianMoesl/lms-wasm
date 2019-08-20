(func $Snippet (export "Snippet") (param $x1 i32)
  i32.const 20
  call $printf
  local.get $x1
  i32.const 1
  i32.gt_s
  if (result i32)
    i32.const 1
  else
    i32.const 10
    call $printf
    local.get $x1
    i32.const 2
    i32.gt_s
  end
  call $printf
)
;; output:
20
10
0
20
1
20
1
