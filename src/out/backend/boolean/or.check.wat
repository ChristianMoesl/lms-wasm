(func $Snippet (export "Snippet") (param $x0 i32)
  i32.const 32
  i32.const 4
  i32.const 0
  i32.const 20
  i32.store
  call $printf1
  i32.const 32
  i32.const 1
  i32.const 0
  local.get $x0
  i32.const 1
  i32.gt_s
  if (result i32)
    i32.const 1
  else
    i32.const 32
    i32.const 4
    i32.const 0
    i32.const 10
    i32.store
    call $printf1
    local.get $x0
    i32.const 2
    i32.gt_s
  end
  i32.store
  call $printf1
)
;; output:
20
10
0
20
1
20
1
