(func $Snippet (export "Snippet") (param $x0 i32)
  i32.const 32
  i32.const 1
  i32.const 0
  local.get $x0
  i32.const 2
  i32.gt_s
  if (result i32)
    local.get $x0
    i32.const 1
    i32.gt_s
  else
    i32.const 0
  end
  if (result i32)
    i32.const 1
  else
    local.get $x0
    i32.const 1
    i32.gt_s
  end
  i32.store
  call $printf
)
;; output:
1
