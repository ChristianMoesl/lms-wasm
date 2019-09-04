(func $Snippet (export "Snippet") (param $x0 i32)
  (local $x1 i32)
  i32.const 1
  local.set $x1
  i32.const 32
  i32.const 4
  i32.const 0
  i32.const 1
  i32.store
  call $printf1
  local.get $x0
  i32.const 0
  i32.gt_s
  if
    i32.const 2
    local.set $x1
  else
    i32.const 3
    local.set $x1
  end
  i32.const 32
  i32.const 4
  i32.const 0
  local.get $x1
  i32.store
  call $printf1
)
;; output:
1
3
1
2
