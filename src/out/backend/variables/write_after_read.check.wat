(func $Snippet (export "Snippet") (param $x0 i32)
  (local $x1 i32)
  i32.const 1
  local.set $x1
  i32.const 1
  call $printf
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
  local.get $x1
  call $printf
)
;; output:
1
3
1
2
