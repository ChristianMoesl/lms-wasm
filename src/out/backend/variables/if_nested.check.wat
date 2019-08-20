(func $Snippet (export "Snippet") (param $x0 i32)
  (local $x1 i32)
  i32.const 1
  local.set $x1
  local.get $x0
  i32.const 10
  i32.gt_s
  if
    local.get $x0
    i32.const 20
    i32.lt_s
    if
      i32.const 2
      local.set $x1
    end
  end
  local.get $x1
  call $printf
)
;; output:
1
2
2
1
