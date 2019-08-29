(func $Snippet (export "Snippet") (param $x0 i32)
  (local $x1 i32)
  (local $x2 i32)
  (local $x3 i32)
  i32.const 1
  local.set $x1
  i32.const 0
  local.set $x2
  i32.const 0
  local.set $x3
  block $2
    loop $3
      local.get $x1
      i32.const 10
      i32.lt_s
      i32.eqz
      br_if $2
      local.get $x3
      local.get $x2
      i32.add
      local.set $x3
      local.get $x2
      i32.const 1
      i32.add
      local.set $x2
      local.get $x1
      i32.const 1
      i32.add
      local.set $x1
      br $3
    end
  end
  local.get $x3
  call $printlnInt
)
;; output:
36
