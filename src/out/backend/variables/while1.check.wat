(func $Snippet (export "Snippet") (param $x0 i32)
  (local $x1 i32)
  i32.const 1
  local.set $x1
  block $2
    loop $3
      local.get $x1
      i32.const 10
      i32.lt_s
      i32.eqz
      br_if $2
      local.get $x1
      i32.const 1
      i32.add
      local.set $x1
      br $3
    end
  end
  local.get $x1
  call $printlnInt
)
;; output:
10
