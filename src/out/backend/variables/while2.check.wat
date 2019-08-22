(func $Snippet (export "Snippet") (param $x0 i32)
  (local $x1 i32)
  i32.const 1
  local.set $x1
  block $0
    loop $1
      local.get $x1
      i32.const 10
      i32.lt_s
      i32.eqz
      br_if $0
      local.get $x1
      call $printlnInt
      local.get $x1
      i32.const 1
      i32.add
      local.set $x1
      br $1
    end
  end
)
;; output:
1
2
3
4
5
6
7
8
9
