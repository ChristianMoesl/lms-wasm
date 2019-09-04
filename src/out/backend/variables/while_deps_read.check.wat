(func $Snippet (export "Snippet") (param $x0 i32)
  (local $x1 i32)
  i32.const 1
  local.set $x1
  local.get $x0
  i32.const 50
  i32.lt_s
  if
    block $0
      loop $1
        local.get $x1
        i32.const 10
        i32.lt_s
        i32.eqz
        br_if $0
        i32.const 32
        i32.const 4
        i32.const 0
        local.get $x1
        i32.store
        call $printf1
        local.get $x1
        i32.const 1
        i32.add
        local.set $x1
        br $1
      end
    end
    i32.const 32
    i32.const 4
    i32.const 0
    i32.const 1
    i32.store
    call $printf1
  else
  end
  i32.const 32
  i32.const 4
  i32.const 0
  local.get $x1
  i32.store
  call $printf1
  i32.const 32
  i32.const 4
  i32.const 0
  local.get $x1
  i32.const 1
  i32.add
  i32.store
  call $printf1
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
1
10
11
1
2
