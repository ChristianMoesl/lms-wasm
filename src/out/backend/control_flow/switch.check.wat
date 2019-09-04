(func $Snippet (export "Snippet") (param $x0 i32)
  block $0
    block $1
      block $2
        local.get $x0
        i32.const 1
        i32.eq
        br_if $2
        br $1
      end
      i32.const 32
      call $printf0
      br $0
    end
    block $1
      block $2
        local.get $x0
        i32.const 2
        i32.eq
        br_if $2
        local.get $x0
        i32.const 3
        i32.eq
        br_if $2
        br $1
      end
      i32.const 40
      call $printf0
      br $0
    end
    i32.const 48
    call $printf0
  end
)
;; output:
1
2
2
3
