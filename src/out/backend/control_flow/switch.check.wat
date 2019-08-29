(func $Snippet (export "Snippet") (param $x0 i32)
  block $2
    block $3
      block $4
        local.get $x0
        i32.const 1
        i32.eq
        br_if $4
        br $3
      end
      i32.const 20
      call $printString
      br $2
    end
    block $5
      block $6
        local.get $x0
        i32.const 2
        i32.eq
        br_if $6
        local.get $x0
        i32.const 3
        i32.eq
        br_if $6
        br $5
      end
      i32.const 26
      call $printString
      br $2
    end
    i32.const 32
    call $printString
  end
)
;; output:
1
2
2
3
