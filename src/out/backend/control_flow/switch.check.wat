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
      i32.const 20
      call $printlnString
      br $0
    end
    block $3
      block $4
        local.get $x0
        i32.const 2
        i32.eq
        br_if $4
        local.get $x0
        i32.const 3
        i32.eq
        br_if $4
        br $3
      end
      i32.const 26
      call $printlnString
      br $0
    end
    i32.const 32
    call $printlnString
  end
)
;; output:
20
26
26
32
