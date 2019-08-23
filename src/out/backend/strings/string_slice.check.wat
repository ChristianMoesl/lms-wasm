(func $Snippet (export "Snippet") (param $x0 i32)
  local.get $x0
  i32.const 3
  i32.const 6
  call $stringSlice
  call $printlnString
)
;; output:
lo
