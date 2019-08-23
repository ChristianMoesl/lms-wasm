(func $Snippet (export "Snippet") (param $x0 i32)
  local.get $x0
  call $stringLength
  call $printlnInt
)
;; output:
12
