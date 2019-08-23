(func $Snippet (export "Snippet") (param $x0 i32)
  local.get $x0
  call $stringToDouble
  call $printlnFloat
)
;; output:
135.15499877929688
