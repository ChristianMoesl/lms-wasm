(func $Snippet (export "Snippet") (param $x0 i32)
  local.get $x0
  call $stringToInt
  call $printlnInt
)
;; output:
135
