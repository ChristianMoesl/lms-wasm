(func $Snippet (export "Snippet") (param $x0 i32)
  ;; This is a very important comment
  local.get $x0
  i32.const 1
  i32.add
  call $printf
)
;; output:
2
