(func $Snippet (export "Snippet") (param $x0 i32)
  (local $x1 i32)
  (local $x2 i32)
  local.get $x0
  i32.const 1
  i32.add
  local.set $x1
  ;;# This is an important block comment
  ;; generated code for This is an important block comment
  local.get $x1
  local.set $x2
  ;;# This is an important block comment
  local.get $x2
  call $printf
)
;; output:
2
