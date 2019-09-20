(func $Snippet (export "Snippet") (param $x0 i32)
  i32.const 7
  i32.const 0
  local.get $x0
  call $stringToDouble
  f32.store
  call $println
)
;; output:
5.55597758e-315
