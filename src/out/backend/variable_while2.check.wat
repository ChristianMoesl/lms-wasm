(module
  ;; *********** Import Section ***********
  (import "wasi_unstable" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (import "wasi_unstable" "args_sizes_get" (func $args_sizes_get (param i32 i32) (result i32)))
  (import "wasi_unstable" "args_get" (func $args_get (param i32 i32) (result i32)))
  ;; *********** Memory Section ***********
  (memory 1)
  ;; *********** Global Section ***********
  (global $_bump (mut i32) (i32.const 1024))
  ;; *********** Export Section ***********
  (export "memory" (memory 0))
  ;; *********** Code Section ***********
  (func $malloc (param $size i32) (result i32)
    (global.get $_bump)
    (i32.add (local.get $size) (global.get $_bump))
    (global.set $_bump)
  )
  (func $_atoi_rec (param $sum i32) (param $str i32) (result i32)
    (if (result i32) (i32.eqz (i32.load8_u (local.get $str)))
      (then (local.get $sum))
    (else
        (call $_atoi_rec
          (i32.add (i32.mul (local.get $sum) (i32.const 10))
            (i32.sub (i32.load8_u (local.get $str)) (i32.const 0x30)))
          (i32.add (local.get $str) (i32.const 1)))
    ))
  )
  (func $atoi (param $str i32) (result i32)
    (call $_atoi_rec (i32.const 0) (local.get $str))
  )
  (func $strlen (param $str i32) (result i32)
    (if (i32.eqz (i32.load8_u (local.get $str)))
      (then
        (return (i32.const 0))
    ))
    (return
      (i32.add
        (call $strlen (i32.add (local.get $str) (i32.const 1)))
        (i32.const 1)
    ))
  )
  (func $puts (param $str i32)
    (i32.store (i32.const 0) (local.get $str))
    (i32.store (i32.const 4) (call $strlen (local.get $str)))
    (drop (call $fd_write
        (i32.const 1) ;; file_descriptor - 1 for stdout
        (i32.const 0) ;; *iovs - The pointer to the iov array, which is stored at memory location 0
        (i32.const 1) ;; iovs_len - We're printing 1 string stored in an iov - so one.
        (i32.const 8))) ;; nwritten - A place in memory to store the number of bytes writen
  )
  (func $main (export "_start")
    (local $argc i32)
    (local $argv_buf_size i32)
    (local $argv i32)
    (local $argv_buf i32)
    (local $arg i32)
    (drop (call $args_sizes_get (i32.const 0) (i32.const 4)))
    (local.set $argc (i32.load (i32.const 0)))
    (local.set $argv_buf_size (i32.load (i32.const 4)))
    (if (i32.lt_u (local.get $argc) (i32.const 2))
      (then
        (call $puts (i32.const 20))
        (return)
      )
    )
    (local.set $argv (call $malloc (i32.mul (local.get $argc) (i32.const 4))))
    (local.set $argv_buf (call $malloc (local.get $argv_buf_size)))
    (drop (call $args_get (local.get $argv) (local.get $argv_buf)))
    ;; get second argument
    (local.set $arg (i32.load (i32.add (local.get $argv) (i32.const 4))))
    (call $Snippet (call $atoi (local.get $arg)))
  )
  (func $_itoa_rec (param $i i32) (param $str i32) (param $base i32) (result i32)
    (local $idx i32)
    (if (i32.eqz (local.get $i))
      (then
        (return (i32.const 0)))
    )
    (call $_itoa_rec (i32.div_u (local.get $i) (local.get $base)) (local.get $str) (local.get $base))
    (local.set $idx)
    (i32.store8
      (i32.add (local.get $str) (local.get $idx))
      (i32.add (i32.rem_u (local.get $i) (local.get $base)) (i32.const 48))
    )
    (i32.add (local.get $idx) (i32.const 1))
  )
  (func $itoa (param $i i32) (param $str i32) (param $base i32) (result i32)
    (if (result i32) (i32.eqz (local.get $i))
      (then
        (i32.store8 (local.get $str) (i32.const 48))
        (i32.const 1)
      )
    (else
        (call $_itoa_rec (local.get $i) (local.get $str) (local.get $base))
    ))
  )
  (func $printf (param $n i32)
    (local $str i32)
    (local $len i32)
    (local.set $str (call $malloc (i32.const 22)))
    (local.set $len (call $itoa (local.get $n) (local.get $str) (i32.const 10)))
    (i32.store8 (i32.add (local.get $str) (local.get $len)) (i32.const 10)) ;; add LF
    (local.set $len (i32.add (local.get $len) (i32.const 1)))
    (i32.store8 (i32.add (local.get $str) (local.get $len)) (i32.const 0))  ;; 0 terminated
    (call $puts (local.get $str))
  )
  (func $Snippet (export "Snippet") (param $x1 i32)
    (local $x2 i32)
    i32.const 1
    local.set $x2
    block $0
      loop $1
        local.get $x2
        i32.const 10
        i32.lt_s
        i32.eqz
        br_if $0
        local.get $x2
        call $printf
        local.get $x2
        i32.const 1
        i32.add
        local.set $x2
        br $1
      end
    end
  )
  ;; *********** Data Section ***********
  (data (i32.const 0) "\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")
  (data (i32.const 20) "usage: ./prog <arg>\n\00")
)
;; output:
1
2
3
4
5
6
7
8
9
