;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This file demonstrates some features of the assembler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                        loadi v0, 123
                        loadi v1, 123
                        loadi v2, 11
                        setgti exec, v0, v1  ; Sets exec mask for all lanes that pass a > b
                        bz exec, do_else
                        subi v0, v0, v1      ; a = a - b
do_else:                xor exec, exec, r55  ; else, invert mask
                        bz exec, done
                        subi v1, v1, v0      ; b = b - a
done:                   move exec, r55       ; reset execution mask
                        halt

