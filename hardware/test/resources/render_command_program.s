;
;   Copyright 2026 Jeff Bush
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing, software
;   distributed under the License is distributed on an "AS IS" BASIS,
;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;   See the License for the specific language governing permissions and
;   limitations under the License.
;


; Scratchpad0 is the address of the next packet to
; process (advanced by this program) and Scratchpad1 is the end of valid
; data (advanced by the host). When they are equal, the buffer is empty and
; this program busy waits.
;
; Register usage
;   r1  Registers base
;   r2  Packet word 0
;   r3  Packet word 1
;   r5  System memory address of current packet start
;   r6  Stack pointer
;   r7  Subroutine return address
;
; Note: loads have a one instruction delay slot.
;

.define DMA_ADDR_LOW 0
.define DMA_ADDR_HIGH 1
.define DMA_LENGTH 2
.define DMA_STATUS 3
.define DMA_DATA 4
.define PIPELINE_FLAGS 5
.define SCRATCHPAD_0 8
.define SCRATCHPAD_1 9
.define TILE_WRITE_ADDR_L 9
.define TILE_WRITE_ADDR_H 10
.define TILE_WRITE_STRIDE 11
.define TILE_WRITE_START 12

.define CMD_LAST 3

_start:         loadi r1, 0xfffffe00        ; DMA registers
                loadi r6, 1024              ; Load stack pointer

main_loop:      lw r5, SCRATCHPAD_0(r1)     ; Beginning of ring
                lw r4, SCRATCHPAD_1(r1)     ; End of ring
                nop
                beq r5, r4, main_loop       ; Buffer empty, wait

                ; Fetch the packet (one 64-bit bus transfer = two words)
                sw r5, DMA_ADDR_LOW(r1)
                clear r0
                sw r0, DMA_ADDR_HIGH(r1)
                movei r2, 0                 ; One 64-bit transfer minus one
                sw r2, DMA_LENGTH(r1)       ; DMA length, starts transfer
wait_dma:       lw r2, DMA_STATUS(r1)       ; DMA status
                nop
                bz r2, wait_dma
                lw r2, DMA_DATA(r1)         ; Packet word 0
                lw r6, DMA_DATA(r1)         ; Packet word 1
                nop

                ; Dispatch on packet type (low byte of word 0)
                andi r4, r2, 255
                sltui r0, r4, CMD_LAST
                bz r0, next_packet          ; Out of bounds
                lw r0, command_table(r4)
                jalr r7, r0
next_packet:    addi r5, r5, 8
                sw r5, SCRATCHPAD_0(r1)     ; Advance start pointer
                b main_loop

command_table:  .word no_op
                .word flush_tile
                .word load_shader_state

no_op:          jalr r7, r7

; Write the color tile buffer to memory and wait for it to complete.
;   r2 = packet word 0, r6 = packet word 1
flush_tile:     loadi r4, 0xffffff00        ; Control registers
                sw r6, TILE_WRITE_ADDR_L(r4) ; TileWriteAddressL
                clear r0
                sw r0, TILE_WRITE_ADDR_H(r4) ; TileWriteAddressH
                srli r2, r2, 8
                sw r2, TILE_WRITE_STRIDE(r4)
                sw r0, TILE_WRITE_START(r4) ; Select color buffer
wait_flush:     lw r2, PIPELINE_FLAGS(r1)   ; Pipeline flags
                nop
                andi r2, r2, 2              ; Bit 1 is tile writer done
                bz r2, wait_flush
                jalr r7, r7

; Load fragment shader address and varying count.
;   r2 = packet word 0, r6 = packet word 1
load_shader_state:
                loadi r4, 0xffffff00        ; Control registers
                sw r6, 4(r4)                ; FragmentShaderAddrL
                clear r0
                sw r0, 5(r4)                ; FragmentShaderAddrH
                srli r2, r2, 8
                sw r2, 6(r4)                ; NumVaryings
                jalr r7, r7
