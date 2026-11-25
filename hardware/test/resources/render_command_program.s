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


; Command packets are 8 bytes and 8-byte aligned (little endian):
;
;   word 0, bits  7:0   type
;   word 0, bits 31:8   type-specific
;   word 1              type-specific
;
; Packet types:
;   0  NOP
;   2  FLUSH_TILE: word0[31:8] = destination stride in bytes
;                  word1 = destination address (low 32 bits)
;   3  LOAD_SHADER_STATE: word0[31:8] = number of varyings
;                         word1 = fragment shader address (low 32 bits)
;
; Unknown packet types are skipped.
;
; Scratchpad0 is the address of the next packet to
; process (advanced by this program) and Scratchpad1 is the end of valid
; data (advanced by the host). When they are equal, the buffer is empty and
; this program busy waits.
;
; Register usage
;   r0  Always 0
;   r1  Registers base
;   r3  System memory address of current packet start
;   r4  Temporary
;   r5  Packet word 0, temporary
;   r6  Packet word 1
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

.define CMD_FLUSH_TILE 2
.define CMD_LOAD_SHADER_STATE 3

_start:         clear r0
                loadi r1, 0xfffffe00        ; DMA registers

main_loop:      lw r3, SCRATCHPAD_0(r1)     ; Beginning of ring
                lw r4, SCRATCHPAD_1(r1)     ; End of ring
                nop
                beq r3, r4, main_loop       ; Buffer empty, wait

                ; Fetch the packet (one 64-bit bus transfer = two words)
                sw r3, DMA_ADDR_LOW(r1)
                sw r0, DMA_ADDR_HIGH(r1)
                movei r5, 0                 ; One 64-bit transfer minus one
                sw r5, DMA_LENGTH(r1)       ; DMA length, starts transfer
wait_dma:       lw r5, DMA_STATUS(r1)       ; DMA status
                nop
                bz r5, wait_dma
                lw r5, DMA_DATA(r1)         ; Packet word 0
                lw r6, DMA_DATA(r1)         ; Packet word 1
                nop

                ; Dispatch on packet type (low byte of word 0)
                andi r4, r5, 255
                movei r7, CMD_FLUSH_TILE
                bne r4, r7, check_load_shader
                jal r7, flush_tile
                b next_packet
check_load_shader:
                movei r7, CMD_LOAD_SHADER_STATE
                bne r4, r7, not_load_shader
                jal r7, load_shader_state
                b next_packet
not_load_shader:

next_packet:    addi r3, r3, 8
                sw r3, SCRATCHPAD_0(r1)     ; Advance start pointer
                b main_loop

; Write the color tile buffer to memory and wait for it to complete.
;   r5 = packet word 0, r6 = packet word 1
flush_tile:     loadi r4, 0xffffff00        ; Control registers
                sw r6, TILE_WRITE_ADDR_L(r4) ; TileWriteAddressL
                sw r0, TILE_WRITE_ADDR_H(r4) ; TileWriteAddressH
                srli r5, r5, 8
                sw r5, TILE_WRITE_STRIDE(r4)
                sw r0, TILE_WRITE_START(r4) ; Select color buffer
wait_flush:     lw r5, PIPELINE_FLAGS(r1)   ; Pipeline flags
                nop
                andi r5, r5, 2              ; Bit 1 is tile writer done
                bz r5, wait_flush
                jalr r4, r7

; Load fragment shader address and varying count.
;   r5 = packet word 0, r6 = packet word 1
load_shader_state:
                loadi r4, 0xffffff00        ; Control registers
                sw r6, 4(r4)                ; FragmentShaderAddrL
                sw r0, 5(r4)                ; FragmentShaderAddrH
                srli r5, r5, 8
                sw r5, 6(r4)                ; NumVaryings
                jalr r4, r7
