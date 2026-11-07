

    beq r0, r1, foo
    and r1, r2, r3
    addi r4, r5, -20

foo:
    loadi r1, 0x12345678
    jal r7, sub0
    move r2, r3
    clear r3


sub0:
    lw r1, 10(r2)
    nop
    lw r2, (r1)
    lw r4, -5(r5)

    sw r1, (r2)
    sw r3, 5(r4)

    jalr r7, r7
