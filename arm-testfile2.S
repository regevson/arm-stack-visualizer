func:
		MOV			r3, #3
loop:
		MOV			r5, #4
		MOV			r8, #1
		MOV			r4, #9
    ADD     r5, r5, r4
    STMFD		sp!, {r5-r9, lr}
    STMFD		sp!, {r6}
		SUB			sp, sp, #1
    LDMFD		sp!, {r2-r4}
    SWI     #0
    MOV     r4, r4
    SUBS    r3, r3, #1
    BNE     loop
		SUB			sp, sp, #2
