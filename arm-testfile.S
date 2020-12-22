  STMFD sp!, {r4-r12, lr}
  SUB sp, sp, #4
  STMFD sp!, {r0}
  CMP r0, #2
  BLT return
  MOV r4, r0
	LDMFD sp!, {r4, r12}
  SUB r0, r0, #1
  BL fib
  MOV r5, r0  
  SUB r0, r4, #2  
  BL fib
  B return
