testlbl:
	MOV r2, #3
loop:
  STMFD sp!, {r3-r5, lr}
	SUBS r2, r2, #1
  LDMFD sp!, {lr}
  STMFD sp!, {tes}
	BNE loop
	xclose test
