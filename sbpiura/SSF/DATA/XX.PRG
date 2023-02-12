SCAN
	xCon = CodCon
	xNum = 0
	SCAN WHILE CodCon = xCon
		xNum = xNum + 1
		IF VAL(NumCob) # xNum
			wait wind 'Error'
			susp
		ENDIF
	ENDSCAN
	SKIP -1
ENDSCAN