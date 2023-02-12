go top
scan
	xNumRI = NumREF
	Cont = 0
	scan while NumRef = xNumRI
		Cont = Cont + 1
	ENDSCAN
	IF Cont > 2
		RETURN
	ENDIF
	SKIP -1
endscan