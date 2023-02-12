USE st_isrep
*SET STEP ON
SCAN
*	?desace
	if leN(observ)>1000
*		susp
*		CLEAR
		REPLACE OBSERV WITH ''
	ENDIF
	if leN(desace)>1000
*		susp
*		CLEAR
		REPLACE desace WITH ''
	ENDIF
ENDSCAN
