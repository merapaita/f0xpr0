SCAN WHILE Periodo=Periw AND Tipdoc="O/C" AND Numdoc=Numow
		*IF !f_lock(1)
		*		UNLOCK ALL
		*		RETURN .F.
		*	ENDIF
			
		*	SELECT Comp
			*IF RLOCK()
				DELETE NEXT 1
			*ENDIF
	 EndScan
