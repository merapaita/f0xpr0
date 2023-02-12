SET TALK ON
CLEAR
*SET STEP ON
ON ERROR DO CTRL_ERROR
DO WHILE LASTKEY()#27
	USE ITEHC EXCL
ENDDO
? " L O    L O G R E  " 
RETURN .T.


PROCEDURE CTRL_ERROR
*-------------------
DO CASE
	CASE ERROR()=1705
		? "Denegado el acceso a archivo."
		RETURN .T.
	CASE ERROR()=1
		? "El archivo ['<archivo>'] no existe."
ENDCASE