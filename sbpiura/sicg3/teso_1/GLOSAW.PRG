vAlias = ALIAS()
SELECT Ri
SET MEMOWIDTH TO 50
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,14 TO 20,66 FLOAT NOCLOSE SHADOW DOUBLE TITLE '? Observaciones ?' FOOTER ' ? ?F10? Graba ? ' COLOR SCHEME 1
ENDIF
IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF
IF ingreso
   IF ALLT(m.tipri)='02' OR ALLT(m.tipri)='73'
	  REPLACE observ WITH SPACE(12)+Val_Para(m.TipRi,"TIPRI ","V",22,30)+SPACE(2)+dConcurso
   ELSE	 
	  REPLACE observ WITH SPACE(12)+Val_Para(m.TipRi,"TIPRI ","V",22,30)
   ENDIF	  
ENDIF	
MODIFY MEMO OBSERV WINDOW OBSERVA
IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci?n '
ENDIF
SELECT (vAlias)
RETURN .T.


