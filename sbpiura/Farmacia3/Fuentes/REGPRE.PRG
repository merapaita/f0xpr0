
CLOSE DATABASES
USE IN 1 StkAlmv ALIAS stkalmv ORDER StkAlmv4
SELECT stkalmv
GOTO TOP
DO lista
DO fin_opcion
RETURN
*
PROCEDURE lista
*--------------
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F2 DO BusDetKar
ON KEY LABEL F6 DO actPreStk

DEFINE WINDOW win_00 FROM 00,00 TO 2,79 FLOAT TITLE ;
       'Lista de Precios de Medicamentos' DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW win_00
@ 0, 44 SAY 'º'
@ 0, 46 SAY 'Vta Publico'
@ 0, 60 SAY 'º'
@ 0, 62 SAY 'Vta Especial'
*@ 1, 30 SAY '     Valor Venta :'

DEFINE WINDOW win_01 FROM 03,00 TO 23,79 FLOAT DOUBLE COLOR SCHEME 10
ACTIVATE WINDOW win_01
BROWSE NOOPTIMIZE NOMENU NOAPPEND NODELETE NOCLEAR WINDOW win_01 NOREFRESH;
 FIELDS desart	:H = 'Nombre' :R :23,;
		lfracc	:H = 'M' :R,;
		unidad  :H = 'Unidad' :R :3,;
		unifrac	:H = 'U. Fr.' :R :3,;
		prepro	:H = 'Precio' :P='9,999.99':R,;
		preven	:H = 'Unidad'  :P='9,999.99',;
		prevenfr:H = 'Fracc' :P='999.99',;
		preven2 :H = 'Unidad'  :P='9,999.99',;
		preven2fr :H = 'Fracc' :P='999.99'
DEACTIVATE WINDOW win_01
RELEASE WINDOW
RETURN

PROCEDURE actPreStk
*-----------------
REPLACE preven    WITH PrePro*1.15
REPLACE prevenfr  WITH PrePro/Fraccion*1.15
REPLACE preven2   WITH PrePro*1.12
REPLACE preven2fr WITH PrePro/Fraccion*1.12

RETURN

*
PROCEDURE fin_opcion
*-------------------
CLOSE DATABASES
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE busdetkar
*------------------
PRIVATE kf2, kf5, kf8
kf2 = ON('KEY', 'F2')
kf5 = ON('KEY', 'F5')
kf8 = ON('KEY', 'F8')
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = IIF( .NOT. EOF(), RECNO(),-1)
cord1 = ORDER()
vperiodo = RIGHT(STR(YEAR(DATE()),4),2)
vbusca = SPACE(30)
vnombre = 'Nombre :'
SET ORDER TO StkAlmV4
IF LASTKEY() <> 27
	DEFINE WINDOW lista FROM 13,12 TO 16, 68 FLOAT TITLE ' °° B£squeda °° '  ;
		DOUBLE COLOR SCHEME 10
     ACTIVATE WINDOW lista
     @ 0, 2 SAY 'Periodo:' GET vperiodo WHEN .F.
     @ 1, 2 SAY vnombre GET vbusca PICTURE '@!'
     READ
     DEACTIVATE WINDOW lista
     RELEASE WINDOW lista
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() = 27
ELSE
     SEEK ALLTRIM(vperiodo + vbusca)
     IF !FOUND()
          DO standby WITH 'Articulo no Existe'
          IF vtemp = -1
               GOTO BOTTOM
          ELSE
               GOTO vtemp
          ENDIF
     ELSE
     ENDIF
ENDIF
SET ORDER TO &cOrd1
ON KEY LABEL F2 &kf2
ON KEY LABEL F5 &kf5
ON KEY LABEL F8 &kf8
RETURN
*
