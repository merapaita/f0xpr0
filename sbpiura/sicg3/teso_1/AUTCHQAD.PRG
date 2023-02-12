*- AutchqAD.PRG
*- Autorizacion de Cheque por Administracion
*- Autor: Federico Montero V.
*- 17 de Octubre de 1995
*----------------------------
* Estados
*  00 Girado
*  10 Autorizado por Tesoreria
*  20 Autorizado por Administracion
*  30 Recepcionado en Caja
*  40 Entregado al Usuario
*  99 Anulado
*  25 Rebajado con H/A

*-Abriendo archivos
USE Cheque   IN 4  ORDER Cheque1  ALIAS Cheque
PRIVATE OK
hide popup pop_01
IF VE_PASSW('ADMI-')
	DEFINE WINDOW FECAUTA FROM 08,30 TO 10,70 COLOR SCHEME 10 
	ACTIVATE WINDOW FECAUTA
	vFECAUTA=date()
	@0,3 SAY 'Fecha de Autorizacion :' get vFECAUTA PICTURE "@D"
	READ 
	IF LASTKEY()=27
	   CLOSE DATA
	   RELEASE WINDOW FECAUTA
	   SHOW POPUP POP_01
		RETURN
	ENDIF	
  DEFINE WINDOW VerSal FROM 1,0 TO 23,79 ;
  COLOR SCHEME 10 TITLE " ®F2¯ Autoriza ±² Revisi¢n de Cheques ²± ®F5¯ Desmarca Autorizaci¢n  ®F8¯ Busca"  
  xdbf = SYS(3)+'.DBF'
  SELECT Cheque
  SELECT * ;
  FROM Cheque c ;
  WHERE ESTADO='10' OR (ESTADO='20' AND FECAUTA=VFECAUTA) AND CODCTC<>'631003044' ;
  INTO TABLE (xdbf)
  USE
  USE (xdbf) IN 0  ALIAS Cheq
  SELECT Cheq
  GO TOP
  ON KEY LABEL F2  DO autCh
  ON KEY LABEL F5  DO Noautchq
  ON KEY LABEL F8  DO busca1
  ON KEY LABEL F10 KEYBOARD CHR(23)
  BROWSE NOEDIT NOAPPEND FIELDS ;
       NumChq :H='Cheque' :R,;
       x1=Numcp+'.'+NumMes :H='C/P' :R,;
       NomGir :H='Girado a ' :R :P='!!!!!!!!!!!!!!!!!!!!',;
       ValChq :H='Importe' :P='999,9999,999.99' :R,;
       CodCtc :H='Cta.Corriente' :R,;
       Estado :H='Est' :R ,;
       FECAUTA :H='Fec.Autorizac.' :R,;
       FecChq :H='Fec.Giro' :R,;
       CodPart:H='Partid.' :R ;
       WINDOW VerSal
  ON KEY LABEL F2  
  ON KEY LABEL F5  
  ON KEY LABEL F8  
  ON KEY LABEL F10 
  CLOSE DATA
  RELE WINDOW FECAUTA
ENDIF  
RETURN

PROCEDURE autch
*---------------
vnummes=nummes
vnumcp=numcp
vcodctc=codctc
SELECT Cheque
SEEK vnummes+vnumcp+vcodctc
IF FOUND() AND RLOCK()
   REPLACE ESTADO WITH '20', FECAUTA with vFECAUTA
   REPLACE Cheq.ESTADO WITH '20', Cheq.FECAUTA with vFECAUTA
   UNLOCK
ENDIF
SELE cheq
IF !EOF()
skip
ENDIF

PROCEDURE NOautchQ
*---------------
vnummes=nummes
vnumcp=numcp
vcodctc=codctc
SELECT Cheque
SEEK vnummes+vnumcp+vcodctc
IF FOUND() AND RLOCK()
   REPLACE ESTADO WITH '10', FECAUTA with {00/00/00}  
   REPLACE Cheq.ESTADO WITH '10', Cheq.FECAUTA with {00/00/00}  
ENDIF
SELE cheq
IF !EOF()
	skip
ENDIF


PROCEDURE Busca
*----------------

vORD = ORDER()
  IF escolor
    DEFINE POPUP _yt  FROM 9,55          SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP _yt  FROM 9,55          COLOR SCHEME C_POPUP
  ENDIF

  DEFINE BAR  1 OF _yt PROMPT "\<Por cheque"
  DEFINE BAR  2 OF _yt PROMPT "\<Por nombre"

  ON SELECTION POPUP _yt DEACTIVATE POPUP _yt
  ACTIVATE POPUP _yt

  RELEASE POPUP _yt

  DEFINE WINDOW VerSal FROM 10,20 TO 14,60 ;
  COLOR SCHEME 5 
  ACTIVATE WINDOW versal
  vreg = RECNO()
  vnumchq = SPACE(14)
  vnomgir = SPACE(20)
DO CASE
	CASE BAR()=1
		SET ORDER TO cheque4
		@ 1,1 SAY " N§ Cheque : "  GET vnumchq 
		READ
		SEEK vnumchq
		SET ORDER TO (vord)
		IF !FOUND()
			GO vreg	
		ENDIF

	CASE BAR()=2
		SET EXACT OFF
		SET ORDER TO cheque5
		@ 1,1 SAY "    Nombre : "  GET vnomgir
		READ
		SEEK ALLT(vnomgir)
		SET ORDER TO (vord)
		IF EOF()
			GO vreg
		ENDIF
ENDCASE
DEACTIVATE WINDOW versal
SET EXACT ON
RETURN 



PROCEDURE Busca1
*----------------

vORD = ORDER()
  IF escolor
    DEFINE POPUP _yt  FROM 9,55          SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP _yt  FROM 9,55          COLOR SCHEME C_POPUP
  ENDIF

  DEFINE BAR  1 OF _yt PROMPT "Por \<Cheque"
  DEFINE BAR  2 OF _yt PROMPT "Por \<Nombre"
  DEFINE BAR  3 OF _yt PROMPT "Por \<Monto"

  ON SELECTION POPUP _yt DEACTIVATE POPUP _yt
  ACTIVATE POPUP _yt

  RELEASE POPUP _yt

  DEFINE WINDOW VerSal FROM 10,20 TO 14,60 ;
  COLOR SCHEME 5
  ACTIVATE WINDOW versal
  vreg = RECNO()
  vnumchq = SPACE(14)
  vnomgir = SPACE(20)
  vmto    = 0
DO CASE
	CASE BAR()=1
		@ 1,1 SAY " N§ Cheque : "  GET vnumchq 
		READ
		LOCATE FOR Numchq = vnumchq
		IF !FOUND()
			GO vreg	
		ENDIF

	CASE BAR()=2
		SET EXACT OFF
		@ 1,1 SAY "    Nombre : "  GET vnomgir
		READ
		LOCATE FOR ALLT(nomgir) = ALLT(vnomgir)
		IF EOF()
			GO vreg
		ENDIF

	CASE BAR()=3
		@ 1,1 SAY " Monto     : "  GET vmto  PICTURE '@ 999,999,999.99'
		READ
		LOCATE FOR valchq = vmto   
		IF !FOUND()
			GO vreg	
		ENDIF
ENDCASE
DEACTIVATE WINDOW versal
SET EXACT ON
RETURN 

