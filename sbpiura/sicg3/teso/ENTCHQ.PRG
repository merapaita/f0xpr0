* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ Entchq.PRG  14/12/95                                L:  240  ณ	
* ณ Entrega de Cheque                                            ณ
* ณ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
* Estados
*  00 Girado
*  99 Anulado
*  25 Rebajado con H/A

PUBLIC nValTot

*-Abriendo archivos
USE Parmae   IN 1  ORDER Parmae1  ALIAS Parma
USE ComPag   IN 2  ORDER ComPag1  ALIAS ComPag
USE Cajas    IN 3  ORDER Cajas1   ALIAS Caja
USE Cheque   IN 4  ORDER Cheque1  ALIAS Cheque
USE AstOrd	 IN 5  ORDER AstOrd1  ALIAS AstOrd

vfecent=date()
*vcaja=0
*VCAJA=1
vctacte = ""
*vctacte = SPACE(14)
*SELECT cheque
*@1,1 SAY 'Fecha de Entrega :' get vfecent PICTURE "@D"
*@3,1 SAY '        Cta.Cte. :' GET vctacte  VALID Val_Fun('Caja','CodCtc','codctc+descri',vCtacte,1,4,10,'descri',27)
*@5,7 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 15
*READ CYCLE 

*IF LASTKEY()=27 OR Okcancel = 2
*   CLOSE DATA
*   RELEASE WINDOW FECENT
*   SHOW POPUP POP_08
*   RETURN
*ENDIF
*ACTIVATE WINDOW Xwait
*@1,1 SAY 'Procesando ....Espere un momento' COLOR W+/BR*

  DEFINE WINDOW VerSal FROM 1,0 TO 23,79 ;
  COLOR SCHEME 10 TITLE "ฎF2ฏ Entrega  ฑฒ Revisiขn de Cheques ฒฑ ฎF5ฏ Desmarca Entrega  ฎF8ฏ Busca"
  SELECT Cheque
*  xdbf = SYS(3)+'.DBF'
*  SELECT * ;
*  FROM Cheque C ;
*  WHERE (ESTADO='00' OR ESTADO='10' OR (ESTADO='40' AND FECENT=VFECENT)) AND CODCTC=vctacte ;
*  INTO TABLE (xdbf)
*  USE
*  USE (xdbf) IN 0  ALIAS Cheq
*  SELECT Cheq
*  vind1 = SYS(3)+'IDX'
*  INDEX ON Numcp TO (vind1)
*  DEACTIVATE WINDOW XWAIT
*  RELEASE WINDOW XWAIT

  GO TOP
  IF EOF()
  	ON KEY
  	CLOSE DATA
  	RELE WINDOW feccaja
  	DO Standby WITH ' No hay cheques por entregar para dicha cuenta  '
  	RETURN
  ENDIF
  ON KEY LABEL F2  DO EntCh
  ON KEY LABEL F5  DO Noentchq
  ON KEY LABEL F8  DO busca1
  ON KEY LABEL F10 KEYBOARD CHR(23)
  
  ** SE SUPRIMIO LA SENTENCIA NOEDIT AL BROWSE
  
  BROWSE NOAPPEND FIELDS ;
       ConEnt :H='Ent.' :R,;
       NumChq :H='Cheque' :R,;
       x1=Numcp+'.'+NumMes :H='C/P' :R,;
       NomGir :H='Girado a ' :R :P='!!!!!!!!!!!!!!!!!!!!',;
       ValChq :H='Importe' :P='999,9999,999.99' :R,;
       Estado :H='Est' :R ,;
       FECENT :H='Fec.Entrega.' ,;
       CodCtc :H='Cta.Corriente' :R,;
       FecChq :H='Fec.Giro' :R,;
       CodPart:H='Partid.' :R ;
       WINDOW VerSal
  ON KEY
  CLOSE DATA
  RELE WINDOW FECENT
RETURN

PROCEDURE Entch
*---------------
ON KEY LABEL F2
IF cheque.estado='99'
	DO StandBy WITH "Cheque Anulado no se puede entregar."
	ON KEY LABEL F2  DO EntCh
	RETURN
ENDIF
vPeriodo=Periodo
vnummes=nummes
vnumcp=numcp
vcodctc=codctc
nReg = RECNO()

IF RLOCK()
	REPLACE estado WITH '40',;
			conent with 'x'
	
	UNLOCK
	
*	fecent with vfecent
ENDIF

SELECT Cheque
SEEK vnummes+vnumcp			&&+vcodctc
nValTot = 0
SCAN WHILE vNumMes=NumMes AND vNumCP=NumCP
	IF Estado='40' AND conent = 'x'
		nValTot=nValTot+ValChq
	ENDIF
ENDSCAN
GO nReg
IF !EOF()
	SKIP
ENDIF
SELE ComPag
Seek ALLTRIM(vnummes)+vnumcp+ALLTRIM(vcodctc)
IF FOUND()
	IF RLOCK()
		REPLACE pres WITH ""
	ENDIF
	UNLOCK
ENDIF
DO AsiOrdCh
*SELE CHEQ
ON KEY LABEL F2  DO EntCh

RETURN

PROCEDURE NOEntchQ
*---------------
vnummes=nummes
vnumcp=numcp
vcodctc=codctc
nReg = RECNO()

IF RLOCK()
	REPLACE estado WITH '10',;
			fecent with {00/00/00},;  
			conent with ' '
	UNLOCK
ENDIF

SELECT Cheque
SEEK vnummes+vnumcp			&&+vcodctc
nValTot = 0
SCAN WHILE vNumMes=NumMes AND vNumCP=NumCP
	IF Estado='40' AND conent = 'x'
		nValTot=nValTot+ValChq
	ENDIF
ENDSCAN
GO nReg
IF !EOF()
	skip
ENDIF
SELE compag
SEEK ALLTRIM(vnummes)+vnumcp+ALLTRIM(vcodctc)
IF found()
   IF rlock()
   	  replace pres with " "
   ENDIF
   UNLOCK
ENDIF   
DO AsiOrdCh
*SELE CHEQ
RETURN


PROCEDURE Busca
*----------------

vORD = ORDER()
  IF escolor
    DEFINE POPUP _yt  FROM 9,55          SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP _yt  FROM 9,55          COLOR SCHEME C_POPUP
  ENDIF

  DEFINE BAR  1 OF _yt PROMPT "\<Por cขdigo"
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
		@ 1,1 SAY " Nง Cheque : "  GET vnumchq 
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
  DEFINE BAR  3 OF _yt PROMPT "Por \<monto"

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
		@ 1,1 SAY " Nง Cheque : "  GET vnumchq 
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

PROCEDURE AsiOrdCh
*-----------------
*SELE Parma
*SEEK 'TIPDOCCHE'
*IF !FOUND()
*	DO StandBy WITH "PARAMETRO NO INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
*	RETURN
*ENDIF
USE AsiAut IN 0 ORDER TAG AsiAut1
SELE AsiAut
SEEK "CHE"+"   "+"ASTORD"

IF !FOUND()
	DO StandBy WITH "PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
	USE IN AsiAut
	RETURN
 ELSE
	cCtaD  = DCuenta
	cCtaH  = HCuenta
ENDIF
SELE AstOrd
*SET STEP ON
*vPeriodo = IIF(!EMPTY(cheque.FecEnt),RIGHT(STR(YEAR(cheque.FecEnt),4),2),RIGHT(STR(YEAR(DATE()),4),2))
SEEK vPeriodo+vNumMes+vNumCP+"CHE"
IF FOUND()
	FOR i = 1 TO 2
		IF f_Lock(1) OR RLOCK()
			REPLACE Periodo WITH vPeriodo ,;
					NUMMES	WITH vNumMes  ,;
					NUMREF	WITH vNumCP   ,;
					TIPDOC	WITH "CHE"     ,;
					FECHA	WITH Cheque.FecChq  ,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,nValTot,0),;
					MTOHAB	WITH IIF(i=2,nValTot,0)
			UNLOCK
			SKIP
		ENDIF
	ENDFOR
 ELSE
	FOR i = 1 TO 2
		IF f_Appd()
			REPLACE Periodo WITH vPeriodo ,;
					NUMMES	WITH vNumMes  ,;
					NUMREF	WITH vNumCP   ,;
					TIPDOC	WITH "CHE"     ,;
					FECHA	WITH Cheque.FecChq  ,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,nValTot,0),;
					MTOHAB	WITH IIF(i=2,nValTot,0)
			UNLOCK
		ENDIF
	ENDFOR
ENDIF
USE IN AsiAut
*DEFINE WINDOW wAstOrd FROM 10,10 TO 15,70 ;
* TITLE ' ASIENTOS DE ORDEN' COLOR SCHEME 02
*ACTIVATE WINDOW wAstOrd
*@ 00,08  SAY 'Cuentas '
*@ 00,18  SAY '        Debe '
*@ 00,34  SAY '        Haber '
*@ 01,04  SAY cCtaD PICTURE '!!!!!!!!!!!'
*@ 01,18  SAY m.ValTot PICTURE '99,999,999.99'
*@ 02,12  SAY cCtaH PICTURE '!!!!!!!!!!!'
*@ 02,34  SAY m.ValTot PICTURE '99,999,999.99'
*WAIT " "
*DEACTIVATE WINDOW wAstOrd
*RELEASE WIND wAstOrd
RETURN