* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ AF-9.PRG        30/04/96                            L: 0168  ณ	
* ณ Emisiขn del AF-9                                             ณ
* ณ AUTOR : Julio Cruz Ortiz                 REGION GRAU - 1996  ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

USE Cajas    IN  2 ORDER Cajas1                  ALIAS Caja
USE Parmae   IN  3 ORDER Parmae1                 ALIAS Parma
USE Itecp    IN  4 ORDER Itecp1 				 ALIAS itecp
USE Subctas  IN  5 ORDER subctas1                ALIAS subcta
USE iteaut   IN  6 ORDER iteaut1                 ALIAS It
vexa = SET('EXACT')

PUBLIC vcodctc
vcodCtc = SPACE(14)
DO proceso

SET EXACT &vexa

PROCEDURE SQL
*------------
  vcol ="w+/w,n/w,W/W,w/W,,w+/b"
  IF Escolor
    DEFINE WINDOW EjePre   FROM 15,42 TO 19,78  SHADOW COLOR &vcol
  ELSE
    DEFINE WINDOW EjePre   FROM 15,42 TO 19,78  COLOR SCHEME C_POPUP
  ENDIF
  ACTIVATE WINDOW EjePre  
 vcodCtc = SPACE(14)
 SELECT Itea
 @ 0, 0 SAY '          AF-9 '
 @ 2, 0 SAY '     Cta.Cte. :' GET vcodctc  PICTURE '@!' VALID Val_Fun('Caja','CodCtc','codctc+descri',vCodctc,1,2,16,'descri',27)
 READ
 DEACTIVATE WINDOW ejepre
 IF LASTKEY()#27
 	DEFINE WINDOW Xwait FROM 20,35 TO 22,75 COLOR SCHEME 5 
	ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando informaciขn para reporte...." COLOR W+/BR*
 	SELECT it.nummes,SUM(it.monasig) AS mtofun ;
	FROM Itaure it;
	WHERE it.Ctcofg = vcodctc  ; 
	GROUP BY 13;
    HAVING VAL(LEFT(it.codpart,2))<6 ;
	UNION ALL SELECT it1.nummes,SUM(it1.monasig) AS mtofun;
		  FROM itaure it1 ;
		  WHERE it1.Ctcofg = it.ctcofg  ; 
		  GROUP BY 13 ;
		  HAVING VAL(LEFT(it1.codpart,2))>6 ;
	INTO CURSOR AF-9
	DEACTIVATE WINDOW Xwait	
 ENDIF
 CLOSE DATA
 RETURN
 
 
 PROCEDURE proceso
 *----------------
  vcol ="w+/w,n/w,W/W,w/W,,w+/b"
  IF Escolor
    DEFINE WINDOW EjePre   FROM 15,52 TO 19,78  SHADOW COLOR &vcol
  ELSE
    DEFINE WINDOW EjePre   FROM 15,52 TO 19,78  COLOR SCHEME C_POPUP
  ENDIF
  ACTIVATE WINDOW EjePre  
 vscta = SPACE(8)
 SELECT It
 @ 0, 0 SAY '          AF-9 '
 @ 2, 0 SAY '     Sub-Cta :' GET vscta  PICTURE '@!' VALID ;
		 Val_Fun("SubCta","Subcta","Subcta+' '+Descri",vscta,1,2,16,'Descri')
 *Val_Fun('Caja','CodCtc','codctc+descri',vCodctc,1,3,16,'descri',27)
 READ
 DEACTIVATE WINDOW ejepre
 IF LASTKEY()#27
 	DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
	ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando informaciขn para reporte...." COLOR W+/BR*
	SELECT Subcta
	SEEK ALLTRIM(vscta)
	IF FOUND()
		vcodctc = subcta.ctarec
	ENDIF
	vdbf = sys(3)+'idx'
	SELECT 8
	CREATE TABLE (vdbf)	(Codctc c(14), nummes C(2) , autcte N(18,2), autcap N(18,2), ;
				  gircte N(18,2) , gircap N(18,2))
	USE
	USE (vdbf) IN 8 EXCLUSIVE                  ALIAS Temp

*********************- MONTOS AUTORIZADOS -***********************

	SELECT it
	SET FILT TO codctc = vcodctc
	GO TOP
	vmes = nummes
	vautcte = 0
	vautcap = 0

	DO WHILE !EOF()
		IF nummes#vmes
			SELECT  temp
			APPEND BLANK
			REPLACE codctc  WITH vcodctc,;
					nummes  WITH vmes ,;
					autcte  WITH vautcte ,;
					autcap  WITH vautcap 
			vautcte = 0
			vautcap = 0
			vmes = it.nummes
		ENDIF
		SELECT it
		IF VAL(LEFT(Codpart,2))<6
			vautcte = vautcte + it.monasig
		ELSE	
			vautcap = vautcap + it.monasig
		ENDIF
		SKIP
	ENDDO
	SELECT temp 
	GO TOP

**********************- MONTOS GIRADOS -**************************

	SELECT Itecp
	vind3 = SYS(3)+'IDX'
	INDEX ON nummes+codctc TO (vind3)  FOR tipdoc#'RE' AND Estado<>'99' ;
				AND Tipdoc#'SU'
	GO TOP

	SELECT Caja
	vind2 = SYS(3)+'IDX'
	INDEX ON Codctc TO (vind2)  FOR Codscta = ALLTRIM(vscta) AND Ofigir='S'
	GO TOP

	FOR I=1 TO VAL(vmes)
		vgircte = 0
		vgircap = 0
		vmes = PADL(INT(I),2,'0')
		GO TOP
		SCAN 
			vcta    = codctc
			SELECT itecp
			SEEK vmes+vcta
			SCAN WHILE nummes+codctc = vmes+vcta
				IF Tipfun = 'F'
					vgircte = vgircte + itecp.impparc
				ELSE	
					vgircap = vgircap + itecp.impparc
				ENDIF
			ENDSCAN 
			SELECT caja
		ENDSCAN
		SELECT Temp
		LOCATE FOR nummes = vmes
		REPLACE gircte  WITH vgircte ,;
				gircap  WITH vgircap
		SELECT Caja
	ENDFOR
	DEACTIVATE WINDOW Xwait
	SELE CAJA
	SET INDEX TO
	SET ORDER TO CAJAS1
	SELECT TEMP
	GO TOP
	DO Reporte WITH 2,"AF-9"," Movimiento de Fondos de Cuenta Corriente "
 ENDIF
 
 CLOSE DATA
 RETURN