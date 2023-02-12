* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ MonPag.PRG        01/10/97                          L: 0168  ณ	
* ณ Emisiขn de Montos Pagados                                    ณ
* ณ AUTOR :                                  REGION GRAU - 1997  ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

USE Cajas    IN  2 ORDER Cajas1                  ALIAS Caja
USE Parmae   IN  3 ORDER Parmae1                 ALIAS Parma
USE Itecp    IN  4 ORDER Itecp1 				 ALIAS itecp
USE Cheque   IN  5 ORDER Cheque1                 ALIAS Cheque
USE Detcta   IN  6 ORDER DetCta1                 ALIAS DetCta
USE MonPag   IN  7 ORDER MonPag1				 ALIAS MonPag
vexa = SET('EXACT')
*SELE MONPAG
*ZAP

PUBLIC vcodctc
vcodCtc = SPACE(14)
DO HACER
SET EXACT &vexa

PROCEDURE HACER
*----------------
 DEFINE WINDOW EjePre   FROM 15,52 TO 19,78 COLOR SCHEME 5
 ACTIVATE WINDOW EjePre  
 vscta = SPACE(8)
 SELE CAJA
 @ 0, 0 SAY 'Montos Pagados '
 @ 2, 0 SAY '     Cta.Cte. :' GET vcodctc  PICTURE '@!' VALID Val_Fun('Caja','CodCtc','codctc+descri',vCodctc,1,2,16,'descri',27) 
 READ
 DEACTIVATE WINDOW ejepre
 IF LASTKEY()#27
 	DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
	ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando informaciขn para reporte...." COLOR W+/BR*
	vdbf = sys(3)+'idx'
	SELECT 8
	CREATE TABLE (vdbf)	(Codctc c(14), nummes C(2) , pagcte N(18,2), pagcap N(18,2), ;
				  gircte N(18,2) , gircap N(18,2))
	USE
	USE (vdbf) IN 8 EXCLUSIVE                  ALIAS Temp
*	CREATE TABLE MonPag	(Codctc c(14), nummes C(2) , pagcte N(18,2), pagcap N(18,2)
*	USE
*	USE MonPag IN 8 
	
*********************- MONTOS PAGADOS -***********************
	SELE Cheque
	SET FILTER TO CODCTC=VCODCTC
	SET ORDER TO TAG CHEQUE1
	GO TOP
	DO WHILE !EOF()
		vmonto=Cheque.valchq
		vnumero=Cheque.numchq
		vmes=Cheque.nummes
		SELE DetCta
		SET FILTER TO CODCTC=VCODCTC
		GO TOP
		SET ORDER TO TAG DETCTA6
		SEEK VCODCTC+vnumero
		IF FOUND()  &&comparo el Nง cheque 
			IF -1*(DetCta.Monto) = vmonto && comparo monto y capturo el nง c/p
&&&&&&FALTA LA RELACION PARA CAPTURAR LA PARTIDA
&&&				SET RELATION TO NUMMES+NUMCP+CODCTC INTO ITECP
				SELE Itecp
				*vpart=SUBST(Itecp.codpart,2,1)
				vpart=Itecp.codpart
				SELE MonPag
				APPEND BLANK
				REPLACE codctc  WITH vcodctc,;
						nummes  WITH vmes ,;
						partida WITH vpart ,;
						monto   WITH vmonto
			ELSE
				vmont2=Cheque.valchq
				SET RELATION TO NUMMES+NUMCP+CODCTC INTO ITECP
				SELE Itecp
				*vpart=SUBST(Itecp.codpart,2,1)
				vpart=Itecp.codpart
				SELE MonPag
				APPEND BLANK
				REPLACE codctc  WITH vcodctc,;
						nummes  WITH vmes ,;
						partida WITH vpart ,;
						monto   WITH vmont2
			ENDIF
		ENDIF
		SELE DetCta
		SKIP
	ENDDO
**********************- MONTOS PAGADOS /CTE,CAPITAL -**************************

	SELE MonPag
	SET FILT TO codctc = vcodctc
	GO TOP
    vmess = nummes
	vpagcte = 0
	vpagcap = 0

DO WHILE !EOF()
		IF nummes#vmess
			SELECT  temp
			APPEND BLANK
			REPLACE codctc  WITH vcodctc,;
					nummes  WITH vmess ,;
					pagcte  WITH vpagcte ,;
					pagcap  WITH vpagcap 
			vpagcte = 0
			vpagcap = 0
			vmess = MonPag.nummes
		ENDIF
		SELECT MonPag
		IF VAL(LEFT(partida,2))<5 AND VAL(LEFT(partida,2))>0
			vpagcte = vpagcte + MonPag.monto
		ELSE	
			vpagcap = vpagcap + MonPag.monto
		ENDIF
		SKIP

	ENDDO
	
	DEACTIVATE WINDOW Xwait
	SELE CAJA
	SET INDEX TO
	SET ORDER TO CAJAS1
	SELECT TEMP
	GO TOP
	DO Reporte WITH 2,"Montos"," Movimiento de Fondos de Cuenta Corriente "

ENDIF			
 CLOSE DATA
 RETURN
 
			

