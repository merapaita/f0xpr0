* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ Anex1GR.PRG           30/04/96                      L: 0157  ณ	
* ณ Anexo 1-GR                                                   ณ
* ณ AUTOR : Julio Cruz Ortiz                 REGION GRAU - 1996  ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
*- Abre archivos

USE AuGiRe  IN  1 ORDER TAG AuGiRe1                 ALIAS AutG
USE ItAuRe  IN  2 ORDER TAG ItAuRe1                 ALIAS IteA
USE Clase   IN  3 ORDER TAG Clase1                  ALIAS Clase
USE Parmae  IN  4 ORDER TAG Parmae1                 ALIAS Parma
USE Calen   IN  5 ORDER TAG Calen1                  ALIAS Calen
USE Cajas   IN  9 ORDER TAG Cajas1                  ALIAS Caja
USE SubCtas IN 10 ORDER TAG Subctas1                ALIAS Subcta
USE Cheque  IN 11 ORDER TAG Cheque1                 ALIAS Cheque

*- Declaraciขn de variables
PUBLIC vcodscta,xmes,xano

vind3 = SYS(3)+'.IDX'
vind2 = SYS(3)+'.IDX'
vind1 = SYS(3)+'.IDX'
vind = SYS(3)+'.IDX'
xMes     = PADL(MONTH(DATE()),2,'0')
xAno     = VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
vcodscta = SPACE(8)
filaux = SYS(3) + '.DBF'

SELECT 8
CREATE TABLE (filaux);
  (CodOfg    C(002),;
   Codctc    C(012),;
   CodPrg    C(002),;
   CodPart   C(005),;
   mtocal    N(18,2),;
   mtoaut    N(18,2),;
   mtogir    N(18,2))

      
USE
USE (filaux) IN 8 EXCLUSIVE                  ALIAS Temp

*- Variables de trabajo (registro a trabajar)

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
  vcol ="w+/w,n/w,W/W,w/W,,w+/b"
  IF Escolor
    DEFINE WINDOW Anexo1gr FROM 13,52 TO 19,78  SHADOW COLOR &vcol
  ELSE
    DEFINE WINDOW Anexo1gr FROM 13,52 TO 19,78  COLOR SCHEME C_POPUP
  ENDIF
  ACTIVATE WINDOW Anexo1gr

 SELECT Itea
 @ 0, 0 SAY '     Genera Anexo 1GR  '
 @ 2, 0 SAY '    Mes y Aคo :' GET xMes      PICTURE '99'
 @ 2,20 GET xAno                            PICTURE '99'
 @ 4, 0 SAY '   Sub_Cuenta :' GET vcodscta  PICTURE '@!' VALID Val_Fun("SubCta","Subcta","Subcta+' '+Descri",vcodscta,1,2,15,'SUBSTR(Descri,1,22)')
 READ
 DEACTIVATE WINDOW Anexo1GR
 DEFINE WINDOW Xwait FROM 20,35 TO 22,75 COLOR SCHEME 5 
 ACTIVATE WINDOW Xwait
 @0,1 SAY "   Procesando Anexo 1GR ......" COLOR W+/BR*
 IF LASTKEY()#27
       DO VALORES
       SELEC Temp
       INDEX ON codPRG TO (vind3)
       GO TOP
 	   DEACTIVATE WINDOW Xwait
       DO REPORTE WITH 2, "Anex1GR", ' Anexo 1 GR ',1,.F.,.T.
       SELECT t.codpart, SUM(t.mtocal) as totcal, SUM(t.mtoaut) as totaut,;
       		  SUM(t.mtogir) as totgir ;
       FROM Temp t ;
       GROUP BY codpart ;
       INTO CURSOR temp1

       SELECT Temp1
       DO REPORTE WITH 2, "Anex1GRa", ' Anexo 1 GR ',1,.F.,.T.
       CLOSE DATABASE
       ERASE (vind)
       ERASE (vind1)
       ERASE (vind2)
 ENDIF

 IF !escolor
    RESTORE SCREEN FROM principal
 ENDIF

RETURN

FUNCTION VALORES
*---------------
SELECT Cheque
INDEX ON codctc+SUBST(Codcal,8,2)+LEFT(ALLT(codpart),2) TO (vind2) FOR ;
		 nummes=ALLT(xmes) and SUBST(codcal,5,3) = 'TRN'
GO TOP

SELECT Calen
INDEX ON Codprg+LEFT(ALLT(codpart),2) TO (vind1) FOR nummes=ALLT(xmes) and codfte = 'TRN' 
GO TOP

SELECT IteA
INDEX ON ctcofg+codprg+LEFT(ALLT(codpart),2) TO (vind) FOR ALLTRIM(Codscta)=vcodscta ;
		AND Nummes = ALLT(xmes) 
GO TOP

vctcofg = ALLT(itea.ctcofg)
vcodprg = itea.codprg
vcodpart= LEFT(ALLT(itea.codpart),2)
vcodofg = itea.codofg
SCAN
	vctcofg = ALLT(itea.ctcofg)
	vcodprg = itea.codprg
	vcodpart= LEFT(ALLT(itea.codpart),2)
	vcodofg = itea.codofg
	SUM Monasig TO vmtoaut WHILE ctcofg+codprg+LEFT(ALLT(codpart),2) = ;
										vctcofg+vcodprg+vcodpart
	SELECT Calen
	SEEK vcodprg+vcodpart
	SUM Valpart TO vmtocal WHILE Codprg+LEFT(ALLT(codpart),2) = vcodprg+vcodpart

	SELECT Cheque
	SEEK vctcofg+vcodprg+vcodpart
	SUM Valchq TO vmtogir WHILE codctc+SUBST(Codcal,8,2)+LEFT(ALLT(codpart),2) = ;
									vctcofg+vcodprg+vcodpart
	SELECT Temp
	APPEND BLANK
	REPLACE codofg  WITH vcodofg ;
			codctc  WITH vctcofg ;
			codprg  WITH vcodprg ;
			codpart WITH vcodpart+'.00' ;
			mtocal  WITH vmtocal ;
			mtoaut  WITH vmtoaut ;
			mtogir  WITH vmtogir 
	SELECT Itea
ENDSCAN
RETURN

FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)


PROCEDURE Salida
*---------------
  RELEASE    POPUP  Anexo1GR
  ACTIVATE   SCREEN
  CLOSE DATA
RETURN