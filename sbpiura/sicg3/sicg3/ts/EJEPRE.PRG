* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ EjePre.PRG      30/04/96                            L: 0242  ณ	
* ณ Reporte de ejecuciขn Presupuestal                            ณ
* ณ AUTOR : Julio Cruz Ortกz                 REGION GRAU - 1996  ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

*- Abre archivos

USE ItAuRe  IN  2  ORDER TAG ItAuRe1                ALIAS IteA
USE Clase   IN  3  ORDER TAG Clase1                 ALIAS Clase
USE Parmae  IN  4  ORDER TAG Parmae1                ALIAS Parma
USE Calen   IN  5  ORDER TAG Calen1                 ALIAS Calen
USE Cajas   IN  9  ORDER TAG Cajas1                 ALIAS Caja
USE IteCp   IN  11 ORDER TAG IteCp1    				ALIAS IteCp
USE Itehc   IN  12 ORDER TAG Itehc1					ALIAS Itehc
USE Hojcon  IN  13 ORDER TAG HojCon1				ALIAS Hoja 
USE MovBco  IN  14 ORDER TAG MovBco1				ALIAS Movb 

*- Declaraciขn de variables
PUBLIC vcodctc,xmes,xano
vind4= SYS(3)+'.IDX'
vind3= SYS(3)+'.IDX'
vind2= SYS(3)+'.IDX'
vind1= SYS(3)+'.IDX'
vind = SYS(3)+'.IDX'
vind5= SYS(3)+'.IDX'
vind6= SYS(3)+'.IDX'
xMes     = PADL(MONTH(DATE()),2,'0')
xAno     = VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
vcodctc  = SPACE(14)
filaux   = SYS(3) + '.DBF'

SELECT 8
CREATE TABLE (filaux);
  (Nummes    C(2)  ,;
   CodOfg    C(2),;
   Codctc    C(12),;
   CodPrg    C(2),;
   CodPart   C(5),;
   CodScta   C(8),;
   mtocal    N(18,2),;
   mtocom    N(18,2),;
   mtoaut    N(18,2),;
   mtoman    N(18,2),;
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
    DEFINE WINDOW EjePre   FROM 14,42 TO 18,78  SHADOW COLOR &vcol
  ELSE
    DEFINE WINDOW EjePre   FROM 14,42 TO 18,78  COLOR SCHEME C_POPUP
  ENDIF
  ACTIVATE WINDOW EjePre  
 SELECT Itea
 @ 0, 0 SAY '       Ejecuciขn Presupuestal '
 @ 2, 0 SAY '     Cta.Cte. :' GET vcodctc  PICTURE '@!' VALID Val_Fun('Caja','CodCtc','codctc+descri',vCodctc,1,2,16,'descri',27)
 READ
 DEACTIVATE WINDOW ejepre
 IF LASTKEY()#27
 		DEFINE WINDOW Xwait FROM 20,35 TO 22,75 COLOR SCHEME 5 
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando informaciขn para reporte...." COLOR W+/BR*

	 	SELE ItecP
	 	INDEX ON nummes+LEFT(CodPart,2) TO (Vind2) FOR SUBST(codcal,5,3) ;
	 				= 'TRN' AND Codctc = vcodctc AND Estado<>'99' AND VAL(NUMMES)<=6
	 	INDEX ON nummes+LEFT(codanal,2) TO (Vind4) FOR SUBST(codcal,5,3) ;
	 				= 'TRN' AND Codctc = vcodctc AND Estado<>'99' AND VAL(NUMMES)<=6
		GO TOP

	 	SELE Movb 
	 	INDEX ON nummes+LEFT(CodPart,2) TO (Vind6) FOR SUBST(codcal,5,3) ;
	 				= 'TRN' AND Codctc = vcodctc AND Estado<>'99' AND VAL(NUMMES)<=6
		
		SELECT Calen
		INDEX ON nummes+Codprg+codpart TO (vind1) FOR codfte = 'TRN' AND VAL(NUMMES)<=6
		GO TOP

		SELECT DISTINCT it.*,Hc.CODCTC ;
	    FROM IteHC It, Hoja Hc;
	    WHERE (It.NumMes+It.Numhc=Hc.NumMes+Hc.NumHc) ;
 	    AND ((Hc.codctc=vcodctc)  AND (Hc.estado='50' OR hc.tipdoc='N/C') AND (It.estado#"99"));
		INTO TABLE ithc
		
		SELE ithc
		GO  TOP    
		SCAN
			DO CASE
				CASE Ithc.estado='94' 
	    		   REPLACE Ithc.valpart WITH Ithc.Valpart*-1	   	   
				CASE Ithc.estado='92'  
		           IF !EMPTY(Ithc.numhm) AND Ithc.Nummeshm=PADL(Ithc.nummes,2,'0')
			       	  IF Ithc.tipope='-'
				          REPLACE Ithc.valpart WITH Ithc.Valpart*-1	   	   
			    	  ENDIF
				   ELSE
				      REPLACE Ithc.valpart WITH 0
				   ENDIF
			    CASE Ithc.estado='00'  
			       IF !EMPTY(Ithc.numPa) AND Ithc.mesPa=PADL(ithc.nummes,2,'0')
			          REPLACE Ithc.valpart WITH Ithc.Valpart*-1	   	   
				   ENDIF
			   ENDCASE  
		ENDSCAN   

		SELECT Ithc
	 	INDEX ON nummes+LEFT(CodPart,2) TO (VIND3) FOR  VAL(NUMMES)<=6
	 	INDEX ON nummes+LEFT(codanal,2) TO (VIND5) FOR  VAL(NUMMES)<=6
	 	GO TOP
    
		SELECT IteA
		INDEX ON nummes+codpart TO (vind) FOR ALLTRIM(Ctcofg)=ALLTRIM(vcodctC) AND VAL(NUMMES)<=6
		GO TOP
	    DO VALORES
        SELEC Temp
        GO TOP

 	   DEACTIVATE WINDOW Xwait
 	   IF !EOF()
       	DO REPORTE WITH 2, "EJEPRE", ' Ejecuciขn Presupuestal ',1,.F.,.T.
       ELSE
       	DO STANDBY WITH "No se encontraron datos"
       ENDIF
       SELECT t.codpart, SUM(t.mtocal) as totcal, SUM(t.mtoaut) as totaut,;
       		  SUM(t.mtogir) as totgir,SUM(t.mtocom) as totcom,SUM(t.mtoman) as totman  ;
       FROM Temp t ;
       GROUP BY codpart ;
       INTO CURSOR temp1

       SELECT Temp1
       IF !EOF()
       	DO REPORTE WITH 2, "resejepre", ' Anexo 1 GR ',1,.F.,.T.
       ELSE
       	DO STANDBY WITH "No se encontraron datos"
       ENDIF
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

DO WHILE !EOF()
	vcodprg = itea.codprg
	vcodpart= itea.codpart
	vcodofg = itea.codofg
	vnummes = itea.nummes
	vcodscta= itea.codscta
	vctcofg = itea.ctcofg
	SUM Monasig TO vmtoaut WHILE nummes+codpart = vnummes+vcodpart
	SELECT Calen
	GO TOP
	IF(VAL(LEFT(vCodpart,2))<6)
		SEEK vnummes+vcodprg+vcodpart
	ELSE
		LOCATE FOR nummes+codprg+LEFT(Codpart,2) = vnummes+vcodprg+LEFT(vCodpart,2)
	ENDIF
	vcond  = 'nummes+codprg+codpart = vnummes+vcodprg+vcodpart'
	vcond1 = 'nummes+codprg+LEFT(codpart,2) = vnummes+vcodprg+LEFT(vcodpart,2)'
	SUM Valpart TO vmtocal WHILE IIF(VAL(LEFT(vCodpart,2))>5,&vcond1,&vcond)

	SELECT itecp
	IF VAL(LEFT(vcodpart,2))>5 AND VAL(LEFT(vcodpart,2))<21 
		SET INDEX TO (vind2)
		SEEK vnummes+LEFT(vcodpart,2)
		SUM impparc TO vmtogir WHILE nummes+LEFT(codpart,2) = ;
									vnummes+LEFT(vcodpart,2)
	ELSE
		SET INDEX TO (vind4)
		SEEK vnummes+LEFT(vcodpart,2)
		SUM impparc TO vmtogir WHILE nummes+LEFT(codanal,2) = ;
									vnummes+LEFT(vcodpart,2)
	ENDIF

	SELECT Movb 
	SEEK vnummes+LEFT(vcodpart,2)
	SUM monto TO vmtoman WHILE nummes+LEFT(codpart,2) = ;
									vnummes+LEFT(vcodpart,2)

	SELECT Ithc
	IF VAL(LEFT(vcodpart,2))>5 AND VAL(LEFT(vcodpart,2))<21 
		SET INDEX TO (vind3)
		SEEK vnummes+LEFT(VCodpart,2)
		SUM Valpart TO vmtocom WHILE nummes+LEFT(codpart,2) = ;
									vnummes+LEFT(vcodpart,2)
	ELSE								
		SET INDEX TO (vind5)
		SEEK vnummes+LEFT(VCodpart,2)
		SUM Valpart TO vmtocom WHILE nummes+LEFT(codanal,2) = ;
									vnummes+LEFT(vcodpart,2)
	ENDIF			
	SELECT Temp
	APPEND BLANK
	REPLACE nummes  WITH vnummes  ;
			codofg  WITH vcodofg ;
			codctc  WITH vctcofg ;
			codprg  WITH vcodprg ;
			codpart WITH vcodpart ;
			codscta WITH vcodscta ;
			mtocal  WITH vmtocal ;
			mtocom  WITH vmtocom ;
			mtoaut  WITH vmtoaut ;
			mtoman  WITH vmtoman ;
			mtogir  WITH vmtogir 
	SELECT Itea
ENDDO

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