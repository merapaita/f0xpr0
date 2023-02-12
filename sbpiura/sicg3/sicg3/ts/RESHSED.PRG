* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ ResHSed.PRG        04/04/96                         L: 0246  ³	
* ³ Resumen de Hojas de Trabajo Sede                             ³
* ³ AUTOR : Julio C‚sar Cruz Ortiz           REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*---------------------------
*  vtipo = 1  --> PRP
*  vtipo = 2  --> TRN
*  vtipo = 3  --> SECTORIAL
*----------------------------

PARAMETERS vctacte,xano,vmes,vtipo

*-Abriendo archivos
USE MovBco   IN 3 	ORDER 	 Movbco1   ALIAS Movb
USE ComPag   IN 4 	ORDER 	 ComPag1   ALIAS ComPag
USE IteCp    IN 5 	ORDER 	 IteCp1    ALIAS IteCp
USE ItAuRe   IN 10   ORDER 	 ItAuRe1   ALIAS IteA
USE Calen    IN 11  ORDER    Calen1    ALIAS Calen
ON KEY LABEL F5 EXIT
Vmens08 = 'No hay registros para procesar'
vsubsidio=.F.
Xind2= SYS(3) +'.idx'
Xind1= SYS(3) +'.idx'

IF LASTKEY() # 27
	 DEFINE WINDOW Xwait FROM 12,06 TO 14,78 COLOR SCHEME 05 
	 ACTIVATE WINDOW Xwait
	 @0,10 SAY " Espere un Momento...Reporte en Proceso!" COLOR W+/BR*
	 xind1= SYS(3) +'IDX'
	 xind2= SYS(3) +'IDX'
	 SELECT Calen
	 INDEX ON codsubpr TO (xind2)  FOR nummes=vmes and codprg = IIF(vtipo = 2,'01','02') AND Codfte = 'TRN'
	 SELECT itea
	 INDEX ON Codpart TO (xind1)  FOR ALLTRIM(Ctcofg)=ALLTRIM(vctacte) ;
	 			AND Nummes = vMes
     vind1= SYS(3) +'.idx'
     vind2= SYS(3) +'.idx'
     vind3= SYS(3) +'.idx'
	filaux = SYS(3) + '.DBF'
	SELECT 7
	CREATE TABLE (filaux);
	  (Codprg C(3),prg C(50), Nummes c(2) ,p1 N(18,2), p2  N(18,2), p3 N(18,2), ;
	  	p4 N(18,2), p5 N(18,2),	p6 N(18,2), p7 N(18,2), p8 N(18,2), p9 N(18,2), ;
	  	p10 N(18,2), p11 N(18,2) ,flag n(1))
	USE 
	USE (filaux) IN 7 EXCLUSIVE ALIAS Temp
	 SELE ITECP
	 vind4 = SYS(3)+'IDX'
	 INDEX ON SUBSTR(Codcal,10,3) TO (VIND1) FOR CodCtC = vCtaCte .AND. ;
	 		nummes=vmes AND PERIODO=STR(XANO,2) AND Estado<>'99' 
     GO TOP
     if !eof()
        DO Tempcp
	 ELSE
        do standby with vmens08
        close data
        return
     ENDIF
	 SELE MOVB
	 INDEX ON SUBSTR(Codcal,10,3)+NUMERO TO (VIND2) FOR CodCtC = vCtaCte .AND. ;
	 		Nummes=vmes and periodo=str(xano,2) AND ESTADO<>'99' AND ;
	 		TIPDOC='N/C' AND (MOTIVO='MAN' OR MOTIVO='AJU')
	 GO TOP
	 IF !EOF()
	    DO proCnc	
	 ENDIF 
	 SELECT temp
	 xind4 = SYS(3)+'.idx'
	 GO TOP
	 deactivate window xwait
	 IF !EOF()
	 	DO CASE
	 		CASE vtipo = 2
				INDEX ON Flag TO (xind4)
	 			GO TOP
	 			DO REPORTE  WITH 2,"resHjtTe"," Hoja de Trabajo "
	 		CASE vtipo = 3
	 			DO REPORTE  WITH 2,"resHjtSe"," Hoja de Trabajo "
	 	ENDCASE
	 ELSE
	 	DO Standby WITH "No se encontraron datos"
	 ENDIF

 ENDIF
 SELECT COMPAG  
 SET INDEX TO
RELEASE    WINDOW lisprg
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


FUNCTION Tempcp
*---------------
SELECT Itecp
DO WHILE !EOF()
	vsubprg  = SUBSTR(itecp.Codcal,10,3)
	vcodpart = itecp.Codpart
    STORE 0 TO vp1,vp2,vp3,vp4,vp5,vp6,vp7,vp8,vp9,vp10,vp11 
    SCAN WHILE vsubprg = SUBSTR(Codcal,10,3)
		DO CASE
		   CASE ITECP.TIPFUN="F"
			DO CASE
			   CASE LEFT(ITECP.CODANAL,2)='01'
	    	        vp1=vp1+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='02'
		            vp2=vp2+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='03'
	        	    vp3=vp3+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='04'
		            vp4=vp4+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='05'
		            vp5=vp5+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='07'
		            vp7=vp7+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='08'
		            vp8=vp8+ITECP.impparc
    			CASE LEFT(itecp.CodANAL,2) = '09'
    				vp9 = vp9 + impparc
    			CASE LEFT(itecp.Codanal,2) = '10'
    				vp10 = vp10 + impparc
			   CASE LEFT(ITECP.CODANAL,2)='11'
		            vp11=vp11+ITECP.impparc
		   ENDCASE
		   CASE ITECP.TIPFUN="I"
			DO CASE
			   CASE LEFT(ITECP.codpart,2)='01'
    		        vp1=vp1+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='02'
	        	    vp2=vp2+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='03'
		            vp3=vp3+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='04'
	        	    vp4=vp4+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='05'
		            vp5=vp5+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='07'
	        	    vp7=vp7+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='08'
		            vp8=vp8+ITECP.impparc
    			CASE LEFT(itecp.Codpart,2) = '09'
    				vp9 = vp9 + impparc
    			CASE LEFT(itecp.Codpart,2) = '10'
    				vp10 = vp10 + impparc
			   CASE LEFT(ITECP.codpart,2)='11'
	        	    vp11=vp11+ITECP.impparc
	    	   ENDCASE     
		 ENDCASE
	ENDSCAN
    SELECT Temp
	IF F_APPD()
            REPLACE codprg  WITH vsubprg 
            REPLACE Prg WITH VAL_SUBP(vSubprg,'CODSUB'+PADR(IIF(vtipo=2,'01','02'),6,' '),'D')
            REPLACE Nummes WITH vmes ,;
            		p1  WITH vp1 ,;
            		p2  WITH vp2 ,;
            		p3  WITH vp3 ,;
            		p4  WITH vp4 ,;
            		p5  WITH vp5 ,;
            		p6  WITH vp6 ,;
            		p7  WITH vp7 ,;
            		p8  WITH vp8 ,;
            		p9  WITH vp9 ,;
            		p10  WITH vp10 ,;
            		p11  WITH vp11 ,;
            		flag WITH IIF(vsubprg = '002',2,IIF(EMPTY(vsubprg),3,1))
     ENDIF
     SELECT ITECP
ENDDO
RETURN


FUNCTION PROCnc
*---------------

SCAN
	vsubprg  = SUBSTR(Codcal,10,3)
	STORE 0 TO vp1,vp2,vp3,vp4,vp5,vp6,vp7,vp8,vp9,vp10,vp11 
    SCAN WHILE vsubprg = SUBSTR(Codcal,10,3)
   	DO CASE 
   		CASE LEFT(Codpart,2) = '01'
   			vp1 = vp1 + Monto
   		CASE LEFT(Codpart,2) = '02'
   			vp2 = vp2 + Monto
   		CASE LEFT(Codpart,2) = '03'
   			vp3 = vp3 + Monto
   		CASE LEFT(Codpart,2) = '04'
   			vp4 = vp4 + Monto
   		CASE LEFT(Codpart,2) = '05'
   			vp5 = vp5 + Monto
   		CASE LEFT(Codpart,2) = '06'
   			vp6 = vp6 + Monto
   		CASE LEFT(Codpart,2) = '07'
   			vp7 = vp7 + Monto
   		CASE LEFT(Codpart,2) = '08'
   			vp8 = vp8 + Monto
   		CASE LEFT(Codpart,2) = '09'
   			vp9 = vp9 + Monto
   		CASE LEFT(Codpart,2) = '10'
   			vp10 = vp10 + Monto
   		CASE LEFT(Codpart,2) = '11'
   			vp11 = vp11 + Monto
	ENDCASE    	
	ENDSCAN
    SELECT Temp
	IF F_APPD()
            REPLACE Codprg WITH vsubprg
            REPLACE Prg    WITH VAL_SUBP(vSubprg,'CODSUB'+PADR(IIF(vtipo=2,'01','02'),6,' '),'D')
            REPLACE Nummes WITH vmes ,;
            		p1  WITH vp1 ,;
            		p2  WITH vp2 ,;
            		p3  WITH vp3 ,;
            		p4  WITH vp4 ,;
            		p5  WITH vp5 ,;
            		p6  WITH vp6 ,;
            		p7  WITH vp7 ,;
            		p8  WITH vp8 ,;
            		p9  WITH vp9 ,;
            		p10  WITH vp10 ,;
            		p11  WITH vp11 ,;
            		flag WITH IIF(vsubprg = '002',2,1)
     ENDIF
     SELECT Movb
ENDSCAN
RETURN

PROCEDURE Totpart
*-----------------
PARAMETERS vpart
SELECT Itea
SEEK PADL(vpart,2,'0')+'.00'
SUM monasig TO vtotaut WHILE LEFT(Codpart,2) = PADL(vpart,2,'0')
SELECT temp
RETURN vtotaut

FUNCTION TAUTSUB
*----------------
PARAMETERS vcodprg
SELECT Calen
vcond = IIF(vcodprg = '002' ,'codsubpr = vcodprg','codsubpr # "002"')
SUM ALL valpart TO  vtotcal FOR &vcond AND RIGHT(Codpart,2) # '00' ;
		AND LEFT(Codpart,2) = 
SELECT temp
RETURN vtotcal