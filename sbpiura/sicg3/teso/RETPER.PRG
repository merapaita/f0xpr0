PARAMETERS newsistem
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RETPER.PRG  15/05/96                                         ³	
* ³ Cuadro de Retenciones                                        ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.     05/1997 ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
*-Abriendo archivos
USE parmae   IN 1  ORDER Parmae1  ALIAS parma 
USE Cajas    IN 2  ORDER Cajas1   ALIAS Caja
USE Reten    IN 3  ORDER Reten6   ALIAS Reten
USE Compag   IN 4  ORDER Compag1  ALIAS Compag
USE MaePre   IN 5  ORDER Maepre1  ALIAS Maepre
use auxil    IN 6  ORDER AUXIL1   ALIAS AUXIL
USE ASTPAT   IN 8  ORDER ASTPAT1  ALIAS ASTPAT
*---mensajes *------
vmens08 = 'No existe registros para procesar'
IF LASTKEY()=27
   CLOSE DATA
   RETURN
ENDIF
DO VerSal
close data
RETURN

PROCEDURE VerSal
*---------------
hide popup pop_01
sele reten
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp = RECNO()
 DEFINE WINDOW Lis FROM 5,5 TO 21,75 FLOAT DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW Lis
 STORE 1        TO vToCta,vTip
 store space(6) to vrub
 STORE DATE()   TO vFecIni, vFecFin
 @ 02,01 SAY "            Rubro : "
 @ 02,22 GET vrub  PICTURE "999999" valid VAL_PARA(alltrim(vrub),'CODRET',' ',22,40)
 @ 04,01 SAY " Fecha de Emisi¢n : "
 @ 04,22 GET vFecIni  PICTURE '@D' COLOR SCHEME 7
 @ 04,32 GET vFecFin  PICTURE '@D' VALID (vFecFin >= vFecIni) COLOR SCHEME 7
 @ 06,01 SAY "     Tipo Reporte : " GET vTip  FUNCTION '^ Tesorer¡a;Personal;Sunat'
 @ 13,18 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
 READ CYCLE

 RELEASE WINDOW Lis
 IF OkCancel = 1
    DEFINE WINDOW Xwait FROM 12,06 TO 14,78 COLOR SCHEME 05 
	ACTIVATE WINDOW Xwait
	@0,10 SAY " Espere un Momento...Reporte en Proceso!" COLOR W+/BR*
   SELECT Reten
   wind1=sys(3)+".IDX"
   wind2=sys(3)+".DBF"
   wind3=sys(3)+".IDX"
   wind4=sys(3)+".DBF"
   wind5=sys(3)+".IDX"
****
*@0,0 SAY " Espere un momento ......... " COLOR W+/BR*
vind1= SYS(3) +'.idx'
filaux = SYS(3) + '.DBF'
SELE 7
CREATE TABLE (filaux);
	  (TIP_DOC   C(001),;
	   NRO_DOC   C(015),;
	   APE_PAT	 C(020),;
	   APE_MAT   C(020),;
	   NOMBRES   C(020),;
	   FEC_NAC   D(008),;
	   NUMCP     C(004),;
	   CODCTC    C(014),;
	   NUMMES    C(002),;
	   MONTO     N(18,2),;
	   basei     N(18,2),;
	   SEXO      C(001))
USE 
USE (filaux) IN 15 EXCLUSIVE ALIAS Temp
*********
   SELE RETEN
   INDEX ON RETEN.CODRET TO (WIND1) FOR BETWEEN(FecPAG,vFecIni,vFecFin) AND ALLTRIM(VRUB)=ALLTRIM(RETEN.CODRET) AND ESTADO<>'99' AND LEFT(NUMCP,1)$'B0123456789'
   IF EOF()
		deactivate window xwait
        DO STANDBY with vmens08
   ELSE   
        SET RELA TO RETEN.NUMMES+RETEN.NUMCP+RETEN.CODCTC INTO COMPAG
        DO TempRET
	    DEACTIVATE WINDOW xwait
        sele temp
        do case
        	case vtip=1
        		DO REPORTE WITH 2,"Lirepe1"," Listado Retenciones por Rubro",2,.F.,.T.
        	case vtip=2
        		DO REPORTE WITH 2,"Lireper"," Listado Retenciones por Rubro",2,.F.,.T.
			case vtip=3
        		DO REPORTE WITH 2,"Lirepe3"," Listado Retenciones por Rubro",2,.F.,.T.
		endcase        		
   ENDIF
   set INDEX TO
 ENDIF
 SELECT reten
 GO vtemp
show popup pop_01
RETURN

FUNCTION TempRET
*---------------
DO standby WITH FILAUX
SCAN
	VNUMMES=NUMMES
  	VNUMCP=NUMCP
  	VCODCTC=CODCTC
  	MESPAGW=MESCPPG
  	NUMCPW=NUMCPPG
  	STORE VALRET TO VMONTO,VMONTO1
  	SELE COMPAG
  	SEEK vnummes+vnumcp+vcodctc
  	IF !EOF()
    	WNOMBRE=SPACE(20)
	  	vbase=import
    	tipow='  '
    	CODW=SPACE(6)
    	DO CASE
     		CASE tipprv="E"
   				tipow='30'
   				codw=codemp
     		CASE tipprv="P"
   				tipow='20'
   				codw=codprv
     		CASE tipprv="O"
   				tipow='09'
   				codw=codotr
     		CASE tipprv="I"
   				tipow='80'
   				codw=codpre
     		OTHERWISE
        		wnombre=nompre
		ENDCASE
		WTIP=TIPPRV
		sele auxil
		seek tipow+codw
		IF !EOF()
			wnombre=descri
			WNOMDOC=SUBSTR(OBSERV,1,15)
		endif
		* 
		WVAR=LEn(ALLTRIM(WNOMBRE))
		STORE 1 TO A,B,C
		STORE SPACE(20) TO PATW,MATW,NOMBW
		DO WHILE A < WVAR
			WPAR=SUBSTR(WNOMBRE,A,1)
			IF  WPAR=' '
				do case
					case c=1
						patw=SUBSTR(WNOMBRE,1,a-1)
					case c=2
						matw=substr(wnombre,((a+1)-b),B-1)						
						exit
				endcase
				C=C+1	
				b=0			
			endif
			b=b+1
			A=A+1
		enddo
		nombw=substr(wnombre,a+1,wvar-a)
		IF VRUB<>'150' .AND. VRUB<>'5082'
			SELE ASTPAT
			SEEK vnummes+vnumcp+vcodctc
			DO WHILE nummes=vnummes and numref=vnumcp and codctc=vcodctc
				IF SUBSTR(CODCTA,1,3)='409' 
					VMONTO=MTOHAB
				ENDIF
				SKIP
			ENDDO
		ENDIF
		IF (CODW<>'96510' AND CODW<>'000259' ) AND VCODCTC<>'110-01-2243861'
			SELECT Temp
    		IF F_appd()
			  replace TIP_DOC with '1'
			  replace APE_PAT with patw
			  replace APE_MAT with matw
			  replace NOMBRES with nombw
			  replace numcp with NUMCPW
			  replace nummes with MESPAGW
			  replace codctc with vcodctc
			  replace basei with vbase-VMONTO
			  replace nro_doc with wnomdoc
	  		replace monto with vMONTO1
*			  ELSE
*			  		REPLACE MONTO WITH VMONTO
*			  ENDIF
			endif	   
		ENDIF 
	endif
	sele reten
ENDSCAN
return


FUNCTION pre_pro1        &&Displaya descripci¢n del proveedor
*----------------
WNOMBRE=SPACE(20)
tipow='  '
DO CASE
   CASE tipprv="E"
   		tipow='30'
   		codw=codemp
   CASE tipprv="P"
   		tipow='20'
   		codw=codprv
   CASE tipprv="O"
   		tipow='09'
   		codw=codotr
   CASE tipprv="I"
   		tipow='80'
   		codw=codpre
   OTHERWISE
        wnombre=nompre
ENDCASE
sele auxil
seek tipow+codw
IF !EOF()
	wnombre=descri
endif
RETURN
