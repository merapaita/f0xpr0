*-----------------------------------------------------------
* INVFIS.PRG
* REPORTE DE INVENTARIO FISICO FINAL Y BALANCE DE INVENTARIO
* AUTOR : UDEP Virna Velaquez 
*------------------------------------------------------------
USE Saldona IN 1 ORDER TAG Saldona1 ALIAS Saln
USE Cuentas IN 2 ORDER TAG Cuentas1 ALIAS Cuen
USE PARMAE  IN 4 ORDER TAG Parmae1  ALIAS Parma
PUBLIC vNivint,vmes1,vmes2,vano,vsedpli,vinv,MTO,MTOTOT,Valor,totalA,TotalP,vdsaldona,vhsaldona,Monto
filaux = SYS(3) + '.DBF'
MTO= 0
Valor = 0
negativo = .F.
Vmens01  = 'Inventario F¡sico'
totala  = 0
totalp  = 0
monto   = 0
DO INICIA
IF LASTKEY()#27
	DO PREGUNTA
	DO Fin_Opcion
ENDIF
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 DEFINE WINDOW Wind_0 FROM 10,12 TO 18,62 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 5
 ACTIVATE SCREEN
 RETURN


PROCEDURE Pregunta
*-----------------
ACTIVATE WINDOW Wind_0
vsedpli=SPACE(13)
xmens  = ''
vano   = SUBSTR(ALLT(str(year(date()))),3,2)
vmes1  = PADL(ALLT(STR(MONTH(date()),2)),2,'0')
vmes2  = SPACE(2)
vInv   = SPACE(2)
vNivint= SPACE(2)
vformato =SPACE(1)
@ 0,1  SAY ' Formato                     : ' GET vformato PICTURE "@m F,B"
@ 1,1  SAY ' A¤o                         : ' GET vano   
@ 2,1  SAY ' Desde - Mes1 a Mes2         : ' GET vmes1
@ 2,42 GET vMes2

@ 5,8 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

IF OKCANCEL=1
		IF ALLTRIM(VFormato)= 'F'
			DO ESPERA WITH 1,'Procesando, espere un momento ...'
			DO PROCESO
		ELSE
			DO ESPERA WITH 1,'Procesando, espere un momento ...'
			DO MoInvFis
		ENDIF			
ENDIF
RETURN


PROCEDURE Proceso
*----------------

SELECT 8
CREATE TABLE (filaux);
  (CUENTA      C(10),;                  
   MES         N(02,0), ;                 
   ANO         N(02,0), ;                 
   FILE        N(02,0), ;                 
   DSUMANA     N(18,4),;           
   HSUMANA     N(18,4),;           
   DSALDONA    N(18,4),;           
   HSALDONA    N(18,4),;
   DSUMAJU     N(18,4),;
   HSUMAJU     N(18,4),;
   DSALDOAJU   N(18,4),;
   HSALDOAJU   N(18,4),;
   DAJUMES     N(18,4),;
   HAJUMES     N(18,4),;
   DAJUANO     N(18,4),;
   HAJUANO     N(18,4),;
   VALMERC     N(18,4),;
   VALLIMI     N(18,4),;
   ACTPAS      C(10),;
   TIPO        C(20))
 USE
USE (filaux)  IN  3 EXCLUSIVE   ALIAS Temp
SELE Saln
SET FILTER TO (BETWEEN(PADL(ALLT(STR(MES)),2,'0'),vMes1,vmes2)) AND (ALLTRIM(STR(Ano))=vAno) AND (RIGHT(Cuenta,8)='00000000') AND (CUENTA<'60')
GO TOP
SCAN
	vcuenta1=cuenta
	vcuenta2=cuenta
	vdsaldona=0
	vhsaldona=0
	DO WHILE vcuenta1 = vcuenta2
		* vdsaldona =vdsaldona + dsaldona+dajuano
		* vhsaldona =vhsaldona + hsaldona+hajuano
		*-----
		* aqui hemos cambiado los campos 
		*-------------------------------------
		vdsaldona =vdsaldona + dsumana+dajumes
		vhsaldona =vhsaldona + hsumana+hajumes
		
		SKIP
		vcuenta2 =cuenta
	ENDDO
	SKIP -1
	SCATTER MEMVAR
	SELECT Temp
	APPEND BLANK
	GATHER MEMVAR
	IF CUENTA<'40'
	   IF Cuenta='3000000000'
		   Monto = (VDSALDONA-VHSALDONA)
	   ElSE
		   REPLACE DSALDONA WITH VDSALDONA-VHSALDONA
	   ENDIF
	ELSE
	   IF CUENTA = '5600000000'
	   		REPLACE DSALDONA WITH (VHSALDONA-VDSALDONA)-MONTO
	   ELSE
	   		IF Cuenta ='5900000000'
		   		REPLACE DSALDONA WITH (VHSALDONA-VDSALDONA) + (DEFICIT(vano))
			ELSE
		   		REPLACE DSALDONA WITH (VHSALDONA-VDSALDONA)
			ENDIF
	   ENDIF
	ENDIF   
	SELECT SALN
ENDSCAN	
SET FILTER TO
DO Recorre
SELE Temp
INDEX ON actpas+cuenta TAG Temp1
GO TOP
DO ESPERA WITH 2
DO REPORTE WITH 2, "InvFis1", ' Inventario Final ',1,.F.,.T.
RETURN

PROCEDURE Recorre
*-----------------
SELE Temp
GO TOP
SCAN
	DO CASE
		CASE BETWEEN(SUBSTR(Cuenta,1,2),'10','29')
			REPLACE ActPas  WITH 'ACTIVO'
			REPLACE Tipo    WITH 'ACTIVO CORRIENTE'	
		CASE BETWEEN(SUBSTR(Cuenta,1,2),'30','39')			
			REPLACE ActPas WITH 'ACTIVO'
			REPLACE Tipo WITH 'ACTIVO NO CORRIENTE'	
		CASE BETWEEN(SUBSTR(Cuenta,1,2),'40','48')			
			REPLACE ActPas WITH 'PASIVO'
			REPLACE Tipo WITH 'PASIVO CORRIENTE'	
		CASE BETWEEN(SUBSTR(Cuenta,1,2),'50','53')			
			REPLACE ActPas WITH 'PASIVO'
			REPLACE Tipo WITH 'PASIVO NO CORRIENTE'	
		CASE BETWEEN(SUBSTR(Cuenta,1,2),'54','59')			
			REPLACE ActPas WITH 'PATRIMONIO'
			REPLACE Tipo WITH 'PATRIMONIO'
	ENDCASE	
ENDSCAN
RETURN

FUNCTION bus_cta
*----------------
PARAMETERS vcuenta 
vAlias = SELECT()
SELECT Cuen
SEEK vcuenta 
IF FOUND()
   SELE(VAlias)
   RETURN  ALLTRIM(Cuen.descri) 
ELSE
   SELE(VAlias)
   RETURN  'Cuenta no Existe'    
ENDIF
RETURN 


FUNCTION TOTAL
*------------
SUM dsaldona TO Valor for actpas='PASIVO' OR actpas='PATRIMONIO'
RETURN Valor


PROCEDURE Fin_Opcion
*-------------------
CLOS DATA
ERASE (FILAUX)
DEACTIVATE WINDOW WIND_0
RETURN


FUNCTION Deficit
*---------------
PARAMETER VANO
VALI=ALIAS()
VORD=ORDER()
SELE PARMA
SET ORDE TO PARMAE1
SEEK 'OPERAT'+'DS'+vano
IF FOUND()
	Valor =  Nument
ENDIF
SELE (VALI)
SET ORDER TO VORD
RETURN Valor

