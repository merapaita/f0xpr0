PARAMETERS newsistem
* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ FDOGAR.PRG  15/05/96                                         ณ	
* ณ Fondos de Garantกa                                           ณ
* ณ AUTOR   : Pilar Pacherre Navaarro          REGION GRAU       ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
*-Abriendo archivos
USE parmae   IN 1  ORDER Parmae1  ALIAS parma 
USE Cajas    IN 2  ORDER Cajas1   ALIAS Caja
USE ASTPAT   IN 3  ORDER ASTPAT3   ALIAS ASTPAT
USE Compag   IN 4  ORDER Compag1  ALIAS Compag
USE Cheque   IN 5  ORDER TAG Cheque1      ALIAS Cheque

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
sele ASTPAT
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp = RECNO()
 DEFINE WINDOW fdogar FROM 5,5 TO 21,75 FLOAT DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW fdogar
 STORE 1        TO vToCta
 STORE SPACE(3) TO VCTAC
 STORE SPACE(14) TO vCta
 store space(20) to wtit
 STORE DATE()   TO vFecIni, vFecFin
 @ 00,01 SAY "   Todas las Ctas : " GET vToCta  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToCta,3,22)
 @ 02,01 SAY "   Cta. Corriente : "
 @ 02,22 GET vCta   WHEN vToCta=2 VALID Val_Fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,2,22)
 @ 04,01 SAY "  Cuenta Contable : " 
* GET vctac FUNCTION '^ 429;427;401;409;403'
 @ 04,22 GET VCTAC PICTURE '999' 
 @ 10,01 SAY " Fecha de Emisiขn : "
 @ 10,22 GET vFecIni  PICTURE '@D' COLOR SCHEME 7
 @ 10,32 GET vFecFin  PICTURE '@D' VALID (vFecFin >= vFecIni) COLOR SCHEME 7
 @ 13,18 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
 READ CYCLE

 RELEASE WINDOW fdogar
 IF OkCancel = 1
    DEFINE WINDOW Xwait FROM 12,06 TO 14,78 COLOR SCHEME 05 
	ACTIVATE WINDOW Xwait
	@0,10 SAY " Espere un Momento...Reporte en Proceso!" COLOR W+/BR*
   SELECT astpat
   wind1=sys(3)+".IDX"
  wind2=sys(3)+".DBF"
  wind3=sys(3)+".IDX"
   wind4=sys(3)+".DBF"
   wind5=sys(3)+".IDX"
    SELE astpat
    INDEX ON astpat.CODCTC+SUBSTR(astpat.CODCTA,1,3)+ASTPAT.NUMREF TO (WIND1) FOR BETWEEN(Fecha,vFecIni,vFecfin) and SUBSTR(CODCTA,1,3)=VCTAC AND LEFT(NUMREF,1)$'B0123456789' AND ASTPAT.TIPDOC='C/P'
    deactivate window xwait
    DO REPPRG  WITH "LISRETRU"," Arqueo de Retenciones "
    set INDEX TO
 ENDIF
DO SALIDA 
RETURN

PROCEDURE LISRETRU
*----------------
PARAMETERS XCOP
PRIVATE FILA,VRET,R1,RT,RP,E1,ET,EP,VCTC
STORE 0 TO R1,RT,E1,ET,EP,RP,PGW
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
@1,1 SAY CHR(15)
fila=8
PGW=1
*SELE ASTPAT
GO TOP
IF VTOCTA=2
	SEEK VCTA
ELSE
	VCTA=codctc
endif		
*DO imp_headRUB
SCAN WHILE !EOF()
	vCTA=ASTPAT.CODCTC
*	IF SW=2
*		FILA=FILA+1
*		@FILA,04  say "CUENTA CORRIENTE : "
*		@FILA,35 SAY val_fun('Caja','codctc',"ALLTRIM(CODCTC)+' '+DESCRI",CODCTC)
*		FILA=FILA+2
*	endif		
	DO imp_headRUB	
	DO WHILE ASTPAT.CODCTC=vCTA AND NOT EOF()
	  VKEY=ASTPAT.NUMMES+ASTPAT.NUMref+ASTPAT.CODCTC
	  if ASTPAT.Tipcta='H'
	     @FILA,2 SAY ASTPAT.numREF+'.'+ASTPAT.NUMMES
		 SELE CHEQUE
		 SEEK VKEY
		 @FILA,10 SAY SUBSTR(CHEQUE.NOMGIR,1,40)	
		 SELE ASTPAT
		 @FILA,51 SAY ASTPAT.mtoHAB PICTURE "@Z 999,999,999.99"
		 FILA=FILA+1
		 SELE COMPAG
		 SEEK ALLTRIM(ASTPAT.NUMMES)+ASTPAT.NUMREF+ALLTRIM(ASTPAT.CODCTC)
		 @FILA,10 SAY SUBST(COMPAG.destino,1,40)	
		 FILA=FILA+1
		 @FILA,10 SAY SUBST(COMPAG.destino,41,40)	
		 SELE ASTPAT
		 R1=R1+ASTPAT.mtoHAB
	  Else	
		@FILA,70 SAY ASTPAT.numref+'.'+ASTPAT.NUMMES
        SELE CHEQUE
		SEEK VKEY
		@FILA,78 SAY SUBSTR(CHEQUE.NOMGIR,1,40)	
		SELE ASTPAT
		@FILA,119 SAY ASTPAT.mtoDEB PICTURE "@Z 999,999,999.99"
		E1=E1+ASTPAT.mtoDEB
	  endif
	  FILA=FILA+1
	  IF FILA>=60
		FILA=8
		DO IMP_HEADRUB
      ENDIF   
	  SKIP
    ENDDO
    IF VTOCTA=2
    	GO BOTT
	ELSE    	
	    SKIP -1
	    sw=2
	ENDIF	    
	IF FILA>=60
		FILA=8
		DO IMP_HEADRUB
	ENDIF   
ENDSCAN
FILA=FILA+1
@FILA,2 SAY REPLICATE("ฤ",135)
FILA=FILA+1
@FILA,10 SAY "TOTAL RETENIDO =>"
@FILA,51 SAY R1 PICTURE "@Z 999,999,999.99"
@FILA,78 SAY "TOTAL EJECUTADO =>"
@FILA,119 SAY E1 PICTURE "@Z 999,999,999.99"
FILA=FILA+1
@FILA,2 SAY REPLICATE("ฤ",135)
FILA=FILA+1
iF FILA=60
	FILA=8
	DO IMP_HEADRUB
ENDIF   
SET DEVICE TO SCREEN 
RETURN




PROCEDURE imp_headRUB
*--------------------
@1,3   SAY ALLTRIM(CIA)
@1,110 say "PAG:"
@1,117 SAY  PGW PICT '9999'
PGW=PGW+1
@2,3   SAY "CuaretFON"
@2,50  say 'CONCILIACION DE : ' + VCTAC
@2,110 SAY "FECHA:"
@2,117 SAY DATE()           
@3,50  say 'DEL '+DTOC(VFECINI)+'  AL  '+DTOC(VFECFIN)
@4,15  say "CUENTA CORRIENTE : "
@4,46 SAY val_fun('Caja','codctc',"ALLTRIM(CODCTC)+' '+DESCRI",CODCTC)
*@04,51 SAY val_fun('Caja','codctc','Descri',ALLTRIM(VCTA))
@5,2 SAY REPLICATE("ฤ",135)
@6,3 SAY " C/P           DETALLE                                        MONTO      C/P            DETALLE                                MONTO"
@7,2 SAY REPLICATE("ฤ",135) 
RETURN

PROCEDURE Salida
*---------------
RELEASE  WINDOW FDOGAR
ACTIVATE SCREEN
CLOSE    DATABASES
RETURN
