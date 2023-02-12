*------------------------------------------------------------------------
* REGHC.Prg
* LISTA NOTA DE CONTABILIDAD
* Autor: Luis Castillo Dur n
* FECHA: 15/02/95
*--------------------------------------------------------------------------
USE astpre   IN 1  order tag Astpre8  ALIAS AstPre
USE Parmae   IN 2  order tag Parmae1  ALIAS Parma
USE Cuentas  IN 3  order tag Cuentas1 ALIAS Cuenta
use hojcon   in 4  order tag Hojcon1 alias hoja

*- Mensajes de aviso al usuario

 Vmens08 = 'No hay registros para procesar'

PUBLIC VKEY,VCODPRG
SELECT ASTPRE
set relation to nummes+numref into hoja
vcodprg='  '
vperiodo='  '
vmes='  '
 DEFINE WINDOW LIS_1 FROM 10,13 TO 16,67 DOUBLE ;
 TITLE ' °° Listado Nota de Contabilidad °° ' FLOAT COLOR SCHEME 5
 ACTIVATE WINDOW LIS_1
 @  1,2 Say '  Periodo : ' GET vPeriodo PICTURE '!!'  VALID !empty(vperiodo)
 @  2,2 Say ' Programa : ' GET vCodPrg  PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',15,30)
 @  3,2 Say '      Mes : ' GET vMes     PICTURE '!!'   VALID VAL_PARA(vMes   ,'FECMES',' ',15,30)
 READ
 DEACTIVATE WINDOW LIS_1
 if lastkey()= 27
      CLOSE DATA
      return
 endif

 if eof()
      do standby with vmens08
 Else
     ACTIVATE WINDOW STANDBY
     @ 1,14 SAY 'Espere un Momento ....' color W*
     *-------------------
     *-------------------
     vInd = sys(3)+".dbf"
 	 copy to (vInd) for SUBSTR(CODCAL,8,2)=ALLTRIM(vCODprg) and NumMes = ALLTRIM(vMes) and hoja.estado#'99' and periodo=alltrim(vperiodo) 
 	 DEACTIVATE WINDOW STANDBY
	 use (vind) in 4 alias vtempo
	 SELECT vtempo
	 GO TOP
     do reporte with 2,"NotCon",' Notas de Contabilidad '
     USE
     *rase vtempo.DBF
 endif
 close data
 return

PROCEDURE SUMADEB
*----------------
RC=RECNO()
vKEY=TIPO+LEFT(CUENTA,2)
sum VALdeb for TIPO+LEFT(CUENTA,2) = Vkey to suma
go rc
return suma

PROCEDURE xSUMADEB
*----------------
RC=RECNO()
vKEY=TIPO+LEFT(CUENTA,2)
SET FILTER TO TIPO+LEFT(CUENTA,2) = Vkey
sum VALdeb to suma
SET FILTER TO
go rc
return suma

PROCEDURE SUMAhab
*----------------

RC=RECNO()
vKEY=TIPO+LEFT(CUENTA,2)
sum VALhab for TIPO+LEFT(CUENTA,2) = Vkey to suma
go rc
return suma

PROCEDURE xSUMAhab
*----------------

RC=RECNO()
vKEY=TIPO+LEFT(CUENTA,2)
SET FILTER TO TIPO+LEFT(CUENTA,2) = Vkey
sum VALhab to suma
SET FILT TO
go rc
return suma

PROCEDURE VALDEB3
*----------------
RC=RECNO()
vKEY=tipo+LEFT(CUENTA,3)
sum VALdeb for tipo+LEFT(CUENTA,3) = Vkey to suma
go rc
return suma

PROCEDURE xVALDEB3
*----------------
RC=RECNO()
vKEY=TIPO+LEFT(CUENTA,3)
SET FILTER TO TIPO+LEFT(CUENTA,3) = Vkey
sum VALdeb to suma
SET FILTER TO
go rc
return suma

PROCEDURE VALHAB3
*----------------
RC=RECNO()
vKEY=tipo+LEFT(CUENTA,3)
sum VALhab for tipo+LEFT(CUENTA,3) = Vkey to suma
go rc
return suma


PROCEDURE xVALHAB3
*----------------

RC=RECNO()
vKEY=TIPO+LEFT(CUENTA,3)
SET FILTER TO TIPO+LEFT(CUENTA,3) = Vkey
sum VALhab to suma
SET FILT TO
go rc
return suma
