*--------------------------------------------------------------------------
* REGHC.Prg
* LISTA NOTA DE CONTABILIDAD
* Autor: Luis Castillo Dur n
* FECHA: 15/02/95
*--------------------------------------------------------------------------
USE HojCon   IN 1   order tag HojCon1  ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1   ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE OrdCom   IN 5   order tag OrdCom1  ALIAS Orden
USE IteOc    IN 6   order tag IteOc1   ALIAS IteOc
USE Promae   IN 7   order tag Promae1  ALIAS Promae
USE Calen    IN 8   order tag calen1   ALIAS calen
USE Iteart   IN 9   order tag Iteart1  ALIAS iteart
USE Clase    IN 10  order tag Clase1   ALIAS Clase
USE Ordser   IN 11  order tag OrdSer1  ALIAS OrdSer
USE Cuentas  IN 12  order tag Cuentas1 ALIAS Cuenta
USE astpre   IN 13  order tag Astpre4  ALIAS AstPre
*- Mensajes de aviso al usuario

 Vmens01 = ' Hoja de Afectaci¢n : REVISION '
 Vmens02 = ' Registro de Hoja de Afectaci¢n '
 Vmens04 = 'Dicho Hoja de Afectaci¢n no fue encontrado'
 Vmens05 = 'No existe Hoja de afectaci¢n anterior'
 Vmens06 = 'No existe Hoja de afectaci¢n siguiente'
 Vmens07 = '¨ Desea ANULAR ‚ste Hoja de afectaci¢n ?'
 Vmens08 = 'No hay registros para procesar'
 Vmens09 = 'Esta Hoja de Afectaci¢n ha sido anulada'
 Vmens10 = 'Este Hoja de afectaci¢n ya fue atendida'
 Vmens11 = 'Este Hoja de Afectaci¢n ha sido devuelto'

PUBLIC VKEY,VCODPRG
SELECT ASTPRE
vcodprg='  '
vmes='  '
 DEFINE WINDOW LIS_1 FROM 10,13 TO 15,67 DOUBLE ;
 TITLE ' °° Listado Nota de Contabilidad °° ' FLOAT COLOR SCHEME 5
 ACTIVATE WINDOW LIS_1
 @  1,2 Say ' Programa : ' GET vCodPrg  PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',15,30)
 @  2,2 Say '      Mes : ' GET vMes     PICTURE '!!'   VALID VAL_PARA(vMes   ,'FECMES',' ',15,30)
 READ
 DEACTIVATE WINDOW LIS_1
 if lastkey()= 27
      return
 endif

 if eof()
      do standby with vmens08
 Else
     SET FILTER TO SUBSTR(CODCAL,8,2)=ALLTRIM(vCODprg) and NumMes = ALLTRIM(vMes)
     GO TOP

     do reporte with 2,"NotCon",' Notas de Contabilidad '
     set filter to
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
sum VALhab for LEFT(CUENTA,2) = Vkey to suma
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