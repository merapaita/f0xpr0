*------------------------------------------------------------------------
* Notcon2.Prg
* Version 3.11 
* Listado de Nota de Contabilidad
* FECHA: 10/05/96
* Autor : FMC
*--------------------------------------------------------------------------
USE astpre   IN 1  order tag Astpre8  ALIAS AstPre
USE Parmae   IN 2  order tag Parmae1  ALIAS Parma
USE Compag   IN 5  order tag Compag1  ALIAS Compag
USE Cuentas  IN 3  order tag Cuentas1 ALIAS Cuenta
USE Hojcon   in 4  order tag Hojcon1  ALIAS hoja
USE Recing   in 6  order tag Recing1  ALIAS Recing
USE Hojmod   in 7  order tag HojMod1  ALIAS Hojmod
USE PteAnu   in 8  order tag PteAnu1  ALIAS Pteanu


*- Mensajes de aviso al usuario
Vmens08 = 'No hay registros para procesar'

PUBLIC VKEY,VCODPRG
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
 
 vCodPrg=ALLTRIM(vCodPrg)
 vMes=ALLTRIM(vMes)    
 
 if lastkey()= 27
      CLOSE DATA
      return
 endif

 if eof()
      do standby with vmens08
 Else
     vInd = sys(3)+".dbf"
     vIndidx = sys(3)+".idx"
	SELECT 9
	CREATE TABLE (vInd);
	  (Tipo      C(01),;
	   Cuenta    C(10),;
	   TipDoc    C(03),;
	   ValDEb    N(015,2),;
	   ValHab    N(015,2))
     USE in 9
     use (vind) in 9 alias vtempo
     INDEX ON TIPO+CUENTA+Tipdoc TO (vIndidx)
     ACTIVATE WINDOW STANDBY
     @ 1,14 SAY 'Espere un Momento ....' color W*
 	 DO leeHjC
 	 DEACTIVATE WINDOW STANDBY
	 SELECT vtempo
	 GO TOP
     * DO REPORTE WITH 2,"NotCon2",' Nota de Contabilidad  Presupuestal '
     * USE
 ENDIF
 
 SELECT vtempo
 SET INDEX TO
 INDEX ON  TIPO+TIPDOC+CUENTA TO (vIndidx)
 BROW
 DISPLAY ALL TO PRIN
 * CLOS DATA
 * ERASE (vind) 
 * ERASE (vIndidx)
 RETURN
 
PROCEDURE SUMADEB
*----------------
RC=RECNO()
vKEY=TIPO+LEFT(CUENTA,2)
sum VALdeb for TIPO+LEFT(CUENTA,2) = Vkey to suma
go rc
return suma


PROCEDURE SUMAhab
*----------------

RC=RECNO()
vKEY=TIPO+LEFT(CUENTA,2)
sum VALhab for TIPO+LEFT(CUENTA,2) = Vkey to suma
go rc
return suma


PROCEDURE VALDEB
*----------------
RC=RECNO()
vKEY=tipo+LEFT(CUENTA,2)
sum VALdeb for tipo+LEFT(CUENTA,2) = Vkey to suma
go rc
return suma


PROCEDURE VALHAB
*----------------
RC=RECNO()
vKEY=tipo+LEFT(CUENTA,2)
sum VALhab for tipo+LEFT(CUENTA,2) = Vkey to suma
go rc
return suma


PROCEDURE leeHJC
*---------------
* Procedimiento que carga los Asientos presupuestales 
* de las hojas de Conrol, C/P, R/I, H/M, Rebajas y Anulaciones 
*-----------------------------------------------------
vIdxhc = sys(3)+".idx"
SELE Astpre
INDEX ON NUMMES+NUMREF TO (vidxhc) FOR Tipdoc='H/C' and PERIODO=vperiodo and Nummes=vmes

SELE Hoja
SET FILTER TO Hoja.Nummes=vmes AND Hoja.Estado<>'99' and SUBSTR(Hoja.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN WHILE Hoja.Nummes=vmes AND Hoja.Estado<>'99' and SUBSTR(Hoja.CODCAL,8,2)=ALLTRIM(vCODprg)
   @ 1,10 SAY 'Leyendo Hojas de Control...'+STR(RECNO(),4) color W* 
   SELE AstPRe
   SEEK Hoja.Nummes+Hoja.Numhc
   SCAN WHILE Hoja.Nummes+Hoja.Numhc = AstPre.Nummes+AstPre.Numref 
      SELE vTempo
      SEEK AstPre.Tipo+SUBSTR(Astpre.Cuenta,1,3)+'0000000'+'H/C'
      IF FOUND()
         IF AstPre.Tipo='D'
            REPLACE vTempo.ValDeb WITH  vTempo.ValDeb+AstPre.ValDeb
         ELSE
            REPLACE vTempo.ValhAB WITH  vTempo.ValHAB+AstPre.ValhaB
         ENDIF
      ELSE
         APPEND BLANK
         REPLACE vTempo.Tipo   WITH  AstPre.Tipo
         REPLACE vTempo.Cuenta WITH  SUBSTR(Astpre.Cuenta,1,3)+'0000000'
         REPLACE vTempo.TIPDOC WITH  'H/C'
         IF AstPre.Tipo='D'
            REPLACE vTempo.ValDeb WITH  AstPre.ValDeb
         ELSE
            REPLACE vTempo.ValhAB WITH  AstPre.ValhaB
         ENDIF
      ENDIF
	 SELE ASTPRE
   ENDSCAN
   Sele Hoja
ENDSCAN

SELE ASTPRE
SET INDEX TO
ERASE (vIdxhc)

*----------
* Ahora los Comprobantes de Pago
*--------------------------------
vIdxhc = sys(3)+".idx"
SELE Astpre
INDEX ON NUMMES+NUMREF+Codctc TO (vidxhc) FOR Tipdoc='C/P' and PERIODO=vperiodo and Nummes=vmes

SELE Compag
SET FILTER TO Compag.Nummes=vmes AND (Compag.Estado<>'99' OR Compag.Estado<>'92')  AND Compag.Tipdoc$'HCRG' and SUBSTR(Compag.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN WHILE Compag.Nummes=vmes AND (Compag.Estado<>'99' OR Compag.Estado<>'92')  AND Compag.Tipdoc$'HCRG' and SUBSTR(Compag.CODCAL,8,2)=ALLTRIM(vCODprg)
   @ 1,10 SAY 'Leyendo Comprobantes de Pago...'+STR(RECNO(),4) color W* 
   SELE AstPRe
   SEEK Compag.Nummes+compag.Numcp+Compag.Codctc
   SCAN WHILE  Compag.Nummes+compag.Numcp+Compag.Codctc = AstPre.Nummes+AstPre.Numref +AstPRe.Codctc
      SELE vTempo
      SEEK AstPre.Tipo+SUBSTR(Astpre.Cuenta,1,3)+'0000000'+'C/P'
      IF FOUND()
         IF AstPre.Tipo='D'
            REPLACE vTempo.ValDeb WITH  vTempo.ValDeb+AstPre.ValDeb
         ELSE
            REPLACE vTempo.ValhAB WITH  vTempo.ValHAB+AstPre.ValhaB
         ENDIF
      ELSE
         APPEND BLANK
         REPLACE vTempo.tipo   WITH  AstPre.Tipo
         REPLACE vTempo.Cuenta WITH  SUBSTR(Astpre.Cuenta,1,3)+'0000000'
         REPLACE vTempo.Tipdoc WITH  'C/P'
         IF AstPre.Tipo='D'
            REPLACE vTempo.ValDeb WITH  AstPre.ValDeb
         ELSE
            REPLACE vTempo.ValhAB WITH  AstPre.ValhaB
         ENDIF
      ENDIF
	 SELE ASTPRE
   ENDSCAN
   Sele Compag
ENDSCAN
SELE ASTPRE
SET INDEX TO
ERASE (vIdxhc)

*--------------
* Sumamos Los Asientos Presupuestales de los REcibos de Ingreso 
*----------------------------------------------------------------
vIdxhc = sys(3)+".idx"
SELE Astpre
INDEX ON NUMMES+NUMREF TO (vidxhc) FOR Tipdoc='R/I' and PERIODO=vperiodo and Nummes=vmes

SELE RECING
SET FILTER TO Recing.Nummes=vmes AND Recing.Estado<>'99' 
* and SUBSTR(Compag.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN WHILE Recing.Nummes=vmes AND Recing.Estado<>'99' 
   @ 1,10 SAY 'Leyendo Recibos de Ingreso ...'+STR(RECNO(),4) color W* 
   SELE AstPre
   SEEK Recing.Nummes+Recing.NumRI
   SCAN WHILE  Recing.Nummes+Recing.NumRI = AstPre.Nummes+AstPre.Numref 
      SELE vTempo
      SEEK AstPre.Tipo+SUBSTR(Astpre.Cuenta,1,3)+'0000000'+'R/I'
      IF FOUND()
         IF AstPre.Tipo='D'
            REPLACE vTempo.ValDeb WITH  vTempo.ValDeb+AstPre.ValDeb
         ELSE
            REPLACE vTempo.ValhAB WITH  vTempo.ValHAB+AstPre.ValhaB
         ENDIF
      ELSE
         APPEND BLANK
         REPLACE vTempo.tipo   WITH  AstPre.Tipo
         REPLACE vTempo.Cuenta WITH  SUBSTR(Astpre.Cuenta,1,3)+'0000000'
         REPLACE vTempo.Tipdoc WITH  'R/I'
         IF AstPre.Tipo='D'
            REPLACE vTempo.ValDeb WITH  AstPre.ValDeb
         ELSE
            REPLACE vTempo.ValhAB WITH  AstPre.ValhaB
         ENDIF
      ENDIF
	 SELE ASTPRE
   ENDSCAN
   Sele Recing
ENDSCAN
SELE ASTPRE
SET INDEX TO
ERASE (vIdxhc)

*--------------
* Sumamos Los Asientos Presupuestales de los Partes de Anulaci¢n 
*----------------------------------------------------------------
vIdxhc = sys(3)+".idx"
SELE Astpre
INDEX ON NUMMES+NUMREF TO (vidxhc) FOR Tipdoc='P/A' and PERIODO=vperiodo and Nummes=vmes

SELE PteAnu
SET FILTER TO MONTH(PteAnu.fecpa)=VAL(vmes) AND PteAnu.Estado<>'99' 
* and SUBSTR(Compag.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN WHILE MONTH(PteAnu.fecpa)=VAL(vmes) AND PteAnu.Estado<>'99' 
   @ 1,10 SAY 'Leyendo Partes de Anulaci¢n...'+STR(RECNO(),4) color W* 
   SELE AstPre
   SEEK vmes+PteAnu.Numpa
   SCAN WHILE vmes+PteAnu.NumPA = AstPre.Nummes+AstPre.Numref 
      SELE vTempo
      SEEK AstPre.Tipo+SUBSTR(Astpre.Cuenta,1,3)+'0000000'+'P/A'
      IF FOUND()
         IF AstPre.Tipo='D'
            REPLACE vTempo.ValDeb WITH  vTempo.ValDeb+AstPre.ValDeb
         ELSE
            REPLACE vTempo.ValhAB WITH  vTempo.ValHAB+AstPre.ValhaB
         ENDIF
      ELSE
         APPEND BLANK
         REPLACE vTempo.tipo   WITH  AstPre.Tipo
         REPLACE vTempo.Cuenta WITH  SUBSTR(Astpre.Cuenta,1,3)+'0000000'
         REPLACE vTempo.Tipdoc WITH  'P/A'
         IF AstPre.Tipo='D'
            REPLACE vTempo.ValDeb WITH  AstPre.ValDeb
         ELSE
            REPLACE vTempo.ValhAB WITH  AstPre.ValhaB
         ENDIF
      ENDIF
	 SELE ASTPRE
   ENDSCAN
   Sele PteAnu
ENDSCAN
SELE ASTPRE
SET INDEX TO
ERASE (vIdxhc)


*--------------
* Sumamos Los Asientos Presupuestales de las Hojas de Modificaci¢n 
*----------------------------------------------------------------

vIdxhc = sys(3)+".idx"
SELE Astpre
INDEX ON NUMMES+NUMREF TO (vidxhc) FOR Tipdoc='H/M' and PERIODO=vperiodo and Nummes=vmes

SELE Hojmod
SET FILTER TO HojMod.Nummes=vmes AND HojMod.Estado<>'99' 
* and SUBSTR(Compag.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN WHILE HojMod.Nummes=vmes AND HojMod.Estado<>'99' 
   @ 1,10 SAY 'Leyendo Hojas de Modificaci¢n...'+STR(RECNO(),4) color W* 
   SELE AstPre
   SEEK HojMod.Nummes+HojMod.NumHM
   SCAN WHILE HojMod.Nummes+HojMod.NumHM = AstPre.Nummes+AstPre.Numref 
      SELE vTempo
      SEEK AstPre.Tipo+SUBSTR(Astpre.Cuenta,1,3)+'0000000'+'H/M'
      IF FOUND()
         IF AstPre.Tipo='D'
            REPLACE vTempo.ValDeb WITH  vTempo.ValDeb+AstPre.ValDeb
         ELSE
            REPLACE vTempo.ValhAB WITH  vTempo.ValHAB+AstPre.ValhaB
         ENDIF
      ELSE
         APPEND BLANK
         REPLACE vTempo.tipo   WITH  AstPre.Tipo
         REPLACE vTempo.Cuenta WITH  SUBSTR(Astpre.Cuenta,1,3)+'0000000'
         REPLACE vTempo.Tipdoc WITH  'H/M'
         IF AstPre.Tipo='D'
            REPLACE vTempo.ValDeb WITH  AstPre.ValDeb
         ELSE
            REPLACE vTempo.ValhAB WITH  AstPre.ValhaB
         ENDIF
      ENDIF
	 SELE ASTPRE
   ENDSCAN
   Sele HojMod
ENDSCAN
SELE ASTPRE
SET INDEX TO
ERASE (vIdxhc)
RETURN
