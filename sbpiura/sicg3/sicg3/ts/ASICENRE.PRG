*-------------------------------
*- ASICENRE.PRG
*- Asiento de Centralizaci¢n Tesorer¡a
*- AUTOR : Federico Montero Valdiviezo
*-------------------------------

USE Compag  IN 1  ORDER TAG Compag1               ALIAS Comp
USE AstPat  IN 2  ORDER TAG AstPat1               ALIAS AstPat
USE Parmae  IN 3  ORDER TAG Parmae1               ALIAS Parma
USE Cuentas IN 4  ORDER TAG Cuentas1              ALIAS Cuen
filaux = SYS(3) + '.DBF'
SELECT 9
CREATE TABLE (filaux);
  (TipDoc    C(003),;
   Fecha     D(008),;
   NumMes    C(002),;
   NumRef    C(004),;
   CodCta    C(010),;
   TipCta    C(001),;
   MtoDeb    N(13,2),;
   MtoHab    N(13,2),;
   Ret       C(001),;
   CodCtc    C(014))
USE
USE (filaux) IN 9 EXCLUSIVE                  ALIAS Temp

PUBLIC vQui,vDMonto,vHMonto,vKey,vDTot,vHTot,vDSal,vHSal,vMonDeb,vMonHab
PUBLIC _y,_z
STORE 0 TO xano,xMes,vdebe,vhaber,vQui,vMonDeb,vMonHab
STORE 0 TO vDMonto,vHMonto,vDTot,vHTot,vDSal,vHSal
STORE .T. TO NoPaso, NoPaso1
STORE SPACE(2) TO WMES
DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
  vcol ="w+/w,n/w,W/W,w/W,,w+/b"
  IF Escolor
    DEFINE WINDOW AsiCenTe FROM 12,52 TO 16,78  SHADOW COLOR &vcol
  ELSE
    DEFINE WINDOW AsiCenTe FROM 12,52 TO 16,78  COLOR SCHEME c_popup
  ENDIF
  ACTIVATE WINDOW AsiCenTe
  xbalance = 'Mensual'
  xdigitos = 2
* SELECT parma
* SEEK 'ANOMESACTUAL'
* vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
* xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
* xAno     = IIF(FOUND(), VAL(SUBSTR(STR(YEAR(vmesact),4),3,2)), VAL(SUBSTR(STR(YEAR(DATE()),4),3,2)))
  xMes     = MONTH(DATE())
  xAno     = VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
  @ 0, 0 SAY ' Genera Asiento:' + xBalance
  @ 2, 0 SAY '     Mes y A¤o:' GET xMes     PICTURE '99' WHEN xBalance = 'Mensual';
             VALID xMes > 0 .AND. xMes <= 12
  @ 2,20 GET xAno                           PICTURE '99'
  READ
  IF XMES<10
     WMES="0"+ALLTRIM(STR(XMES))
  ELSE
     WMES=STR(XMES,2)   
  ENDIF   
  IF LASTKEY() # 27
  DEFINE WINDOW Xwait FROM 20,06 TO 22,78 COLOR SCHEME 05 
  ACTIVATE WINDOW Xwait
    vind1= SYS(3) +'.idx'
   @0,10 SAY " Espere un Momento...Asiento de Centralizaci¢n en Proceso!" COLOR W+/BR*
    DO CASE
      CASE xbalance = 'Mensual'
          SELECT AstPat
          DO Libro   &&proceso que genera los asientos de ingreso y salidad de caja
                     &&101 al debe y haber y toma solo el monto liquido de la opera-
                     &&ci¢n en el caso de retenciones.
                
          SELECT Temp
          vTemp = SYS(3) + '.Idx'
          INDEX ON TipCta+CodCta TO (vTemp)
		DEACTIVATE WINDOW Xwait
          DO REPORTE WITH 2, "AsiCenRE", ' Asiento de Centralizaci¢n ',1,.F.,.T.
          CLOSE DATA
    ENDCASE
    CLOSE DATABASE
    ERASE (vind1)
  ENDIF
  DEACTIVATE WINDOW AsiCenTe

  IF !escolor
    RESTORE SCREEN FROM principal
  ENDIF
  SHOW POPUP menu, pop_01
RETURN


FUNCTION Libro  &&FILTRA EN EL CASO DE RETENCIONES SOLO EL MONTO LIQUIDO
*-------------
vAli=ALIAS()
SELECT AstPat
vInd9 = SYS(3) + '.Idx'
VDBF9 = SYS(3) + '.DBF'
INDEX ON NumMes+NumRef TO (vInd9) FOR NUMMES = WMes ;
         .AND. VAL(RIGHT(ALLTRIM(STR(YEAR(Fecha))),2)) = xAno AND RET='S' AND TIPCTC='RE' 
GO TOP
COPY TO (VDBF9)
USE IN 2
USE (VDBF9) IN 6  ALIAS RET
USE AstPat  IN 2  ORDER TAG AstPat1         ALIAS AstPat
SELE ASTPAT
vInd8 = SYS(3) + '.Idx'
VDBF8 = SYS(3) + '.DBF'
INDEX ON NumMes+NumRef TO (vInd8) FOR NUMMES = WMes ;
         .AND. VAL(RIGHT(ALLTRIM(STR(YEAR(Fecha))),2)) = xAno .AND. RET<>'S' AND TIPCTC='RE'
GO TOP
vTipDoc = TipDoc
vFecha  = Fecha
vNumMes = NumMes
vNumRef = NumRef
vCodCta = CodCta
vTipCta = TipCta
vMtoDeb = MtoDeb
vMtoHab = MtoHab
*vRet    = Ret
vCodCtc = CodCtc
SCAN
    vTipDoc = TipDoc
    vFecha  = Fecha
    vNumMes = NumMes
    vNumRef = NumRef
    vCodCta = CodCta
    vTipCta = TipCta
    vMtoDeb = MtoDeb
    vMtoHab = MtoHab
*    vRet    = Ret
    vCodCtc = CodCtc
    SCAN WHILE vTipDoc = TipDoc .AND. vFecha = Fecha .AND. vNumMes = NumMes .AND. vNumRef = NumRef
         vRec1 = RECNO()
         vTipDoc = TipDoc
         vFecha  = Fecha
         vNumMes = NumMes
         vNumRef = NumRef
         vCodCta = CodCta
         vTipCta = TipCta
         vMtoDeb = MtoDeb
         vMtoHab = MtoHab
*        vRet    = Ret
         vCodCtc = CodCtc
         vMto    = 0
         IF vTipCta = 'D' AND VTIPDOC='C/P'
            SELE RET
            SCAN WHILE vTipDoc = TipDoc .AND. vFecha = Fecha .AND. vNumMes = NumMes .AND. vNumRef = NumRef
	            VMTO=VMTO+MTOHAB
            ENDSCAN
         ENDIF
         SELECT Temp
         IF F_APPD()
            REPLACE TipDoc WITH vTipDoc        
            REPLACE Fecha  WITH vFecha         
            REPLACE NumMes WITH vNumMes        
            REPLACE NumRef WITH vNumRef        
            REPLACE CodCta WITH vCodCta        
            REPLACE TipCta WITH vTipCta        
            REPLACE MtoDeb WITH vMtoDeb - vMto 
            REPLACE MtoHab WITH vMtoHab        
*           REPLACE Ret    WITH vRet           
            REPLACE CodCtc WITH vCodCtc        
            @0,01 SAY STR(RECNO(),6)
         ENDIF
         SELECT AstPat
    ENDSCAN
    GO vRec1
    SELECT AstPat
ENDSCAN
SELECT (vAli)
RETURN

FUNCTION DESCRI1
*---------------
vAli=ALIAS()
xCuenta=LEFT(Temp.CodCta,3)
vCuenta=LEFT(Temp.CodCta,3)+'0000000'
SELECT Cuen
SEEK vCuenta
IF FOUND()
   _z=Val_Fun('Cuen','CodCta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN


FUNCTION DSUMA
*-------------
vKey = substr(Temp.CodCta,1,3)
STORE 0 TO vHMonto
SCAN WHILE substr(Temp.CodCta,1,3) = vKey .AND. Temp.TipCta = 'H'
        vHmonto = vHMonto + MtoHab
        vRec=RECNO()
ENDSCAN
GO vRec
RETURN vHMonto


FUNCTION HSUMA
*-------------
vKey = substr(Temp.CodCta,1,3)
STORE 0 TO vDMonto
SCAN WHILE substr(Temp.CodCta,1,3) = vKey .AND. Temp.TipCta = 'D'
        vDMonto = vDMonto + MtoDeb
        vRec=RECNO()
ENDSCAN
GO vRec
RETURN vDMonto


FUNCTION UNO
*-----------
SELECT Temp
GO TOP
SUM Temp.MtoDeb TO vUno FOR Temp.TipCta='D' .AND. LEFT(CODCTA,3)#'101'
RETURN vUno


FUNCTION UNOS
*------------
SELECT Temp
GO TOP
SUM Temp.MtoHab TO vUnos FOR Temp.TipCta='H' .AND. LEFT(CODCTA,3)#'101'
RETURN vUnos


FUNCTION vDebe
*-------------
SELECT Temp
GO TOP
SCAN
    IF Temp.TipCta='D'
       vMonDeb = vMonDeb + Temp.MtoDeb
    ELSE
       vMonHab = vMonHab + Temp.MtoHab
    ENDIF
ENDSCAN
=vHaber()
RETURN vMonDeb


FUNCTION vHaber
*--------------
RETURN vMonHab


PROCEDURE Salida
*---------------
RELEASE    WINDOW AsiCenTe
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)

PROCEDURE PENDIENTE
        IF xMes = MONTH(vmesact) .AND. xAno = VAL(SUBSTR(STR(YEAR(vmesact),4),3,2))
        ELSE
          vpath = '..\'+ALLTRIM(STR(xmes,2))+STR(xano,2)
          vdir    = '..\'+ALLTRIM(STR(xmes,2)) + STR(xano,2) + '\'
          vbackup = ALLTRIM(STR(xmes,2)) + STR(xano,2)+'.zip'
          IF !FILE(vdir + vbackup)
            DO Standby WITH 'Archivo : de BackUp en Disco del mes ' ;
            + ALLTRIM(STR(xmes)) + ' '+ALLTRIM(STR(xano,2)) + ' no existe'
             RETURN
          ENDIF
          !CD &vpath >NUL
          !FOXSWAP PKUNZIP -o &vbackup Parmae.dbf AstPat.dbf Compag.dbf Cuentas.dbf >NUL
          !FOXSWAP PKUNZIP -o &vbackup Parmae.cdx AstPat.cdx Compag.cdx Cuentas.cdx >NUL
          !CD ..\FUENTES
          CLOSE DATA
          SET PATH TO &vpath
          USE Parmae  IN 1 ORDER TAG Parmae1     ALIAS parma
          USE AstPat  IN 2                       ALIAS AstPat
          USE Cuentas IN 4 ORDER TAG Cuentas1    ALIAS Cuen
          USE Compag  IN 5 ORDER TAG Compro1     ALIAS Comp
          SELECT AstPat
         
          DO Libro
				DEACTIVATE WINDOW Xwait
          
          DO REPORTE WITH 2, "AsiCenRE", ' Asiento de Centralizaci¢n ',1,.F.,.T.
          CLOSE DATA
          SET PATH TO &xruta
          vdbf = vdir + '*.dbf'
          vidx = vdir + '*.idx'
          vcdx = vdir + '*.cdx'
         !ERASE &vdbf >NUL
         !ERASE &vidx >NUL
         !ERASE &vcdx >NUL
        ENDIF
