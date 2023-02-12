**----------------------------------------------
** GenAnal.PRG
** Reporte Analisis de Cuentas Por Rango de Fecha 
** Autor   : UDEP - FMC  12/08/1996
** Versi¢n : 3.11
** Fecha   : 07/08/1996
**----------------------------------------------
* Abrir Archivos de trabajo
USE Parmae    IN  01 ORDER TAG Parmae1         ALIAS parma
USE Diario    IN  02 ORDER TAG Diario1         ALIAS Diar
USE Compro    IN  05 ORDER TAG Compro7         ALIAS Comp
*--------------------------------------------------------------------------
* Se usa el mismo alias para evitar modificar los Reportes y el Resto de 
* Programas que Involucra
* Se debe de ejecutar despues del Cierre de Mes,
* Cuando ya se han emitido todo los reportes concernientes 
* al area de Contabilidad
*--------------------------------------------------------------------------
PUBLIC vMonDebe, vMonHabe, vCuenta, vTipAux, vRegistro, vDesde, vHasta
PUBLIC mDSalAnt, mHSalAnt
PUBLIC vDebeTot, vHabeTot, vErdad, vDebe, vHaber, vTotDH, vcuenini,vcuenfin
STORE 0   TO xano, xMes, vMontoD, vMontoH, vDebeTot, vHabeTot
STORE 0   TO vSaldo1, vSaldo2, vDebe, vHaber, vTotDH
vErdad = .F.
DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
  xbalance = 'Mensual'
  xdigitos = 2
  SELECT Parma
  SEEK 'MESANOACTUAL'
  vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
  vdesde   = CTOD('01/01/'+RIGHT(STR(YEAR(vmesact),4),2))
  vhasta   = vmesact 
  xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
  xAno     = IIF(FOUND(), VAL(SUBSTR(STR(YEAR(vmesact),4),3,2)), VAL(SUBSTR(STR(YEAR(DATE()),4),3,2)))
  DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE '  Generaci¢n de An lisis de Cuentas  ' COLOR SCHEME 5
  ACTIVATE WINDOW LIS
  IF LASTKEY()=27
	 RETURN
  ENDIF
	@ 05,01 SAY "             Desde : " GET vdesde
	@ 07,01 SAY "             Hasta : " GET vhasta  VALID vhasta<= DATE() AND vhasta>=vdesde ERROR "Fecha final no v lida"
	@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
	READ CYCLE
	RELEASE WINDOW LIS
	  IF LASTKEY() # 27 .AND. OKCANCEL = 1
	     DO liquidoc
	  ENDIF
	  IF !escolor
    	 RESTORE SCREEN FROM principal
	  ENDIF
	USE IN  01 
	USE IN  02 
	USE IN  05 
	USE IN  10 
RETURN

PROCEDURE Salida
*---------------
RELEASE  WINDOW AnaCue
ACTIVATE SCREEN
RETURN

PROCEDURE Liquidoc
*---------------------
* Proceso que liquida los documentos los casos 1 y 2 
* Cuando C/P ---> O/S
* Cuando C/P ---> O/C
*-------------------------------------------------------

filaux  = SYS(3) + '.DBF'
Idxaux1 = SYS(3) + '.IDX'
Idxaux2 = SYS(3) + '.IDX'
Idxaux3 = SYS(3) + '.IDX'
Idxaux4 = SYS(3) + '.IDX'
Idxaux5 = SYS(3) + '.IDX'
Idxaux6 = SYS(3) + '.IDX'
Idxaux7 = SYS(3) + '.IDX'
Idxaux8 = SYS(3) + '.IDX'
Idxaux9 = SYS(3) + '.IDX'
Idxau10 = SYS(3) + '.IDX'
Idxau11 = SYS(3) + '.IDX'
Idxau12 = SYS(3) + '.IDX'

ames='  '

DO ESPERA WITH 1,"Verificando Reproceso ...!" 
SELE Diar
SET ORDER TO TAG Diario11
INDEX ON DTOC(FECEMI) TO (Idxaux9) FOR (!EMPTY(Diar.Liquida) AND !EMPTY(Diar.LiquSU)) AND  BETWEEN(Diar.Fecemi,vdesde,vhasta)
GO TOP
 IF !EOF()
	SCAN 
    	   IF f_lock(1)
			  REPLACE Diar.Liquida  WITH  ' '
			  REPLACE Diar.LiquSU   WITH  ' '
		   ENDIF
		   UNLOCK
		@ 0,0 SAY PADC('Reprocesando ---->'+STR(RECNO(),12), WCOLS()) COLOR W+/BR*   
	    SELE Diar
	ENDSCAN
ENDIF
SET INDEX TO 
DO ESPERA WITH 2

DO ESPERA WITH 1,"Creando archivo Temporal de Trabajo...!" 

SELE Diar 
SET ORDER TO Diario11
COPY TO (filaux) FOR BETWEEN(Diar.Fecemi,vdesde,vhasta)
do espera with 2

USE (filaux)  IN  10 EXCLUSIVE                 ALIAS Tempo

DO ESPERA WITH 1,"Preparando Informaci¢n  del Archivo Temporal....!" 
SELE Tempo
INDEX ON ALLTRIM(LEFT(Comprob,7))+Comadi+Codfte+Tipo+Cuenta TO (idxAux1)  FOR  estado#'99'
DO espera with 2

DO ESPERA WITH 1,"Preparando Informaci¢n  del Archivo Diario....!" 
SELE Diar 
INDEX ON ALLTRIM(Comprob)+Comadi+Codfte+Tipo+Cuenta         TO (idxAux2) FOR BETWEEN(Fecemi,vdesde,vhasta) AND !EMPTY(Tipdcr) AND !EMPTY(DOCREF) AND estado#'99'
do espera with 2
GO TOP


DO ESPERA WITH 1," Leyendo ..........! " 

SCAN
    vTipo=IIF(Diar.tipo='D','H','D') 
    * ames=IIF(!EMPTY(Diar.Nummesref),Diar.Nummesref,PADL(ALLTRIM(STR(MONTH(Diar.Fecemi),2)),2,'0'))
    vclave=ALLTRIM(Diar.Tipdcr+'0'+ALLTRIM(left(Diar.Docref,4))+Diar.Docadi+Diar.Codfte+vTipo+Diar.Cuenta)
    SELE Tempo 
    SEEK (vclave)
    @ 0,0 SAY PADC( ALLTRIM('Procesando ----> '+vclave) , WCOLS() )COLOR W+/BR*
    
    IF FOUND()
        DO CASE
           CASE Diar.Montos = Tempo.Montos
               REPLACE Tempo.liquida WITH  'û'
               REPLACE Tempo.LiquSU  WITH  'û'
               SELE Diar
	   	       IF f_lock(1)
		   	   		REPLACE Diar.Liquida  WITH  'û'
		   	   		REPLACE Diar.LiquSU   WITH  'û'
	           ENDIF
	     	   UNLOCK
		  * CASE Diar.Montos > Tempo.Montos
		      * Pago demas
			  * REPLACE Tempo.liquida WITH  'û'
          * CASE Diar.Montos < Tempo.Montos
        	  * Pago a Cuenta
        ENDCASE   
    ENDIF
    SELE Diar
ENDSCAN
SET FILTER TO 
*-------------------------
* SE ACTUALIZA EL DIARIO
*-------------------------
SET INDEX TO 

SELE TEMPO
SET INDEX TO 

@ 0,0 SAY PADC( ALLTRIM('Cancelando Documentos......!') , WCOLS() )COLOR W+/BR*
SELE Diar 
INDEX ON ALLTRIM(Comprob)+Comadi+Codfte+Tipo+Cuenta TO (idxAux3)  FOR BETWEEN(Fecemi,vdesde,vhasta) 
GO TOP

SELE Tempo
INDEX ON ALLTRIM(Comprob)+Comadi+Codfte+Tipo+Cuenta TO (idxAux4) FOR Diar.liquida='û' 
GO TOP

SCAN
    vclave=ALLTRIM(Tempo.Comprob+Tempo.Comadi+Tempo.Codfte+Tempo.Tipo+Tempo.Cuenta)
    @ 0,0 SAY PADC( ALLTRIM('Actualizando ----> '+vclave) , WCOLS() )COLOR W+/BR*
    SELE Diar
    SEEK (vclave)
    IF FOUND()
       IF f_lock(1)
		  REPLACE Diar.Liquida  WITH  'û'
		  REPLACE Diar.LiquSU   WITH  'û'
	   ENDIF
	   UNLOCK
    ENDIF
    SELE Tempo
ENDSCAN
SET INDEX TO 

*-------------------------------------------
* AHORA HAY QUE CANCELAR LOS DOCUM EN LOS 
* SOLO PARA EL CASO 6
* Comprobantes de Pago de Renteciones a
* Comprobnates del Compromiso de la Retenci¢n 
*-------------------------------------------
SELE DIAR
SET INDEX TO 

vdoc=SPACE(40)
vDtotdoc=0
vhtotdoc=0

SELE Tempo
INDEX ON ALLTRIM(Comprob)+Comadi+Codfte TO (idxAux5) FOR LEFT(Diar.Comprob,2)='20' AND EMPTY(LIQUIDA)

SELE Diar 
INDEX ON Nummesref+ALLTRIM(left(Docref,4))+ALLTRIM(Codctc)+Tipo+Cuenta TO (idxAux6) FOR Diar.Tipdcr='20'  AND BETWEEN(Fecemi,vdesde,vhasta) 
GO TOP

SCAN
    IF alltrim(vdoc)=Diar.Nummesref+ALLTRIM(left(Diar.Docref,4))+ALLTRIM(Diar.Codctc)
       vDtotdoc=vDtotdoc+IIF(Diar.Tipo='D',Diar.Montos,0)
       vhtotdoc=vHtotdoc+IIF(Diar.Tipo='H',Diar.Montos,0)
    ELSE 
	    SELE Tempo
	    SEEK (vclave)
        @ 0,0 SAY PADC( ALLTRIM('Procesando ----> '+vclave) , WCOLS() )COLOR W+/BR*
    	IF FOUND()
    	   vreg=RECNO()
    	   SCAN WHILE vclave=ALLTRIM(Tempo.Comprob)+Tempo.Comadi+Tempo.Codfte
		       vDtottem=vDtottem+IIF(Tempo.Tipo='D',Tempo.Montos,0)
		       vhtottem=vHtottem+IIF(Tempo.Tipo='H',Tempo.Montos,0)
    	       SELE Tempo
    	   ENDSCAN
        	  IF (vDtotdoc = vDtottem OR vhtotdoc=vhtottem)
        	     SELE Tempo
        	     GO vreg 
         	     SCAN WHILE vclave=ALLTRIM(Tempo.Comprob)+Tempo.Comadi+Tempo.Codfte
    	              REPLACE Tempo.liquida WITH  'û'
    	              SELE Tempo
	    	     ENDSCAN
	    	     
	    	     SELE Diar
   	    	     IF f_lock(1)
			   	 	REPLACE Diar.Liquida  WITH  'û'
			   	 	REPLACE Diar.LiquSU   WITH  'û'
		         ENDIF
		    	 UNLOCK
		    	 
	      	 ENDIF
    	ENDIF
      ENDIF	
  	vdoc=Diar.Nummesref+ALLTRIM(left(Diar.Docref,4))+ALLTRIM(Diar.Codctc)
    vclave=ALLTRIM(ALLTRIM(Diar.Comprob)+Diar.Comadi+Diar.Codfte)
    SELE Diar
ENDSCAN
*-------------------------
* SE ACTUALIZA EL DIARIO
*-------------------------
SET INDEX TO 

SELE TEMPO
SET INDEX TO 

@ 0,0 SAY PADC( ALLTRIM('Preparando Informaci¢n ... !') , WCOLS() )COLOR W+/BR*
SELE Diar 
INDEX ON ALLTRIM(Comprob)+Comadi+Codfte+Tipo+Cuenta TO (idxAux7) FOR EMPTY(Liquida) AND BETWEEN(Fecemi,vdesde,vhasta) 
GO TOP

SELE Tempo
INDEX ON ALLTRIM(Comprob)+Comadi+Codfte+Tipo+Cuenta TO (idxAux8) FOR Tempo.liquida='û'
GO TOP

SCAN
    vclave=ALLTRIM(ALLTRIM(Tempo.Comprob)+Tempo.Comadi+Tempo.Codfte+Tempo.Tipo+Tempo.Cuenta)
    SELE Diar
    SEEK (vclave)
    @ 0,0 SAY PADC( ALLTRIM('Procesando ----> '+vclave) , WCOLS() )COLOR W+/BR*
    IF FOUND()
       IF f_lock(1)
		  REPLACE Diar.Liquida  WITH  'û'
		  REPLACE Diar.LiquSU   WITH  'û'
	   ENDIF
	   UNLOCK
    ENDIF
    SELE Tempo
ENDSCAN

	SET INDEX TO 
	USE IN  10 
	USE IN  01 
	USE IN  02 
	USE IN  05 
	USE IN  10 
	ERASE (filaux)
	ERASE (Idxaux1)
	ERASE (Idxaux2)
	ERASE (Idxaux3)
	ERASE (Idxaux4)
	ERASE (Idxaux5)
	ERASE (Idxaux6)
	ERASE (Idxaux7)
	ERASE (Idxaux8)
	ERASE (Idxaux9)
RETURN


FUNCTION Calcula
*---------------
vRec=RECNO()
vOrd=ORDER()
STORE 0 TO vDebe,vHaber,vTotDH
vCuenta=Diar.Cuenta
SET FILTER TO vCuenta=Diar.Cuenta
GO TOP
SCAN WHILE vCuenta=Diar.Cuenta
     IF Diar.Tipo='D'
        IF Diar.SaldPen#0
           vDebe = vDebe  + Diar.SaldPen
        ELSE
           vDebe = vDebe  + Diar.Montos
        ENDIF
     ELSE
        IF Diar.SaldPen#0
           vHaber = vHaber + Diar.SaldPen
        ELSE
           vHaber = vHaber + Diar.Montos
        ENDIF
     ENDIF
ENDSCAN
IF vDebe > vHaber     
   vTotDH = vDebe - vHaber
ELSE
   vTotDH = vHaber - vDebe
ENDIF
SET FILTER TO
GO vRec
RETURN vTotDH


FUNCTION Reproce
*---------------
SELECT Diar
vOrd=ORDER()
GO TOP
REPLACE ALL Diar.Estado  WITH '  ';
            Diar.SaldPen WITH 0
vErdad = .F.
vComprob =Diar.DocRef
vComAdi  =Diar.DocAdi
vTipo    =Diar.Tipo
vCuenta  =Diar.Cuenta
vTipAux  =Diar.TipAux
vRegistro=Diar.Registro
vMontos  =Diar.Montos
SCAN
     vRec=RECNO()
     vComprob =Diar.DocRef
     vComAdi  =Diar.DocAdi
     vTipo    =Diar.Tipo
     vCuenta  =Diar.Cuenta
     vTipAux  =Diar.TipAux
     vRegistro=Diar.Registro
     vMontos  =Diar.Montos

     SET ORDER TO TAG Diario9
     SEEK vComprob+vComAdi+vCuenta+vTipAux+vRegistro
     IF FOUND()
        IF Diar.Estado='50' &&CANCELADO
           vRec11=RECNO()
           GO vRec
           REPLACE Diar.Estado WITH '00' &&PENDIENTE DE CANCELAR
           GO vRec11
        ENDIF
        IF vTipo=Diar.Tipo
           REPLACE Diar.Estado WITH '00' &&PENDIENTE DE CANCELAR
        ELSE
           IF vMontos=Diar.Montos
              REPLACE Diar.Estado WITH '50' &&CANCELADO
           ELSE
              IF vMontos > Diar.Montos
                 IF Diar.Estado='00' &&PENDIENTE
                    IF Diar.Montos > Diar.SaldPen
                       REPLACE Diar.SaldPen WITH Diar.Montos - vMontos &&Diar.SaldPen
                    ELSE
                       REPLACE Diar.SaldPen WITH vMontos - Diar.Montos &&Diar.SaldPen
                    ENDIF
                    IF Diar.SaldPen=0
                       REPLACE Diar.Estado WITH '50' &&CANCELADO
                    ENDIF
                 ELSE
                    REPLACE Diar.Estado  WITH '00' &&PENDIENTE DE CANCELAR
                    REPLACE Diar.SaldPen WITH vMontos - Diar.Montos
                 ENDIF   
              ELSE
                 IF Diar.Estado='00' &&PENDIENTE
                    IF Diar.Montos > Diar.SaldPen
                       REPLACE Diar.SaldPen WITH Diar.Montos - vMontos &&Diar.SaldPen
                    ELSE
                       REPLACE Diar.SaldPen WITH vMontos - Diar.Montos  &&Diar.SaldPen - Diar.Montos
                    ENDIF
                    IF Diar.SaldPen=0
                       REPLACE Diar.Estado WITH '50' &&CANCELADO
                    ENDIF
                 ELSE
                    REPLACE Diar.Estado  WITH '00' &&PENDIENTE DE CANCELAR
                    REPLACE Diar.SaldPen WITH Diar.Montos - vMontos
                 ENDIF   
              ENDIF
           ENDIF
        ENDIF
     ELSE
        GO vRec
        IF Diar.Estado#'50'
           REPLACE Diar.Estado WITH '00' &&PENDIENTE DE CANCELAR
        ENDIF
*        vErdad = .T.
     ENDIF
   
     SET ORDER TO TAG vOrd
     GO vRec
ENDSCAN      
RETURN


FUNCTION Reproce1
*----------------

SELECT Diar
vOrd=ORDER()
GO TOP
REPLACE ALL Diar.Estado  WITH '  ';
            Diar.SaldPen WITH 0
vErdad = .F.
vComprob =Diar.DocRef
vComAdi  =Diar.DocAdi
vTipo    =Diar.Tipo
vCuenta  =Diar.Cuenta
vTipAux  =Diar.TipAux
vRegistro=Diar.Registro
vMontos  =Diar.Montos
SCAN
     vRec=RECNO()

     vComprob =Diar.DocRef
     vComAdi  =Diar.DocAdi
     vTipo    =Diar.Tipo
     vCuenta  =Diar.Cuenta
     vTipAux  =Diar.TipAux
     vRegistro=Diar.Registro
     vMontos  =Diar.Montos

     SET ORDER TO TAG Diario9
     SEEK vComprob+vComAdi+vCuenta+vTipAux+vRegistro
     IF FOUND()
        IF Diar.Estado='50' &&CANCELADO
           REPLACE Diar.Estado WITH '00' &&PENDIENTE DE CANCELAR
        ENDIF
        IF vTipo=Diar.Tipo
           REPLACE Diar.Estado WITH '00' &&PENDIENTE DE CANCELAR
        ELSE
           IF vMontos=Diar.Montos
              REPLACE Diar.Estado WITH '50' &&CANCELADO
           ELSE
              IF vMontos > Diar.Montos
                 IF Diar.Estado='00' &&PENDIENTE
                    REPLACE Diar.SaldPen WITH vMontos - Diar.SaldPen
                    IF Diar.SaldPen=0
                       REPLACE Diar.Estado WITH '50' &&CANCELADO
                    ENDIF
                 ELSE
                    REPLACE Diar.Estado  WITH '00'      &&PENDIENTE DE CANCELAR
                    REPLACE Diar.SaldPen WITH vMontos - Diar.Montos - Diar.SaldPen
                 ENDIF
              ELSE
                 IF Diar.Estado='00' &&PENDIENTE
                    REPLACE Diar.SaldPen WITH Diar.SaldPen - vMontos
                    IF Diar.SaldPen=0
                       REPLACE Diar.Estado WITH '50' &&CANCELADO
                    ENDIF
                 ELSE
                    REPLACE Diar.Estado  WITH '00' &&PENDIENTE DE CANCELAR
                    REPLACE Diar.SaldPen WITH Diar.Montos - vMontos - Diar.SaldPen
                 ENDIF
              ENDIF
           ENDIF
        ENDIF
     ELSE
        GO vRec
        IF Diar.Estado#'50'
           REPLACE Diar.Estado WITH '00' &&PENDIENTE DE CANCELAR
        ENDIF
*        vErdad = .T.
     ENDIF
   
     SET ORDER TO TAG vOrd
     GO vRec
ENDSCAN      
RETURN

