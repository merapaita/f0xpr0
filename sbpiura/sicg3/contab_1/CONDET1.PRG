* ----------------------------------------------------
* Programa de Consistencia Detallada de Datos 
* Reporte Consistencia Detallada de Documentos de Contabilidad 
* y emisi¢n de hojas de trabajo por tipo de doc.
* AUTOR UDEP - OAOC 1995
* Last Update: Set-96 por J.F.G.
* ----------------------------------------------------
PUBLIC vindpar,vtiprep,vMens01,vMens02,vMens03,vsedpli,old_glosa,xf1,xf2,vtipo
PUBLIC wglosa,vFin,xano,xMes,dmov_ant,hmov_ant,vDVan,vHVan,vdtot,vhtot,vmondeb,vmonhab
PUBLIC diarT,vCorNC

vindpar   = SYS(3)+'.idx'
diart     = SYS(3)+'.dbf'
tiporep   = 'condet1'
old_glosa = 'Ninguna'
STORE 0 TO vDVan,vHVan,vdtot,vhtot,vfin,vmondeb,vmonhab

*- Mensajes de Aviso al Usuario:
vMens01 = "No existe informaci¢n para procesar"
vMens02 = "No existen documentos del tipo especificado"
vMens03 = "No existen H/C con incidencia contable"

*- Abrir Archivos de Trabajo
USE Compro  IN 1 ORDER TAG Compro7   ALIAS Comp
USE Diario  IN 2 ORDER TAG Diario1   ALIAS Diar
USE Auxil   IN 3 ORDER TAG Auxil1    ALIAS Auxi  
USE Parmae  IN 4 ORDER TAG Parmae1   ALIAS parma
USE Cuentas IN 5 ORDER TAg Cuentas1  ALIAS Cuen
USE HojCon  IN 6 ORDER TAG HojCon1   ALIAS Hoja
USE Glosas  IN 7  ORDER TAG Glosas1          ALIAS Glos

*- Establecimiento de relaci¢n de archivos
SELE DIAR
SET RELATION TO diar.TipAux+diar.Registro            INTO Auxi
SET RELATION TO diar.comprob+diar.comadi+diar.codfte INTO Comp  ADDITIVE
SET RELATION TO diar.cuenta                          INTO Cuen  ADDITIVE
DO Inicia
DO salida
RETURN
*- Fin del Programa



PROCEDURE Inicia
*---------------
DEFINE WINDOW LIS FROM 4,15 TO 20,65 FLOAT DOUBLE TITLE ' Consistencia Detallada de Documentos ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vLista

*- Mes actual
SELECT parma
SEEK 'MESANO' + 'ACTUAL'
IF FOUND()
	xf1 = CTOD('01/'+SUBS(parma.descri,5,2)+'/'+ALLTRIM(STR(YEAR(DATE()))))
	xf2 = LASTDAY(MONTH(xf1))
 ELSE
	STORE DATE() TO xf1, xf2
ENDIF
*--   
STORE SPACE(2) TO ran1
STORE SPACE(2) TO ran2
STORE 1 TO vTipRep
vtipo   = SPACE(2)
vsedpli = 'Sede   '
con_inc = .F.
vOrden  =  1     && Orden
vTipoGa = .F.    && Ordenado por tipo de gasto
vFormat = 'LisCon1'
IF LASTKEY()=27
   RETURN
ENDIF

@ 01,01 SAY "         Documentos: " 
@ 00,22 GET vLista FUNCTION '^ Rango de Tipos de Doc.;Por Tipo de Doc.;Todos' 
@ 04,01 SAY "  Tipo de Documento: "
@ 04,22 GET vtipo  WHEN vlista=2 PICTURE '!!' VALID Val_para7(vTipo,'TIPDOC',' ',22,20)
@ 05,01 SAY "  Documento Inicial: "
@ 05,22 GET ran1 WHEN vlista=1 PICTURE '!!' VALID Val_para7(ran1,'TIPDOC',' ',22,20)
@ 06,01 SAY "  Documento Final  : "  
@ 06,22 GET ran2 WHEN vlista=1 PICTURE '!!!' VALID Val_para7(ran2,'TIPDOC',' ',22,20)
@ 07,01 SAY "        Fecha Desde: "
@ 07,22 GET xf1 
@ 08,01 SAY "        Fecha Hasta: "
@ 08,22 GET xf2  VALID(xf2>=xf1) ERROR "Debe ser Mayor o Igual al 1ro"
@ 10,01 SAY "  Cuentas a Incluir: "
@ 09,22 GET vTipCue FUNCTION '^ Todas;Patrimoniales;Presupuestales;Orden'  DEFAULT 1
@ 13,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

*IF OKCANCEL = 1 AND LASTKEY()<>27

*	con_inc = .F.
*	con_ret = .F.

*	IF vtiprep=1
*		*- Si es por tipo de doc. y lista Hojas de Control, preguntar si solo desea
*		*- las que tienen incidencia contable.
*		IF vLista=2 AND ALLTRIM(vTipo)="03"
*			con_inc = yesno("¨Desea s¢lo H/C que tienen incidencia contable? (No=Todas)")   
*		ENDIF
*		IF vLista=2 AND ALLTRIM(vTipo)="20"
*			con_ret = yesno("¨Desea s¢lo C/P que tienen retenci¢n? (No=Todos)")   
*			tiporep = "condet3"
*		ENDIF
*	ENDIF
*
*ENDIF

RELEASE WINDOW LIS
IF OKCANCEL = 1 AND LASTKEY()<>27
	idxaux = SYS(3) + '.IDX'
	ran1 = ALLTRIM(ran1)
	ran2 = ALLTRIM(ran2)
	
	ClaInd = 'Comprob+ComAdi+codfte'
	vFormat= 'LisCon1'
	
	DO ESPERA WITH 1,PADC('Espere un Momento;  Preparando informaci¢n ...',50,' ')
	
	do ind_par
	SELECT diar
	
	DO CASE
		CASE vlista=1                        && Rango de Tipo de Documentos
			INDEX ON &ClaInd  TO (idxaux) FOR BETWEEN( LEFT(Diar.Comprob,2),ran1,ran2) .AND. BETWEEN(Diar.Fecemi,xf1,xf2) ;
					AND ESTADO#'99'
			DO FILTRA_CUE WITH vTipCue
			DO ESPERA WITH 2 
			GO TOP
			IF EOF()
				DO standby WITH vMens01
			 ELSE
				DO REPORTE WITH 2, vformat, ' Consistencia detallada de Documentos ',2,.F.,.T.
			ENDIF
		CASE vlista=2                        && Por tipo de Documento
			*-----
			SELECT Diar
			Ok_Index = .F.
			vEstados = IIF(vlista=2 AND vtiprep=2 , "5599" , "99" )
			
			*- En el caso de C/Pago debera aparecer ordenado por CtaCte.
			*IF alltrim(vtipo)='98'
			*	IF con_ret
			*		INDEX ON &ClaInd  TO (idxaux) FOR;
			*				SUBS(diar.comprob,1,2)=ALLT(vtipo) AND BETW(Diar.Fecemi,xf1,xf2) AND;
			*		niv_int() AND diar.reten>0
			*	 ELSE
			*		INDEX ON &ClaInd  TO (idxaux) FOR SUBSTR(COMPROB,1,2)=ALLTRIM(VTIPO).AND.BETWEEN(FECEMI,XF1,XF2).AND.ESTADO#vEstados
			*	ENDIF
			*	Ok_Index = .T.
			*ENDIF
			
			*- En el caso de H/C con o sin incidencia contable
			*IF alltrim(vtipo)='03'
			*	IF con_inc         && H/C con incidencia contable
			*		INDEX ON &ClaInd TO (idxaux) FOR SUBSTR(DIAR.COMPROB,1,2)=ALLTRIM(VTIPO).AND.BETWEEN(DIAR.FECEMI,XF1,XF2).AND.ESTADO#vEstados AND;
			*				ambito(vsedpli) AND HC_IC(diar.comprob)
			*	 ELSE               && Todas las H/C 
			*		INDEX ON &ClaInd  TO (idxaux) FOR SUBSTR(COMPROB,1,2)=ALLTRIM(VTIPO).AND.BETWEEN(FECEMI,XF1,XF2).AND.ESTADO#vEstados
			*	ENDIF
			*	Ok_Index = .T.
			*ENDIF
			
			*- Si aun no ha sido indexado el archivo se procede a indexar
			IF !Ok_Index
				INDEX ON &ClaInd TO (idxaux) FOR SUBSTR(COMPROB,1,2)=ALLTRIM(VTIPO).AND.BETWEEN(FECEMI,XF1,XF2).AND.ESTADO#vEstados
			ENDIF
			*-----
			
			DO FILTRA_CUE WITH vTipCue
			DO ESPERA WITH 2 
			GO TOP
			
			*-------------------------------------
			*  Condici¢n para la hoja de Trabajo:
			*IF vTipRep = 2 AND vLista=2
			*	vFormat='httipdoc'
			*ENDIF
			*------------------------------------
			
			IF EOF()
				DO standby WITH IIF(con_inc,vMens03,vMens02)
			 ELSE
				DO REPORTE WITH 2, vformat, ' Consistencia detallada de Documentos ',2,.F.,.T.
			ENDIF
		
		OTHERWISE                             && Todos los documentos
			INDEX ON &ClaInd  TO (idxaux) FOR BETWEEN(Diar.Fecemi,xf1,xf2) 
			DO FILTRA_CUE WITH vTipCue
			DO ESPERA WITH 2
			GO TOP
			IF EOF()
				DO standby WITH vMens01
			 ELSE
				DO REPORTE WITH 2, vformat, ' Consistencia detallada de Documentos ',2,.F.,.T.
			ENDIF
	ENDCASE
	
	*IF vtiprep=2
	*	DO resumen
	*ENDIF
	
	SET INDEX TO
	ERASE (idxaux)
ENDIF
 
RETURN


PROCEDURE FILTRA_CUE
*-------------------
*-Establece un filtro para las cuentas patrimoniales,presupuestales o todas.
PARAMETER xT
DO CASE
	CASE xT=1                 && Todas las cuentas
		RETURN
	CASE xT=2                 && Cuentas patrimoniales
		SET FILTER TO LEFT(Diar.Cuenta,1) # "9" AND  LEFT(Diar.Cuenta,1) # "8"
		RETURN
	CASE xT=3                 && Cuentas presupuestales
		SET FILTER TO LEFT(Diar.Cuenta,1) = "8"
	CASE xT=4                 && Cuentas Orden
		SET FILTER TO LEFT(Diar.Cuenta,1) = "9"
		RETURN
ENDCASE
RETURN


PROCEDURE Salida
*---------------
RELEASE    WINDOW Consis
ACTIVATE   SCREEN
CLOSE DATABASE
ERASE(vindpar)
ERASE(diart)
RETURN



PROCEDURE ind_par
*----------------
SELECT parma
SET ORDER TO
INDEX ON TIPO+CODIGOAUX TO (vindpar)
RETURN

FUNCTION xDescri
*---------------
PARAMETER xtipo,xcodigoAux
Area = ALIAS()
SELE PARMA
SET INDEX TO (vindpar)
SEEK ALLTRIM(xtipo)+xcodigoAux
r = parma.descri
SET ORDER TO TAG parmae1
SELECT(Area)
RETURN alltrim(r)

FUNCTION Glosas
*--------------
r  = ""
gl = ALLTRIM( UPPER ( Comp.Glosa1 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+gl
ENDIF
gl = ALLTRIM( UPPER ( Comp.Glosa2 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
gl = ALLTRIM( UPPER ( Comp.Glosa3 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
gl = ALLTRIM( UPPER ( Comp.Glosa4 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
IF LEFT(comprob,2)='20'
   vret=IIF(!Empty(r),SUBS(r,25,LEN(r)-24),'No se encontr¢ detalle/glosa para este Documento.')   
ELSE   
   vret=IIF(!Empty(r),ALLTRIM(r),'No se encontr¢ detalle/glosa para este Documento.')
ENDIF
RETURN vret


FUNCTION HC_IC
*-------------
PARAMETER xComprob

*- Retorna si la H/C tiene incidencia contable.
*- En la cadena vInc, se especifican los tipos de documentos que tienen I.C.

vRet = .F.
vInc = "PLA,VAL,FPE,O/C,O/S"
SELE Hoja
SEEK SUBSTR(xcomprob,8,2) + SUBSTR(xcomprob,4,4)
IF FOUND()
   vRet = ALLTRIM(Hoja.TipDoc) $ vInc
ENDIF
SELE Diar
RETURN vRet
   

FUNCTION desc_niv
*----------------
PARAMETER vsedpli
RETURN IIF(alltrim(vsedpli)='Sede',"PROGRAMA : SEDE CENTRAL",IIF(alltrim(vsedpli)='Adm.Cen',"ADMINISTRACION CENTRAL","PLIEGO 01: REGION GRAU"))


FUNCTION niv_int
*---------------
RETURN IIF(allt(vsedpli)='Sede',SUBS(Diar.Cuenta,4,2)$'0102',IIF(allt(vsedpli)='Adm.Cen',SUBS(Diar.Cuenta,4,4)$'0101',.T.))


FUNCTION DESCTC
*--------------
PARAMETER vcodctc
IF EMPTY(vcodctc)
   RETURN REPL(' ',40)
ENDIF   
AREA = SELECT()
SELECT Caja
SEEK vcodctc
vRet = IIF(FOUND(),descri,repl('?',40))
SELECT(area)
RETURN vret


PROCEDURE RESUMEN
*----------------
SELECT diar
GO TOP
vEmiteRes = IIF(EOF(),.f.,YESNO('¨Imprime el res£men de la Hoja de Trabajo?'))

IF !vEmiteRes
   RETURN
ENDIF

*- Busca el siguiente correlativo del parmae 
SELECT parma
SET ORDER TO TAG parmae1
SEEK 'CORREL'+'NCRESU'

IF FOUND()
   vcorNC = parma.nument + 1
ELSE
   DO standby WITH 'No existe correlativo para N/C por tipo de Doc.'
   vCorNC = 1
ENDIF
ACTIVATE WINDOW STANDBY
@ 1,2 SAY 'N§ Correlativo de la N/C :  ' GET vCorNc PICTURE "9,9999" COLOR SCHEME 5
READ
DEACTIVATE WINDOW STANDBY
*-


      
SELECT diar
DO ESPERA WITH 1,'Res£men de Hoja de Trabajo en proceso, espere ...'
SET FILTER TO && Quita el filtro de tipo de cuenta 
SELECT Diar
SET INDEX TO
INDEX ON LEFT(COMPROB,2)+TIPO+CUENTA+CODFTE TO (Idxaux) FOR;
SUBS(COMPROB,1,2)=ALLT(VTIPO).AND.BETW(FECEMI,XF1,XF2).AND.ESTADO#"5599"
*SET RELATION TO LEFT(Comprob,2) INTO PARMA ADDITIVE &&
GO BOTT
vFin=RECNO()
GO TOP
DO ESPERA WITH 2
DO REPORTE WITH  2, 'HTresu',' Res£men Hoja de Trabajo ',2,.F.,.T.

*- actualiza el correlativo del parmae 
SELECT parma
SET ORDER TO TAG parmae1
SEEK 'CORREL'+'NCRESU'
IF FOUND()
   IF f_lock(1)
      REPLACE parma.nument WITH vCorNc
   ENDIF
ELSE
   DO standby WITH 'No existe correlativo para N/C por tipo de Doc.'
ENDIF
*-
SELECT Diar
SET INDEX TO


RETURN

FUNCTION DESCRI
*--------------
vAli=ALIAS()
xCuenta=LEFT(Diar.Cuenta,2)
vCuenta=LEFT(Diar.Cuenta,2)+'00000000'
SELECT Cuen
SEEK vCuenta
IF FOUND()
   =ZZ()
   _z=Val_Fun('Cuen','Cuenta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN


FUNCTION DESCRI1
*---------------
vAli=ALIAS()
xCuenta=LEFT(Diar.Cuenta,3)
vCuenta=LEFT(Diar.Cuenta,3)+'0000000'
SELECT Cuen
SEEK vCuenta
IF FOUND()
   =YY()
   _z=Val_Fun('Cuen','Cuenta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN


FUNCTION DSUMA
*--------------
vRec=RECNO()
vKey = SUBSTR(Diar.Comprob,1,2)+Diar.tipo+substr(Cuenta,1,2)
STORE 0 TO vDMonto,vHMonto
SCAN WHILE SUBSTR(Diar.Comprob,1,2)+Diar.tipo+substr(Cuenta,1,2) = vKey
        vDMonto = vDMonto + Diar.Montos
        vDTot   =  vDTot  + Diar.Montos
ENDSCAN
GO vRec
RETURN vDMonto

FUNCTION HSUMA
*--------------
vRec=RECNO()
vKey = SUBSTR(Diar.Comprob,1,2)+Diar.tipo+substr(Cuenta,1,2)
STORE 0 TO vDMonto,vHMonto
SCAN WHILE SUBSTR(Diar.Comprob,1,2)+Diar.tipo+substr(Cuenta,1,2) = vKey
        vhMonto = vhMonto + Diar.Montos
        vhTot   =  vhTot  + Diar.Montos
ENDSCAN
GO vRec
RETURN vhMonto


FUNCTION sub_total
*-----------------
IF Diar.Tipo='D'
	vDVan =  vDVan + DSUMA()
ELSE	
	vHVan =  vHVan + HSUMA()
ENDIF	

RETURN ' '


FUNCTION leeglo
*--------------

PARAMETER wano,wdoc

SELE glos
SEEK (wano+wdoc)
IF FOUND()
   STORE Glos.Glosa TO wglosa 
ENDIF
RETURN wglosa

FUNCTION ZZ
*----------
RETURN xCuenta

FUNCTION YY
*----------
RETURN xCuenta

FUNCTION fin
*-----------
RETURN vfin=recno()

FUNCTION vDebe
*-------------
SELECT Diar
GO TOP
SCAN
    IF Diar.Tipo='D'
       vMonDeb = vMonDeb + Diar.Montos
    ELSE
       vMonHab = vMonHab + Diar.Montos
    ENDIF
ENDSCAN
=vHaber()
RETURN vMonDeb


FUNCTION vHaber
*--------------
RETURN vMonHab

FUNCTION nivel
*-------------
PARAMETER xniv
RETURN IIF(ALLTRIM(xniv)="Sede",SUBSTR(CUENTA,4,2)$"0102",IIF(ALLTRIM(xniv)="Adm.Cen",SUBSTR(CUENTA,4,4)$"0101",.T.))
