** --------------------------------
** AUXSTA.PRG
** Genera reporte Auxiliar Standar
** AUTOR: UDEP - 1994. JECHS
** Modificado Integralmente por : FMC
** Reporte AuxSta por JFG
** --------------------------------

* Abrir Archivos de trabajo
USE Diario  IN 1  ORDER TAG Diario11        ALIAS Diar
USE Compro  IN 2  ORDER TAG Compro7         ALIAS Comp
USE Cuentas IN 3  ORDER TAG Cuentas1        ALIAS Cuen
USE Parmae  IN 4  ORDER TAG Parmae1         ALIAS Parma
USE Auxil   IN 5  ORDER TAG Auxil1          ALIAS Auxi
USE Cajas   IN 10 ORDER TAG Cajas1          ALIAS Caja
USE Saldona IN 13 ORDER TAG Saldona1        ALIAS Sald

PUBLIC vCuenta, vDMonto, vHMonto, vDigito, vPrograma,vcodcal,vIndPar,VMes,VAno,VanDebe,VanHaber,vsedpli,XMES,XANO,vIndpar
PUBLIC vnivel
vIndpar=SYS(3)+'.idx'
vsedpli='Sede   '

STORE SPACE(10) TO vProv,vCodP,vCodi
STORE SPACE(15) TO vCuenta
STORE SPACE(19) TO vProg,vCodcal
STORE SPACE(06) TO vRegA,vTipA,vCodA,vCodaux
STORE 0 TO xano, xMes, vDigito
STORE SPACE(02) TO vMes
STORE SPACE(04) TO vCueIni,vCueFin
STORE SPACE(02) TO vtipaux,vnivel
STORE SPACE(01) TO vTipObr 
STORE SPACE(04) TO vCodobra,vAno
STORE 0 TO FINARCHIVO

OKCANCEL = 1
DO Inicia
DO salida
RETURN

PROCEDURE Inicia
*---------------
  vcol =" W+/BG, N/W, N/W, GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "
  
  IF Escolor
     DEFINE POPUP _imp FROM 08,52 SHADOW COLOR &vcol
   ELSE
     DEFINE POPUP _imp FROM 08,52 COLOR SCHEME c_popup
  ENDIF
  
     DEFINE BAR 1 OF _imp PROMPT "\<Una cuenta       "
     DEFINE BAR 2 OF _imp PROMPT "\<Rango de Cuentas "
     DEFINE BAR 3 OF _imp PROMPT "\<Todas las Cuentas"
*     DEFINE BAR 4 OF _imp PROMPT "\-"
*     DEFINE BAR 5 OF _imp PROMPT "Por \<Cta.Cte      "
*     DEFINE BAR 6 OF _imp PROMPT "Por Pr\<Ograma     "
*     DEFINE BAR 7 OF _imp PROMPT "Por Pro\<veedor    "
*     DEFINE BAR 8 OF _imp PROMPT "Por O\<Bra         "

     ON SELECTION POPUP _imp DEACTIVATE POPUP
     ACTIVATE POPUP _imp
  
  IF LASTKEY()=27
     RETURN
  ENDIF
  
  DO Indexa_Par
  DO CASE
     CASE BAR()=1
     	  DEFINE WINDOW _fe FROM 07,15 TO 20,65 FLOAT DOUBLE TITLE ' Auxiliar Standar de una Cuenta  ' COLOR SCHEME 5
          ACTIVATE WINDOW _fe
          vDigito=1
          @ 01,02 SAY "            Digitos : " GET vDigito  FUNCTION '^ 4 D¡gitos;1 D¡gito;15 D¡gitos'
          @ 04,02 SAY "             Cuenta : " GET vCuenta
          @ 06,02 SAY "            Mes/Ano : " GET vMes
          @ 06,29 GET vAno
          * @ 08,02 SAY 'Adm.Cen/Sede/Pliego : ' GET vsedpli  PICTURE '@M Adm.Cen,Sede   ,Pliego '  
		  @ 10,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
          READ CYCLE
          vsedpli=upper(vsedpli)
          
          IF VAL(vmes)=1
          
             *xmes=12
             *xano=VAL(vano)-1
             *- Si se reporta el primer mes ya no lee los acumulados
             *- a diciembre del periodo anterior sino que el acumulado
             *- es CERO. (Felipe Paz - Jorge Zapata)
             xmes=00
             xano=00
             
          ELSE
             xmes=VAL(VMES)-1
             xano=VAL(Vano)
          ENDIF   
          
          vnivel=IIF(vsedpli='ADM.CEN','01',IIF(vsedpli='SEDE   ','02','03'))
          
		  RELEASE WINDOW _fe
		  IF OKCANCEL = 1
          SELE Diar
          IF LASTKEY()#27
             DO ESPERA WITH 1,'Espere un Momento; Procesando Informaci¢n ....!'
             vind = SYS(3) + '.idx'
             SET RELA TO Comprob+ComAdi INTO Comp
             DO CASE
                CASE vDigito=1
                	  INDEX ON Cuenta+DTOC(FecEmi)+Comprob+ComAdi+Codfte;
                      FOR LEFT(Cuenta,4)=LEFT(vCuenta,4) AND MONTH(FECEMI)=VAL(vMes) AND STR(YEAR(FECEMI),4)=vAno   TO (vind)
                      =ComparaFin()
                CASE vDigito=2
                  	  INDEX ON Cuenta+DTOC(FecEmi)+Comprob+ComAdi+Codfte;
                      FOR LEFT(Cuenta,1)=LEFT(vCuenta,1) AND MONTH(FECEMI)=VAL(vMes) AND STR(YEAR(FECEMI),4)=vAno   TO (vind)
                      =ComparaFin()
                CASE vDigito=3
                  	  INDEX ON Cuenta+DTOC(FecEmi)+Comprob+ComAdi+Codfte;
                      FOR Cuenta=vCuenta AND MONTH(FECEMI)=VAL(vMes) AND STR(YEAR(FECEMI),4)=vAno   TO (vind)
                      =ComparaFin()
             ENDCASE         
             
             DO ESPERA WITH 2,' '
             IF EOF()
                DO STANDBY WITH "No hay registros para procesar"
             ELSE
                DO REPORTE WITH 2, "AUXSTA2", ' Auxiliar Standar ',1,.F.,.T.
             ENDIF
             CLOSE INDEX
             ERASE (vind)
          ENDIF
     ENDIF 
         
     CASE BAR()=2                        && Rango de Cuentas
     	  DEFINE WINDOW _fe FROM 12,15 TO 20,65 FLOAT DOUBLE TITLE ' Listado de Auxiliar Standar Rango de Cuentas ' COLOR SCHEME 5
          ACTIVATE WINDOW _fe
          vDigito=1
          @ 01,5 SAY "          Mes y Ano: " GET vMes
          @ 01,32 GET vAno
          @ 02,5 SAY "      Cuentas Desde: " GET vCueINI PICTURE '!!!!'
          @ 03,5 SAY "              Hasta: " GET vCuefin PICTURE '!!!!'
          * @ 04,5 SAY 'Adm.Cen/Sede/Pliego: ' GET vsedpli  PICTURE '@M Adm.Cen,Sede   ,Pliego '  
		  @ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
          READ CYCLE
          vsedpli=upper(vsedpli)
          IF VAL(vmes)=1
             xmes=00
             xano=00
          ELSE
             xmes=VAL(VMES)-1
             xano=VAL(Vano)
          ENDIF
          vnivel=IIF(vsedpli='ADM.CEN','01',IIF(vsedpli='SEDE   ','02','03'))
          
		  RELEASE WINDOW _fe
		  IF OKCANCEL = 1
          vind = SYS(3) + '.idx'
          SELE Diar
          SET RELA TO Comprob+ComAdi INTO Comp
          DO ESPERA WITH 1,'Espere un Momento; Procesando Informaci¢n ....!'
       	  INDEX ON Cuenta+DTOC(FecEmi)+Comprob+ComAdi+Codfte;
                      FOR MONTH(FECEMI)=VAL(vMes) AND STR(YEAR(FECEMI),4)=vAno  AND BETWEEN(LEFT(diar.cuenta,4),vCueIni,vCueFin) TO (vind)
          =ComparaFin()
          DO ESPERA WITH 2,' '
          IF EOF()
	          DO STANDBY WITH "No hay registros para procesar"
    	  ELSE
	          DO REPORTE WITH 2, "AUXSTA2", ' Auxiliar Standar ',1,.F.,.T.    	     
    	  ENDIF
          CLOSE INDEX
          ERASE (vind)
          ENDIF
          
     CASE BAR()=3                        && Todas las cuentas
     	  DEFINE WINDOW _fe FROM 12,15 TO 20,65 FLOAT DOUBLE TITLE ' Auxiliar Standar de Todas Las Cuentas ' COLOR SCHEME 5
          ACTIVATE WINDOW _fe
          vDigito=1
          @ 02,13 SAY "Mes/Ano: " GET vMes
          @ 02,27 GET vAno
          * @ 04,13 SAY 'Adm.Cen/Sede/Pliego : ' GET vsedpli  PICTURE '@M Adm.Cen,Sede   ,Pliego '  
		  @ 06,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
          READ CYCLE
          vsedpli=upper(vsedpli)
          IF VAL(vmes)=1
             xmes=00
             xano=00
          ELSE
             xmes=VAL(VMES)-1
             xano=VAL(Vano)
          ENDIF   
          vnivel=IIF(vsedpli='ADM.CEN','01',IIF(vsedpli='SEDE   ','02','03'))
		  RELEASE WINDOW _fe
		  IF OKCANCEL = 1
          vind = SYS(3) + '.idx'
          vDbf = SYS(3) + '.dbf'
          SELE Diar
          DO ESPERA WITH 1,'Espere un Momento; Procesando Informaci¢n ....!'
          COPY TO &vDbf FOR MONTH(FECEMI)=VAL(vMes) AND STR(YEAR(FECEMI),4)=vAno
susp
*****
          USE &vDbf ALIAS Diar
          REPLACE Cuenta WITH 'O'+SUBSTR(Cuenta,2,9) FOR LEFT(Cuenta,1)='0'
*****
          SET RELA TO Comprob+ComAdi INTO Comp
       	  INDEX ON Cuenta+DTOC(FecEmi)+Comprob+ComAdi+Codfte TO (vind)
       	  cArch = "aux"+righ(vAno,2)+vmes
       	  COPY TO (cArch)
*       	  INDEX ON Cuenta+DTOC(FecEmi)+Comprob+ComAdi+Codfte;
*                   FOR MONTH(FECEMI)=VAL(vMes) AND SUBSTR(STR(YEAR(FECEMI),4),3,2)=vAno  TO (vind)
          =ComparaFin()
          DO ESPERA WITH 2,' '
          IF EOF()
	          DO STANDBY WITH "No hay registros para procesar"
    	  ELSE
	          DO REPORTE WITH 2, "AUXSTA2", ' Auxiliar Standar ',1,.F.,.T.
    	  ENDIF
          CLOSE INDEX
          ERASE (vind)
          USE
          ERASE (vDbf)
          ENDIF
  ENDCASE
RETURN


FUNCTION Par
*-----------
PRIVATE vAli
vAli = SELECT()
vOrd=ORDER()
SELECT Auxil
SET ORDER TO AUXIL4

SEEK ALLTRIM(vTipA)

SET FILTER TO Tipo = ALLTRIM(vTipA)

DEFINE POPUP VerCli FROM 4,10 TO 16,60 PROMPT FIELD Codigo+' '+Descri
ON SELECTION POPUP VerCli DEACTIVATE POPUP
ACTIVATE POPUP VerCli

vTipA = Auxi.Tipo
vRegA = Auxi.Codigo

XX=SUBSTR(Descri,1,17)
@ 03,28 SAY XX
  
SET FILTER TO
SELECT (vALi)
SET ORDER TO vOrd
RETURN


FUNCTION DESCRI
*--------------
vAli=ALIAS()
vOrd=ORDER()
vComprob=Diar.DocRef
vComAdi =Diar.DocAdi
SELECT Comp
SET ORDER TO TAG Compro1
SEEK vComprob+vComAdi
IF FOUND()
   RETURN Comp.Glosa1
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '


FUNCTION DESCRI1
*---------------
vAli=ALIAS()
vOrd=ORDER()
vComprob=Diar.Comprob
vComAdi =Diar.ComAdi
SELECT Comp
SET ORDER TO TAG Compro1
SEEK vComprob+vComAdi
IF FOUND()
   RETURN Comp.Glosa3
   * RETURN Comp.Glosa1
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '


FUNCTION CODI
*------------
IF !EMPTY(vRegA)
   SELECT Parma
   SEEK vTipA+vRegA
   @ 3,26 SAY Parma.Descri
ENDIF
RETURN


FUNCTION GLOSA
*-------------
vAli=ALIAS()
vOrd=ORDER()
vCuenta=LEFT(Diar.Cuenta,2)+'00000000'
SELECT Cuen
SET ORDER TO TAG Cuentas1
SEEK vCuenta
IF FOUND()
   RETURN Cuen.Descri
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '


FUNCTION GLOSA1
*--------------
vAli=ALIAS()
vOrd=ORDER()
vCuenta=LEFT(Diar.Cuenta,3)+'0000000'
SELECT Cuen
SET ORDER TO TAG Cuentas1
SEEK vCuenta
IF FOUND()
   RETURN Cuen.Descri
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '

FUNCTION GLOSA2
*--------------
vAli=ALIAS()
vOrd=ORDER()
vCuenta=Diar.Cuenta
SELECT Cuen
SET ORDER TO TAG Cuentas1
SEEK vCuenta
IF FOUND()
   RETURN Cuen.Descri
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '




PROCEDURE PROGRAMA
*-----------------
vAli=ALIAS()
vOrd=ORDER()
vProg=SUBSTR(Comp.CodCal,8,2)
SELECT Parma
SEEK 'CODPRG'+vProg
IF FOUND()
   RETURN Parma.Descri
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '


PROCEDURE Indexa_par
*-------------------
*- Indexa parmae
SELE PARMA
SET ORDER TO
INDEX ON codigoaux TO (vIndPar) FOR parma.tipo='TIPDOC'
SET INDEX TO
SET ORDER TO 1
=ResetV()
RETURN


FUNCTION RTipoDoc
*---------------
PARAMETER numD
Area  = SELECT()
xOrd = ORDER()
SELECT Parma
SET INDEX TO (vIndPar)
SEEK numD
ret = parma.codigo
SELECT(Area)
SET ORDER TO (xOrd)
RETURN ret


PROCEDURE Salida
*---------------
ACTIVATE   SCREEN
CLOSE DATABASE
erase(vindpar)
RETURN

FUNCTION Fin
*-----------
PARAMETER nR
RETURN nR=FinArchivo

FUNCTION Resetv
*---------------
VanDebe  = 0
VanHaber = 0
RETURN ''


FUNCTION Acumula
*---------------
VanDebe  = VanDebe+IIF(Diar.Tipo='D',Diar.Montos,0)
VanHaber = VanHaber+IIF(Diar.Tipo='H',Diar.Montos,0)
RETURN ''


FUNCTION Glosas
*--------------
PARAMETER conObra
IF PARAMETER()=0
   conObra=.T.
ENDIF   
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

*-HOJA DE CONTROL
*r=r+' '+IIF(!Empty(Diar.NumHC),'Ref. H/C :'+ Diar.NummesHC+'-'+Diar.NumHC,'')
*--

IF conObra
*--OBRA
IF !EMPTY(diar.codobra)
   vAli=SELECT()
   SELECT obra
   SEEK diar.codobra
   IF FOUND()
      r=r+' ( OBRA: '+diar.codobra+' - '+ALLTRIM(obra.descri)+' )'
   ENDIF
   SELECT(vAli)
ENDIF      
*--
ENDIF

RETURN IIF(!Empty(r),ALLTRIM(r),'No se encontr¢ detalle/glosa para este Documento.')


FUNCTION ComparaFin
*------------------
* Establece el fin del archivo
GO BOTTOM
FinArchivo=RECNO()
GO TOP
RETURN .T.

*FUNCTION ambito
*--------------
PARAMETER vsedpli
RETURN IIF(alltrim(vsedpli)='SEDE',SUBSTR(Diar.Cuenta,4,2)$'0102',IIF(alltrim(vsedpli)='ADM.CEN',SUBSTR(Diar.Cuenta,4,4)='0101',.T.))

        

*FUNCTION Rambito  && Para que en el reporte aparezca de que se trata.
*---------------
PARAMETER vsedpli
RETURN IIF(alltrim(vsedpli)='SEDE','PROGRAMA : SEDE CENTRAL',IIF(alltrim(vsedpli)='ADM.CEN','PROGRAMA : ADMINISTRACION CENTRAL','PLIEGO 01: REGION GRAU'))


FUNCTION desc_prov
*-----------------
PARAMETERS xtipo,xreg
area=SELECT()
vRet=""
SELECT auxi
SEEK xtipo+xreg
IF FOUND()
   vRet=LEFT(auxi.descri,26)
ENDIF
SELECT(area)
RETURN vret   


FUNCTION DAcuAnt
*---------------
PARAMETERS xcuen
vAli=SELECT()
SELECT SalD
vret=0
FOR J=0 TO xMes
    SEEK PADR(xcuen,15,'0')+ STR(j,2) + str(xAno,4)
    vRet = vret + IIF(FOUND(),salD.DSumana,0)
ENDFOR    
SELECT(vAli)
RETURN vRet


FUNCTION HAcuAnt
*---------------
PARAMETERS xcuen
vAli=SELECT()
SELECT SalD
vret = 0
FOR J=0 TO xMes
    SEEK PADR(xcuen,15,'0')+ STR(j,2) + str(xAno,4)
    vRet = vret + IIF(FOUND(),salD.HSumana,0)
ENDFOR
SELECT(vAli)
RETURN vRet



FUNCTION Cod_proy
*----------------
vperiodo  = LEFT(comp.codcal,2)
vcodprg   = SUBSTR(comp.codcal,8,2)
vcodsubpr = SUBSTR(comp.codcal,10,3)
vcodproy  = SUBSTR(comp.codcal,13,3)
vcodact   = SUBSTR(comp.codcal,16,2)
vret = vcodprg+'.'+vcodsubpr+'.'+vcodproy+'.'+vcodact
RETURN vret


FUNCTION des_proy
*----------------
vperiodo  = LEFT(comp.codcal,2)
vcodprg   = SUBSTR(comp.codcal,8,2)
vcodsubpr = SUBSTR(comp.codcal,10,3)
vcodproy  = SUBSTR(comp.codcal,13,3)
vret      = SPACE(0)
vcodprg   = vperiodo+vcodprg+vcodsubpr+vcodproy
IF LEN(alltrim(vcodprg))=0
   RETURN 'No existe C¢digo del Proyecto'
ENDIF   
Alias = SELECT()
SELECT maepre
SEEK vcodprg
IF FOUND() 
   vret = maepre.descri
ENDIF
SELECT(alias)   
RETURN vret

   
FUNCTION vObra
*-------------
PARAMETER vtc
IF vtc=2
   vAli=Select()
   vCuenta=''
   SELECT Parma
   SEEK 'CTAOBR'
   SCAN WHILE parma.tipo='CTAOBR'
        vcuenta = vcuenta + ALLTRIM(parma.codigo)+', '
   ENDSCAN
   vcuenta=SUBS(vcuenta,1,len(vcuenta)-2)
   @ 04,20 SAY LEFT(vcuenta,21)+'...'
   SELECT(vAli)
ENDIF
RETURN .T.
   