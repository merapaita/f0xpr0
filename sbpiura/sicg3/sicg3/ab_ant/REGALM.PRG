* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  regalm.Prg                           Sistema REGION GRAU           º
* º  Emite un Reporte de Control Visible Almacen                        º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*- Abriendo Archivos
 USE Parmae  IN 1  ORDER TAG Parmae1  ALIAS Parma
 USE ArtMae  IN 2  ORDER TAG ArtMae1  ALIAS Produ
 USE IteArt  IN 3  ORDER TAG IteArt1  ALIAS ITEART
 USE IteOc	 IN 6  ORDER TAG IteOC1	  ALIAS IteOc
 USE Itepec	 IN 5  ORDER TAG Itepec1  ALIAS Itepec
 USE OrdCom  IN 15 ORDER TAG OrdCom1  ALIAS Orden
 USE Pecosa  IN 16 ORDER TAG Pecosa1  ALIAS Pecosa
 ON KEY LABEL F2 DO FUNBUSDET

 DEFINE WINDOW Lis FROM 06,10 TO 18,70 DOUBLE ;
 TITLE ' °° Control Visible de Almac‚n °° ' FLOAT COLOR SCHEME 5

 DEFINE WINDOW Wind_3 FROM 01,04 TO 21,77 DOUBLE ;
 TITLE 'Relaci¢n de Art¡culos'     COLOR SCHEME 10

 PUBLIC vmesk,vBanpec,vBanOc

 ACTIVATE WINDOW Lis
 STORE SPACE(10)  TO vCodArt
 STORE DATE()     TO vFecIni, vFecFin
 vTE = 1
 SELECT Parma
 SEEK 'CIERRE'
 vFecIni = IIF( FOUND(), Parma.Fecha+1, {  /  /  } )
 vCodFte = '  '
 SELECT Produ

 @ 01,02 SAY "     Entre los d¡as: "
 @ 01,24 GET vFecIni  PICTURE '@D' COLOR SCHEME 7
 @ 01,34 GET vFecFin  PICTURE '@D' VALID (vFecFin >= vFecIni) AND As_mes() COLOR SCHEME 7
 @ 03,02 SAY "Fuente de Financiam: " get vCodFte PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE',' ',24,20)

 @ 05,02 SAY "Todos los productos: " GET vTE  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6  VALID LmpLin(vTE, 4, 24)
 @ 06,02 SAY "C¢digo del producto: "
* @ 06,24 GET vCodArt  WHEN vTE=2   PICTURE '!!!!!!!!!!' VALID VAL_ART(vCodArt,.T.,4,34)  COLOR SCHEME 7
 @ 06,24 GET vCodArt  WHEN vTE=2   PICTURE '!!!!!!!!!!' VALID FBUT()  COLOR SCHEME 7
 @ 08,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
 READ CYCLE
 RELEASE WINDOW Lis
 IF OkCancel = 1
    ACTIVATE WINDOW STANDBY
    @ 01,04 SAY 'Espere un momento........' 

    USE Kardex  IN 4 ORDER TAG kardex1        ALIAS kardexa
    SELECT KARDEXa
    vInd = SYS(3) + '.IDX'
    xInd = SYS(3) + '.DBF'
    COPY STRU TO (xInd)
    USE (xind) IN 10  ALIAS Kardex EXCLU
    ***********
    * DE PECOSA
    ***********
    sele itepec
    set relation to periodo+numpec INTO PECOSA
    set filt to pecosa.codfte=alltrim(vcodfte) and itepec.estado='50' and IIF( vTE=1, .T., CodArt = ALLTRIM(vCodArt) ) and between(itepec.fecdesp,vfecini,vfecfin)
    go top
    SCAN 
       SCATTER MEMVAR
       m.tipdoc='PEC'
       m.tipord='2'
       m.numdoc=m.numpec
       m.coddep=pecosa.coddep
       m.fecdoc=pecosa.fecpec
       m.fecsal=m.fecdesp
       m.fecHA =m.fecdesp       
       m.salcan=m.candesp
       m.numref=m.numoc
       m.preunis=iif(m.tippec='S',m.cosmed,m.preuni)
       m.salimp=m.candesp*iif(m.tippec='S',m.cosmed,m.preuni)
       SELECT KARDEX
       APPEND BLANK
       GATHER Memvar
       m.salcan=0
       m.salimp=0
       m.fecsal = ctod("  -  -  ")
       selec itepec
    ENDSCAN     
    SET FILTER TO
    SET RELA TO
    
    **********************
    * DE ORDENES DE COMPRA
    **********************
    sele iteOC
    set relation to periodo+numOC INTO ORDEN
    set filt to orden.codfte=alltrim(vcodfte) and iteoc.estado='50'  and IIF( vTE=1, .T., CodArt = ALLTRIM(vCodArt) ) and between(iteoc.fecdesp,vfecini,vfecfin)
    go top
    SCAN 
       SCATTER MEMVAR
       m.tipdoc='O/C'
       m.tipord='1'
       m.numdoc=m.numOC
       m.coddep=m.coddep
       m.fecdoc=ORDEN.fecOC
       m.entcan=m.candesp
       m.preunie=m.preuni
       m.fecent=m.fecdesp       
       m.feCHA=m.fecdesp       
       m.numref=m.numpec
       m.entimp = m.candesp*m.preuni
       SELECT KARDEX
       APPEND BLANK
       GATHER Memvar
       m.entcan=0
       m.entimp=0
       selec iteOC
    ENDSCAN     
    SET FILTER TO
    SET RELA TO
    
    sele kardex
    INDEX ON CodArt +DTOS(fecha) +tipord  TO (vInd)
    go top  
    DEACTIVATE WINDOW STANDBY
    
    IF !EOF()
      vStkIni = 0
      vCtoIni = 0
      DO REPORTE WITH 2,'Covial2' ,' Emisi¢n del Kardex de Existencias '
    ELSE
      DO STANDBY WITH 'No se tienen movimientos'
    ENDIF
    CLOSE DATA
    ERASE (vInd)
 ENDIF
 ON KEY LABEL F2
 CLOSE DATA
 RETURN

PROCEDURE AS_MES
*---------------
if month(vfecini) >= 1 AND month(vfecini) <= 5
   vMesk = 	'12' + right(alltrim(str(year(vfecini)-1)),2)
else
   vMesk = 	'06' + right(alltrim(str(year(vfecini))),2)
endif
return .t.   

PROCEDURE FBUT
*-------------
SELE ITEART
SET ORDE TO 3
SEEK VCODART
IF !FOUND() OR EMPTY(VCODART)
    SET ORDE TO 2
	DO FUNBUSDET
	
	BROWSE WINDOW Wind_3 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     Iteart.CodArt  :H='Articulo':10 ,;
     iteart.Descri  :H='Descripci¢n' :35,;
     Produ.CodArt  :H='Det':7,;
     Produ.Descri  :H='Detalle'
	SET ORDER TO 1
	SET RELA TO
ENDIF	
VCODART = ITEART.CODART
RETURN .T.


FUNCTION LmpLin
*--------------
PARAMETERS _Todo, _X,_Y
PRIVATE _Todo, _X, _Y
IF _Todo = 1
  @ _X,_Y CLEAR TO _X,79
ENDIF
RETURN .T.

Function sal_ini
*---------------
private vkey
*vkey = Query.Codart
vkey = KARDEX.Codart
select iteart
SET ORDER TO IteArt3
seek vkey
if found()
   vCampo = 'INV'+vMesk+vCodfte
   xSaldo = iteart.&vcampo
else      
   xSaldo = 0
endif
SET ORDER TO IteArt1
select KARDEX
return xsaldo


FUNCTION val_artk    && Valida Articulos
*---------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE medita, mmsg, malias, v_fun, _oldwind,_campo
medita = (PARAMETERS()>=2)
mmsg   = (PARAMETERS()=4) .AND.  _tipo

_campo = VARREAD()

malias = ALIAS()
SELECT iteart
_oldwnd = WOUTPUT()
IF !medita
	SEEK xcod
	v_fun = IIF(FOUND(),descri,"")
ELSE
	IF EMPTY(xcod)
		SET ORDER TO 1
		SET FILTER TO SUBSTR(codart,2,2)=SUBSTR(ALLTRIM(parma.codigo),2,2)  &&AND LEFT(CODART,1)='B'
		ACTIVATE WINDOW standby
		@ 1,14 SAY 'Espere un Momento ....' COLOR W*
		GO TOP

		IF EOF()
			DEACTIVATE WINDOW standby
			ACTIVATE SCREEN
			SET FILTER TO
			v_fun = .F.
		ELSE

			DEACTIVATE WINDOW standby
			ACTIVATE SCREEN
			ON KEY LABEL f10 KEYBOARD CHR(23)
			ON KEY LABEL f2 DO funbus
			DEFINE WINDOW _busart FROM 2,02 TO 22,77
			BROWSE WINDOW _busart TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU FIELDS;
				codart   :H='C¢digo'    :W=EMPTY(SUBSTR(codart,5,3)),;
				descri   :H='Nombre':60 :W=EMPTY(descri)
			ON KEY LABEL f10
			ON KEY LABEL f2
			RELEASE WINDOW _busart
			SET ORDE TO 1
			SET FILTER TO
			IF LASTKEY()=27
				v_fun = .F.
			ELSE
				xcod = codart
				IF mmsg
					@ _x,_y SAY descri
				ENDIF
				SELECT (malias)
				IF !_tipo
					REPLACE &_campo WITH  xcod
				ENDIF
				v_fun = .T.
			ENDIF
		ENDIF
	ELSE
		SEEK xcod
		IF mmsg .AND. FOUND()
			@ _x,_y SAY descri
		ENDIF
		v_fun = FOUND()
	ENDIF
ENDIF
SELECT (malias)
RETURN v_fun

