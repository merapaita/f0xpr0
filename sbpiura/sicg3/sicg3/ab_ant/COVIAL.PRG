* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  COVIAL.Prg                           Sistema REGION GRAU           º
* º  Emite un Reporte de Control Visible Almacen                        º
* º  Autor: Ing. Luis Castillo Dur n                                    º
* º  Fecha: Piura 23 - 09 - 94                                          º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*- Abriendo Archivos
 USE Parmae  IN 1 ORDER TAG Parmae1        ALIAS Parma
 USE ArtMae  IN 2 ORDER TAG ArtMae1        ALIAS Produ
 USE IteArt  IN 3 ORDER TAG IteArt1        ALIAS Iteart
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
 vCodFte = '   '
 SELECT Produ

 @ 01,02 SAY "     Entre los d¡as: "
 @ 01,24 GET vFecIni  PICTURE '@D' COLOR SCHEME 7
 @ 01,34 GET vFecFin  PICTURE '@D' VALID (vFecFin >= vFecIni) AND As_mes() COLOR SCHEME 7
 @ 03,02 SAY "Fuente de Financiam: " get vCodFte PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE',' ',24,20)

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

    USE Kardex  IN 4 ORDER TAG kardex1        ALIAS kardex

    SELECT KARDEX
    vInd = SYS(3) + '.IDX'
*    INDEX ON CodArt + DTOS( Fecdoc )+ TipOrd  TO (vInd) ;
*       FOR FecEnt <= vFecFin   AND ;
*           IIF( vTE=1, .T., vCodArt = CodArt ) AND substr(codcal,5,3)=alltrim(vcodfte)

    SELECT IIF(EMPTY(KARDEX.FECSAL),KARDEX.FECENT,KARDEX.FECSAL) AS vfecha,  KARDEX.TIPDOC, KARDEX.TIPORD,;
  	KARDEX.NUMDOC, KARDEX.CODART, KARDEX.ENTCAN, KARDEX.SALCAN, KARDEX.codcal ;
 	FROM KARDEX ;
	WHERE IIF( vTE=1, .T., CodArt = vCodArt ) AND substr(codcal,5,3)=alltrim(vcodfte);
 	INTO CURSOR Query
    SELECT Query
    INDEX ON CodArt + DTOS(vFecha)+ TipOrd  TO (vInd) ;
		FOR vFecha <= vFecFin
    SET RELATION TO 'B'+LEFT(CODART,11) INTO ITEART
    GO TOP
    DEACTIVATE WINDOW STANDBY
    
    IF !EOF()
      vStkIni = 0
      vCtoIni = 0
      DO REPORTE WITH 2,'Covial1' ,' Emisi¢n del Kardex de Existencias '
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
vkey = Query.Codart
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
select query
return xsaldo


