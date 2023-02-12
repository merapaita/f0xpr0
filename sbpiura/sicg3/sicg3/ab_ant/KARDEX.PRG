* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  KARDEX.Prg                           Sistema REGION GRAU           º
* º  Emite un kardex de las existencias                                 º
* º  Autor: Ing. Luis Castillo Dur n                                    º
* º  Fecha: Piura 22 - 09 - 94                                          º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*- Abriendo Archivos
 USE Parmae  IN 1 ORDER TAG Parmae1        ALIAS Parma
 USE ArtMae  IN 2 ORDER TAG ArtMae1        ALIAS Produ
 USE IteArt  IN 3 ORDER TAG IteArt1        ALIAS Iteart
 USE IteOc	 IN 6 ORDER TAG IteOC1		   ALIAS IteOc
 USE Itepec	 IN 5 ORDER TAG Itepec1		   ALIAS Itepec

 ON KEY LABEL F2 DO FUNBUSDET 
 DEFINE WINDOW Lis FROM 06,10 TO 18,70 DOUBLE ;
 TITLE ' °° Emisi¢n del Kardex °° ' FLOAT COLOR SCHEME 5

 DEFINE WINDOW Wind_3 FROM 04,01 TO 21,77 DOUBLE ;
 TITLE 'Relaci¢n de Art¡culos'     COLOR SCHEME 10
 public vmesk,vBanpec,vBanOc, vCodFte
 
 ACTIVATE WINDOW Lis
 STORE SPACE(10)  TO vCodArt
 STORE DATE()     TO vFecIni, vFecFin
 vBanOC  = .F. 
 vBanpec = .F. 
 vAcuOc = 0
 ventoc = 0
 vAcupec = 0
 ventpec = 0

 vTE = 1
 SELECT Parma
 SEEK 'CIERRE'
 vFecIni = IIF( FOUND(), Parma.Fecha+1, {  /  /  } )
 vCodFte = '   '
 SELECT Produ

 @ 01,02 SAY "     Entre los d¡as: "
 @ 01,24 GET vFecIni  PICTURE '@D' COLOR SCHEME 7
 @ 01,34 GET vFecFin  PICTURE '@D' VALID (vFecFin >= vFecIni) AND AS_MES() COLOR SCHEME 7
 @ 03,02 SAY "Fuente de Financiam: " get vCodFte PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE',' ',24,20)

 @ 05,02 SAY "Todos los productos: " GET vTE  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6  VALID LmpLin(vTE, 4, 24)
 @ 06,02 SAY "C¢digo del producto: "
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
    
*    INDEX ON CodArt +tipord+ DTOS( Fecent )  TO (vInd) ;
*       FOR FecEnt <= vFecFin   AND ;
*           IIF( vTE=1, .T., vCodArt = CodArt ) AND codfte = alltrim(vcodfte)  &&substr(codcal,5,3)=alltrim(vcodfte)

    SELECT IIF(EMPTY(KARDEX.FECSAL),KARDEX.FECENT,KARDEX.FECSAL) AS vfecha,  KARDEX.TIPDOC, KARDEX.TIPORD,;
  	KARDEX.NUMDOC, KARDEX.CODART, KARDEX.ENTCAN, KARDEX.SALCAN, KARDEX.codcal, kardex.preunis, kardex.preunie, KARDEX.CTOPRM ;
 	FROM KARDEX ;
	WHERE IIF( vTE=1, .T., CodArt = vCodArt ) AND codfte = alltrim(vcodfte);
 	INTO CURSOR Query
    SELECT Query
    
    INDEX ON CodArt +DTOS(vFecha)+tipord TO (vInd) ;
       FOR vfecha <= vFecFin  &&substr(codcal,5,3)=alltrim(vcodfte)
           
    SET RELATION TO 'B'+LEFT(CODART,11) INTO ITEART
    GO TOP
    *DEACTIVATE WINDOW STANDBY
    IF !EOF()
      vStkIni = 0
      vStkFIN = 0
      vCtoIni = 0
      DO cost_pro
      DEACTIVATE WINDOW STANDBY
      DO REPORTE WITH 2,'EmiKar2' ,' Emisi¢n del Kardex de Existencias '
    ELSE
      DEACTIVATE WINDOW STANDBY
      DO STANDBY WITH 'No se tienen movimientos'
    ENDIF
    CLOSE DATA
    ERASE (vInd)
 ENDIF
 CLOSE DATA
 ON KEY LABEL F2
 RELEASE WINDOW WIND_3
RETURN

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

PROCEDURE AS_MES
*---------------
if month(vfecini) >= 1 AND month(vfecini) <= 5
   vMesk = 	'12' + right(alltrim(str(year(vfecini)-1)),2)
else
   vMesk = 	'06' + right(alltrim(str(year(vfecini))),2)
endif
return .t.   

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
private vkey,acp

*vkey = kardex.Codart
vkey = query.Codart
select iteart
SET ORDER TO IteArt3
seek vkey
if found()
   vCampo = 'INV'+vMesk+vCodfte
   xSaldo = iteart.&vcampo
else      
   xSaldo = 0
endif
*select kardex
select query
return xsaldo



function saldoINI
*----------------
private vkey
vkey = Query.Codart
select iteart
SET ORDER TO IteArt3
seek vkey

if found()
   vCampo = 'SDO'+vMesk+vCodfte
   xSaldo = iteart.&vcampo
else      
   xSaldo = 0
endif
SET ORDER TO Iteart1
select Query
return xsaldo

FUNCTION StkIni
*--------------
PARAMETERS vCA, vFI
PRIVATE vCA, vFI, vAlias
vAlias = ALIAS()

vStkIni = 0
vCtoIni = 0

SELECT Saldo
SEEK vCA
IF FOUND() AND ( Saldo.Fecha < vFI )
  SCAN WHILE ( vCA = Saldo.CodArt ) AND ( Saldo.Fecha < vFI )
  ENDSCAN

  SKIP -1
  IF Saldo.CodArt == vCA
    vStkIni = Saldo.StkAct
    vCtoIni = Saldo.CtoAct
    vFecCie = Saldo.Fecha
  ELSE
    STORE 0 TO vStkIni, vCtoIni
    vFecCie = {  /  /  }
  ENDIF
ELSE
  vStkIni = Produ.StockI
  vCtoIni = Produ.CostoTotal
  vFecCie = {  /  /  }
ENDIF

SELECT ItMov
IF EMPTY( vFecCie )
  SEEK vCA
ELSE
  SEEK vCA + DTOS(vFecCie + 1)
ENDIF

SCAN WHILE ( ItMov.CodArt = vCA AND Movim.Fecha <= vFI )
  vStkIni = vStkIni + IIF( ItMov.TipTra = 'I', ItMov.Cantidad, -1 * ItMov.Cantidad )
  vCtoIni = vCtoIni + IIF( ItMov.TipTra = 'I', 1, -1) * ROUND( ItMov.Costo * ItMov.Cantidad, 2 )
ENDSCAN
SELECT (vAlias)
RETURN ' '

PROCEDURE SALDO
*---------------
if bof()
   vSalant = 0
else   
   SKIP -1
   vSaldant = IIF(kardex.TipDoc ='PEC',Kardex.SalCan,kardex.Entcan) 
endif
   
   
FUNCTION CtoPrm
*----------------
IF kardex.TipDoc ='O/C'
	SELECT IteOC
	SET ORDER TO IteOC1
	SEEK Kardex.periodo+Kardex.NumDoc+kardex.CodFte
	IF EMPTY(NumPec)
		vBanOC=.T.
	ELSE
		SELECT kardex
		vBanOC=.F.
	ENDIF
	IF vBanOC
		vAcuOC = vAcuOc + kardex.entCan * kardex.preuniE
		vEntOc = vEntoc + kardex.EntCan
		SELECT kardex
		return 0
	ELSE
		IF vEntOC<>0
			vretOC = vAcuOC /vEntOc
		ELSE
			vretOC = 0
		ENDIF
		vAcuOC = 0
		vEntOc = 0
		SELECT kardex
		RETURN vretOC
	ENDIF
ELSE
	SELECT ItePEC
	SET ORDER TO ItePEC1
	SEEK Kardex.periodo+Kardex.NumDoc+kardex.CodFte
	IF EMPTY(NumOC)
		vBanPec=.T.
	ELSE
		vBanPec=.F.
	ENDIF
	IF vBanPec
		vAcuPec = vAcuPec + kardex.entCan * kardex.preuniE
		vEntPec = vEntPec + kardex.EntCan
		SELECT kardex
		RETURN 0
	ELSE
		IF vEntPec<>0
			vretPec = vAcuPec /vEntPec
		ELSE
			vretpec = 0
		ENDIF
		vAcuPec = 0
		vEntPec = 0
		SELECT kardex
		RETURN vretPec
	ENDIF
ENDIF
return

PROCEDURE Cost_Pro
*-----------------
 ZInd = SYS(3) + '.IDX'
 xInd = SYS(3) + '.IDX'
 IF VTE=2
 	SELECT IteOc
 	INDEX ON CodArt+DTOS(FecDesp)+codfte+tipfun TO (ZInd) FOR !EMPTY(FecDesp) AND EMPTY(Numpec) ;
 		 and CodFte=ALLTRIM(vCodFte) AND CODART=ALLTRIM(VCODART)
  	SELECT Itepec
 	INDEX ON CodArt+DTOS(FecDesp)+codfte+tipfun TO (xInd) FOR !EMPTY(FecDesp) AND TipPec<>'O' ;
 	     AND PreUni=0 and CodFte=ALLTRIM(vCodFte) AND CODART=ALLTRIM(VCODART)
 ELSE
 	SELECT IteOc
 	INDEX ON CodArt+DTOS(FecDesp)+codfte+tipfun TO (ZInd) FOR !EMPTY(FecDesp) AND EMPTY(Numpec) ;
 	     and CodFte=ALLTRIM(vCodFte)
   	SELECT Itepec
 	INDEX ON CodArt+DTOS(FecDesp)+codfte+tipfun TO (xInd) FOR !EMPTY(FecDesp) AND TipPec<>'O' ;
 	     AND PreUni=0 and CodFte=ALLTRIM(vCodFte)
 ENDIF	
SELE ITEART
GO TOP
SCAN
 Vcodigo = CodArt
 SELECT Itepec
 seek vCodigo
 if found()
 SCAN FOR vcodigo=itepec.codart
 	* busco art¡culo en la o/c
 	vKey=Itepec.CodArt+Itepec.CodFte+Itepec.TipFun
 	SELECT IteOc
	SEEK ITEPEC.CODART
	IF FOUND()
		DO WHILE .T.
			IF CodArt+CodFte+TipFun<>vKey 
				SKIP
				IF CODART<>ITEPEC.CODART or eof()
					vPrecio=.F.
					EXIT
				ENDIF	
				LOOP
			ELSE
				vPrecio=.T.
				vCanTot=CanDesp
	   			vPreUni=PreUni
	   			EXIT
			ENDIF
		ENDDO
	ELSE
		vPrecio=.F.
	ENDIF
	IF vPrecio
	   SELECT Itepec
	   SCAN FOR CodArt+CodFte+TipFun=vKey 
	   		IF vCanTot>=CanDesp
	   		   *REPLACE CosMed with iteoc.Preuni
	   		   vCanTot=vCanTot - CanDesp
	   		ELSE
	   		   vDiferencia = CanDesp - vCanTot
	   		   SELECT IteOc
	   		   IF vCanTot<>0
	   		   		vDividendo = (Preuni*candesp)
	   		   		vDivisor   = candesp
	   		   ELSE
	   		   		vDividendo = 0
	   		   		vDivisor   = 0
	   		   ENDIF		
	   		   ok1=.t.
	   		   ok =.t.
	   		   SKIP
	   		   DO WHILE .T. and !eof()
	   		   	 	IF CodArt+CodFte+TipFun=vKey 
	   		   	 	   * cantidad de despacho en o/c
	   		   	  	   vCanTot=vCanTot+CanDesp
	   		   	  	   IF vCanTot<Itepec.CanDesp
	   		   	  	   	  vDividendo = vDividendo+(Preuni*candesp)
	   		   	  	   	  vDivisor   = vDivisor+candesp
 	  	   	  	   		  vDiferencia= CanDesp - vCanTot
	   		   	  	   	  SKIP
	   		   	  	   	  IF EOF()
	   		   	  	   	  	* si la cantidad pedida(pec) es menor 
	   		   	  	   	  	* a la comprada (O/C)
	   		   	  	   	     ok=.f.
	   		   	  	   	     EXIT
	   		   	  	   	  ENDIF
	   		   	  	   	  LOOP
	   		   	  	   ELSE
	   		   	  	   	  * salgo a reemplazar el precio unitario
	   		   	  	      vDividendo = vDividendo+(Preuni*vDiferencia)
	   		   	  	   	  vDivisor   = vDivisor+vdiferencia
	   		   	  	   	  EXIT
	   		   	  	   ENDIF	   		   	  	   
	   		   	  	ELSE
	   		   	  	 	* Ya no existen compras para este art¡culo
	   		   	  	   OK1=.F.
	   		   	  	   EXIT
	   		   	  	ENDIF 
	   		   ENDDO
	   		   SELECT ItePec
	   		   IF !OK
	   		   	  *REPLACE CosMed WITH -1
	   		   ENDIF	  
	   		   IF !OK1
	   		   	  *REPLACE CosMed WITH -2
	   		   ENDIF	  
	   		   IF OK and OK1
	   		   	  *REPLACE CosMed WITH (vdividendo/vdivisor)
	   		   	  vCanTot=vCanTot-CanDesp
	   		   ENDIF
	  		ENDIF
		ENDSCAN
	ENDIF
	SELECT Itepec
 ENDSCAN
 ENDIF
 SELE ITEART
ENDSCAN 
SELECT IteOc
SET INDEX TO 
ERASE (ZInd)
SET ORDER TO IteOC1		   
SELECT Itepec
SET INDEX TO 
ERASE (xInd)
SET ORDER TO Itepec1
RETURN


