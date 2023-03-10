nmes = 11

set exclu off
set dele on
set cent on
set date to french
set path to h:\sicgdata\data2010

USE Diario in 1 ORDER TAG Diario11

select distin dt.comprob+dt.comadi codcom;
	from diario dt;
	where (dt.cuenta='110101' or dt.cuenta='110102') and mont(dt.fecemi)=nMes;
	into cursor DC

select d.comprob,d.comadi, fecemi, d.cuenta,d.tipo,d.montos,d.coddep,d.reten;
	from diario d ;
	where d.comprob+d.comadi in (select distin dt.comprob+dt.comadi codcom;
							from diario dt;
							where (dt.cuenta='110101' or dt.cuenta='110102') and mont(dt.fecemi)=nMes);
		  and d.cuenta#'8' and d.cuenta#'9' and d.cuenta#'110101' and d.cuenta#'110102' and d.reten#'S';
	into cursor DCaja

select dc.CodCom, dcaja.comprob, dcaja.comadi, dcaja.fecemi, dcaja.cuenta, dcaja.tipo, dcaja.montos, dcaja.coddep, dcaja.reten;
	from dC;
		left join dCaja on (dc.codcom=dcaja.comprob+dcaja.comadi);



*GO TOP
*SCAN 
*	xCom = Comprob+ComAdi
*	xDebe  = 0
*	xHaber = 0
*	SCAN WHILE Comprob+ComAdi = xCom
*		xDebe  = xDebe + IIF(tipo='D',Montos,0)
*		xHaber = xHaber + IIF(tipo='H',Montos,0)
*	ENDSCAN
*	IF xDebe # xHaber
*		WAIT WIND xCom
*	ENDIF
*	SKIP -1
*ENDSCAN

*return




*USE FOLIOS exclusive
*PACK
*=ordena("REPORTE+MES+PERIODO+TIPDOC+ORDEN+CUENTA","Folios1")
*=ordena("REPORTE+MES+PERIODO+ORDEN+CUENTA","Folios2")


*USE Cuentas EXCLUSIVE
*PACK
*=ordena("CUENTA","Cuentas1")
*=ordena("dContra","Cuentas2",".NOT.EMPTY(DCONTRA)")
*=ordena("hContra","Cuentas3",".NOT.EMPTY(HCONTRA)")
*=ordena("Cuenta","Cuentas4","UPPER(DETALLE)='S'")
*=ordena("UPPER(DESCRI)","Cuentas5")
*=ordena("Cuenta","Cuentas6","CUENTA='9'")
*=ordena("TIPO+CUENTA","Cuentas7")
*USE

*USE IteFP EXCLUSIVE
*PACK
*=ordena("ALLTRIM(NUMFP)+ALLTRIM(NUMMES)+DTOC(FECDOC,1)","Itefp1")
*=ordena("ALLTRIM(NUMFP)+ALLTRIM(NUMMES)+CODPART+CODCLA","Itefp2")
*USE

*USE IteCla EXCLUSIVE
*PACK
*=ordena("CODPART+CODCLA+tipdoc","IteCla1")
*USE


*USE CatAsi EXCLUSIVE
*PACK
*=ordena("TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2+ESPN3+ESPN4","CatAsi1","CATASI.TIPPRE="1".AND.CATASI.DETALLE="S")
*=ordena("TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2+ESPN3+ESPN4","CatAsi2","CATASI.TIPPRE="2".AND.CATASI.DETALLE="S")
*=ordena("TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2+ESPN3+ESPN4","CatAsi3","CATASI.TIPPRE="1")
*=ordena("TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2+ESPN3+ESPN4","CatAsi4","CATASI.TIPPRE="2")
*=ordena("TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2+ESPN3+ESPN4","CatAsi5")
*USE

*USE IteCp EXCLUSIVE
*PACK
*=ordena("ALLTRIM(NumMes)+NumCp+ALLTRIM(CodCtc)","IteCp1")
*=ordena("CodPart+NumMes","IteCp2")
*=ordena("CodCtc+NUMMES+NUMCP","IteCp3")
*=ORDENA("PERIODO+NUMMES+CODCTC+CODPART","ITECP4")
*USE

*USE IteHc EXCLUSIVE
*PACK
*=ordena("NumMes+NumHc","IteHc1")
*=ordena("CodPart","IteHc2")
*=ordena("NumMes+NumHc","IteHc4","estado='92'")
*USE

*use iteos1 EXCLUSIVE
*PACK
*=ordena("PERIODO+NUMOS","iteoS11")
*=ordena("PERIODO+NUMOS+CODCAD","iteoS12")
*=ordena("PERIODO+NUMOS+CODCAD+CODCOM+CODMET+CODPART","iteoS13")
*USE

*USE ITEOC1 EXCLUSIVE
*PACK
*=ordena("PERIODO+NUMOC","iteoc11")
*=ordena("CODCAD+CODCOM","iteoc12")
*=ordena("PERIODO+NUMOC+CODCAD+CODCOM+CODMET+CODPART","iteoc13")
*=ordena("NUMPOL","iteoc14")
*USE

*USE IteCre EXCLUSIVE
*PACK
*=ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+UNIGES+UNIEJE+ALLTRIM(CODFTE)","IteCre1")
*=ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+CODCAD+ALLTRIM(CODFTE)+CodPart","IteCre2")
*=ordena("PERIODO+CODCAD+CODFTE+CODPART","itecre3")
*USE

*USE itepari exclu
*PACK
*=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART","itepari1")
*=ordena("PERIODO+CODCAD+CODFTE+CODPART","itepari2")
*=ordena("PERIODO+LEFT(ESTFUN,30)+CODFTE+CODPART","itepari3")
*=ordena("PERIODO+LEFT(ESTFUN,25)+CODFTE+CODPART","itepari4")
*=ordena("PERIODO+LEFT(ESTFUN,20)+CODFTE+CODPART","itepari5")
*=ordena("PERIODO+LEFT(ESTFUN,14)+CODFTE+CODPART","itepari6")
*=ordena("PERIODO+LEFT(ESTFUN,10)+CODFTE+CODPART","itepari7")
*=ordena("PERIODO+LEFT(ESTFUN,7)+CODFTE+CODPART","itepari8")
*=ordena("PERIODO+CODCAD+CODFTE+TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2","ItePari9")
*USE
*return

*USE Calen  EXCLUSIVE
*PACK
*=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+NUMMES+CODPART","Calen1")
*=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART+NUMMES","Calen2")
**=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+NUMMES+CODPART","Calen3")		&& Identico a calen1
*=ordena("PERIODO+LEFT(ESTFUN,30)+CODFTE+NUMMES+CODPART","Calen4") && PARA META
*=ordena("PERIODO+LEFT(ESTFUN,25)+CODFTE+NUMMES+CODPART","Calen5") && PARA COMPONENTE
*=ordena("PERIODO+LEFT(ESTFUN,20)+CODFTE+NUMMES+CODPART","Calen6") && PARA ACTIVIDAD/PROYECTO
*=ordena("PERIODO+LEFT(ESTFUN,14)+CODFTE+NUMMES+CODPART","Calen7") && PARA SUBPROGRAMA
*=ordena("PERIODO+LEFT(ESTFUN,10)+CODFTE+NUMMES+CODPART","Calen8") && PARA PROGRAMA
*=ordena("PERIODO+LEFT(ESTFUN,7)+CODFTE+NUMMES+CODPART","Calen9")  && PARA FUNCION


*USE ItePar EXCLUSIVE
*PACK
*=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART","ItePar1")
*=ordena("PERIODO+CODCAD+CODFTE+TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2","ItePar2")
*=ordena("PERIODO+CODCAD+CODFTE+CODPART","ItePar3")
*=ordena("PERIODO+LEFT(ESTFUN,30)+CODFTE+CODPART","ItePar4") && PARA META
*=ordena("PERIODO+LEFT(ESTFUN,25)+CODFTE+CODPART","ItePar5") && PARA COMPONENTE
*=ordena("PERIODO+LEFT(ESTFUN,20)+CODFTE+CODPART","ItePar6") && PARA ACTIVIDAD/PROYECTO
*=ordena("PERIODO+LEFT(ESTFUN,14)+CODFTE+CODPART","ItePar7") && PARA SUBPROGRAMA
*=ordena("PERIODO+LEFT(ESTFUN,10)+CODFTE+CODPART","ItePar8") && PARA PROGRAMA
*=ordena("PERIODO+LEFT(ESTFUN,7)+CODFTE+CODPART","ItePar9")  && PARA FUNCION
*use

*USE IteTra EXCLUSIVE
*PACK
*=ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+UNIGES+UNIEJE+CODFTE+CODCAD","IteTra1")
*=ordena("PERIODO+CODCAD+ALLTRIM(CODFTE)+ CodPart","IteTra2")


FUNCTION ordena
*--------------
PARAMETER key,idx,condi
   IF PARAMETERS()<3
      INDEX ON &key TAG (idx)
   ELSE
      INDEX ON &key TAG (idx) FOR &condi
   ENDIF
RETURN ""




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

RETURN IIF(!Empty(r),ALLTRIM(r),'No se encontr? detalle/glosa para este Documento.')

