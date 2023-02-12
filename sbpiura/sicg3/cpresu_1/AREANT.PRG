FUNCTION xcresup
*--------------
private vkey
VALIAS = ALIAS()
vCodpart=codpart
*vKey= IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,.t.)+IIF(!empty(vCodPry),CODPROY,.t.),CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,.t.)+IIF(!EMPTY(vCodAct),CODACT,.t.))
vKey= IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,''),CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,''))
SELECT itecre
SET ORDE TO IIF(ALLTRIM(vTipFun)='I',2,3)
IF EOF()
	RETURN 0
ENDIF

SELECT cresup
SET RELATION TO PERIODO+ALLTRIM(TIPDOC)+ALLTRIM(NUMDOC)+ALLTRIM(TIPFUN) INTO itecre

SELECT itecre
GO TOP
* se filtra de acuerdo a la fecha del reporte vNumMes
IF RIGHT(vCodpart,2)='00'
	SUM monasig TO vcresup FOR RIGHT(Codpart,2) # '00' AND MONTH(fecha)=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Cresup.estado # '00'
ELSE	
	SUM monasig TO vcresup FOR Codpart=vcodpart	AND MONTH(fecha)=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Cresup.estado # '00'
ENDIF	
SELECT (vAlias)
RETURN vcresup

FUNCTION ZTransf
*--------------
private vkey
VALIAS = ALIAS()
vCodpart=codpart
*vKey= IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,.t.)+IIF(!empty(vCodPry),CODPROY,.t.),CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,.t.)+IIF(!EMPTY(vCodAct),CODACT,.t.))
vKey= IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,''),CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,''))
SELECT itetra
SET ORDE TO IIF(ALLTRIM(vTipFun)='I',2,3)
IF EOF()
	RETURN 0
ENDIF
SELECT trapar
SET RELATION TO PERIODO+ALLTRIM(TIPDOC)+ALLTRIM(NUMDOC)+ALLTRIM(TIPFUN) INTO itetra

SELECT itetra
GO TOP
* se filtra de acuerdo a la fecha del reporte vNumMes
vtransf=0
IF RIGHT(vCodpart,2)='00'
	SCAN 
		IF MONTH(fecha)=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Trapar.estado # '00' AND LEFT(Codpart,2)=LEFT(vCodpart,2)
			vtransf=vtransf+iif(tipope='-',-1,1)*montra
		ENDIF
	ENDSCAN
ELSE	
	SCAN 
		IF MONTH(fecha)=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Trapar.estado # '00' AND Codpart=vCodpart
			vtransf=vtransf+iif(tipope='-',-1,1)*montra
		ENDIF
	ENDSCAN
ENDIF	
SELECT (vAlias)
RETURN vtransf

FUNCTION Zcresup
*--------------
private vkey
VALIAS = ALIAS()
vCodpart=codpart
vKey= IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,''),CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,''))

* PRIMERO CON CREDITOS SUPLEMENTARIOS

SELECT itecre
SET ORDE TO IIF(ALLTRIM(vTipFun)='I',2,3)

SELECT cresup
SET RELATION TO PERIODO+ALLTRIM(TIPDOC)+ALLTRIM(NUMDOC)+ALLTRIM(TIPFUN) INTO itecre

SELECT itecre
GO TOP
* se filtra de acuerdo a la fecha del reporte vNumMes
IF RIGHT(vCodpart,2)='00'
	SUM monasig TO vcresup FOR left(Codpart,2)=left(vcodpart,2) AND RIGHT(Codpart,2) # '00' AND MONTH(fecha)<=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Cresup.estado # '10'
ELSE	
	SUM monasig TO vcresup FOR Codpart=vcodpart	AND MONTH(fecha)<=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Cresup.estado # '10'
ENDIF	

* AHORA CON TRANSFERENCIAS

SELECT itetra
SET ORDE TO IIF(ALLTRIM(vTipFun)='I',2,3)
SELECT trapar
SET RELATION TO PERIODO+ALLTRIM(TIPDOC)+ALLTRIM(NUMDOC)+ALLTRIM(TIPFUN) INTO itetra

SELECT itetra
GO TOP
* se filtra de acuerdo a la fecha del reporte vNumMes
vtransf=0
IF RIGHT(vCodpart,2)='00'
	SCAN 
		IF MONTH(fecha)<=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Trapar.estado # '10' AND LEFT(Codpart,2)=LEFT(vCodpart,2)
			vtransf=vtransf+iif(tipope='-',-1,1)*montra
		ENDIF
	ENDSCAN
ELSE	
	SCAN 
		IF MONTH(fecha)<=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Trapar.estado # '10' AND Codpart=vCodpart
			vtransf=vtransf+iif(tipope='-',-1,1)*montra
		ENDIF
	ENDSCAN
ENDIF	
vtotalct=vcresup+vtransf
SELECT (vAlias)
RETURN vtotalct
