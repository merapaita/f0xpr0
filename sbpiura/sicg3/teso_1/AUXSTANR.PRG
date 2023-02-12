*-------------------------------
*- AUXSTANR.PRG
*- Auxiliar Standar
*- AUTOR : UDEP 1994 - SECS,FMV
*-------------------------------
CLOS DATA
USE Compag  IN 1  ORDER TAG Compag1               ALIAS CompAG
USE AstPat  IN 2  ORDER TAG AstPat1               ALIAS AstPat
USE RecIng  IN 3  ORDER TAG RecIng1  			  ALIAS RECING
USE MovBco  IN 4  ORDER TAG MovBco2				  alias MOVBCO
USE Parmae  IN 6  ORDER TAG Parmae1               ALIAS Parma
USE Cuentas IN 7  ORDER TAG Cuentas1              ALIAS CuenTA
USE HojAnu  IN 8  ORDER TAG Hojanu1               ALIAS hojanu
use Reversio IN 9 ORDER TAG Rever1                ALIAS rever
SELECT ASTPAT
STORE SPACE(2) TO WMES
DO Inicia
DO salida
RETURN

PROCEDURE Inicia
*---------------
Public vcodcta,vNumMes,vNumRef,vCodCtc,vtipo1,VDESCRI,VGLOSA,VFUENTE,VPER
vTipo1=space(3)
vcol ="w+/w,n/w,W/W,w/W,,w+/b"
DEFINE WINDOW lis FROM 3,10 TO 20,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vlista
STORE SPACE(2) TO vMes
STORE 0 TO VANO
VMes = MONTH(DATE())
VAno =VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
STORE SPACE(10) TO VCODCTA
STORE DATE() TO vfecini, vfecfin
@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Nivel 3;Nivel 10;Rango de Fecha'
@ 05,01 SAY "     Cuenta Mayor : "
@ 05,22 GET vCodcTA PICT '!!!!!!!!!!'
@ 07,01 SAY "              Mes : " GET vMes   PICTURE '99'  VALID VMES>=1 AND VMES<=12 when vlista<>4
@ 09,01 SAY "              A¤o : " GET vano   PICTURE '99' when vlista<>4
@ 11,01 SAY " Fecha de Emisi¢n : "
@ 13,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7   WHEN vlista=3
@ 13,32 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7 WHEN vlista=3 
@ 15,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
  IF VMES<10
     WMES="0"+ALLTRIM(STR(VMES))
  ELSE
     WMES=STR(VMES,2)   
  ENDIF   
RELEASE WINDOW lis
IF okcancel = 1
    	vind1= SYS(3) +'.idx'
	  DEFINE WINDOW Xwait FROM 12,06 TO 14,78 COLOR SCHEME 05 
	  ACTIVATE WINDOW Xwait
      @0,10 SAY " Espere un Momento...Auxiliar Standar en Proceso!" COLOR W+/BR*
    SELECT AstPat
	filaux = SYS(3) + '.DBF'
	SELECT 25
	CREATE TABLE (filaux);
	  (PERIODO   C(002),;
	   TipDoc    C(003),;
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
	USE (filaux) IN 25 EXCLUSIVE  ALIAS Temp
	SELECT AstPat
	vInd9 = SYS(3) + '.Idx'
	VDBF9 = SYS(3) + '.DBF'
	INDEX ON NumMes+NumRef TO (vInd9) FOR NUMMES = WMes ;
     .AND. VAL(RIGHT(ALLTRIM(STR(YEAR(Fecha))),2)) = vAno AND RET='S' AND TIPCTC='RE'
	GO TOP
	COPY TO (VDBF9)
	USE IN 2
	USE (VDBF9) IN 16  ALIAS RET
	USE AstPat  IN 2  ORDER TAG AstPat1         ALIAS AstPat
	SELE ASTPAT
	vInd8 = SYS(3) + '.Idx'
	VDBF8 = SYS(3) + '.DBF'
	INDEX ON NumMes+NumRef TO (vInd8) FOR NUMMES= WMes ;
         .AND. VAL(RIGHT(ALLTRIM(STR(YEAR(Fecha))),2)) = vAno .AND. RET<>'S' AND TIPCTC='RE'
	GO TOP
	VPER=PERIODO
	vTipDoc = TipDoc
	vFecha  = Fecha
	vNumMes = NumMes
	vNumRef = NumRef
	XCodCta = CodCta
	vTipCta = TipCta
	vMtoDeb = MtoDeb
	vMtoHab = MtoHab
	vCodCtc = CodCtc
	SCAN
	    vTipDoc = TipDoc
	    vFecha  = Fecha
	    vNumMes = NumMes
	    vNumRef = NumRef
	    XCodCta = CodCta
	    vTipCta = TipCta
	    vMtoDeb = MtoDeb
	    vMtoHab = MtoHab
	    vCodCtc = CodCtc
	    VPER=PERIODO
	    SCAN WHILE vTipDoc = TipDoc .AND. vFecha = Fecha .AND. vNumMes = NumMes .AND. vNumRef = NumRef
	         vRec1 = RECNO()
	         vTipDoc = TipDoc
	         vFecha  = Fecha
	         vNumMes = NumMes
    	     vNumRef = NumRef
	         XCodCta = CodCta
	         vTipCta = TipCta
	         vMtoDeb = MtoDeb
	         vMtoHab = MtoHab
	         vCodCtc = CodCtc
	         VPER=PERIODO
	         vMto    = 0
	         IF vTipCta = 'D' AND VTIPDOC='C/P'
    	        SELE RET
	            SCAN WHILE vTipDoc = TipDoc .AND. vFecha = Fecha .AND. vNumMes = NumMes .AND. vNumRef = NumRef
		            VMTO=VMTO+MTOHAB
	            ENDSCAN
	         ENDIF
	         SELECT Temp
	         IF F_APPD()
	            REPLACE PERIODO WITH VPER
	            REPLACE TipDoc WITH vTipDoc        
	            REPLACE Fecha  WITH vFecha         
	            REPLACE NumMes WITH vNumMes        
	            REPLACE NumRef WITH vNumRef        
	            REPLACE CodCta WITH XCodCta        
	            REPLACE TipCta WITH vTipCta        
	            REPLACE MtoDeb WITH vMtoDeb - vMto 
	            REPLACE MtoHab WITH vMtoHab        
	            REPLACE CodCtc WITH vCodCtc        
                @0,01 SAY STR(RECNO(),6)
       		 ENDIF
         SELECT AstPat
    ENDSCAN
    GO vRec1
    SELECT AstPat
ENDSCAN
SELECT TEMP
do case
    	case vlista=1
    		INDEX ON CODCTC+DTOC(Fecha,1) TO (vind1) FOR  SUBSTR(TEMP.codcta,1,3)=alltrim(vcodcta)  
		case vlista=2
			vlen=LEN(vcodcta) 
    		INDEX ON CODCTC+DTOC(Fecha,1) TO (vind1) FOR  substr(TEMP.codcta,1,vlen)=alltrim(vcodcta)
    	case vlista=3
    		vlen=LEN(vcodcta) 
    		INDEX ON CODCTC+DTOC(Fecha,1) TO (vind1) FOR  substr(TEMP.codcta,1,vlen)=alltrim(vcodcta)  and BETWEEN(TEMP.Fecha,vfecini,vfecfin) 		
endcase 
GO TOP
if eof()
   		do standby with "No existen movimientos"
		DEACTIVATE WINDOW AuxStand
		DEACTIVATE WINDOW xwait
		IF !escolor
    		RESTORE SCREEN FROM principal
  		ENDIF
  		SHOW POPUP menu, pop_01
		RETURN
endif	
	DEACTIVATE WINDOW xwait
   	DO REPORTE WITH 2,"AuxStanR", ' Auxiliar Standar '
  	SET INDEX TO
 	ERASE(vInd1) 
endif
retur

PROCEDURE Salida
*---------------
RELEASE    WINDOW AsiCenTe
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


FUNCTION valprv2
*--------------
parameter prov,MALIAS
malias = alias()
SET ORDE TO 2
USE Auxil    IN 15  ORDER TAG Auxil2       		  ALIAS Auxil
IF prov='0000' .OR. EMPTY(prov)
ELSE   
   sele auxil
   SET FILT TO TIPO="20"
   seek prov
   v_fun = iif(found(),Descri,"")
ENDIF
SET FILT TO 
SELE (MALIAS)
SELE COMPAG
USE IN 15
RETURN v_fun


FUNCTION valemP2
*--------------
parameter emp,MALIAS
malias = alias()
SET ORDE TO 2
USE Auxil    IN 15  ORDER TAG Auxil2       		  ALIAS Auxil
IF emp='0000' .OR. EMPTY(emp)
ELSE   
   sele auxil
   SET FILT TO TIPO="30"
   seek emp
   v_fun = iif(found(),Descri,"")
ENDIF
SET FILT TO 
SELE (MALIAS) 
USE IN 15
RETURN v_fun

FUNCTION VARIA
*-------------
SELECT TEMP
vtipo1=alltrim(TEMP.tipdoc)
VPER=TEMP.PERIODO
vNumMes=TEMP.nummes  
vNumRef=TEMP.numref
vCodCtc=TEMP.codctc
RETUR " " 

FUNCTION XVARIA
*-------------
SELECT TEMP
skip
if !eof()
	vtipo1 =alltrim(TEMP.tipdoc)
	VPER=TEMP.PERIODO
	vNumMes=TEMP.nummes  && de astpat
	vNumRef=TEMP.numref
	vCodCtc=TEMP.codctc
endif	
RETUR " " 

FUNCTION USUARIO
*--------------
parameter vtipo
private malias
malias = alias()
DO CASE 
	CASE vTIPO='C/P'
		select compag
		SET ORDE TO 1
		seek vnummes+vnumref+vcodctc
		IF FOUND()
		DO CASE
			CASE TipDoc='RE'
			    vDesCri=VAL_PARA(codret,'CODRET','V')
			CASE TipDoc='ME' OR TIPDOC='SE'
				vDesCri=nompre
			CASE TipDoc='HC' OR TIPDOC='RG'
				DO CASE
			    	case tipprv="E"
	    				vDesCri=valemp2(codemp)
	    			case tipprv="P"
	    				vDesCri=valprv2(codprv)
		    		OTHER
		    			vDesCri=nomPre
		    	ENDCASE
		ENDCASE
		ELSE
		  vDescri = '***** No Hallado *****'
		endif
	case vTIPO='R/I'
		selec recing
		seek VPER+alltrim(vnummes)+vnumref
		vtipaux=recing.tipaux
		if found()
       		vdescri=Val_AUXI(codprv,vTIPAUX,"V",30)
		ELSE
		  vDescri = '***** No Hallado *****'
		endif
	case vTIPO='N/C' or vTIPO='N/A' or vTIPO='B/D' OR VTIPO='NOC'
		select MovBco
		Seek ALLTRIM(vTipo)+ALLTRIM(vNumref)+alltrim(vCodCtc)
		if found()		
			vDesCri=val_Para(codban,'BANCOS','V')
		ELSE
		  vDescri = '***** No Hallado *****'
		endif
	case vTipo='H/A'
		select Hojanu
		seek vnummes+vnumref
		if eof()			
			DO CASE
				CASE TipDoc='RE'
		    		vDesCri=VAL_PARA(codret,'CODRET','V')
				CASE TipDoc='ME' OR TIPDOC='SE'
					vDesCri=nompre
				CASE TipDoc='HC' OR TIPDOC='RG'
					DO CASE
	    				case tipprv="E"
	    					vDesCri=valemp2(codemp)
				    	case tipprv="P"
		    				vDesCri=valprv2(codprv)
		    			OTHER
				    		vDesCri=nombre
				    ENDCASE
			ENDCASE
		ELSE
    	  vDescri = '***** No Hallado *****'
		endif
	Case vtipo='REV'
		SELECT REVERSIO
		seek alltrim(vnummes)+alltrim(vnumref)
		if eof()			
			vdescri=VAL_PARA(codOFG,'OFIGIR','V')
		ELSE
    	  vDescri = '***** No Hallado *****'
		endif
ENDCASE
SELECT (MALIAS)
RETUR vdesCri

FUNCTION DESCRI1
*---------------
PARAMETERS vCuenta
vAli=ALIAS()
SELECT CuenTA
SEEK vCuenta
IF FOUND()
   _z=Val_Fun('CuenTA','CodCta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN

Function glosa
*--------------
PARAMETER vtipo
private malias
malias=alias()
do case
	case vTIPO='C/P'
		select compag
		SET ORDE TO 1
		seek vnummes+vnumref+vcodctc
		vglosa=glosa
	case vTIPO='R/I'
		selec recing
		seek VPER+alltrim(vnummes)+vnumref
		set memowidth to 85 
		Vglosa=ALLTRIM(MLINE(RECING.Observ,3))
	case vTIPO='N/C' or vTIPO='N/A' or vTIPO='B/D' OR VTIPO='NOC'
		select MovBco
		Seek ALLTRIM(vTipo)+ALLTRIM(vNumref)+alltrim(vCodCtc)
		vglosa=NOMBRE
	case vTipo='H/A'
		select Hojanu
		seek vnummes+vnumref
		vglosa=justif
	Case vtipo='REV'
		select reversio
		seek alltrim(vnummes)+alltrim(vnumref)
		vglosa=glosa
endcase		
SELECT (MALIAS)
RETURN vglosa

Function fuente
*--------------
parameter vtipo
private malias
malias=alias()
do case
	case vTIPO='C/P'
		select compag
		seek alltrim(vnummes)+vnumref+alltrim(vcodctc)
		vfuente=alltrim(codfte)
	case vTIPO='R/I'
		selec recing
		seek VPER+alltrim(vnummes)+vnumref
		vfuente=subs(recing.codcal,5,3)
	case vTIPO='N/C' or vTIPO='N/A' or vTIPO='B/D' OR VTIPO='NOC'
		select MovBco
		Seek ALLTRIM(vTipo)+ALLTRIM(vNumref)+alltrim(vCodCtc)
		vfuente=alltrim(codfte)
	case vTipo='H/A'
		select Hojanu
		seek vnummes+vnumref
		vfuente=alltrim(codfte)
	Case vtipo='REV'
		select reversio
		seek alltrim(vnummes)+alltrim(vnumref)
		vfuente=" "
endcase		
SELECT (MALIAS)
RETUR vFUENTE

FUNCTION Val_AUXI
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol
  PRIVATE mAlias
   DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
      mDist = 10
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 10
    CASE PARAMETERS() = 4
      mLong = 25               && Longitud campo DESCRI
      mTIP  = 24
      mDist = 10
    CASE PARAMETERS() = 5
      mDist = 10
  ENDCASE
  mAlias  = ALIAS()
  USE Auxil    IN 15  ORDER TAG Auxil1       		  ALIAS Auxi
  velig=.f.
  SELECT AUXI
  SEEK ALLTRIM(Filtro)+mValor
  IF !FOUND() .AND. !mVariable $'VZ'
     SET FILTER TO TIPO = ALLTRIM(Filtro)
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   return	
    ENDIF
    vidx = SYS(3) + '.idx'
*   INDEX ON Descri TO &vidx FOR TIPO = ALLTRIM(Filtro)
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(Descri,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF
    RELEASE POPUP parametro
    mTipo  = AUXI.Tipo
  	mValor = AUXI.Codigo
  	mDescr = SUBSTR(AUXI.Descri, 1, mLong)
  	velig=.t.
    SET INDEX TO
    SET ORDER TO TAG AUXIL1
    ERASE (vidx)
  ENDIF
  if velig=.f.
   	mTipo  = AUXI.Tipo
  	mValor = AUXI.Codigo
  	mDescr = SUBSTR(AUXI.Descri, 1, mLong)
  endif	
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  USE IN 15
  DO CASE
    CASE mVariable=' '   && En edici¢n
    * @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
      m.CodPrv=mvalor
      m.TipAux=mtipo
*     @ ROW(),mCol-mtip  SAY mvalor  &&mtipo
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE



FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)

