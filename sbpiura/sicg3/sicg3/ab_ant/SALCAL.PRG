*----------------------------------------------------------------------
** SALCAL.PRG
** Reporte Ejecucion ESTADOS
** INFROMATICA
** Autor : XXX
*----------------------------------------------------------------------
PARAMETER VOPCION
* Abrir Archivos de trabajo
CLOSE DATA
USE Parmae  IN 1 ORDER TAG Parmae1         ALIAS parma
USE Calen   IN 2 ORDER TAG Calen1          ALIAS Calen
USE Maepre  IN 3 order tag Maepre1         ALIAS Maepre
USE MaePar  IN 4 ORDER TAG MaePar1         ALIAS MaePar
USE ItePar  IN 5 ORDER TAG ItePar1         ALIAS ItePar
USE Clase   IN 6 ORDER TAG Clase1          ALIAS Clase
USE HojCon  IN 7 ORDER TAG Hojcon1         ALIAS Hoja
USE IteHc   IN 8 ORDER TAG Itehc1          ALIAS Itehc
*USE Repoe5  IN 9                           ALIAS repo   
*USE Repopre IN 10                          ALIAS repoP
IF VOPCION=1
   USE OrdCom  IN 11   order tag OrdCom1  ALIAS Orden
   USE IteOc   IN 12   order tag IteOc1   ALIAS IteOc
   USE IteOc1  IN 13   order tag IteOc11  ALIAS IteOc1
ELSE
   USE OrdSer  IN 11   order tag OrdSer1  ALIAS OrdSe 
   USE IteOs1  IN 13   order tag IteOs11  ALIAS IteOs1   
ENDIF
USE Hojmod  IN 14 ORDER TAG Hojmod1         ALIAS HojMod

*----
PUBLIC TOT,VACUM,TOT1,TOTCOM
TOT = 0
TOT1 = 0
DO Inicia
DO salida
CLOSE DATA
RETURN

PROCEDURE Inicia
*--------------
PUBLIC vFecini,vFecFin
m.periodo = SPACE(2)
m.calend  = SPACE(2)
m.codcad  = SPACE(4)
m.codfte  = SPACE(2)
=val_CODCAD(ALLT(m.codcad),m.periodo,'C')
 
 DEFINE WINDOW Lis FROM 6,6 TO 17,74 DOUBLE ;
 TITLE ' °° SALDO CALENDARIOS ANALITICO °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,2 say '          Periodo : '
 @  2,2 say '       Calendario : '
 @  3,2 say '      Cadena Fun. : '
 @  4,2 say '   Fte. Financia. : '
 @  5,2 say '          Funci¢n : ' 
 @  6,2 say '         Programa : '
 @  7,2 say '      SubPrograma : '
 @  8,2 say '   Activ./Proyec. : '

 @  1,22  GET m.Periodo   PICTURE '!!'     VALID !EMPTY(m.Periodo)
 @  2,22  GET m.Calend    PICTURE '!!'     VALID VAL_PARA(m.Calend,'FECMES',' ',22,30) 

 @  3,22 GET m.CodCad  PICTURE '!!!!' 	VALID VAL_CODCAD(m.codcad,m.periodo,' ',22,30)
 @  4,22 GET m.Codfte  PICTURE '!!'  	VALID VAL_PARA(m.Codfte,'CODFTE',' ',22,30)
 READ VALID Val_Read()

 IF LASTKEY()=27 
 	GO REC
    DO vista
	RETURN
 ENDIF
 @  5,22  SAY VAL_PARA(maepre.codfun,'CODFUN','V',22,40)
 @  6,22  SAY VAL_PARA1(maepre.codprg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @  7,22  SAY VAL_PARA1(maepre.codspr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @  8,22  SAY VAL_PARA(maepre.actpry,'ACTPRY','V',22,40)
 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF LASTKEY()=27
    RETURN
 ELSE
	QQ = IIF(VOPCION=1,1,2)
    cd = analcal(0,QQ)
    IF CD
  	   IF YESNO('¨ Imprime consulta ?')
          SELE CALEN
          GO TOP
          IF !found()
    		 DO REPORTE WITH 2,"LISCALF",' Listado Saldo de Calendario ',1,.F.,.T.
    	  ELSE
    		 DO REPORTE WITH 2,"LISCALI",' Listado Saldo de Calendario ',1,.F.,.T.
    	  ENDIF	
	   ENDIF
    ENDIF
ENDIF 
CLOSE DATA
RETURN

PROCEDURE PasaF
*-------------
SELE ITEHC
SCAN
   vCanreq=0
   vCa1 = Codprg+Codsubpr+CodAct+LEFT(CodanaL,2)
   vCa2 = Codprg+Codsubpr+CodAct+LEFT(Codanal,2)
   vAcum = 0
   vMesx=0
   DO WHILE vCa1 = vCa2
     *vCanreq = vCanreq+Valpart
      vCanreq = vCanreq+IIF(!empty(hoja.numanu),valpart-hoja.valanu,IIF(!empty(hoja.numreb),valpart-hoja.Valreb,IIF(ITEHC.ESTADO='92' AND TIPOPE='-',-valpart,VALPART)))
      SKIP
      vCa1 = Codprg+Codsubpr+CodAct+LEFT(Codanal,2)
   ENDD
   SKIP -1
   DO AGREGAR
   SELE ITEHC
ENDSCAN

RETURN

PROCEDURE Pasai
*-------------
SELE ITEHC
SCAN
   vCanreq=0
   vCa1 = Codprg+Codsubpr+Codproy+CodSupry+CodPart
   vCa2 = Codprg+Codsubpr+Codproy+CodSupry+CodPart
   vAcum = 0
   vMesx = 0
   DO WHILE vCa1 = vCa2
      *vCanreq = vCanreq+IIF(ITEHC.ESTADO='92' AND TIPOPE='-',-valpart,VALPART)
      vCanreq = vCanreq+IIF(!empty(hoja.numanu),valpart-hoja.valanu,IIF(!empty(hoja.numreb),valpart-hoja.Valreb,IIF(ITEHC.ESTADO='92' AND TIPOPE='-',-valpart,VALPART)))
      SKIP
      vCa1 = Codprg+Codsubpr+Codproy+CodSupry+CodPart
   ENDD
   SKIP -1
   DO AGREGAR
   SELE Itehc
ENDSCAN
RETURN

PROCEDURE AGREGAR
*-----------------
SELE ITEHC
SCATTER MEMVAR

IF ALLTRIM(vTipFun)='I'
   vPartid = m.Codprg+m.Codsubpr+m.Codproy+m.CodSupry+m.CodPart 
ELSE
   vpartid = m.Codprg+m.Codsubpr+m.CodAct +LEFT(m.Codanal,2)
ENDIF 	

SELECT REPOP
seek vPartid
if !found() 
     if m.tipfun = 'F'
        m.codpart = LEFT(m.codanal,2)+'.00'
     endif
     m.periodo   = hoja.periodo
     m.valpart   = 0
     m.Valafec   = vCanreq     
     m.saldo     = m.valpart - m.valafec
     m.dessupry  = buscpry()
     IF F_APPD() 
	  	 gather memvar
	 ENDIF
else 
     replace valafec with vCanreq,saldo with valpart - valafec 
     if m.tipfun = 'F'
       replace codpart with left(m.codanal,2)+'.00'
     endif
endif	 
vcanreq = 0
sele itehc
RETURN .T.

PROCEDURE XAGREGAR
*-----------------
vVal = itehc.valpart
vPer = Hoja.periodo

SCATTER MEMVAR
*SELECT repo   
*APPEND BLANK
*m.ValPart   = vVal
*m.periodo   = vPer
*m.DelMes    = vMesx
*m.totpres   = vAcum
*GATHER MEMVAR

IF ALLTRIM(vTipFun)='I'
   vPartid = ITEHC.codprg+ITEHC.codsubpr+ITEHC.Codproy+ITEHC.CodSupry+ITEHC.CODPART 
ELSE
   vpartid = ITEHC.codprg+ITEHC.codsubpr+ITEHC.CodAct +ITEHC.Codanal 
ENDIF 	

SELECT REPO
IF ALLTRIM(vTipFun)='I'
   INDEX ON codprg+codsubpr+Codproy+CodSupry+Codpart TO PP
ELSE
   INDEX ON codprg+codsubpr+CodAct+Codanal TO PP
ENDIF 	
*GO TOP
scatter memvar

SELECT REPOP
seek alltrim(vPartid)
if !found() 
     if itehc.tipfun='F'
        m.codpart = m.codanal
     endif
     m.valpart = 0
     m.Valafec   = vCanreq     
     m.saldo     = m.valpart - m.valafec
     IF F_APPD() 
	  	 gather memvar
	 ENDIF
else 
     vfoc = 0
     SELE REPO
     seek alltrim(vPartid)
     IF ALLTRIM(vTipFun)='I'    
         SCAN WHILE  vPartid = repo.codprg+repo.codsubpr+repo.Codproy+repo.CodSupry+repo.CODPART 
            vFoc = vFoc + repo.valpart
         ENDSCAN     
     else    
         SCAN WHILE  vPartid = REPO.codprg+REPO.codsubpr+REPO.CodAct+REPO.Codanal 
            vFoc = vFoc + repo.valpart
         ENDSCAN     
     endif     
     sele repop
     replace valafec with vfoc,saldo with valpart - valafec
     
endif	 
vcanreq = 0

sele itehc
RETURN .T.

PROCEDURE ACUMULA
*----------------
PRIVATE VTEMP,QQ
vTemp=Recno()
QQ=ALIAS()
vAcum = 0
vMesx = 0
VKey = alltrim(vCODFTE)+alltrim(vCodPrg)+alltrim(vCodsub)+IIF(ALLTRIM(vTipfun)='I',alltrim(vProyec),alltrim(vCODACT))
SELE REPOP

IF ALLTRIM(vTipFun)='I'
	INDEX ON codprg+codsubpr+Codproy+CodSupry+CODPART TO (XInd) 
ELSE
    INDEX ON codprg+codsubpr+CodAct+CODPART TO (XInd) 
ENDIF 	
SET INDEX TO (XIND)

SELECT CALEN
IF alltrim(vTipfun)='I'
   SET ORDER TO calen1
ELSE   
   SET ORDER TO calen2
ENDIF   
SET FILT TO CODFTE+CodPrg+CodSubPr+IIF(alltrim(vTIPFUN)='I',CODPROY,CODACT)=VKEY AND TIPFUN=ALLTRIM(vTIPFUN) AND NUMMES=ALLTRIM(VCALEND)
GO TOP
SCAN
    SCATTER MEMVAR  
    SELE REPOP
    IF F_APPD()
       GATHER MEMVAR
    ENDIF
    SELE CALEN
ENDSCAN
SET FILT TO
SELE (QQ)

RETURN


PROCEDURE BUSCPRY
*----------------
SELECT CALEN
IF alltrim(vTipfun)='I'
   SET ORDER TO calen1
ELSE   
   SET ORDER TO calen2
ENDIF   
IF ALLTRIM(vTipFun)='I'
   INDEX ON codprg+codsubpr+Codproy+CodSupry TO PP
   vPry = m.Codprg+m.Codsubpr+m.Codproy+m.CodSupry
ELSE
   INDEX ON codprg+codsubpr+CodAct TO PP
   vPry = m.Codprg+m.Codsubpr+m.CodAct 
ENDIF 	
seek vpry
IF found() 
   m.dessupry = calen.dessupry
ELSE
   m.dessupry = 'Sin descripci¢n'
ENDIF 	
select repop
return m.dessupry

PROCEDURE BUSCPRG
*----------------
Private vKey
zx = ALIAS()
qw = order()
vKey=ALLTRIM(VPeriodo)+ALLTRIM(Vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec)
SELE MAEPRE
SET ORDE TO maepre2
seek vkey
vProyecto = maepre.Descri
select (zx)
set order to (qw)
return (vProyecto)

PROCEDURE Salida
*---------------
RELEASE    WINDOW LibDir
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)


PROCEDURE RepDiar
*----------------
SELE Diar
vIndi1=SYS(3)+".IDX"
INDEX ON LEFT(COMPROB,2)+CUENTA+TIPO TO (vInd1)

DO REPORTE WITH 2, "REPOE5", 'Reporte de Ejecuci¢n E-5',1,.F.,.T.

CLOSE DATA
ERASE (vIndi1)
RETURN


PROCEDURE SUMAFE
*---------------
RETURN 0
SELECT REPO   && REPOE5
vTotal = 0
IF RIGHT(REPOP.CODPART,2) = '00'
	SEEK LEFT(REPOP.CODPART,2)
    SCAN WHILE LEFT(REPOP.CODPART,2) = IIF(REPOP.TIPFUN='I',LEFT(REPO.CODPART,2),LEFT(REPO.CODANAL,2)) 
	   	 vTotal = vtotal + valpart
	ENDSCAN
ELSE
	*-----   
	SEEK REPOP.CODPART
	IF FOUND()
     	SCAN WHILE REPOP.CODPART = IIF(REPOP.TIPFUN='I',REPO.CODPART,REPO.CODANAL)
	     	vTotal = vtotal + valpart
	    ENDSCAN
	ENDIF
ENDIF   
GO TOP
SELECT REPOP
RETURN VTOTAL	

PROCEDURE buscact
*----------------
PARAMETER vact
PRIVATE AS
AS=ALIAS()
**BUSCAR ACTIVIDAD
PRIVATE vkey
vkey=ALLTRIM(periodo)+ALLTRIM(codprg)+ALLTRIM(codsubpr)+ALLTRIM(vact)
SELE maepre
SET ORDE TO maepre3
SEEK vkey
vactividad = IIF(FOUND(),maepre.descri,'?')
SELE (AS)
RETURN (vactividad)

PROCEDURE NOVALE
*---------------
	    ACTIVATE WINDOW STANDBY
	    @ 1,14 SAY 'Espere un Momento ....' color W*

	    SELECT REPO
	    vdbf = sys(3)+'.dbf'
	    copy stru to (vdbf)
	    USE (VDBF) IN 9  ALIAS REPO

	    SELECT REPOP
	    vdbf = sys(3)+'.dbf'
	    copy stru to (vdbf)
	    USE (VDBF) IN 10  ALIAS REPOP


	    vInd = SYS(3) + '.IDX'
	    xInd = SYS(3) + '.IDX'
	    yInd = SYS(3) + '.IDX'
	    zInd = SYS(3) + '.IDX'
	    if opcion 
	    	SELECT itehc
		   	SET RELATION to nummes+numhc INTO hoja
			if alltrim(vtipfun)='I'
    	  		INDEX on codprg+codsubpr+Codproy+CodSupry+CodPart to (yInd) for hoja.nummes=alltrim(vcalend) .AND. Codprg=ALLTRIM(vCodPrg) AND tipfun = alltrim(vtipfun) AND estado#'9' AND CodFte=ALLTRIM(vCodFte)
	   		else   
	      		INDEX on codprg+codsubpr+CodAct+CodAnal to (yInd) for hoja.nummes=alltrim(vcalend) .AND. Codprg=ALLTRIM(vCodPrg) AND tipfun = alltrim(vtipfun) AND estado#'9' AND CodFte=ALLTRIM(vCodFte)
		  	endif
			SET FILTER TO IIF(!EMPTY(vCodsub),CodSubPr=ALLTRIM(vCodsub),.t.) .AND. ;
	        	 IIF(alltrim(vtipfun)='F',iif(!empty(vCodAct),CodAct=ALLTRIM(vCodAct),.t.),iif(!empty(vProyec),Codproy=ALLTRIM(vProyec),.t.)))  
	   		GO TOP
	   	endif 	
	   	if opcion
	   		do acumula
	   	endif	
		IF EOF() AND OPCION
	     	DEACTIVATE WINDOW STANDBY
	     	DO STANDBY WITH 'No se tiene registros a Procesar'
	     	SELE ITEHC
		 	SET FILTER TO
	  	ELSE
	  		IF OPCION	
		 		SELECT REPO
		 		INDEX ON IIF(ALLTRIM(VTIPFUN)='F',CODANAL,CODPART) TO (vInd) 
	     		SET INDEX TO (VIND)
	     		SELECT REPOP
	     		IF ALLTRIM(VTIPFUN)='F' 
					DO PASAF
	     		ELSE
			      DO PASAI
		   		ENDIF   
		   		DEACTIVATE WINDOW STANDBY
	        	SELECT REPOP
	       		*SET FILT TO VALAFEC#0
	       		GO TOP
	   			IF ALLTRIM(VTIPFUN)='I'
	   		    	DO REPORTE WITH 2,"salcal",' Listado Saldo de Calendario ',1,.F.,.T.
	       	  	ELSE   
	          		DO REPORTE WITH 2,"salcalF",' Listado Saldo de Calendario ',1,.F.,.T.
	       	  	ENDIF   
			    SELE ITEHC
		    	SET FILTER TO
			    SET INDEX TO
			    CLOSE DATA
				ERASE (VIND)
				ERASE (XIND)
	   		ELSE
	   			SELECT CALEN
	   			INDEX ON PERIODO+NUMMES+CODFTE+CODPRG+CODSUBPR+IIF(ALLTRIM(vTipfun)='I',CODPROY+codsupry,CodAct)+CODPART TO (ZInd) FOR CODPRG = ALLTRIM(VCODPRG) AND CODFTE = ALLTRIM(VCODFTE) AND PERIODO=VPERIODO
	   			SET FILT TO IIF(!EMPTY(vCodsub),CodSubPr=ALLTRIM(vCodsub),.t.) .AND. ;
	        	IIF(alltrim(vtipfun)='F',iif(!empty(vCodAct),CodAct=ALLTRIM(vCodAct),.t.),iif(!empty(vProyec),Codproy=ALLTRIM(vProyec),.t.)))
	   			SET INDEX TO (ZInd) 
	   			GO TOP
   			
	   			SCAN
	   			    SCATTER MEMVAR
	   				SELECT REPOP
	        	       IF F_APPD()
	        	          gATHER MEMVAR
	        	       ENDIF
	        	       UNLOCK
	            	   SELE CALEN
	        	ENDSCAN
	        	SET INDEX TO
	        	SELE REPOP
	        	GO TOP
	        	SCAN
	        	   VKL = Periodo+CodPrg  +CodFte+TipFun  +CodPart +CodSubpr +CodAct
	        	   VPART = REPOP.VALPART
				   SELECT ITEPAR
				   SET ORDE TO ITEPAR3
				   SEEK VKL
				   IF FOUN()
				      SELECT REPOP 
				      REPLACE VALPART WITH ITEPAR.VALPART
	        	       IF ALLTRIM(vCODFTE)='TRN'
	        	       	  REPLA FTETRN WITH VPART, VALPART WITH ITEPAR.VALPART
	        	       ELSE
	        	       	  REPLA FTEPRP WITH VPART, VALPART WITH ITEPAR.VALPART
    	    	       ENDIF	  
				   ELSE
				   ENDIF
				   SELECT REPOP
				 ENDSCAN          	
	             GO TOP
	       		DEACTIVATE WINDOW STANDBY
	       		IF ALLTRIM(VTIPFUN)='I'
	          		*DO REPORTE WITH 2,"salcal2,' Listado Saldo de Calendario ',1,.F.,.T.
	      		ELSE   
	          		DO REPORTE WITH 2,"salcalTR",' Listado Saldo de Calendario ',1,.F.,.T.
	       		ENDIF
	       	endif	
	    ENDIF        

PROCEDURE SUMGEN
*---------------
PRIVATE VPAR,VSUMA,VREV
VPAR = CODPRG+CODSUBPR+CODACT+LEFT(CODPART,2)
VREV=RECNO()
SUM VALPART TO VSUMA FOR CODPRG+CODSUBPR+CODACT+LEFT(CODPART,2) = VPAR
GO VREV
RETURN VSUMA
