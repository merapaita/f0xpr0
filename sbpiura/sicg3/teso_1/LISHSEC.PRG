* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ LisHsec.PRG       04/01/96                         L: 0376   ณ	
* ณ Hojas de Trabajo Sectorial                                   ณ
* ณ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

PARAMETERS vctacte,xano,vmes,vcodsub
*-Abriendo archivos
USE MovBco   IN  2 ORDER TAG Movbco1         ALIAS Movb
USE ComPag   IN 3 ORDER ComPag1   ALIAS ComPag
USE IteCP    IN 4 ORDER IteCP1    ALIAS IteCP
USE Cheque   IN 9 ORDER Cheque1   ALIAS Cheque
USE Reten    IN 10 ORDER Reten1   ALIAS Reten
USE maepre   IN 16   order tag maepre1      ALIAS maepre
USE itepar   in 25  order tag itepar1      ALIAS ITEPAR          

PUBLIC _tmto1,_tmto2,_tmto3,_tmto4,_tmto5,_tmto7,_tmto8,_tmto11,_tmto12,_tmto13,_tmto14,_tmto15,_tmto16,sw
PUBLIC _pmto1,_pmto2,_pmto3,_pmto4,_pmto5,_pmto7,_pmto8,_pmto11,_pmto12,_pmto13,_pmto14,_pmto15,_pmto16

STORE 0 TO _tmto1,_tmto2,_tmto3,_tmto4,_tmto5,_tmto7,_tmto8,_tmto11,_tmto12,_tmto13,_tmto14,_tmto15,_tmto16,sw
STORE 0 TO _pmto1,_pmto2,_pmto3,_pmto4,_pmto5,_pmto7,_pmto8,_pmto11,_pmto12,_pmto13,_pmto14,_pmto15,_pmto16
Vmens08 = 'No hay registros para procesar'
vsubsidio=.F.
  IF (ALLTRIM(vCtaCte)='631017312') AND yesno('Imprime Hoja de Trabajo de Subsidios [Si] u Otros [No]?')
     vsubsidio=.T.
  ENDIF   
  IF LASTKEY() # 27
	 DEFINE WINDOW Xwait FROM 12,06 TO 14,78 COLOR SCHEME 05 
	 ACTIVATE WINDOW Xwait
     vind1= SYS(3) +'.idx'
     vind2= SYS(3) +'.idx'
	 @0,10 SAY " Espere un Momento...Reporte en Proceso!" COLOR W+/BR*
	 SELE COMPAG
   	 VIND1=SYS(3)+".DBF"
     INDEX ON ALLTRIM(CODCAL)+NUMCP TO (VIND1) FOR CodCtC = vCtaCte and ;
                nummes = vmes AND periodo=str(xano,2) AND ;
                SUBSTR(CODCAL,10,3)=VCODSUB   AND IIF(vsubsidio=.F.,TIPDOC#'SS' AND TIPDOC#'RS',TIPDOC='SS' OR TIPDOC='RS') AND TipDOC#'IN'
	 SET RELA TO NUMMES+NUMCP+CODCTC INTO CHEQUE
	 SET SKIP TO cheque
     GO TOP
	 ACTIVATE WINDOW Xwait
     if eof()
        do standby with vmens08
        close data
        return
     else
        deactivate window xwait
        DO REPPRG  WITH "HOJTRA"," Hoja de Trabajo "
     endif
 	 IF Yesno('Desea visualizar los inutilizados?')
	 	DEFINE WINDOW Xwait FROM 12,06 TO 14,78 COLOR SCHEME 05 
	 	ACTIVATE WINDOW Xwait
	 	@0,10 SAY " Espere un Momento...Reporte en Proceso!" COLOR W+/BR*
	 	SELECT Compag
	 	vind1 = SYS(3)+'.IDX'
     	INDEX ON Numcp TO (Vind1) FOR CodCtC = vCtaCte and ;
				 Nummes=vmes   AND	periodo=str(xano,2) AND SUBSTR(CODCAL,10,3)=VCODSUB  
	 	DEACTIVATE WINDOW Xwait
	 	IF !EOF()
	 		DO Reporte WITH 2,"LisInu","Listado de Comprobantes Inutilizados"
	 	ENDIF
	 ENDIF
     SELE COMPAG  
     SET INDEX TO
   endif  
RELEASE    WINDOW lisprg
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


PROCEDURE HOJTRA	
*----------------
PARAMETERS XCOP
PRIVATE FILA,VCAL,VKEY,_qmto1,_qmto2,_qmto3,_qmto4,_qmto5,_qmto7,_qmto8,_qmto11,_qmto12
PRIVATE _tmto1,_tmto2,_tmto3,_tmto4,_tmto5,_tmto7,_tmto8,_tmto11,_tmto12
STORE 0 TO _qmto1,_qmto2,_qmto3,_qmto4,_qmto5,_qmto7,_qmto8,_qmto11,_qmto12
STORE 0 TO _tmto1,_tmto2,_tmto3,_tmto4,_tmto5,_tmto7,_tmto8,_tmto11,_tmto12
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
SELE COMPAG
@1,1 SAY CHR(15)
fila=8
DO imp_header  
VCAL=COMPAG.CODCAL

SCAN
	IF COMPAG.CODCAL<>VCAL
	    @FILA,2 SAY REPLICATE("ฤ",230)
	    FILA=FILA+1
	    @FILA,02 SAY "TOTAL ACTIVIDAD =>"+Val_SUBP(substr(VCAL,10,3),'CODSUB'+substr(VCAL,8,2)+'    ','D',22,20)
		@FILA,52 SAY _pmto1 PICTURE "@Z 999,999,999.99"
		@FILA,65 SAY _pmto2 PICTURE "@Z 999,999,999.99"
		@FILA,79 SAY _pmto3 PICTURE "@Z 999,999,999.99"
		@FILA,93 SAY _pmto4 PICTURE "@Z 999,999,999.99"
		@FILA,107 SAY _pmto5 PICTURE "@Z 999,999,999.99"
		@FILA,121 SAY _pmto7 PICTURE "@Z 999,999,999.99"
		@FILA,135 SAY _pmto8 PICTURE "@Z 999,999,999.99"
		@FILA,149 SAY _pmto11 PICTURE "@Z 999,999,999.99"
		@FILA,163 SAY _pmto12 PICTURE "@Z 999,999,999.99"
		@FILA,177 SAY _pmto13 PICTURE "@Z 999,999,999.99"
		@FILA,191 SAY _pmto14 PICTURE "@Z 999,999,999.99"
		@FILA,205 SAY _pmto15 PICTURE "@Z 999,999,999.99"
		@FILA,219 SAY _pmto16 PICTURE "@Z 999,999,999.99"
		STORE 0 TO _pmto1,_pmto2,_pmto3,_pmto4,_pmto5,_pmto7,_pmto8,_pmto11,_pmto12,_pmto13,_pmto14,_pmto15,_pmto16
	    FILA=FILA+1
	    @FILA,2 SAY REPLICATE("ฤ",230)
	    FILA=FILA+1
	IF FILA>=60
	   FILA=8
    DO IMP_HEADER
	ENDIF   
	ENDIF    
    @FILA,3 SAY COMPAG.NUMCP
    @FILA,8 SAY IIF(COMPAG.DOCREF='H/C',LEFT(COMPAG.NUMREF,4),' ')
    @FILA,13 SAY CHEQUE.NUMCHQ
    @FILA,28 SAY IIF(Compag.estado = '99',' A N U L A D O',LEFT(CHEQUE.NOMGIR,20))
	VKEY=COMPAG.NUMMES+COMPAG.NUMCP+ALLTRIM(COMPAG.CODCTC)
	SELE ITECP
	SEEK IIF(Compag.estado = '99','#####',VKEY)
	IF FOUND()
		SCAN WHILE ITECP.NUMMES+ITECP.NUMCP+ALLTRIM(ITECP.CODCTC)=VKEY
		DO CASE
		   CASE ITECP.TIPFUN="F"
			DO CASE
			   CASE LEFT(ITECP.CODANAL,2)='01'
	    	        _qmto1=_qmto1+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='02'
		            _qmto2=_qmto2+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='03'
	        	    _qmto3=_qmto3+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='04'
		            _qmto4=_qmto4+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='05'
		            _qmto5=_qmto5+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='07'
		            _qmto7=_qmto7+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='08'
		            _qmto8=_qmto8+ITECP.impparc
			   CASE LEFT(ITECP.CODANAL,2)='11'
		            _qmto11=_qmto11+ITECP.impparc
		       OTHERWISE    
    		        _qmto12=_qmto12+ITECP.impparc
		       ENDCASE     
		   CASE ITECP.TIPFUN="I"
			DO CASE
			   CASE LEFT(ITECP.codpart,2)='01'
    		        _qmto1=_qmto1+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='02'
	        	    _qmto2=_qmto2+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='03'
		            _qmto3=_qmto3+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='04'
	        	    _qmto4=_qmto4+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='05'
		            _qmto5=_qmto5+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='07'
	        	    _qmto7=_qmto7+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='08'
		            _qmto8=_qmto8+ITECP.impparc
			   CASE LEFT(ITECP.codpart,2)='11'
	        	    _qmto11=_qmto11+ITECP.impparc
		       OTHERWISE
    		        _qmto12=_qmto12+ITECP.impparc
	    	   ENDCASE     
		 ENDCASE
		ENDSCAN
	ENDIF   	
	SELE COMPAG
	_tmto1=_tmto1+_qmto1
	_tmto2=_tmto2+_qmto2
	_tmto3=_tmto3+_qmto3
	_tmto4=_tmto4+_qmto4
	_tmto5=_tmto5+_qmto5
	_tmto7=_tmto7+_qmto7
	_tmto8=_tmto8+_qmto8
	_tmto11=_tmto11+_qmto11
	_tmto12=_tmto12+_qmto12
	_pmto1=_pmto1+_qmto1
	_pmto2=_pmto2+_qmto2
	_pmto3=_pmto3+_qmto3
	_pmto4=_pmto4+_qmto4
	_pmto5=_pmto5+_qmto5
	_pmto7=_pmto7+_qmto7
	_pmto8=_pmto8+_qmto8
	_pmto11=_pmto11+_qmto11
	_pmto12=_pmto12+_qmto12
	@FILA,51 SAY _qmto1 PICTURE "@Z 999,999,999.99"
	@FILA,65 SAY _qmto2 PICTURE "@Z 999,999,999.99"
	@FILA,79 SAY _qmto3 PICTURE "@Z 999,999,999.99"
	@FILA,93 SAY _qmto4 PICTURE "@Z 999,999,999.99"
	@FILA,107 SAY _qmto5 PICTURE "@Z 999,999,999.99"
	@FILA,121 SAY _qmto7 PICTURE "@Z 999,999,999.99"
	@FILA,135 SAY _qmto8 PICTURE "@Z 999,999,999.99"
	@FILA,149 SAY _qmto11 PICTURE "@Z 999,999,999.99"
	@FILA,163 SAY _qmto12 PICTURE "@Z 999,999,999.99"
	@FILA,177 SAY IIF(Estado='99',0,CHEQUE.VALCHQ) PICTURE "@Z 999,999,999.99"
	@FILA,191 SAY IIF(CHEQUE.codpart<>'16.01',IIF(Compag.estado='99',0,COMPAG.import),0) PICTURE "@Z 999,999,999.99"
	@FILA,205 SAY IIF(CHEQUE.codpart="16.01",IIF(Compag.estado='99',0,COMPAG.import),0) PICTURE "@Z 999,999,999.99"
	@FILA,219 SAY IIF(COMPAG.import-CHEQUE.VALCHQ>0,IIF(Compag.estado='99',0,COMPAG.import-CHEQUE.VALCHQ),0) PICTURE "@Z 999,999,999.99"
	_tmto13=_tmto13+IIF(Compag.Estado='99',0,CHEQUE.VALCHQ)
	_tmto14=_tmto14+IIF(CHEQUE.codpart<>'16.01',IIF(Compag.Estado='99',0,COMPAG.import),0)
	_tmto15=_tmto15+IIF(CHEQUE.codpart="16.01",IIF(Compag.Estado='99',0,COMPAG.import),0) 
	_tmto16=_tmto16+IIF(COMPAG.import-CHEQUE.VALCHQ>0,IIF(Compag.Estado='99',0,COMPAG.import)-IIF(cheque.Estado='99',0,CHEQUE.VALCHQ),0) 
	_pmto13=_pmto13+IIF(Estado='99',0,CHEQUE.VALCHQ)
	_pmto14=_pmto14+IIF(CHEQUE.codpart<>'16.01',IIF(Compag.Estado='99',0,COMPAG.import),0)
	_pmto15=_pmto15+IIF(CHEQUE.codpart="16.01",IIF(Compag.Estado='99',0,COMPAG.import),0) 
	_pmto16=_pmto16+IIF(COMPAG.import-CHEQUE.VALCHQ>0,IIF(Compag.Estado='99',0,COMPAG.import)-IIF(Cheque.Estado='99',0,CHEQUE.VALCHQ),0) 
	FILA=FILA+1
	IF FILA>=60
	   FILA=8
    DO IMP_HEADER
	ENDIF   
	STORE 0 TO _qmto1,_qmto2,_qmto3,_qmto4,_qmto5,_qmto7,_qmto8,_qmto11,_qmto12
	VCAL=COMPAG.CODCAL
ENDSCAN
sw=1
SELE MOVB
INDEX ON ALLTRIM(CODCAL)+NUMERO TO (VIND2) FOR CodCtC = vCtaCte .AND. Nummes=vmes   and periodo=str(xano,2) AND SUBSTR(CODCAL,10,3)=VCODSUB AND ESTADO<>'99' AND TIPDOC='N/C' AND (MOTIVO='15' OR MOTIVO='14' )
IF !EOF()
VCAL=MOVB.CODCAL
SCAN
	IF MOVB.CODCAL<>VCAL
	    @FILA,2 SAY REPLICATE("ฤ",230)
	    FILA=FILA+1
	    @FILA,02 SAY "TOTAL ACTIVIDAD =>"+Val_SUBP(substr(VCAL,10,3),'CODSUB'+substr(VCAL,8,2)+'    ','D',22,20)
		@FILA,52 SAY _pmto1 PICTURE "@Z 999,999,999.99"
		@FILA,65 SAY _pmto2 PICTURE "@Z 999,999,999.99"
		@FILA,79 SAY _pmto3 PICTURE "@Z 999,999,999.99"
		@FILA,93 SAY _pmto4 PICTURE "@Z 999,999,999.99"
		@FILA,107 SAY _pmto5 PICTURE "@Z 999,999,999.99"
		@FILA,121 SAY _pmto7 PICTURE "@Z 999,999,999.99"
		@FILA,135 SAY _pmto8 PICTURE "@Z 999,999,999.99"
		@FILA,149 SAY _pmto11 PICTURE "@Z 999,999,999.99"
		@FILA,163 SAY _pmto12 PICTURE "@Z 999,999,999.99"
		@FILA,177 SAY _pmto13 PICTURE "@Z 999,999,999.99"
		@FILA,191 SAY _pmto14 PICTURE "@Z 999,999,999.99"
		@FILA,205 SAY _pmto15 PICTURE "@Z 999,999,999.99"
		@FILA,219 SAY _pmto16 PICTURE "@Z 999,999,999.99"
		STORE 0 TO _pmto1,_pmto2,_pmto3,_pmto4,_pmto5,_pmto7,_pmto8,_pmto11,_pmto12,_pmto13,_pmto14,_pmto15,_pmto16
	    FILA=FILA+1
	    @FILA,2 SAY REPLICATE("ฤ",230)
	    FILA=FILA+1
	ENDIF    
    @FILA,3 SAY 'N/C'
    @FILA,8 SAY MOVB.NUMERO
    @FILA,13 SAY MovB.numhc
    @FILA,24 SAY LEFT(MOVB.NOMBRE,25)
    DO CASE
	   CASE LEFT(MOVB.codpart,2)='01'
	        _qmto1=_qmto1+MOVB.monto
	   CASE LEFT(MOVB.codpart,2)='02'
	        _qmto2=_qmto2+MOVB.monto
	   CASE LEFT(MOVB.codpart,2)='03'
	   	    _qmto3=_qmto3+MOVB.monto
	   CASE LEFT(MOVB.codpart,2)='04'
	        _qmto4=_qmto4+MOVB.monto
	   CASE LEFT(MOVB.codpart,2)='05'
	        _qmto5=_qmto5+MOVB.monto
	   CASE LEFT(MOVB.codpart,2)='07'
	        _qmto7=_qmto7+MOVB.monto
	   CASE LEFT(MOVB.codpart,2)='08'
	        _qmto8=_qmto8+MOVB.monto
	   CASE LEFT(MOVB.codpart,2)='11'
	        _qmto11=_qmto11+MOVB.monto
	   OTHERWISE    
            _qmto12=_qmto12+MOVB.monto
	 ENDCASE     
	_tmto1=_tmto1+_qmto1
	_tmto2=_tmto2+_qmto2
	_tmto3=_tmto3+_qmto3
	_tmto4=_tmto4+_qmto4
	_tmto5=_tmto5+_qmto5
	_tmto7=_tmto7+_qmto7
	_tmto8=_tmto8+_qmto8
	_tmto11=_tmto11+_qmto11
	_tmto12=_tmto12+_qmto12
	_pmto1=_pmto1+_qmto1
	_pmto2=_pmto2+_qmto2
	_pmto3=_pmto3+_qmto3
	_pmto4=_pmto4+_qmto4
	_pmto5=_pmto5+_qmto5
	_pmto7=_pmto7+_qmto7
	_pmto8=_pmto8+_qmto8
	_pmto11=_pmto11+_qmto11
	_pmto12=_pmto12+_qmto12
	@FILA,51 SAY _qmto1 PICTURE "@Z 999,999,999.99"
	@FILA,65 SAY _qmto2 PICTURE "@Z 999,999,999.99"
	@FILA,79 SAY _qmto3 PICTURE "@Z 999,999,999.99"
	@FILA,93 SAY _qmto4 PICTURE "@Z 999,999,999.99"
	@FILA,107 SAY _qmto5 PICTURE "@Z 999,999,999.99"
	@FILA,121 SAY _qmto7 PICTURE "@Z 999,999,999.99"
	@FILA,135 SAY _qmto8 PICTURE "@Z 999,999,999.99"
	@FILA,149 SAY _qmto11 PICTURE "@Z 999,999,999.99"
	@FILA,163 SAY _qmto12 PICTURE "@Z 999,999,999.99"
	@FILA,177 SAY MOVB.monto PICTURE "@Z 999,999,999.99"
	@FILA,191 SAY MOVB.monto PICTURE "@Z 999,999,999.99"
	_tmto13=_tmto13+MOVB.monto
	_tmto14=_tmto14+MOVB.monto
	_pmto13=_pmto13+MOVB.monto
	_pmto14=_pmto14+MOVB.monto 
	FILA=FILA+1
	IF FILA>=60
	   FILA=8
    DO IMP_HEADER
	ENDIF   
	STORE 0 TO _qmto1,_qmto2,_qmto3,_qmto4,_qmto5,_qmto7,_qmto8,_qmto11,_qmto12
	VCAL=MOVB.CODCAL
ENDSCAN
ENDIF
@FILA,2 SAY REPLICATE("ฤ",230)
FILA=FILA+1
@FILA,02 SAY "TOTAL ACTIVIDAD =>"+Val_SUBP(substr(VCAL,10,3),'CODSUB'+substr(VCAL,8,2)+'    ','D',22,20)
@FILA,52 SAY _pmto1 PICTURE "@Z 999,999,999.99"
@FILA,65 SAY _pmto2 PICTURE "@Z 999,999,999.99"
@FILA,79 SAY _pmto3 PICTURE "@Z 999,999,999.99"
@FILA,93 SAY _pmto4 PICTURE "@Z 999,999,999.99"
@FILA,107 SAY _pmto5 PICTURE "@Z 999,999,999.99"
@FILA,121 SAY _pmto7 PICTURE "@Z 999,999,999.99"
@FILA,135 SAY _pmto8 PICTURE "@Z 999,999,999.99"
@FILA,149 SAY _pmto11 PICTURE "@Z 999,999,999.99"
@FILA,163 SAY _pmto12 PICTURE "@Z 999,999,999.99"
@FILA,177 SAY _pmto13 PICTURE "@Z 999,999,999.99"
@FILA,191 SAY _pmto14 PICTURE "@Z 999,999,999.99"
@FILA,205 SAY _pmto15 PICTURE "@Z 999,999,999.99"
@FILA,219 SAY _pmto16 PICTURE "@Z 999,999,999.99"
FILA=FILA+1
@FILA,2 SAY REPLICATE("ฤ",230)
FILA=FILA+1
@FILA,02 SAY "TOTAL GENERAL =>"
@FILA,51 SAY _tmto1 PICTURE "@Z 999,999,999.99"
@FILA,65 SAY _tmto2 PICTURE "@Z 999,999,999.99"
@FILA,79 SAY _tmto3 PICTURE "@Z 999,999,999.99"
@FILA,93 SAY _tmto4 PICTURE "@Z 999,999,999.99"
@FILA,107 SAY _tmto5 PICTURE "@Z 999,999,999.99"
@FILA,121 SAY _tmto7 PICTURE "@Z 999,999,999.99"
@FILA,135 SAY _tmto8 PICTURE "@Z 999,999,999.99"
@FILA,149 SAY _tmto11 PICTURE "@Z 999,999,999.99"
@FILA,163 SAY _tmto12 PICTURE "@Z 999,999,999.99"
@FILA,177 SAY _tmto13 PICTURE "@Z 999,999,999.99"
@FILA,191 SAY _tmto14 PICTURE "@Z 999,999,999.99"
@FILA,205 SAY _tmto15 PICTURE "@Z 999,999,999.99"
@FILA,219 SAY _tmto16 PICTURE "@Z 999,999,999.99"
FILA=FILA+1
@FILA,2 SAY REPLICATE("ฤ",230)
SET DEVICE TO SCREEN 
RETURN


PROCEDURE imp_header
*----------------
@1,3   SAY ALLTRIM(CIA)
@1,225 say "PAG:"
@1,230 SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "ResCP"
@2,55  say 'REPORTE HOJA DE TRABAJO -  '+VMES    +Val_SUBP(substr(CODCAL,10,3),'CODSUB'+substr(CODCAL,8,2)+'    ','D',22,20)
@2,215 SAY "FECHA:"
@2,225 SAY DATE()           
IF SW=1
@3,75  say "CUENTA CORRIENTE : "+ALLTRIM(Val_Fun('Caja','CodCtc',"ALLTRIM(Codctc)+' '+Descri",CodCtc))+'  -->'
*@3,95  say "CUENTA CORRIENTE : "+MOVB.CODCTC
ELSE
@3,75  say "CUENTA CORRIENTE : "+ALLTRIM(Val_Fun('Caja','CodCtc',"ALLTRIM(Codctc)+' '+Descri",CodCtc))+'  -->'
*@3,95  say "CUENTA CORRIENTE : "+COMPAG.CODCTC
endif
@4,2 SAY REPLICATE("ฤ",230)
@5,3 SAY " C/P  H/C  CHEQUE       RAZON SOCIAL                   01.00         02.00         03.00        04.00        05.00         07.00         08.00          11.00            OTROS      LIQUIDO     BRUTO                RETENCIONES"
@6,3 SAY "                                                                                                                                                                                    GIRADO    COMPROMETIDO       DEBE          HABER"
@7,2 SAY REPLICATE("ฤ",230) 
RETURN