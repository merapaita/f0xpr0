** ---------------------------------------------------
** REPOE5.PRG
** Reporte Ejecucion E-5
** UDEP
** Autor : Ing. Luis Castillo Dur n
** ---------------------------------------------------

* Abrir Archivos de trabajo

USE Parmae  IN 1 ORDER TAG Parmae1         ALIAS parma
USE Calen   IN 2 ORDER TAG Calen1          ALIAS Calen
USE Maepre  IN 3 order tag Maepre1         ALIAS Maepre
USE MaePar  IN 4 ORDER TAG MaePar1         ALIAS MaePar
USE ItePar  IN 5 ORDER TAG ItePar1         ALIAS ItePar
USE Clase   IN 6 ORDER TAG Clase1          ALIAS Clase
USE HojCon  IN 7 ORDER TAG Hojcon1         ALIAS Hoja
USE IteHc   IN 8 ORDER TAG Itehc2          ALIAS Itehc
USE Repoe5  IN 9                           ALIAS repo   EXCLUSIVE
USE Repopre IN 10                          ALIAS repoP  EXCLUSIVE

SELECT REPO
ZAP
CInd = SYS(3) + '.IDX'
INDEX ON CODPART TO (CInd) 
SELECT REPOP
ZAP
DInd = SYS(3) + '.IDX'
INDEX ON CODPART TO (DInd) 

*----
DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*--------------
vtemp    = RECNO()
as=order()
vPeriodo  = '  '
vNumMes   = '  '
vCodPrg   = '  '
vCodFte   = '   '
vTipFun   = ' '
vCodSubPr = '   '
vCodAct   = '  '
vCodPry   = '   '
vCodSpy   = '  '

 DEFINE WINDOW Lis FROM 6,12 TO 17,68 DOUBLE ;
 TITLE ' °° Listado E-5 °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,2 say '        Periodo : '
 @  2,2 say '            Mes : '
 @  3,2 say '         Fuente : '
 @  4,2 say '         TipFun : ' 
 @  5,2 say '       Programa : ' 
 @  6,2 say '    SubPrograma : '
 @  7,2 say '       Proyecto : '
 @  8,2 say '      Actividad : '

 @  1,21  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)
 @  2,21  GET vNumMes    PICTURE '!!'  VALID Val_Para(vNumMes  ,'FECMES',' ',21,30) AND !EMPTY(vNumMes)
 @  3,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30) AND !EMPTY(vCodFte)
 @  4,21  GET vTipFun    PICTURE '!'   VALID vTipFun$' IF'  &&val_PARA(vTipFun  ,'TIPFUN',' ',21,30) AND !EMPTY(vTipFun)
 @  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30) AND !EMPTY(vCodPrg) 
 @  6,21  GET vCodSubPr  PICTURE '!!!' &&VALID VAL_CALE(vCodSubPr,'CODSUB'+VCodPrg,' ',21,30) OR EMPTY(vCodPrg)
 @  7,21  GET vCodPry    PICTURE '!!!' 
 @  8,21  GET vCodAct    PICTURE '!!' WHEN EMPTY(vCodPry)

 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF EMPTY(vNumMES) .or. LASTKEY()=27
    RETURN
 ELSE
 	SELECT ITEHC
	IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
       vInd = SYS(3) + '.IDX'
       xInd = SYS(3) + '.IDX'
       vAcumanF = 0
       vAcumanI = 0
       vAcu = 0
       do case 
          case alltrim(vtipfun)='F'
               DO PASAF
          case alltrim(vtipfun)='I'               
          	   DO PASAI
          OTHER
               DO PASAF
               DO PASAI
       endcase          	   
       DO CRUZAR       
       SELE REPOP
       SET RELATION TO CODPART INTO REPO
       GO TOP
       DO REPORTE WITH 2,"REPOE5_1",' Listado E-5 '
    ENDIF
 ENDIF
RETURN

PROCEDURE PasaI
*-------------
DO ACUMULA WITH 'I'
SELE ITEHC
SET ORDE TO 2

SET FILTER TO VAL(Nummes)<=VAL(vNumMes) .AND. Codprg=ALLTRIM(vCodPrg) .AND. ;
                  CodFte=ALLTRIM(vCodFte) .AND. IIF(!EMPTY(vCodSubPr),CodSubPr=ALLTRIM(vCodSubPr),.t.) .AND. ;
                  iif(!empty(vCodAct),CodAct=ALLTRIM(vCodAct),.t.) .AND. ;
                  iif(!empty(vCodPry),CodProy=ALLTRIM(vCodPry),.t.) .AND. ;
                  ESTADO#'99' AND TIPFUN='I'
GO TOP                  
vCanreq = 0
vMesx = val(vNummes)
FOR vM = 1 to vMesx &&- 1
    SCAN FOR NUMMES = PADL(ALLTRIM(STR(vM,2)),2,'0')
		vCanreq=0
		vCa1 = CodPart
   		vCa2 = CodPart
		vAcum = 0
	    DO WHILE  vCa1 = vCa2
	       vCanreq=vCanreq+Valpart
		   IF !EOF()
        	  SKIP
	          vCa1 = CodPart
    	   ELSE
	          EXIT
	       ENDIF      
        ENDD
        SKIP -1
        SCATTER MEMVAR
        DO AGREG_E5
        SELE ITEHC
	ENDSCAN
    vAcumanI = vAcumanI + vCanreq	
    GO TOP	
ENDFOR	

*vAcum  = 0
*SCAN FOR NUMMES = PADL(ALLTRIM(STR(vM,2)),2,'0')
*     vCanreq = 0
*	 vCa1 = CodPart
*   	 vCa2 = CodPart
*	 DO WHILE  vCa1 = vCa2
*    vCanreq=vCanreq+Valpart
*    IF !EOF()
*      	  SKIP
*      vCa1 = CodPart
*   	ELSE
*      EXIT
*    ENDIF      
*    ENDDO
*    SKIP -1
*    SCATTER MEMVAR
*    DO AGREG_E5
*    SELE ITEHC
*ENDSCAN
SET FILTER TO 
RETURN

PROCEDURE PasaF
*--------------
DO ACUMULA WITH 'F'
SELE ITEHC
SET ORDE TO 3
SET FILTER TO VAL(Nummes)<=VAL(vNumMes) .AND. Codprg=ALLTRIM(vCodPrg) .AND. ;
                  CodFte=ALLTRIM(vCodFte) .AND. IIF(!EMPTY(vCodSubPr),CodSubPr=ALLTRIM(vCodSubPr),.t.) .AND. ;
                  iif(!empty(vCodAct),CodAct=ALLTRIM(vCodAct),.t.) .AND. ;
                  iif(!empty(vCodPry),CodProy=ALLTRIM(vCodPry),.t.) .AND. ;
                  ESTADO#'99' AND TIPFUN='F'
GO TOP                  
vCanreq = 0
vMesx = val(vNummes)
FOR vM = 1 to vMesx 
    SCAN FOR NUMMES = PADL(ALLTRIM(STR(vM,2)),2,'0')
		vCanreq=0
		vCa1 = CodAnal
   		vCa2 = CodAnal
		vAcum = 0
	    DO WHILE  vCa1 = vCa2
	       vCanreq=vCanreq+Valpart
		   IF !EOF()
        	  SKIP
	          vCa1 = CodAnal
    	   ELSE
	          EXIT
	       ENDIF      
        ENDD
        SKIP -1
        SCATTER MEMVAR
        
        DO AGREG_E5
        SELE ITEHC
	ENDSCAN
    vAcumanF = vAcumanF + vCanreq		
 GO TOP	
ENDFOR	

*SCAN FOR NUMMES = PADL(ALLTRIM(STR(vM,2)),2,'0')
*    vCanreq = 0
* vCa1 = CodAnal
*  	 vCa2 = CodAnal
* DO WHILE  vCa1 = vCa2
*    vCanreq=vCanreq+Valpart
*    IF !EOF()
*      	  SKIP
*      vCa1 = CodAnal
*   	ELSE
*      EXIT
*    ENDIF      
*    ENDDO
*    SKIP -1
*    SCATTER MEMVAR
*    DO AGREG_E5
*    SELE ITEHC
*ENDSCAN
SET FILTER TO 
RETURN


PROCEDURE AGREG_E5
*------------------
SELE repo
APPEND BLANK
  m.codpart   = iif(Itehc.tipfun='I',itehc.codpart,itehc.codanal)
  m.codanal   = '     '    
  m.ValPart   = vCanReq
  IF vMesx < val(vNummes)
     m.AcuMesAnt = vAcumanI+vAcumanF
     vAcu = m.AcuMesAnt
  ENDIF
 *m.AcuMesAnt = vAcu
  m.DelMes    = vCanreq
GATHER MEMVAR
RETURN .T.

PROCEDURE ZAGREG_E5
*-----------------
SELE repo
seek m.codpart
if !found()
APPEND BLANK
  m.codpart   = iif(Itehc.tipfun='I',itehc.codpart,itehc.codanal)
  m.codanal   = '     '    
  m.ValPart   = vCanReq
  m.AcuMesAnt = vAcuman
  m.DelMes    = vCanreq
GATHER MEMVAR
endif
RETURN .T.

PROCEDURE BUSCPRG
*----------------
Private vKey
zx = ALIAS()
qw = order()
vKey=ALLTRIM(VPeriodo)+ALLTRIM(Vcodprg)+ALLTRIM(VCodSubPr)+ALLTRIM(VCodPry)
SELE MAEPRE
SET ORDE TO 2
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

PROCEDURE ACUMULA
*----------------
PARAMETER XTIPFUN
vTemp=Recno()
QQ=ALIAS()
vAcum = 0
vMesx = 0
vKey = alltrim(vCODFTE)+alltrim(vCodPrg)+alltrim(vCodSubPr)+IIF(XTIPFUN='I',alltrim(vCODPRY),alltrim(vCODACT))
SELECT ITEPAR
INDEX ON CODPART TO (VInd) 
SET INDEX TO (VIND)
SET FILT TO CODFTE+CodPrg+ALLTRIM(CodSubPr)+IIF(XTIPFUN='I',ALLTRIM(CODPROY),ALLTRIM(CODACT))=VKEY AND TIPFUN=ALLTRIM(XTIPFUN)
GO TOP
*----
SCAN 
     SCATTER MEMVAR  
     vCanre = 0
	 vCa1 = CodPart
   	 vCa2 = CodPart
	 DO WHILE  vCa1 = vCa2
	    vCanre =vCanre +Valpart &&+CRESUP
	    IF !EOF()
       	  SKIP
	      vCa1 = CodPart
    	ELSE
	      EXIT
	    ENDIF      
     ENDDO
     SKIP -1
     SELECT REPOP
     SEEK m.Codpart
     IF !FOUND()
     	IF F_APPD()
        	m.valpart = vcanre
	        GATHER MEMVAR
	    ENDIF    
     ENDIF
     SELE ITEPAR
ENDSCAN
SET FILT TO
SELE (QQ)
RETURN

PROCEDURE CRUZAR
*---------------
SELE REPO
GO TOP
SCAN 
   AS = REPO.CODPART
   SCATTER MEMVAR
   SELE REPOP
   GO TOP
   SEEK ALLTRIM(AS)
   IF !FOUND()
      IF F_APPD()
         m.valpart = 0
         GATHER MEMVAR
      ENDIF
   ENDIF
   SELE REPO
ENDSCAN
GO TOP
RETURN      
      
*---------------------------------------------------------------------------

PROCEDURE ZPasa
*-------------
SELE ITEHC

SCAN 
   vCanreq=0
   vCa1 = CodPart
   vCa2 = CodPart
   vAcum = 0
   vMesx=0
   DO WHILE  vCa1 = vCa2
      vCanreq=vCanreq+Valpart
      DO ACUMULA
      IF !EOF()
         SKIP
         vCa1 = CodPart
      ELSE
         EXIT
      ENDIF      
   ENDD
   SKIP -1
   SCATTER MEMVAR
   DO AGREG_E5
   SELE ITEHC
ENDSCAN
RETURN


PROCEDURE ZACUMULA
*----------------
vTemp=Recno()
SELE ITEpar
vmesC = val(vNummes)
vAcum = 0
vMesx = 0
vKey = alltrim(vPeriodo)+alltrim(vCodPrg)+alltrim(vCODFTE)+alltrim(itehc.TIPFUN)+IteHC.CODPART+alltrim(vCODSUBPR)+IIF(ITEHC.TIPFUN='I',alltrim(vCODPRY),alltrim(vCODACT))
SELECT ITEPAR
*("Periodo+CodPrg  +CodFte+TipFun  +CodPart +CodSubpr +CodAct","ItePar3","TipFun='F'")
*("Periodo+CodPrg  +CodFte+TipFun  +CodPart +CodSubpr +CodProy +CodSuPry","ItePar4","TipFun='I'")

SET ORDER TO IIF(alltrim(Itehc.Tipfun)='F',3,4)
SET FILTER TO PERIODO+CODPRG+CODFTE+TIPFUN+CODPART+CODSUBPR+IIF(ITEHC.TIPFUN='I',CODPROY,CODACT)=VKEY
GO TOP
SCAN
  for xx = 1 to vMesC-1
      vM = padl(alltrim(str(xx,2)),2,'0')
      vAcum = vAcum + MES_&vM
  next
  vMesx = vMesx + Mes_&vNummes
ENDSCAN
SET FILT TO
SELECT ITEHC
go vTemp
RETURN

*   INDEX ON iif(ALLTRIM(vtipfun)='F',CodAnal,CodPart) TO PP FOR ;             
				  VAL(Nummes)<=VAL(vNumMes) .AND. Codprg=ALLTRIM(vCodPrg)
*   SET FILTER TO CodFte=ALLTRIM(vCodFte) .AND. IIF(!EMPTY(vCodSubPr),CodSubPr=ALLTRIM(vCodSubPr),.t.) .AND. ;
                  iif(!empty(vCodAct),CodAct=ALLTRIM(vCodAct),.t.) .AND. ;
                  iif(!empty(vCodPry),CodProy=ALLTRIM(vCodPry),.t.) .AND. TipFun=alltrim(vTipFun)

PROCEDURE ZAGREG_E5
*-----------------
SELE repo
APPEND BLANK
  m.ValPart   = vCanReq
  m.AcuMesAnt = vAcum
  m.DelMes    = vMesx
  GATHER MEMVAR
RETURN .T.
