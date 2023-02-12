** ---------------------------------------------------
** CONCOMP.PRG
** Reporte de compromisos
** ---------------------------------------------------

PUBLIC vIdx1,Temp,vperiodo,vcalend


vIdx1=SYS(3)+'.idx'
temp=SYS(3)+'.dbf'

* Abrir Archivos de trabajo
CLOS DATA
USE PARMAE   IN 1   ORDER TAG parmae1  ALIAS parma
USE maepre   IN 2   ORDER TAG maepre1  ALIAS maepre
USE itehc    IN 3   ORDER TAG itehc1   ALIAS iteHC
USE hojcon   IN 4   ORDER TAG hojcon1  ALIAS hoja
*----

CREATE TABLE (temp) ;
  ( periodo C(2),;
    nummes  C(2),;
    codfte  C(2),;
    codcad  C(4),;
    uniges  C(2),;
    unieje  C(3),;
    codfun  C(2),;
    codprg  C(3),;
    codspr  C(4),;
    actpry  C(6),;
    codcom  C(5),;
    codmet  C(5),;
    codpart C(6),;
    tipope  C(1),;
    valpart N(18,2) )
    
USE IN 5
SELECT 5
USE (temp) IN 5 ALIAS temp    
INDEX ON periodo+nummes+codfte+uniges+unieje+codcad+codpart TO (vIdx1)

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*--------------
PUBLIC vFecini,vFecFin
vperiodo = RIGHT(STR(YEAR(DATE()),4),2)
VTOTAL = 2
vtipo  = 2
vUniges = '01'
vUnieje = '001'
store space(2) to vcodfte,vcalend,VCODFUN
store space(3) to vcodprg
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 

DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
	TITLE ' Consulta de Compromisos ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1

@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
@  1,2 SAY '         Mes : ' GET vcalend    PICTURE '!!'    VALID val_para(vcalend,'FECMES',' ',18,30)

@  3,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID IIF(!EMPTY(vUniGes),val_para(vUniGes,'UNIGES',' ',18,30),.T.)
@  4,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID IIF(!EMPTY(vUniEje),val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30),.T.)
@  5,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		    when vTotal=2
@  6,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.)    when vTotal=2
@  7,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
@  8,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			    when vTotal=2
@  9,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			    when vTotal=2
@ 10,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID val_para(vcodfte,'CODFTE',' ',18,30)

READ VALID val_read()

DEACTIVATE WINDOW lis_1
IF LASTKEY()= 27
	RETURN
ENDIF
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*

SELECT itehc
SET RELATION TO ALLT(VPERIODO)+UNIGES+UNIEJE+CODCAD INTO MAEPRE

SET FILTER TO itehc.nummes=alltrim(vcalend)  AND ;
              itehc.codfte=alltrim(vcodfte)  AND ;
              itehc.uniges=alltrim(vuniges)  AND ;
              itehc.unieje=alltrim(vunieje)  AND ;
              itehc.estado#'99'              AND ;
              hoja.estado#'99'
      
GO TOP
SCAN
      m.periodo = vperiodo
      m.nummes  = itehc.nummes
      m.codfte  = itehc.codfte
      m.uniges  = itehc.uniges
      m.unieje  = itehc.unieje
      m.codcad  = itehc.codcad
      m.codfun  = maepre.codfun
      m.codprg  = maepre.codprg
      m.codspr  = maepre.codspr
      m.actpry  = maepre.actpry
      m.codcom  = itehc.codcom
      m.codmet  = itehc.codmet
      m.codpart = itehc.codpart
      m.tipope  = itehc.tipope
      m.valpart = itehc.valpart

      select temp
      seek m.periodo+m.nummes+m.codfte+m.uniges+m.unieje+m.codcad+m.codpart
      if found()
         replace temp.valpart with temp.valpart + iif(m.tipope='-',m.valpart*-1,m.valpart)
         REPLACE temp.codfun  WITH maepre.codfun
         REPLACE temp.codprg  WITH maepre.codprg
         REPLACE temp.codspr  WITH maepre.codspr
         REPLACE temp.actpry  WITH maepre.actpry
      else
         append blank
         gather memvar
         REPLACE temp.codfun  WITH maepre.codfun
         REPLACE temp.codprg  WITH maepre.codprg
         REPLACE temp.codspr  WITH maepre.codspr
         REPLACE temp.actpry  WITH maepre.actpry
      endif
      select itehc
ENDSCAN            

SELECT TEMP
SET FILT TO IIF(empty(vcodfun),.t.,allt(vcodfun)=codfun) AND ;
            IIF(empty(vcodprg),.t.,allt(vcodprg)=codprg) AND ;
            IIF(empty(vcodsPr),.t.,allt(vcodspr)=codspr) AND ;
            IIF(empty(vactpry),.t.,allt(vactprY)=actpry) AND ;
            IIF(empty(vcodcom),.t.,allt(vcodcom)=codcom) AND ;
            IIF(empty(vcodmet),.t.,allt(vcodmet)=codmet) 

SET RELA TO ALLT(VPERIODO)+UNIGES+UNIEJE+CODCAD INTO MAEPRE ADDI

DEACTIVATE WINDOW STANDBY
GO TOP
IF EOF()
	DO STANDBY WITH 'No existen registros para procesar'
else	   
	do reporte with 2,'concomp',' Reporte de compromisos del mes ',1,.F.,.T.
endif	

RETURN

PROCEDURE Salida
*---------------
ACTIVATE   SCREEN
CLOSE DATABASES
ERASE(vIdx1)
ERASE(temP)
release vIdx1,Temp,vperiodo,vcalend
RETURN

FUNCTION SUMAGG
*--------------
vreg=recno()
VkEY=periodo+nummes+codfte+uniges+unieje+codcad+LEFT(codpart,2)
vtot=0
scan while periodo+nummes+codfte+uniges+unieje+codcad+left(codpart,2)=vkey
     vtot=vtot+valpart
endscan
goto vreg
return vtot
     