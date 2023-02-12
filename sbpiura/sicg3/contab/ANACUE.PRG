* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º Analisis.Prg                                                     º
* º Programa de Proceso e impresi¢n del an lisis de Cuenta           º
* º Fecha      : 04-11-96                                            º
* º Programador: Javier Fr¡as G.                                     º
* º Obs.       : *** m¢dulo de prueba ***                            º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
PUBLIC vTipCue,vCuenta,vDigitos,vSedPli,vFecMes,vDivisi,vPeriodo
PUBLIC Mov_Prv,Mov_PrvI,Sal_prv,Sal_prvI,vInd1,vind2,vind3,vind4
PUBLIC t_sedeD,t_sedeH,t_sectD,t_sectH
STORE 0 TO t_sedeD,t_sedeH,t_sectD,t_sectH

*-apertura de archivos:

USE Parmae    IN  1    ORDER TAG Parmae1         ALIAS parma
USE Diario    IN  2    ORDER TAG Diario1         ALIAS Diar
USE SalDoNa   IN  3    ORDER TAG SaldoNa1        ALIAS SalN
USE Cuentas   IN  4    ORDER TAG Cuentas1        ALIAS Cuen
USE Compro    IN  5    ORDER TAG Compro7         ALIAS Comp
USE Auxil     IN  6    ORDER TAG Auxil1          ALIAS Auxil
USE SalAux    IN  7    ORDER TAG Salaux1         ALIAS SalA



*-Inicia Proceso
DO inicia
DO termina
RETURN
*-Fin del Proceso


PROCEDURE inicia
*---------------
DEFINE WINDOW vInfo FROM 06,15 TO 21,65 COLOR SCHEME 5 TITLE " ± An lisis de Cuentas por Proveedor ± "
ACTIVATE WINDOW vInfo 
vTipCue  = 1
vCuenta  = SPACE(3)
vPeriodo = subs(dtoc(date()),7,2)
vDigitos = 2
vSedPli  = '02'
vFecMes   = SPACE(2)
SelCta   = SYS(3)+'.dbf'
SelCtai  = SYS(3)+'.idx'
Sal_prv  = SYS(3)+'.dbf'
Sal_PrvI = SYS(3)+'.idx'
Mov_Prv  = SYS(3)+'.dbf'
Mov_Prvi = SYS(3)+'.idx'
vInd1    = SYS(3)+'.idx'
vInd2    = SYS(3)+'.idx'
vInd3    = SYS(3)+'.idx'
vInd4    = SYS(3)+'.idx'

@ 02,02 SAY 'An lisis de: '
@ 01,15 GET vTipCue FUNCTION "^ Una Cuenta;Todas las Cuentas" valid valtip()
@ 05,02 SAY '    D¡gitos:'
@ 04,15 GET vDigitos FUNCTION "^ 2;3" DEFAULT 3 when vtipcue=1
@ 07,02 SAY 'Cuenta:'
@ 07,15 GET vCuenta PICTURE '!!!' when vtipcue=1;
  valid val_cta(@vcuenta,07,20,29,vdigitos)
@ 08,02 SAY '    Per¡odo:'
@ 09,02 SAY '        Mes:'
@ 08,15 GET vperiodo PICTURE '99'
@ 09,15 GET vFecmes PICTURE '!!';
  VALID val_para(vfecmes,'FECMES',' ',15,20)
* @ 10,02 SAY ' Nivel Info.' 
* @ 10,15 GET vSedPli PICTURE '!!' ;
*  VALID val_para(vsedpli,'NIVINT',' ',15,20)
@ 13,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW vInfo 
IF okcancel=1 AND LASTKEY()<>27
   DO procesa
ENDIF   
RETURN



PROCEDURE procesa
*----------------
* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º                                                                  º
* º           Rutina de Proceso del an lisis de Cuenta               º
* º                                                                  º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

*- Creaci¢n de archivos temporales:
SELECT 9 
CREATE TABLE (Mov_Prv);
       ( Periodo C(2)    ,;
         tipdoc  C(2)    ,;
         Mes     C(2)    ,;
         Cuenta  C(10)   ,;
         TipAux  C(2)    ,;
         CodAux  C(6)    ,;
         DocRef  C(3)    ,;
         NumRef  C(4)    ,;
         MesRef  C(2)    ,;
         Codctc  C(14)   ,;
         Comprob C(14)   ,;
         Comadi  C(2)    ,;
         Codfte  C(3)    ,;
         Debe    N(18,2) ,;
         Haber   N(18,2) ,;
         docrefA C(2)    ,;
         tipdcr  C(2)    ,;
         nummesref C(2)  ,;
         periodoref C(2) ,;
         descPrv   C(40) ,;
         analiza C(2)     )

         
SELECT 10
CREATE TABLE (Sal_Prv);
       ( Mes     C(2)    ,;
         Periodo C(2)    ,;
         tipdoc  C(2)    ,;
         DocRef  C(3)    ,;
         NumRef  C(4)    ,;
         MesRef  C(2)    ,;
         Comprob C(14)   ,;
         Comadi  C(2)    ,;
         Codfte  C(3)    ,;
         Cuenta  C(10)   ,;
         TipAux  C(2)    ,;
         CodAux  C(6)    ,;
         Debe    N(18,2) ,;
         Haber   N(18,2) ,;
         Analiza C(2)    )
         
CLOS DATA

USE Parmae    IN  1    ORDER TAG Parmae1         ALIAS parma
USE Diario    IN  2    ORDER TAG Diario1         ALIAS Diar
USE SalDoNa   IN  3    ORDER TAG SaldoNa1        ALIAS SalN
USE Cuentas   IN  4    ORDER TAG Cuentas1        ALIAS Cuen
USE Compro    IN  5    ORDER TAG Compro7         ALIAS Comp
USE Auxil     IN  6    ORDER TAG Auxil1          ALIAS Auxil
USE SalAux    IN  7    ORDER TAG Salaux1         ALIAS SalA

USE (mov_prv) IN 9 ALIAS mov_prv                EXCLUSIVE

USE (sal_prv) IN 10 ALIAS sal_prv               EXCLUSIVE
SELE sal_prv
INDEX ON periodo + cuenta + tipaux + codaux TO (sal_prvI)

ACTIVATE WINDOW standby
@ 01,08 SAY ' Procesando An lisis Cuenta '+LEFT(vCuenta,IIF(vDigitos=1,2,3))+'... ' COLOR '7+*/5'

SELECT diar
vindd=sys(3)+'.idx'
vdig =IIF(vDigitos=1,2,3)

INDEX ON cuenta to (vindd) FOR LEFT(ALLTRIM(vCuenta),vdig)=LEFT(diar.Cuenta,vdig) and ;
month(fecemi)<=val(vfecmes) AND SUBS(DTOC(fecemi),7,2)=ALLT(vperiodo) and analiza() and noliqui()
scan
         DO Procesa_Mov
         DO Procesa_Sal
         SELE diar
endscan

DO Sal_Ant   
DEACTIVATE WINDOW standby
SELECT parma
INDEX ON codigoaux FOR TIPO='TIPDOC' TO (vind4)

SELECT mov_prv
INDEX ON Cuenta+TipAux+CodAux+Periodo+Mes TO (vInd1)


SET RELATION TO comprob+comadi+codfte            INTO  comp     ADDITIVE
SET RELATION TO TipAux+CodAux                    INTO  Auxil    ADDITIVE
SET RELATION TO Cuenta+TipAux+CodAux             INTO  Sal_prv  ADDITIVE
SET RELATION TO left(comprob,2)                  INTO  parma    ADDITIVE

SELECT sal_prv
REPLACE sal_prv.analiza WITH 'Si' FOR (sal_prv.debe-sal_prv.haber)<>0
INDEX ON Cuenta+TipAux+CodAux TO (vInd2) FOR sal_prv.analiza='Si'


SELECT cuen
SET FILTER TO 

SELECT mov_prv
GO TOP
SCAN
    vAnali=sal_prv.analiza
    REPLACE mov_prv.analiza WITH vAnali
    * IF SUBS(mov_prv.cuenta,4,4)#'0101'
    *   DELETE
    * ENDIF
ENDSCAN    

SELECT mov_prv
SET RELATION OFF INTO sal_prv
* SELECT sal_prv
* GO TOP
* SCAN for SUBS(Sal_prv.cuenta,4,4)#'0101'
*       scatter memvar
*       SELECT mov_prv
*       IF F_appd()
*         GATHER MEMVAR
*         REPLACE mov_prv.tipaux  WITH '99'
*         REPLACE mov_prv.codaux  WITH '99'
*         REPLACE mov_prv.analiza WITH 'Si'
*         REPLACE mov_prv.tipdoc  WITH sal_prv.tipdoc
*       ENDIF
* ENDSCAN
SELECT mov_prv
SET FILTER TO mov_prv.analiza='Si'
GO TOP
IF EOF()
   DO STANDBY WITH "No hay registros para procesar."
ELSE   
   DO REPORTE WITH  2, "Analisis", " Analisis de Cuenta "
   IF yesno("¨Imprime res£men por proveedor?")
      DO resu_prv
   ENDIf
ENDIF

SELECT diar
SET INDEX TO
ERASE(vindd)

RETURN



FUNCTION analiza
*---------------
* v = IIF(allt(vsedpli)='02',SUBS(Cuenta,4,2)$'0102',IIF(allt(vsedpli)='01',SUBS(Cuenta,4,4)='0101',.T.))
v = .T.
return v 

FUNCTION NoLiqui
*---------------
*RETURN .T.
RETURN diar.liquida=" " AND diar.liqusu=" "

        
PROCEDURE procesa_mov
*--------------------

mPeriodo  =   SUBS(DTOC(diar.fecemi),7,2)
mMes      =   SUBS(DTOC(diar.fecemi),4,2)
mCuenta   =   diar.cuenta
mTipAux   =   diar.tipaux
mCodAux   =   diar.registro
mDocRef   =   LEFT(diar.comprob,2) 
mNumRef   =   SUBS(diar.comprob,4,4)
mMesRef   =   SUBS(DTOC(diar.fecemi),4,2)
mcodctc   =   diar.codctc
mDebe     =   IIF(diar.tipo='D',Diar.montos,0) &&- IIF(diar.tipo='D',diar.saldpen,0)
mHaber    =   IIF(diar.tipo='H',diar.montos,0) &&- IIF(diar.tipo='H',diar.saldpen,0)
mcomprob  =   diar.comprob
mcomadi   =   diar.comadi
mcodfte   =   diar.codfte
mtipdoc   =   left(diar.comprob,2)
mdocrefA   =  diar.docref
mtipdcr    =  diar.tipdcr
mnummesref =  diar.nummesref
mperiodoref=  diar.periodoref

SELECT mov_prv
IF f_appd()
   REPLACE mov_prv.Periodo    WITH   mPeriodo 
   REPLACE mov_prv.tipdoc     WITH   mtipdoc  
   REPLACE mov_prv.Mes        WITH   mMes
   REPLACE mov_prv.Cuenta     WITH   mCuenta
   REPLACE mov_prv.TipAux     WITH   mTipAux
   REPLACE mov_prv.CodAux     WITH   mCodAux     
   REPLACE mov_prv.DocRef     WITH   mDocRef
   REPLACE mov_prv.Numref     WITH   mNumRef
   REPLACE mov_prv.MesRef     WITH   mMesRef
   REPLACE mov_prv.codctc     WITH   mcodctc
   REPLACE mov_prv.Debe       WITH   mDebe
   REPLACE mov_prv.Haber      WITH   mhaber
   REPLACE mov_prv.comprob    WITH   mcomprob
   REPLACE mov_prv.comadi     WITH   mcomadi
   REPLACE mov_prv.codfte     WITH   mcodfte
   REPLACE mov_prv.tipdoc     WITH   mtipdoc
   REPLACE mov_prv.docrefA    WITH   mdocref
   REPLACE mov_prv.tipdcr     WITH   mtipdcr
   REPLACE mov_prv.nummesref  WITH   mnummesref
   REPLACE mov_prv.periodoref WITH   mperiodoref
ENDIF   
RETURN


PROCEDURE procesa_sal
*--------------------
mMes      =   SUBS(DTOC(diar.fecemi),4,2)
mPeriodo  =   SUBS(DTOC(diar.fecemi),7,2)
mCuenta   =   diar.cuenta
mTipAux   =   diar.tipaux
mCodAux   =   diar.registro
mtipo     =   diar.tipo
mDebe     =   IIF(diar.tipo='D',diar.montos,0)
mHaber    =   IIF(diar.tipo='H',diar.montos,0)
mtipdoc   =   left(diar.comprob,2)
mDocRef   =   LEFT(diar.comprob,2) 
mNumRef   =   SUBS(diar.comprob,4,4)
mMesRef   =   SUBS(DTOC(diar.fecemi),4,2)
mcomprob  =   diar.comprob
mcomadi   =   diar.comadi
mcodfte   =   diar.codfte


SELECT sal_prv
SEEK mperiodo + mcuenta + mtipaux + mcodaux
IF FOUND()
   IF f_lock(1)
      IF mtipo='D'
         REPLACE sal_prv.debe  WITH sal_prv.debe  + mdebe
      ENDIF
      IF mtipo='H'
         REPLACE sal_prv.haber WITH sal_prv.haber + mhaber
      ENDIF
   ENDIF
ELSE   
    IF f_appd()
       REPLACE sal_prv.mes        WITH   mmes
       REPLACE sal_prv.Periodo    WITH   mPeriodo 
       REPLACE sal_prv.Cuenta     WITH   mCuenta
       REPLACE sal_prv.TipAux     WITH   mTipAux
       REPLACE sal_prv.CodAux     WITH   mCodAux     
       REPLACE sal_prv.Debe       WITH   mDebe
       REPLACE sal_prv.Haber      WITH   mhaber
       REPLACE sal_prv.analiza    WITH   'No'
       REPLACE sal_prv.tipdoc     WITH   mtipdoc
       REPLACE sal_prv.DocRef     WITH   mDocRef
       REPLACE sal_prv.Numref     WITH   mNumRef
       REPLACE sal_prv.MesRef     WITH   mMesRef
       REPLACE sal_prv.comprob    WITH   mcomprob
       REPLACE sal_prv.comadi     WITH   mcomadi
       REPLACE sal_prv.codfte     WITH   mcodfte
    ENDIF
ENDIF    
RETURN


PROCEDURE sal_ant
*----------------
RETURN


PROCEDURE Termina
*----------------
CLOS DATA
ERASE(Sal_prv)
ERASE(Sal_PrvI)
ERASE(Mov_Prv)
ERASE(Mov_Prvi)
ERASE(vInd1)
ERASE(vInd2)
ERASE(vInd3)
ERASE(vInd4)
RETURN


FUNCTION valtip
*--------------
IF vtipcue=1
   SHOW GET vdigitos ENABLED
   SHOW GET vCuenta  ENABLED
ELSE
   SHOW GET vdigitos DISABLED
   SHOW GET vCuenta  DISABLED
ENDIF
RETURN .T.   


FUNCTION Glosas
*--------------
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
*-Num.Cta.Cte.
if left(comp.tipdoc,2)='20'
   r = r + " Ref. : Cta.Cte. "+comp.codctc+" "
endif

*-Hoja de Control
* r=r+IIF(!Empty(Diar.NumHC),' H/C :'+ Diar.NummesHC+'-'+Diar.NumHC,'')
*--

*-documento de referencia
if !empty(mov_prv.docrefA)
   r = r + " Doc.Ref: "+alltrim(mov_prv.tipdcr)+'.'+alltrim(mov_prv.docrefA)+'.'+alltrim(mov_prv.nummesref)+'/'+alltrim(mov_prv.periodoref)
endif   
*-

RETURN IIF(!Empty(r),ALLTRIM(r),'No se encontr¢ detalle/glosa para este Documento.')


FUNCTION Glosa
*-------------
vAli=ALIAS()
vOrd=ORDER()
vCuenta=LEFT(mov_prv.Cuenta,2)+'00000000'
SELECT Cuen
SET ORDER TO TAG Cuentas1
SEEK vCuenta
IF FOUND()
   RETURN Cuen.Descri
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '


FUNCTION Glosa1
*--------------
vAli=ALIAS()
vOrd=ORDER()
vCuenta=LEFT(mov_prv.Cuenta,3)+'0000000'
SELECT Cuen
SET ORDER TO TAG Cuentas1
SEEK vCuenta
IF FOUND()
   RETURN Cuen.Descri
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '

   
FUNCTION val_cta
*---------------
PARAMETER xcuenta,xfil,xcol,xspa,xniv
IF PARAMETERS()=4
   xniv=3
ENDIF   
Area    = SELECT()
xcuenta = PADR(alltrim(xcuenta),10,'0')
SELECT cuen
SEEK xcuenta
IF FOUND()
   @ xfil,xcol say left(cuen.descri,xspa)
   vret=.T.
ELSE
   DEFINE WINDOW v_cta FROM 05,10 TO 23,65 TITLE ' ± Plan Contable ±  ' SHADOW COLOR SCHEME 1
   SELECT cuen
   SET FILTER TO cuen.nivel=xniv
   ON KEY LABEL ENTER KEYBOARD CHR(23)
   GO TOP
   BROW FIELDS CUENTA,DESCRI WINDOW v_cta
   vcuenta=LEFT(cuen.cuenta,xniv)
   @ROW(),COL() SAY vcuenta
   @ xfil,xcol say left(cuen.descri,xspa)
   vret=.t.
   ON KEY LABEL ENTER
ENDIF
SELECT(area)
RETURN vret

   
PROCEDURE resu_prv
*------------------
SELECT mov_prv
ZAP
SELECT sal_prv
GO TOP
SCAN FOR sal_prv.analiza='Si'
     SCATTER MEMVAR
     SELECT mov_prv
     APPEND BLANK
     GATHER MEMVAR
     REPLACE mov_prv.descprv WITH auxiL.descri
     SELECT sal_prv
ENDSCAN
SELECT mov_prv
vidx9=sys(3)+'.idx'
* INDEX ON IIF(SUBS(mov_prv.cuenta,4,4)='0101','A','B') + mov_prv.descprv TO (vidx9)
INDEX ON mov_prv.descprv TO (vidx9)
GO TOP
DO REPORTE WITH  2, "ResuPRV", " Analisis de Cuenta (Res£men por proveedor) "
SET INDEX TO
ERASE(vidx9)
RETURN
