*- Polizas.PRG
*- Listados de Polizas
*- Luis Castillo
*- Setiembre 1994 ,Dic 1994,Oct 1995
*----------------------------
*-Abriendo archivos

USE OrdCom   IN 1   order tag OrdCom1  ALIAS OrdCOM
USE IteOc    IN 2   order tag IteOc1   ALIAS IteOc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE Pecosa   IN 5   order tag Pecosa1  ALIAS Pecosa
USE Itepec   IN 6   order tag ItePec1  ALIAS Itepec
USE AuxCot   IN 7   order tag AuxCot1  ALIAS Auxcot
USE Promae   IN 8   order tag Promae1  ALIAS Promae
USE calen    IN 9   order tag calen1   ALIAS calen
USE Iteart   IN 10  order tag iteart1  ALIAS iteart
USE Poliza   IN 11  order tag poliza1  ALIAS poliza
USE Itepol   IN 12  order tag itepol1  ALIAS itepol

Vmens08 = 'No hay registros para procesar'

Public vfecfin,vfecini

IF escolor
   DEFINE POPUP LisPrg FROM 10,51 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP LisPrg FROM 10,51 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF LisPrg PROMPT "\<Entrada  "
DEFINE BAR 2 OF LisPrg PROMPT "\<Salida   "

ON SELECTION POPUP LisPrg DEACTIVATE POPUP

ACTIVATE POPUP LisPrg

vChoice = BAR()

IF LASTKEY()=27
   close data
   RETURN
ENDIF

DEFINE WINDOW LisPrg FROM 2,0 TO 23,79 COLOR SCHEME 10 ;
   TITLE " Listados "

DO CASE
   CASE BAR()=1
     sele ordcom
     IF EOF()
       DO standby WITH Vmens08
       RETURN
     ENDIF
     DEFINE WINDOW Lis FROM 5,5 TO 18,75 FLOAT DOUBLE COLOR SCHEME 5
     ACTIVATE WINDOW Lis
     vTipfun = ' '
     STORE 1        TO vToCta,vTip
     STORE SPACE(3) TO vCta
     STORE DATE()   TO vFecIni, vFecFin
     
     @ 03,01 SAY "           Periodo: "
     @ 03,22 GET vCta     PICTURE '!!!' VALID Val_para(vCta,"CODFTE",' ',22,20)
     
     @ 03,01 SAY "            Fuente: "
     @ 03,22 GET vCta     PICTURE '!!!' VALID Val_para(vCta,"CODFTE",' ',22,20)
     
     @ 05,02 say "     Tipo de Gasto: " 
     @ 05,22 GET vTipFun picture '!' VALID Val_para(vTipFun,"TIPFUN",' ',22,20)
          
     @ 08,01 SAY "         Fecha :"
     @ 09,22 GET vFecIni  PICTURE '@D' COLOR SCHEME 7
     @ 09,32 GET vFecFin  PICTURE '@D' VALID (vFecFin >= vFecIni) COLOR SCHEME 7
     
     @ 11,18 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
     READ CYCLE

     RELEASE WINDOW Lis
     IF OkCancel = 1
        ACTIVATE WINDOW STANDBY
        xColor = '*' + SET('COLOR')
        @ 01,05 SAY 'Espere un momento...Reporte en proceso' COLOR &xColor
        SELECT iteoc
        set relation to periodo+numoc+codfte into ordcom
        INDEX ON PERIODO+NUMOC+CODFTE+CODART TO PP FOR iif(vToCta=1,.t.,substr(Ordcom.Codcal,5,3)=alltrim(vCta)) and BETWEEN(ordcom.FecOc,vFecIni,vFecFin) AND ordcom.ESTADO='5' and ordcom.tipfun = alltrim(vtipfun)
        GO TOP
        SCAN 
          vPart1 = left(IteOc.CodArt,2)
          vPart2 = left(IteOc.CodArt,2)
          vTot = 0
          DO WHILE  vPart1 = vPart2
              vTot = vTot + Valtot
              SKIP
              vPart1 = left(IteOc.CodArt,2)
          ENDD
          SKIP -1
          DO AGREG_pol
          SELE ITEOC
        ENDSCAN
        
        DEACTIVATE WINDOW STANDBY
        IF !EOF()
           DO REPORTE WITH 2,"LisPeb"," Listado P¢liza de Entrada de Bienes "
        ELSE
           DO standby WITH Vmens08
        ENDIF
      ENDIF        
   case bar()=2   
      
     sele pecosa
     IF EOF()
       DO standby WITH Vmens08
       RETURN
     ENDIF
     DEFINE WINDOW Lis FROM 5,5 TO 18,75 FLOAT DOUBLE COLOR SCHEME 5
     ACTIVATE WINDOW Lis
     STORE 1        TO vToCta,vTip
     STORE SPACE(3) TO vCta
     STORE DATE()   TO vFecIni, vFecFin
     @ 01,01 SAY " Todas las Fuentes: " GET vToCta  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToCta,3,22)

     @ 03,01 SAY "            Fuente: "
     @ 03,22 GET vCta   WHEN vToCta=2 VALID Val_para(vCta,"CODFTE",' ',22,20)
     
     @ 08,01 SAY "         Fecha :"
     @ 09,22 GET vFecIni  PICTURE '@D' COLOR SCHEME 7
     @ 09,32 GET vFecFin  PICTURE '@D' VALID (vFecFin >= vFecIni) COLOR SCHEME 7

     @ 11,18 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
     READ CYCLE

     RELEASE WINDOW Lis
     IF OkCancel = 1
        ACTIVATE WINDOW STANDBY
        xColor = '*' + SET('COLOR')
        @ 01,02 SAY 'Espere un momento...Reporte en proceso' COLOR &xColor
        SELECT pecosa
        set relation to periodo+numpec into itepec
        SET SKIP TO ITEpec
        set filter to iif(vToCta=1,.t.,substr(Pecosa.Codcal,5,3)=alltrim(vCta)) and BETWEEN(pecosa.FecPec,vFecIni,vFecFin)
        GO TOP
        DEACTIVATE WINDOW STANDBY
        IF !EOF()
           DO REPORTE WITH 2,"LisPSB"," Listado P¢liza de Salida de Bienes "
        ELSE
           DO standby WITH Vmens08
        ENDIF
      ENDIF        
ENDCASE
RELEASE WINDOW LisPrg
close data
RETURN

PROCEDURE Agreg_Pol
*-----------------
AS=ALIAS()
sele itepol
IF F_appd()
   REPLACE  NumHc WITH m.NumHc ,;
 		   NumMes WITH m.NumMes,;
          ValParT WITH vTot    ,;
                  CodPart WITH Iteart.Codpart,;
                   TipFun WITH m.TipFun ,;
                   Estado WITH '00' ,;
                   CodFte WITH m.Codfte ,;
                   CodPrg WITH vCodprg ,;
                   tipdoc WITH m.tipdoc ,;
                   tipope with 'è',;
                 CodSubpR WITH vCodSub,;
                  CodPart WITH IIF(ALLTRIM(m.Tipfun)='I',HOJA.Codpart,'     ') ,;
                  CodAnal WITH Iteart.Codpart

          	 if alltrim(m.tipfun)='I'
                 replace CodProy WITH vProyec ,;
                         CodSuPry WITH vSubPry
	         else
    	         replace CodAct WITH vCodAct
        	 endif
   SELE &AS
   RETURN .T.
ENDIF
SELE &AS
RETURN .F.


PROCEDURE BUSCA 
*--------------
as=alias()
SELECT iteart
seek 'B'+iteoc.codart

xx=iif(found(),iteart.Preuni,0)
select (as)
return xx 

PROCEDURE BUSCA1
*--------------
as=alias()
SELECT iteart
seek 'B'+iteoc.codart

xx=iif(found(),iteart.CODART,' -- ')
select (as)
return xx 