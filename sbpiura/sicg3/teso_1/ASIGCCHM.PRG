* 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
* ? AsigccHM.PRG  15/05/96                               L: 101  ?	
* ? Asigna Cuenta Corriente a Hojas de Modificaci줻              ?
* ? AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ?
* ? Adecuaci줻 :   A.S. Oswaldo Arturo Oliva Carl죒.     05/1997 ?
* 突袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴槁

vmens04 = 'Dicha H/M no fue encontrada'
vmens08 = 'No hay registros para procesar'

USE HojMod   IN 1 ORDER TAG HojMod1   ALIAS Hoja
USE IteHC    IN 2 ORDER TAG IteHC4    ALIAS IteHC
USE Cajas    IN 6 ORDER TAG Cajas2    ALIAS CajA
USE MaePre   IN 7 ORDER TAG MaePre1   ALIAS maepre

* 郞袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴?
* ? Estados                           ? 
* ?  00 H/M Emitida                   ?
* ?  20 H/M Con Cuenta Corriente      ?
* ?  99 H/M Anulada                   ?
* 突袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴?

SELECT Hoja
SET FILT TO operac$'TC'
SET RELATION TO Periodo+CodCad INTO MaePre
SET RELATION TO NumMesHc+NumHc INTO IteHC
SET SKIP TO IteHC
GO BOTT
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,22 TO 20,57 FLOAT NOCLOSE SHADOW DOUBLE TITLE '? Observaciones ?' FOOTER ' ? 췊sc? Sale ? ' COLOR SCHEME 1
ENDIF
ON KEY LABEL F9 DO busca
ON KEY LABEL F3 DO AsgCC
ON KEY LABEL F4 MODIFY MEMO Observa WINDOW Observa
ON KEY LABEL F10 KEYBOARD CHR(23)
DEFINE WINDOW RevHoj FROM 1,0 TO 23,79 COLOR SCHEME 10 TITLE "췋10칂ale   췋9츭usca   췋4츾bserv.  췋3츬sg.Cta.Cte."
BROWSE NOEDIT NOAPPEND  WINDOW RevHoj FIELDS ;
   x1=NumHM+'.'+NumMes :H='H/M',;
   FecHm :H='Fec H/M',;
   x2=NumCP+'.'+NumMesCP :H='C/P',;	
   x3=iif(tipprv="P",CodPrv,Codemp) :H='Prov',;
   IteHC.CodPart :H='Part.',;
   IteHC.ValParT :H='Asignaci줻',;
   Imptot : h='Total' , ;
   CodCTC :H='CtaCte',;
   Itehc.CodFte   :H='Fte' ,;
   Itehc.CodCad   :H='Cad.Fun.',;
   Maepre.CodFun  :H='Fun' ,;
   Maepre.CodPrg  :H='Prg' ,;
   Maepre.CodSpr  :H='Spr' ,;   
   Maepre.ActPry  :H='ActPry' 


ON KEY LABEL F8
ON KEY LABEL F9
ON KEY LABEL F10
CLOSE DATA
RETURN

PROCEDURE AsgCC     && F3
*--------------
   DEFINE WINDOW CC FROM 08,03 TO 13,78 COLOR SCHEME 10 TITLE " Asignaci줻 de Cta.Cte. "
   ACTIVATE WINDOW CC
   @ 1,2 SAY "Cuenta Corriente N?: " GET vCtaCte PICTURE "@!" ;
     VALID Val_Fun('Caja','CodCtc',"Codctc+' '+Descri",vCtaCte,1,1,25) ;
     DEFAULT SPACE(14)
   READ
   IF LASTKEY()#27 AND YesNo("Confirme la CtaCte")
      SELECT Hoja
      REPLACE CodCTC WITH vCtaCte, estado with '20' 
   ENDIF
   RELEASE WINDOW CC
   RETURN


PROCEDURE busca
*--------------
 SELECT HOJA
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp    = RECNO()
 vPeriodo = RIGHT(DTOC(DATE()),2)
 vNum_MES = '00'
 vNum_HC  = '0000'
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N즡ero H/M : '
 @ 1,23 GET vNum_Mes PICTURE '!!'
 @ 1,25 SAY '.'
 @ 1,26 GET vNum_Hc  PICTURE '!!!!'
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_Hc) .or. LASTKEY()=27
    RETURN
 ELSE
   SEEK alltrim(vNum_Mes) + vNum_hc
   IF !FOUND()
     DO standby WITH Vmens04
     go top
     GOTO Vtemp
   ENDIF
 ENDIF
RETURN