PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 14,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_1H FROM 00,00 TO 23,79  DOUBLE ;
 TITLE 'Registro Hoja Modificacion' COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 15,00 TO 23,79 DOUBLE ;
 TITLE ' Estad¡stica Diaria por Objeto del Gasto ' COLOR SCHEME 10

 DEFINE WINDOW Wind_2A FROM 15,41 TO 23,79 DOUBLE ;
 TITLE 'Est. Diaria por Objeto del Gasto' COLOR SCHEME 10

 DEFINE WINDOW Wind_2B FROM 17,00 TO 23,40 DOUBLE ;
 TITLE ' Detalle: ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 08,64 TO 10,78 ;
 TITLE ' TOTAL ' COLOR SCHEME 10

 DEFINE WINDOW Wind_4 FROM 20,63 TO 22,77 ;
 TITLE ' PARTIDA ' COLOR SCHEME 10

 DEFINE WINDOW Wind_5 FROM 13,10 TO 17,70 ;
 TITLE ' COMPROMISO PRESUPUESTAL ' COLOR SCHEME 10

 DEFINE WINDOW Wind_6 FROM 14,01 TO 16,79 ;
 TITLE ' Destino ' 

 DEFINE WINDOW Wind_7 FROM 11,08 TO 18,71 ;
 DOUBLE TITLE ' Agrega Item ' COLOR SCHEME 10

 DEFINE WINDOW Wind_8 FROM 11,08 TO 18,71 ;
 DOUBLE TITLE ' Corrige Item ' COLOR SCHEME 10

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
PRIVATE VTEMPW
VALCS = .T.
SELECT Hoja
XS=ORDER()
DO PANTALLA
SCATTER MEMVAR BLANK
 vTempW = recno()
 m.NumRef = space(4)
 m.PartRet= space(5)
 m.periodo = RIGHT(STR(YEAR(DATE()),4),2)
 vCodPrg  = space(2)
 vCodSub  = space(3)
 vProyec  = space(3)
 vSubpry  = space(4)
 vCodAct  = space(3)
 
 m.pliego   = '401'
 m.ungest   = SPACE(2)
 m.unejec   = SPACE(3)
 m.funcion  = SPACE(2)
 m.codprg   = SPACE(3)
 m.codsubpr = SPACE(4)
 m.actpry   = SPACE(6)
 m.ungest   = SPACE(2)
 m.unejec   = SPACE(3)
 m.funcion  = SPACE(2)

 STORE DATE() TO m.fechc,m.fecref
 @ 0,22 GET m.periodo picture '!!' valid !empty(m.periodo)
 @ 1,22 GET m.NumMes PICTURE '!!' VALID VAL_PARA(m.NumMes,'FECMES','C',22,30) AND VAL_MES()
 @ 1,24 SAY '.'
 @ 1,25 GET m.NumHC PICTURE '!!!!' VALID VALER() AND !EMPTY(m.NumHc) 
 @ 1,60 GET m.FechC 
 @ 2,22 GET m.TipDoc PICTURE '!!!'  VALID val_para(m.TipDoc,'TIPDOC',' ',22,30) 
 @ 3,22 GET m.NumRef PICTURE '!!!!' when m.tipdoc = 'O/'   
 @ 3,27 GET m.PerRef PICTURE '!!'   when m.tipdoc = 'O/'   &&     o O/S
 @ 3,22 GET m.DesRef PICTURE '@S15' when m.tipdoc # 'O/'
 @ 3,60 GET m.FecRef

 @ 4,22 GET m.TipPrv    PICTURE '@M P,E,O' VALID LIMPIA() 
 @ 4,24 GET m.Codprv    PICTURE '!!!!'   VALID Val_aux(m.Codprv,'20',' ',24)  WHEN m.TipPrv='P'
 @ 4,24 GET m.CodEmp    PICTURE '!!!!!'  VALID Val_aux(m.CodEmp,'30',' ',24)   WHEN m.TipPrv='E'
 @ 4,24 GET m.Codotr    PICTURE '!!!!!!' VALID Val_aux(m.Codotr,'09',' ',24)     when m.TipPrv='O'

 @  5,22 GET m.ungest PICTURE '!!' VALID val_para(m.ungest,'UNIGES',' ',22,40)
 @  6,22 GET m.unejec PICTURE '!!!' VALID val_para(m.unejec,'UNIEJE',' ',22,40)
 
 @  7,22 GET m.funcion picture '!!'  VALID val_estr(ALLTRIM(m.pliego)+ALLTRIM(m.ungest)+ALLTRIM(m.unejec)+m.funcion,22,45)
 @  8,22 GET m.codprg  picture '!!!' VALID val_estr(ALLTRIM(m.pliego)+ALLTRIM(m.ungest)+ALLTRIM(m.unejec)+ALLTRIM(m.funcion)+m.codprg,22,45)
 @  9,22 GET m.codsubpr picture '!!!!' VALID val_estr(ALLTRIM(m.pliego)+ALLTRIM(m.ungest)+ALLTRIM(m.unejec)+ALLTRIM(m.funcion)+ALLTRIM(m.codprg)+m.codsubpr,22,45)
 @ 10,22 GET m.actpry   picture '!!!!!!' VALID val_estr(ALLTRIM(m.pliego)+ALLTRIM(m.ungest)+ALLTRIM(m.unejec)+ALLTRIM(m.funcion)+ALLTRIM(m.codprg)+ALLT(m.codsubpr)+m.actpry,22,45)
 @ 11,22 GET m.codfte PICTURE '!!!' VALID val_para(m.codfte,'CODFTE',' ',22,40)

 
 READ VALID Val_Read() AND VAL_MS()
 m.nombre=AUXI.Descri
 
 IF LASTKEY() # 27 and vgun

    oK = Trab_hijo()
    IF OK .AND. LASTKEY()#27
       *-calcula total de parciales
       SELECT ITEHC
       SEEK ALLTRIM(m.NumMes) + m.NumHc
       vTotal = 0
       SCAN WHILE NumMes = ALLTRIM(m.NumMes) and NumHc = m.NumHc
               vtotal = vtotal + valpart
               IF valpart=0
                  if rlock()
                     delete next 1
                  endif
                  unlock
		       endif
          ENDSCAN
          GO TOP
          SELECT HOJA
          m.Imptot  = vtotal
          m.Codcal  = ALLTRIM(str(year(m.fechc)-1900))+alltrim(m.Nummes)+'25'+ALLT(m.pliego)+ALLT(m.ungest)+ALLT(m.unejec)+ALLT(m.funcion)+ALLT(m.codprg)+ALLT(m.codsubpr)+ALLT(m.actpry)+ALLT(m.compone)+ALLT(m.meta)
          vImpr = m.Periodo+ALLTRIM(m.Nummes)+m.Numhc
          vseek = m.Periodo+m.Numref+ALLTRIM(m.Codfte)
          *- Aumento el correlativo
          	SELECT Parma
          	SEEK 'HOJCON'+ALLTRIM(m.NumMes)
          	REPLACE NumEnt WITH NumEnt + 1
            ACTIVATE WINDOW wind_6
    	     @ 0,0 SAY 'Destino: ' get m.destino picture '@S73'
	         READ
	         DO COMPRE
	         DEACTIVATE WINDOW wind_6
   	         SELE HOJA
  		     m.user = sys(0)
  		     m.user_fc = date()
	         m.user_TP = 'I'
             m.Estado = '00'
             IF F_Appd()
                GATHER MEMVAR
             ENDIF
             vTempW = recno()
             =observa()
        ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 SELE HOJA
 GO TOP
 go vtempW
 DO VISTA
 ON KEY LABEL F2 DO VISOBS
RETURN

PROCEDURE Trab_hijo            && Revisi¢n de BD en browse 
*------------------
as    = ALIAS()
vTemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = PADC(' ®F5¯ Agregar ±± ®F6¯ Corrige ±± ®F8¯ Elimina ±± ®F10¯ Terminar ',79,'±')
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F6  DO corri_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itehc
SET ORDE TO ITEHC1
seek alltrim(m.Nummes)+m.Numhc
SET FILT TO ESTADO#'92'
if !found()
   DO agreg_item
endif
BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMODIFY NOMENU key alltrim(m.Nummes)+m.Numhc FIELDS ;
Compone  : H= 'Compon' :P='!.!!!!' :6 ,;
CodAnal  : H= 'Partid' :6 ,;
aa = VAL_PARa2(SUBSTR(Codanal,5,2),'CODPAR','D') :H='Descripci¢n' :48 ,;
ValPart :H='Total' :P='99,999,999.99' 


vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
ON KEY LABEL F5
on key label F6
ON KEY LABEL F8
ON KEY LABEL F10
SET FILTER TO
SELE HOJA
if lastkey()=27
   return .f.
endif
RETuRN .t.
PROCEDURE Agreg_Item
*-------------------
sele iteHc
vp = codpart
activate window wind_7
m.compone=old_compo
vcatgas  = SPACE(1)
vgrugen  = SPACE(1)
vmodapl  = SPACE(2)
vespeci  = SPACE(2)
m.codanal=space(6)
m.valpart=0
@ 00,02 say 'Componente : ' get m.compone PICTURE '!.!!!!';
  valid !EMPTY(m.compone) AND val_com(m.compone,16,33)
  
@ 01,02 say 'Categ.Gasto: ' get vcatgas PICTURE '!';
  VALID VAL_PARA(vcatgas,'CATGAS',' ',16,33,8) 

@ 02,02 say 'Gr.Gen‚rico: ' get vgrugen PICTURE '!';
  VALID VAL_PARA(vgrugen,'GRUGEN',' ',16,33,8) 
  
@ 03,02 say 'Mod.Aplicac: ' get vmodapl picture '!!';
  VALID VAL_PARA(vmodapl,'MODAPL',' ',16,33,8)   
  
@ 04,02 say 'Espec¡fica : ' get vespeci picture '!!';
   VALID VAL_PARA(vespeci,'CODPAR',' ',16,33,8)   
   
@ 05,02 say 'Monto      : ' get m.valpart picture '999,999.99'  

read VALID val_read()
old_compo = m.compone
m.codanal = allt(vcatgas) + allt(vgrugen) + allt(vmodapl) + allt(vespeci)
DEactivate window wind_7

IF LASTKEY()=27 OR m.valpart<1
   RETURN
ENDIF

SELECT ITEHC
IF F_appd()
      REPLACE NumHc  WITH m.NumHc ,;
              NumMes WITH m.NumMes,;
              codpart with m.codpart ,;
              codanal with m.codanal ,;
              Estado WITH '00' ,;
              CodFte WITH ALLT(m.Codfte) ,;
              tipdoc WITH m.tipdoc ,;
              tipope with '*',;
              Pliego   with m.pliego ,;
              ungest   with m.ungest ,;
              unejec   with m.unejec ,;
              funcion  with m.funcion ,;
              codprg   with m.codprg ,;
              codsubpr with m.codsubpr ,;
              actpry   with m.actpry ,;
              compone  with m.compone ,;
              valpart  with m.valpart
ENDIF
RETURN .T.


PROCEDURE corri_Item
*-------------------
sele iteHc
vp = codpart
activate window wind_8
m.compone=itehc.compone
m.codanal=itehc.codanal
vcatgas  = SUBS(m.codanal,1,1)
vgrugen  = SUBS(m.codanal,2,1)
vmodapl  = SUBS(m.codanal,3,2)
vespeci  = SUBS(m.codanal,5,2)


m.valpart=itehc.valpart
@ 00,02 say 'Componente : ' get m.compone PICTURE '!.!!!!';
  valid val_com(m.compone,16,33)
  
@ 01,02 say 'Categ.Gasto: ' get vcatgas PICTURE '!';
  VALID VAL_PARA(vcatgas,'CATGAS',' ',16,33,8) 

@ 02,02 say 'Gr.Gen‚rico: ' get vgrugen PICTURE '!';
  VALID VAL_PARA(vgrugen,'GRUGEN',' ',16,33,8) 
  
@ 03,02 say 'Mod.Aplicac: ' get vmodapl picture '!!';
  VALID VAL_PARA(vmodapl,'MODAPL',' ',16,33,8)   
  
@ 04,02 say 'Espec¡fica : ' get vespeci picture '!!';
   VALID VAL_PARA(vespeci,'CODPAR',' ',16,33,8)   
   
@ 05,02 say 'Monto      : ' get m.valpart picture '999,999.99'  

read VALID val_read()
old_compo = m.compone
m.codanal = allt(vcatgas) + allt(vgrugen) + allt(vmodapl) + allt(vespeci)

DEactivate window wind_8

IF LASTKEY()=27 OR m.valpart<1
   RETURN
ENDIF

SELECT ITEHC
IF F_LOCK(1)
      REPLACE codanal with m.codanal ,;
              compone  with m.compone ,;
              valpart  with m.valpart
ENDIF
RETURN .T.



PROCEDURE Elimi_Item
*-----------------
sele iteHc
if rlock()
   REPLACE VALPART WITH 0
   delete next 1
endif
return
