*--------------------------------------------------------------------------
* RegSol.Prg
* Registra los Solicitud de Servicios que se emiten en cada dependencia
* Estado del Solicitud de Servicio:
*   '00' Emitida     ** Este es el que se registra en el pecosa
*   '20' Con Correlativo
*   '50' Con O/S
*   '70' Devuelta
*   '99' Anulada
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE Solser   IN 2   order tag Solser1  ALIAS Solser
USE calen    IN 3   order tag calen1   ALIAS calen
USE maepre   IN 4   order tag maepre1  ALIAS maepre
USE itepar   in 5   order tag itepar1  ALIAS ITEPAR          
USE Obra     in 6   order tag obra1    ALIAS obra
USE OrdSer   IN 7   ORDER TAG OrdSer1  ALIAS Ordse
USE HojCon   IN 8   ORDER TAG HojCon1  ALIAS Hoja
USE Cheque   IN 9   ORDER TAG Cheque1  ALIAS Cheque
USE Compag   IN 10  ORDER TAG Compag1  ALIAS compag
USE Promae   IN 11  order tag Promae1  ALIAS Promae

PRIVATE VRUC,VDIREC
PUBLIC FOND

*- Mensajes de aviso al usuario
Vmens01 = ' Solicitud de Servicios : REGISTRO '
Vmens02 = 'Registro de Solicitud de Servicio'
Vmens04 = 'Dicho Solicitud de Servicio no fue encontrado'
Vmens05 = 'No existe Solicitud de Servicio anterior'
Vmens06 = 'No existe Solicitud de Servicio siguiente'
Vmens07 = '¨ Desea ELIMINAR ‚ste Solicitud de Servicio ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Solicitud de Servicio ha sido anulado'
Vmens10 = 'El Solicitud de Servicio ya fue atendido'
Vmens11 = 'El Solicitud de Servicio ha sido devuelto'
Vmens12 = 'El Solicitud de Servicio ya tiene O/S'

SELECT Solser
set filter to coddep=substr(vcoddep,1,vnumdep)
GO BOTTOM
public con,conx,cony,conz
*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
on key label f4 do imprimIR
on key label f5 do imprim_A
*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
HIDE POPUP ALL
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular   Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Solicitud de Servicio' ;
 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD anula  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  1, 2 SAY "            Fecha :"
 @  1,40 SAY " N£mero Solicitud :"
 @  2, 2 SAY "               De :"
 @  3, 2 SAY "      Dependencia :"

 @  5, 2 SAY " F.Financiamiento :"
 @  5,40 SAY "             Tipo :"
 @  6, 2 SAY "         Programa :"
 @  7, 2 SAY "      Subprograma :"
 @  8, 2 SAY " Activid/Proyecto :"
 @  9, 2 SAY "          Destino :"
 @ 10, 2 SAY "    Tipo Servicio :"
 @ 12, 2 SAY "        Ubicaci¢n :"
 @ 13, 2 SAY "          Usuario :"
 @ 14, 2 SAY "         Defectos :"
 @ 15, 2 SAY "            Causa :"
 @ 17, 2 SAY "      Descripci¢n :"

 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Solser
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 ON KEY LABEL F7 DO Estado WITH 'SS','Solser.Periodo+Solser.Numos+Solser.Codfte'
 SCATTER MEMVAR
 vPartida=m.codcal

 @  0,60 SAY IIF( m.Estado= '00','Pendiente   ',IIF( m.Estado = '20','C/Sol.Servic',IIF(m.Estado='99','Anulada     ',IIF(m.Estado='50','Atendido    ','Con O/S:'+NumOs))))
 @  1,22 SAY m.FecSS
 @  1,60 SAY m.periodo
 @  1,63 SAY m.NumSs

 @  2,22 SAY m.atte
 @  3,22 SAY val_para(m.CodDep,'CODDEP','A',22,56,7)

 @  5,22 SAY val_para(m.CodFte,'CODFTE','V',26,20)
 @  5,60 SAY val_para(m.Tipfun,'TIPFUN','A',60,15,2)
 @  6,22 SAY val_para(substr(m.CodCal, 8,2),'CODPRG','V',26,20)
 
 IF alltrim(m.Tipfun)='I' 
	 @  7,22 SAY IIF(!EMPTY(substr(m.CodCal,10,3)),VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40),SPAC(56))
	 @  7, 2 SAY IIF(ALLTRIM(m.TipFun)='F','        Actividad :','         Proyecto :')
	 @  8,22 SAY IIF(!EMPTY(substr(m.CodCal,13,3)),VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4),SPAC(56))
     @  8,25 SAY '.'
	 @  8,26 SAY IIF(!EMPTY(substr(m.CodCal,16,2)),VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5),SPAC(56))
 else
    @  6,22 clear to 8,79
 endif	
 @  9,22 SAY m.Destino PICTURE '@S56'
 @ 10,22 SAY VAL_PARA(m.TipSer,'TIPSER','V',22,30)
 @ 12,22 SAY m.Ubicac
 @ 13,22 SAY m.Usuario
 @ 14,22 SAY m.Defect
 @ 15,22 SAY m.Causas
 @ 17,22 SAY SUBSTR(m.DesSs,  1,56)
 @ 18,22 SAY SUBSTR(m.DesSs, 57,56)
 @ 19,22 SAY SUBSTR(m.DesSs,113,56)
 @ 20,22 SAY SUBSTR(m.DesSs,169,56)
 @ 21,22 SAY SPAC(56)
 @ 21,22 SAY SUBSTR(m.DesSs,225,56)
 @ 21,00 SAY PADC('° ®F4¯ Imprime °       ®F5¯ Imprime O/S        ° ®F7¯  Seguimiento °',79,' ') COLOR W+/B
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT Solser
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 ON KEY LABEL F7
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumSs  :H='N§S/S',;
 Est = IIF(Estado= '00','Pendte',IIF(Estado = '20','S/Cotz',IIF(Estado='99','Anulad',IIF(Estado='50','Atend.','('+NUMOS+')')))) :H=' O/S ' ,;
 FecSs  :H='Fecha ' ,;
 CodDep :H='DEPEND ',;
 Codfte :H='fte ' ,;
 DESSS  :H='Descripci¢n'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
ON KEY LABEL F7 
vtemp    = RECNO()
vPeriodo = RIGHT(DTOC(DATE()),2)
vNumSs   = '    '

ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero Solicitud : ' GET vPeriodo PICTURE '!!'
 @ 1,32 SAY '-' GET vNumss PICTURE '!!!!' VALID vBusca()
 READ
DEACTIVATE WINDOW standby
 IF EMPTY(vNumSs) .or. LASTKEY()=27
    ON KEY LABEL F7 DO Estado WITH 'SS','Solser.Periodo+Solser.Numos+Solser.Codfte'
    RETURN
 ELSE
    SEEK vPeriodo + vNumSs
    IF !FOUND()
       DO standby WITH Vmens04
       GOTO Vtemp
    ELSE
       DO Vista
    ENDIF
 ENDIF
 RETURN

PROCEDURE vBusca
*---------------
*vNumss=Padl(alltrim(str(vnumss,4)),4,'0')
vNumss=Padl(alltrim(Vnumss),4,'0')
retur .t.

PROCEDURE Anter
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF !BOF()
    SKIP -1
 ENDIF
 IF BOF()
    GO TOP
    DO standby WITH Vmens05
 ELSE
    DO Vista
 ENDIF
 RETURN


PROCEDURE Proxi
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH Vmens06
   GO BOTTOM
 ELSE
   DO Vista
 ENDIF
 RETURN


PROCEDURE Corri
*--------------
PUBLIC vMes,vPart
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 ON KEY LABEL F7 
 IF Estado = '99'
   * Anulada
   DO STANDBY WITH Vmens09
   RETURN
 ENDIF
 IF Estado = '70'
   * El pecosa ha sido devuelto
   DO STANDBY WITH Vmens11
   RETURN
 ENDIF
 IF Estado = '50'
   * El pecosa ya tiene O/C
   DO STANDBY WITH Vmens12
   RETURN
 ENDIF
 *****
 SCATTER MEMVAR
 vDesSs1 = SUBSTR(m.DesSs,1,56)
 vDesSs2 = SUBSTR(m.DesSs,57,56)
 vDesSs3 = SUBSTR(m.DesSs,113,56)
 vDesSs4 = SUBSTR(m.DesSs,169,56)
 vDesSs5 = SUBSTR(m.DesSs,225,56)

 xDesSs1 = SUBSTR(m.DesSs1,1,56)    
 xDesSs2 = SUBSTR(m.DesSs1,57,56)
 xDesSs3 = SUBSTR(m.DesSs1,113,56)
 xDesSs4 = SUBSTR(m.DesSs1,169,56)
 xDesSs5 = SUBSTR(m.DesSs1,225,56)

 yDesSs1 = SUBSTR(m.DesSs2,1,56)    
 yDesSs2 = SUBSTR(m.DesSs2,57,56)
 yDesSs3 = SUBSTR(m.DesSs2,113,56)
 yDesSs4 = SUBSTR(m.DesSs2,169,56)
 yDesSs5 = SUBSTR(m.DesSs2,225,56)
 
 zDesSs1 = SUBSTR(m.DesSs3,1,56)    
 zDesSs2 = SUBSTR(m.DesSs3,57,56)
 zDesSs3 = SUBSTR(m.DesSs3,113,56)
 zDesSs4 = SUBSTR(m.DesSs3,169,56)
 zDesSs5 = SUBSTR(m.DesSs3,225,56)
 
 vCodFte  = substr(m.codcal,05,3)
 vCodPrg  = substr(m.codcal,08,2)
 vCodSub  = substr(m.codcal,10,3)

 vProyec  = substr(m.codcal,13,3) 
 vCodact  = substr(m.codcal,13,2) 
 vSubpry  = substr(m.codcal,16,4)

 DO PANTALLA
 @  1,22 GET m.FecSs     PICTURE '@D'
 @  1,60 GET m.Periodo   PICTURE '!!-'  DISABLE
 @  1,63 GET m.NumSs     PICTURE '!!!!' DISABLE
 @  2,22 GET m.atte
 @  3,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,30,7)

 @  5,22 GET m.CodFte    PICTURE '!!!' VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,20)
 @  5,60 GET m.tipfun    PICTURE '!'   VALID VAL_PARA(M.tipfun,'TIPFUN',' ',60,15,2) &&AND af_proy()
 @  6,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg ,'CODPRG',' ',22,40) &&disable
 @  7,22 GET vCodSub     PICTURE '!!!' VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40) &&disable
 @  8,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
 @  8,26 GET vSubPry     PICTURE '!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
 @  9,22 GET m.Codobra   PICTURE '!!!!'  VALID VAL_OBRA(m.CodObra,m.periodo+Alltrim(vCodprg)+alltrim(vcodsub)+alltrim(vProyec)+alltrim(vSubPry),'m.CodObra',' ',22,40) WHEN ALLTRIM(M.TIPFUN)='I' 
 @  9,27 GET m.TipObr    PICTURE '!'    VALID VAL_para(m.tipobr ,'TIPOBR','C',26,40) AND AFDEST() WHEN !EMPTY(m.CodObra) and ALLTRIM(M.TIPFUN)='I'  

 @  9,29 GET m.Destino   PICTURE '@S49'
 @ 10,22 GET m.TipSer    PICTURE '!!' VALID VAL_PARA(m.TipSer,'TIPSER',' ',22,20)
 @ 12,22 GET m.Ubicac    PICTURE '@S56'
 @ 13,22 GET m.Usuario   PICTURE '@S56'
 @ 14,22 GET m.Defect    PICTURE '@S56'
 @ 15,22 GET m.Causas    PICTURE '@S56'
 @ 17,22 GET vDesSs1 PICTURE '@S56'
 @ 18,22 GET vDesSs2 PICTURE '@S56'
 @ 19,22 GET vDesSs3 PICTURE '@S56'
 @ 20,22 GET vDesSs4 PICTURE '@S56'
 @ 21,22 GET vDesSs5 PICTURE '@S30' valid cont()
 
 @ 17,22 GET xDesSs1 PICTURE '@S56'  when con
 @ 18,22 GET xDesSs2 PICTURE '@S56'  when con
 @ 19,22 GET xDesSs3 PICTURE '@S56'  when con
 @ 20,22 GET xDesSs4 PICTURE '@S56'  when con
 @ 21,22 GET xDesSs5 PICTURE '@S30'  when con  valid contx()
 
 @ 17,22 GET yDesSs1 PICTURE '@S56'  when conx
 @ 18,22 GET yDesSs2 PICTURE '@S56'  when conx
 @ 19,22 GET yDesSs3 PICTURE '@S56'  when conx
 @ 20,22 GET yDesSs4 PICTURE '@S56'  when conx
 @ 21,22 GET yDesSs5 PICTURE '@S30'  when conx valid conty()

 @ 17,22 GET zDesSs1 PICTURE '@S56'  when cony
 @ 18,22 GET zDesSs2 PICTURE '@S56'  when cony
 @ 19,22 GET zDesSs3 PICTURE '@S56'  when cony
 @ 20,22 GET zDesSs4 PICTURE '@S56'  when cony
 @ 21,22 GET zDesSs5 PICTURE '@S30'  when cony 
 
 READ VALID Val_Read()

 IF LASTKEY() # 27
    m.DesSs  = vDesSs1 + vDesSs2 +vDesSs3 +vDesSs4 +vDesSs5
    m.DesSs1 = xDesSs1 + xDesSs2 +xDesSs3 +xDesSs4 +xDesSs5
    m.DesSs2 = yDesSs1 + yDesSs2 +yDesSs3 +yDesSs4 +yDesSs5
    m.DesSs3 = zDesSs1 + zDesSs2 +zDesSs3 +zDesSs4 +zDesSs5
    m.codcal = m.periodo+padl(alltrim(str(month(m.fecSS),2)),2,'0') +ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+alltrim(vProyec)+alltrim(vSubPry)    
    GATHER MEMVAR
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT Solser
DO Vista

* @ 20,22 GET m.DesSs PICTURE '@S56'
* @  8,22 GET vProyecto   VALID VAL_PROY(vProyecto,ALLTRIM(vCodfte)+ALLTRIM(vCodprg)+ALLTRIM(vCodsub),' ',22,40) disable
*  m.codcal=m.periodo+vmes+ALLTRIM(vCodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+ALLTRIM(vProyecto)
RETURN

procedure cont
*-------------
if yesno('Continua descripci¢n 2 ?')
   con = .t.
else
   con = .f.
endif 
retu

procedure contx
*--------------
if yesno('Continua descripci¢n 3 ?')
   conx = .t.
else
   conx = .f.
endif 
retu

procedure conty
*-------------
if yesno('Continua descripci¢n 4 ?')
   cony = .t.
else
   cony = .f.
endif 
retu


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ON KEY LABEL F7 
 PUBLIC vMes,vPart
 SELECT Solser
 DO Pantalla
 SCATTER MEMVAR BLANK
 STORE SPAC(56) TO vDesSs1,vDesSs2,vDesSs3,vDesSs4,vDesSs5 
 STORE SPAC(56) TO xDesSs1,xDesSs2,xDesSs3,xDesSs4,xDesSs5 
 STORE SPAC(56) TO yDesSs1,yDesSs2,yDesSs3,yDesSs4,yDesSs5 
 STORE SPAC(56) TO zDesSs1,zDesSs2,zDesSs3,zDesSs4,zDesSs5 
 vCodPrg  = space(2)  
 vCodSub  = space(3)    
 vProyec  = space(3) 
 vSubpry  = space(2)
 vCodAct  = space(3)
 m.periodo = str(year(DATE())-1900,2)
 m.FecSs  = date()
*m.coddep = substr(vcoddep,1,vnumdep)
 = REPASA()
 @  1,22 GET m.FecSs     PICTURE '@D' &&DISABLE
 @  1,60 GET m.Periodo   PICTURE '!!'
 @  1,62 SAY '-'
 @  1,63 GET m.NumSs     PICTURE '!!!!' VALID VALIDAR() &&DISABLE

 @  2,22 GET m.atte
 @  3,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_DEP( m.CodDep,'CODDEP',' ',22,40,7)
 
 @  5,22 GET m.CodFte    PICTURE '!!!'  VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,20)
 @  5,60 GET m.tipfun    PICTURE '!'    VALID VAL_PARA(M.tipfun,'TIPFUN',' ',60,15,2) &&AND af_proy()
 @  6,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) WHEN alltrim(m.tipfun)='I'
 @  7,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40) WHEN alltrim(m.tipfun)='I'
 @  8,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
 @  8,26 GET vSubPry     PICTURE '!!'   VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
 @  9,22 GET m.Codobra   PICTURE '!!!!'  VALID VAL_OBRA(m.CodObra,m.periodo+Alltrim(vCodprg)+alltrim(vcodsub)+alltrim(vProyec)+alltrim(vSubPry),'m.CodObra',' ',22,40) WHEN ALLTRIM(M.TIPFUN)='I' 
 @  9,27 GET m.TipObr    PICTURE '!'    VALID VAL_para(m.tipobr ,'TIPOBR','C',26,40) AND AFDEST() WHEN !EMPTY(m.CodObra) and ALLTRIM(M.TIPFUN)='I'  
 @  9,29 GET m.Destino   PICTURE '@S49'
 @ 10,22 GET m.TipSer    PICTURE '!!' VALID VAL_PARA(m.TipSer,'TIPSER',' ',22,20)
 @ 12,22 GET m.Ubicac    PICTURE '@S56'
 @ 13,22 GET m.Usuario   PICTURE '@S56'
 @ 14,22 GET m.Defect    PICTURE '@S56'
 @ 15,22 GET m.Causas    PICTURE '@S56'
 @ 17,22 GET vDesSs1     &&PICTURE '@S56'
 @ 18,22 GET vDesSs2     &&PICTURE '@S56'
 @ 19,22 GET vDesSs3     &&PICTURE '@S56'
 @ 20,22 GET vDesSs4     &&PICTURE '@S56'
 @ 21,22 GET vDesSs5     valid conT() PICTURE '@S30'

*@ 17,22 GET xDesSs1 when con &&PICTURE '@S56'
*@ 18,22 GET xDesSs2 when con &&PICTURE '@S56'
*@ 19,22 GET xDesSs3 when con &&PICTURE '@S56'
*@ 20,22 GET xDesSs4 when con &&PICTURE '@S56'
*@ 21,22 GET xDesSs5 when con &&PICTURE '@S56'

 @ 17,22 GET xDesSs1 PICTURE '@S56'  when con
 @ 18,22 GET xDesSs2 PICTURE '@S56'  when con
 @ 19,22 GET xDesSs3 PICTURE '@S56'  when con
 @ 20,22 GET xDesSs4 PICTURE '@S56'  when con
 @ 21,22 GET xDesSs5 PICTURE '@S30'  when con  valid contx() 
 
 @ 17,22 GET yDesSs1 PICTURE '@S56'  when conx
 @ 18,22 GET yDesSs2 PICTURE '@S56'  when conx
 @ 19,22 GET yDesSs3 PICTURE '@S56'  when conx
 @ 20,22 GET yDesSs4 PICTURE '@S56'  when conx
 @ 21,22 GET yDesSs5 PICTURE '@S30'  when conx valid conty()

 @ 17,22 GET zDesSs1 PICTURE '@S56'  when cony
 @ 18,22 GET zDesSs2 PICTURE '@S56'  when cony
 @ 19,22 GET zDesSs3 PICTURE '@S56'  when cony
 @ 20,22 GET zDesSs4 PICTURE '@S56'  when cony
 @ 21,22 GET zDesSs5 PICTURE '@S30'  when cony 
 
 READ VALID Val_Read()

 IF LASTKEY() # 27 &&AND VSALE
    SELE SOLSER
    IF F_Appd()
       m.codcal = m.periodo+padl(alltrim(str(month(m.fecSS),2)),2,'0') +ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+alltrim(vProyec)+alltrim(vSubPry)
       m.fecemi=date()
       m.Estado = '00'
       m.DesSs = vDesSs1 + vDesSs2 +vDesSs3 +vDesSs4 +alltrim(vDesSs5)
       m.DesSs1 = xDesSs1 + xDesSs2 +xDesSs3 +xDesSs4 +alltrim(xDesSs5)
       m.DesSs2 = yDesSs1 + yDesSs2 +yDesSs3 +yDesSs4 +yDesSs5
       m.DesSs3 = zDesSs1 + zDesSs2 +zDesSs3 +zDesSs4 +zDesSs5
       GATHER MEMVAR
       *- Aumento el correlativo del pago
       SELECT Parma
       SEEK 'CORREL' + 'SOLSER'
       REPLACE NumEnt WITH NumEnt + 1
    ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
   GO BOTT
 ENDIF
 UNLOCK ALL
 SELECT Solser
 DO Vista
 
* READ
* IF LASTKEY()=27
*    DO VISTA
*    RETURN
* ENDIF
*@  5,22 GET vCodFte     PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE',' ',22,20)
*@  6,22 GET vCodPrg     PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) disable
*@  7,22 GET vCodSub     PICTURE '!!!' VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40) disable
*@  8,22 GET vProyecto   VALID (VAL_PROY(vProyecto,ALLTRIM(vCodfte)+ALLTRIM(vCodprg)+ALLTRIM(vCodsub),' ',22,40) OR EMPTY(vProyecto)) disable
* READ VALID VSALE or LASTKEY()=27 &&VALID VAL_PROY(vProyecto,ALLTRIM(vCodfte)+ALLTRIM(vCodprg)+ALLTRIM(vCodsub),' ',22,40)
* IF LASTKEY() = 27
*    DO VISTA
*    RETURN
* ENDIF
*      m.codcal=m.periodo+vmes+ALLTRIM(vCodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+ALLTRIM(vProyecto)
* @ 20,22 GET m.DesSs     PICTURE '@S56' &&WHEN VSALE
* @ 20,22 GET vDesSs1    && WHEN VSALE
* @ 21,22 GET vDesSs2    && WHEN VSALE

 RETURN


PROCEDURE AFDEST
*---------------
m.destino = 'OBRA '+M.CODOBRA+ALLTRIM(M.tipobr)+' : '+ALLTRIM(VAL_PARA(m.TipObr,'TIPOBR','D',22,40))+' - '+SUBSTR(Obra.descri,1, 100 )
RETURN .T.

procedure af_proy
*---------------
OB = IIF(alltrim(m.tipfun)='I',Iif(yesno('Ingresar Obra?'),.t.,.f.),.f.)
IF OB
    @  9,22 GET m.TipObr    PICTURE '!'    VALID VAL_para(m.tipobr ,'TIPOBR','C',26,40) WHEN ALLTRIM(M.TIPFUN)='I' 
    @  9,24 GET m.Codobra   PICTURE '!!!'  VALID VAL_OBRA(m.CodObra,m.periodo+Alltrim(vCodprg)+alltrim(vcodsub)+alltrim(vProyec)+alltrim(vSubPry),'m.CodObra',' ',22,40) WHEN ALLTRIM(M.TIPFUN)='I' 
    READ
endif    
Return       

 
PROCEDURE Validar
*----------------
PRIVATE AS
AS=RECNO()
seek m.Periodo+m.Numss
if found()
   do standby with 'La Solicitud ya esta registrada'
   GOTO AS
   return .f.
endif   
return .t.


PROCEDURE Anula
*--------------
 SELECT SOLSER
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado = '99'
   * ya pas¢
   DO STANDBY WITH Vmens09
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR ‚ste Solicitud?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
   REPLACE ESTADO WITH '99' ,FECVER WITH DATE()
   DO Vista
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Lista
*--------------
select SOLSER
vtemp =recno()
IF LASTKEY()=27
   DO VISTA
   RETURN
ENDIF
if eof()
   do standby with vmens08
   return
else
   ON KEY LABEL F7
   DO LISSER
endif
SELE SOLSER
GO vtemp
do Vista
RETURN

PROCEDURE lisSER
*---------------
vOrde = ORDER()
DEFINE WINDOW LIS FROM 2,15 TO 21,65 FLOAT DOUBLE TITLE 'Listado Solicitud de Servicios' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1   TO vToCLI,vORDEN,vTipPro,vListado
vCli = SPAC(4)
vAno = str(year(DATE())-1900,2)
@ 01,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ Resumido;Detallado' 

@ 05,01 SAY "        Total S/S : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22) when vlistado =2
@ 07,01 SAY "              S/S : "
@ 07,22 GET vAno    picture '!!'   WHEN VTOCLI=2 and vlistado=2
@ 07,25 SAY '-'
@ 07,26 GET vCli    WHEN VTOCLI=2 and vlistado=2 PICTURE '!!!!' VALID Bussol()

@ 09,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Dependencia;Emision' WHEN VTOCLI=1 

@ 12,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos' WHEN VTOCLI=1 

@ 16,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,Periodo+NumSs,IIF(vOrden=2,CodDep,DTOS(FECss))) TO (vInd) ;
         FOR IIF(vToCli=1,.T.,Periodo+NumSs = vAno+vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',Estado = '50' ))
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' Pendientes ',' Atendidos '))
   IF !EOF()
      if vlistado=2
         do reporte with 2,"Lisss1",' Solicitud de Servicios ',2
      else
         do reporte with 2,"LisSol",' Solicitud de Servicios ',1,.F.,.T.      
      endif
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   CLOSE INDEX
   ERASE (VIND)
ENDIF
SELECT solser
SET ORDE TO (VORDE)
GO TOP
RETURN

PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON
SELECT solser
VCON = RECNO()
SCATTER MEMVAR
vNumss = m.Numss
set filter to numss=vnumss &&and coddep=substr(vcoddep,1,vnumdep)
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"Lisss1",' Solicitud de Servicio '
endif
set filter to coddep=substr(vcoddep,1,vnumdep)
SELECT Solser
GO VCON
DO VISTA
RETURN

PROCEDURE IMPRIM_A
*-----------------

PRIVATE VCON
SELECT solser
VCON = RECNO()
SCATTER MEMVAR
IF m.estado # '30'
   do standby with 'A£n esta sin Orden de Servicio'
   do vista
   return
endif      
vNumss = m.Numss
set filter to coddep=substr(vcoddep,1,vnumdep)
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"Lisss1_A",' Solicitud de Servicio '
endif
set filter to
SELECT Solser
GO VCON
DO VISTA
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  ON KEY LABEL F7 
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  ON KEY LABEL F7 
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN



function valSs
*-----------------
parameter vnumss
private vfun
vfun = .t.
m.numss=padl(alltrim(str(vnumss,4)),4,'0')
if m.numss = '0000' or empty(m.numss)
   vfun = .f.
endif
return vfun

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.

XX = Val_Art(CodArt,.F.)
IF XX
  SELECT Itepec
  REPLACE ;
          coduni  WITH Produ.coduni,;
          preuni  WITH Produ.preuni
  vFun = .T.
ENDIF
RETURN vFun


PROCEDURE BusSol             && Revisi¢n de BD en browse
*--------------
SELECT Solser
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 vCli=Padl(alltrim(vCli),4,'0')
 seek vano+vcli
 if !found()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumSs  :H=' N§ ',;
 Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
 FecSs  :H='Fecha' ,;
 CodDep :H='DEP',;
 Codcal :H='Calendario' ,;
 DESSS  :H='Descripci¢n'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 endif
 vano=periodo
 vcli=numss
 ON KEY LABEL F10
 RETURN

PROCEDURE buscprv
*----------------
PRIVATE XC
XC = ALIAS()

SELECT ORDSE
SEEK SOLSER.PERIODO+SOLSER.NUMOS

VCODPRV=.F.
IF FOUND()
	IF !EMPTY(NUMOS) 
		VCODPRV = val_prv(ORDSE.codprv,.T.)
		VCODPRV=.T.
	ENDIF	
ENDIF	
SELE (XC)
FOND=VCODPRV
RETURN ' '

function repasa
*--------------
parameter vrec,vfun,ALI
vfun = .t.
vrec = recno()
ALI= ALIAS()
select SOLSER
set filter to 
set orde to SOLSER1

go top
numr = 1

do while .t.
   if val(numSS)=numr
      numr = numr + 1
      skip
      loop
   else 
      exit   
   endif   
enddo

m.numSS=padl(alltrim(str(numr,4)),4,'0')
if m.numSS = '0000' or empty(m.numSS)
   vfun = .f.
endif

SELECT Parma
SEEK 'CORREL' + 'SOLSER'
REPLACE NumEnt WITH NumR 
SELECT (ALI)
set filter to coddep=substr(vcoddep,1,vnumdep)
return vfun


FUNCTION val_DEP
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong , mdist
PRIVATE malias
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
		mdist = 6
	CASE PARAMETERS() = 5
		mdist = 6
ENDCASE
malias  = ALIAS()

SELECT parma
SEEK filtro+mvalor

IF !FOUND() .AND. !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	SET FILTER TO tipo = filtro AND CODIGO = substr(vcoddep,1,vnumdep)
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
		ENDIF
		RETURN
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = parma.codigo
mcuenta= parma.descriau2
mdescr = SUBSTR( parma.descri, 1, mlong )
mdescriaux = SUBSTR( parma.descriaux, 1, mlong)
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable=' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable='D'   && En vista
		RETURN mdescr
	CASE mvariable='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE
