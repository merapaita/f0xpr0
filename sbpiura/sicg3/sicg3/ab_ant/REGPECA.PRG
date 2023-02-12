*--------------------------------------------------------------------------
* RegPec.Prg
* Registra los Pecosas que se emiten en cada dependencia
* Estado del Pecosa:
*   '00' Emitida     ** 	Este es el que se registra en el pecosa
*   '20' Con Correlativo
*   '50' Con O/C
*   '70' Devuelta
*   '99' Anulada
*--------------------------------------------------------------------------
*- Abriendo Archivos

USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE calen    IN 2   order tag calen3   ALIAS calen
USE Pecosa   IN 3   order tag Pecosa1  ALIAS Pecosa
USE ItePec   IN 4   order tag ItePec1  ALIAS Itepec
USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE IteArt   IN 6   order tag IteArt1  ALIAS Iteart
USE cdrnec   IN 7   order tag Cdrnec1  ALIAS cuadro
USE itecn    IN 8   order tag itecn3   ALIAS itecn
USE maepre   IN 10  order tag maepre1  ALIAS maepre
USE itepar   in 11  order tag itepar1  ALIAS ITEPAR          
USE Obra     in 12  order tag obra1    ALIAS obra
USE OrdCom   IN 13  ORDER TAG Ordcom1  ALIAS Orden
PUBLIC TOT
on key label F4 do imprimir

*- Mensajes de aviso al usuario
Vmens01 = ' Pecosas : REVISION '
Vmens02 = 'Registro de Pecosa'
Vmens04 = 'Dicho Pecosa no fue encontrado'
Vmens05 = 'No existe Pecosa anterior'
Vmens06 = 'No existe Pecosa siguiente'
Vmens07 = '¨ Desea Anular ‚ste Pecosa ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Pecosa ha sido anulado'
Vmens10 = 'El Pecosa ya est  Atendido'
Vmens11 = 'El Pecosa ha sido devuelto'
Vmens12 = 'El Pecosa ya tiene O/C'

PUBLIC VANT,VFECLIQ
SELECT Pecosa
GO BOTTOM
vfecliq=date()

ACTIVATE WINDOW STANDBY 
@ 1,1 SAY "Fecha Liquidaci¢n -> " get vfecliq
read
DEACTIVATE WINDOW STANDBY 

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
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
 TITLE ' Detalle: Pecosa         ®F9¯ Detalle : Item         ®F4¯ Imprime ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: Pecosa         ®F7¯ Seguimiento    ' COLOR SCHEME 10
 
 DEFINE WINDOW Wind_4 FROM 20,63 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nular '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
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
 ACTIVATE WINDOW Wind_1
 CLEAR

 @  1, 2 SAY "    Fecha Emisi¢n :"
 @  1,40 SAY "    N£mero Pecosa :"
 @  2, 2 SAY "      Dependencia :"
 @  3, 2 SAY "             Atte :"
 @  3,40 SAY "      Liquidaci¢n :" 
 @  4, 2 SAY " F.Financiamiento :"
 @  4,40 SAY "             Tipo :"
 @  5, 2 SAY "         Programa :"
 @  6, 2 SAY "      Subprograma :"
 @  7, 2 SAY " Activid/Proyecto :"
 @  8, 2 SAY "          Destino :"
 @  9, 2 SAY "    Observaciones :"

 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Pecosa
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 ON KEY LABEL F9 DO vista_det
 SCATTER MEMVAR
 vPartida=m.codcal
 
 @  0,02 SAY IIF(m.TipPec='S','Pecosa Stock','            ')
 @  0,60 SAY vEstPec(m.Estado)
 @  1,22 SAY m.FecPec
 @  1,60 SAY m.periodo
 @  1,63 SAY m.NumPec
 @  2,22 SAY val_para(m.CodDep,'CODDEP','A',22,50,7)
 @  3,22 SAY m.atte PICTURE '@S20'
 @  3,60 SAY m.fecdesp
 @  4,22 SAY Val_Para(m.CodFte,'CODFTE','V',26,20)
 @  4,60 SAY val_para(m.Tipfun,'TIPFUN','A',60,15,2)
 IF alltrim(m.Tipfun)='I' 
	 @  5,22 SAY substr(m.CodCal, 8,2)
	 @  6,22 SAY substr(m.CodCal,10,3)
	 @  6, 2 SAY IIF(ALLTRIM(m.TipFun)='F','        Actividad :','         Proyecto :')
	 @  7,22 SAY substr(m.CodCal,13,3)
     @  7,25 SAY '.'
	 @  7,26 SAY substr(m.CodCal,16,2)
 else
    @  5,22 clear to 7,79
 endif	
 @  8,22 SAY m.destino picture '@S56'
 @  9,22 SAY m.Observa             && PRUEBA

 DO VISTA_HIJO
 
 RETURN
 
PROCEDURE aTOTAL
*--------------
ACTIVATE WINDOW WIND_4
@ 0,0 SAY TOT picture '9,999,999.99'
return
PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_4
IF m.tippec='S'
   @ 0,0 SAY calcula_S() picture '9,999,999.99'
else  
   @ 0,0 SAY calcula() picture '9,999,999.99' 
endif   
return

procedure calcula_S
*---------------- 
select itepec
seek m.periodo+m.numpec+alltrim(m.codfte)
vTotal = 0
scan while itepec.periodo=m.periodo and itepec.numpec=m.numpec and itepec.codfte=alltrim(m.codfte)
     vTotal = vTotal + itepec.cosmed*itepec.candesp*IIF(ITEPEC.ESTADO='50' OR !EMPTY(FECDESP),1,0)
endscan
seek m.periodo+m.numpec+alltrim(m.codfte)
SELECT PECOSA
return vtotal


procedure calcula
*---------------- 
select itepec
seek m.periodo+m.numpec+alltrim(m.codfte)
vTotal = 0
scan while itepec.periodo=m.periodo and itepec.numpec=m.numpec and itepec.codfte=alltrim(m.codfte)
     vTotal = vTotal + itepec.preuni*itepec.candesp*IIF(ITEPEC.ESTADO='50' OR !EMPTY(FECDESP),1,0)
endscan
seek m.periodo+m.numpec+alltrim(m.codfte)
SELECT PECOSA
return vtotal


PROCEDURE VISTA_HIJO
*-------------------
private vest
hide popup all
SELECT ItePec
GO TOP
SEEK m.periodo+m.numpec+alltrim(m.codfte) 
vEst00 = 0
vEst20 = 0
vEst30 = 0
vEst50 = 0
vEst99 = 0
vFte = 0
TOT = 0

SCAN while itepec.periodo+itepec.numpec+itepec.codfte=m.periodo+m.numpec+alltrim(m.codfte) 
     TOT = TOT + PREUNI*CANREQ
      if itepec.CODfte # alltrim(m.codfte)
  		 vfte = vfte + 1	    
      endif 
      do case 
         case itepec.estado='00'
         vEst00 = vEst00 + 1
         case itepec.estado='20'
         vEst20 = vEst20 + 1
         case itepec.estado='30'
         vEst30 = vEst30 + 1
         case itepec.estado='50'
         vEst50 = vEst50 + 1
         case itepec.estado='99'
         vEst99 = vEst99 + 1
      endCAS
 ENDSCAN
 if vfte#0
     do standby with "Cambio de Fuente de Financiamiento..Revise"
 endif    
IF M.ESTADO#'50'
 DO CASE
    CASE vEst00 # 0 
         SELECT PECOSA              
	     if rlock()
            repla pecosa.estado with '00'
         endif   
         UNLOCK
    CASE vEst20 # 0 and vest30=0
         SELECT PECOSA              
	     if rlock()
            repla pecosa.estado with '20'
         endif   
         UNLOCK
    CASE vEst30 # 0 and vest50=0
         SELECT PECOSA              
	     if rlock()
            repla pecosa.estado with '30'
         endif   
         UNLOCK
ENDCASE
ENDIF
DO TOTAL
SELE ITEPEC   && +alltrim(m.codfte)
GO TOP
BROWSE ; 
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.numpec+ALLTRIM(m.Codfte) TIMEOUT 0.0001 ;
   WINDOW Wind_2 ;
   FIELDS;
   CodArt      : H= 'C¢digo' :V=VALART():F:W=EMPTY(CodArt),;
   Descri      : H= 'Descripci¢n' :39 :W=.F.  ,;
   Coduni      : H= 'Uni'      :W=.F. :4 ,;
   Canreq      : H= 'Cantd' :P='99,999.99' :W=!EMPTY(CodArt) ,;
   CC=iif(COSMED#0,cosmed*candesp,PREUNI*candesp)   : h= 'Precio ' :P='999,999.99',;
   ss=iif(!empty(NewFte),Newfte,Pecosa.Codfte)   : H= 'Fte' :P='!!!' ,;
   Ess=IIF( Estado= '00','Pendte',IIF( Estado = '20',"SC"+NumSc,IIF(Estado='99','Anulad',IIF(Estado='50','Atendi',"OC"+NumOc)))) :H='Estado' :W=.F.

   SELE PECOSA
RETURN

PROCEDURE VISTA_det
*------------------
hide popup all
ON KEY LABEL F9
ON KEY LABEL F7 DO Estado WITH 'PE','ItePec.Periodo+Itepec.Numoc+Itepec.Codfte'

SELECT ItePec
GO TOP
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.numpec+ALLTRIM(M.CODFTE) ;
   WINDOW Wind_3 ;
   FIELDS;
   CodArt      : H= 'C¢digo' :V=VALART():F:W=EMPTY(CodArt),;
   Descri      : H= 'Descripci¢n' :39 :W=.F.  ,;
   Coduni      : H= 'Uni'      :W=.F. :4 ,;
   Canreq      : H= 'Cantd' :P='99,999.99' :W=!EMPTY(CodArt) ,;
   CC=Preuni*CANREQ      : h= 'Precio t' :P='999,999.99',;
   ss=iif(!empty(NewFte),Newfte,alltrim(m.Codfte))   : H= 'Fte' :P='!!!' ,;   
   Ess=IIF( Estado= '00','Pendte',IIF( Estado = '20',"SC"+NumSc,IIF(Estado='99','Anulad',IIF(Estado='50','Atendi',"OC"+NumOc)))) :H='Estado' :W=.F.


ON KEY LABEL F9 DO vista_det
ON KEY LABEL F7 
SELECT ITEPEC
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.numpec+ALLTRIM(m.Codfte) TIMEOUT 0.0001 ;
   WINDOW Wind_2 ;
   FIELDS;
   CodArt      : H= 'C¢digo' :V=VALART():F:W=EMPTY(CodArt),;
   Descri      : H= 'Descripci¢n' :42 :W=.F.  ,;
   Coduni      : H= 'Uni'      :W=.F. :4 ,;
   Canreq      : H= 'Cantd' :P='99,999.99' :W=!EMPTY(CodArt) ,;
   CC=Preuni*CANREQ      : h= 'Precio t' :P='999,999.99',;   
   Ess=IIF( Estado= '00','Pendte',IIF( Estado = '20',"SC"+NumSc,IIF(Estado='99','Anulad',IIF(Estado='50','Atendi',"OC"+NumOc)))) :H='Estado' :W=.F.
SELE PECOSA

RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
ON KEY LABEL F7 
ON KEY LABEL F9
SELE PECOSA
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO PERIODO+NUMPEC INTO ITEPEC
 SET SKIP TO ITEPEC
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumPec :H=' N§ ' ,;
 Est = IIF(Itepec.Estado= '00','Pend',IIF(Itepec.Estado = '30',itepec.NumOc,IIF(Estado='99','Anul',IIF(Itepec.Estado='50','Aten','S/Ct')))) :H='Estd' ,;
 Codfte :H='fte ' ,;
 FecPec :H='Fecha' ,;
 Coddep :H='DEP',;
 Itepec.CanReq : H='Cantid':P='99,999.99',;
 itepec.Descri :H='Detalle '
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SELE PECOSA
 SET RELA TO
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
ON KEY LABEL F7 
ON KEY LABEL F9
vtemp    = RECNO()
vPeriodo = RIGHT(DTOC(DATE()),2)
vNum_pec = '     '
vCod_fte = '   '
ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero Pecosa: ' GET vPeriodo PICTURE '!!'
 @ 1,27 SAY '-' GET vNum_Pec PICTURE '!!!!!' VALID vBusca()
 @ 1,33 SAY '-' GET vcod_fte PICTURE '!!!' VALID VAL_PARA(vCod_Fte ,'CODFTE','C',33,20)
READ
DEACTIVATE WINDOW standby

 IF EMPTY(vNum_pec) .or. LASTKEY()=27
 	ON KEY LABEL F9 DO vista_det
    RETURN
 ELSE
    SEEK vPeriodo + vNum_pec + alltrim(vcod_fte)
    IF !FOUND()
       DO standby WITH Vmens04
       GOTO Vtemp
    ELSE
       DO Vista
    ENDIF
 ENDIF
ON KEY LABEL F9 DO vista_det
RETURN

PROCEDURE vBusca
*---------------
vnum_pec=padl(alltrim(vnum_pec),5,'0')
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
ON KEY LABEL F7 
ON KEY LABEL F9
PUBLIC vMes,vPart,Ob
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
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
 IF Estado = '50' and codmay#'X'
   * El pecosa ya tiene O/C
   DO STANDBY WITH Vmens12
   RETURN
 ENDIF
 
 IF ESTADO = '30'
    DO STANDBY WITH 'El pecosa ya tiene O/C' 
	return
 endif	
 
 IF ESTADO = '40'
    DO STANDBY WITH 'El pecosa ya est  despachado '
    return
 endif    

 *****
 SELECT Pecosa
 SCATTER MEMVAR
  ACTIVATE WINDOW Wind_1
 DO PANTALLA
 Ob = .F.
 vCodPrg  = substr(m.codcal,08,2)
 vCodSub  = substr(m.codcal,10,3)
 vProyec  = substr(m.codcal,13,3) 
 vCodact  = substr(m.codcal,13,2) 
 vSubpry  = substr(m.codcal,16,4)
 IF YESNO('¨Es un Pecosa de Stock?')
    m.TipPec = 'S'
 ELSE
    m.TipPec = 'O'
 ENDIF      
 @  1,22 GET m.FecPec    PICTURE '@D'
 @  1,60 GET m.Periodo   PICTURE '!!'  DISABLE
 @  1,63 SAY '-'
 @  1,63 GET m.NumPec    PICTURE '!!!!'  valid VAL_PE()  DISABLE
 @  2,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,40,7) 
 @  3,22 GET m.atte      PICTURE '@S20'   VALID !EMPTY(m.CodDep) 
 @  3,60 GET m.Fecdesp   PICTURE '@D'     
 @  4,22 GET m.CodFte    PICTURE '!!!'  VALID VAL_PARA(m.CodFte ,'CODFTE',' ',22,20) AND SIHAY() DISABLE
 @  4,60 GET m.tipfun    PICTURE '!'    VALID VAL_PARA(m.tipfun,'TIPFUN',' ',60,15,2) 

 @  5,22 GET vCodPrg     PICTURE '!!'   WHEN alltrim(m.tipfun)='I'
 @  6,22 GET vCodSub     PICTURE '!!!'  WHEN alltrim(m.tipfun)='I'
 @  7,22 GET vProyec     PICTURE '!!!'  when m.tipfun='I'
 @  7,26 GET vSubPry     PICTURE '!!'   when !EMPTY(vProyec) and m.tipfun='I'
 @  8,28 GET m.destino   picture '@S50' 
 @  9,22 GET m.Observa   picture '@S40' 
 READ
    
    IF LASTKEY() # 27 && Si se presion¢ Esc, no graba
       DO WHILE .T.
          Ok = Trabaja_Hijo()
          IF LASTKEY() # 27
             IF YESNO('¨ Conforme la correcci¢n ?')
                EXIT
             ENDIF
          ELSE
             IF YESNO('¨ Cancela la correcci¢n ?')
                ok = .F.
                EXIT
             ENDIF
          ENDIF
       ENDDO
       ** verifica si estan correctos
       m.codcal = m.periodo+padl(alltrim(str(month(m.fecpec),2)),2,'0') +ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+alltrim(vProyec)+alltrim(vSubPry)
       SELECT Itepec
       SEEK m.periodo+m.numpec +alltrim(m.Codfte)
	   SCAN WHILE m.Periodo = Periodo and m.Numpec = Numpec  AND  CODFTE = alltrim(m.Codfte)
		    IF RLOCK()
		      IF EMPTY(CODART) OR EMPTY(DESCRI) or  canreq=0
	  			 DELETE NEXT 1
    		  ELSE
		         REPLACE CodCal  WITH m.CodCal , TipPec with m.TipPec , Codfte with m.CodFte
		      ENDIF
		    ENDIF
       ENDSCAN

       IF Ok .AND. LASTKEY() # 27
          SELECT Pecosa
		  m.user = sys(0)
          m.user_fc = date()
          m.user_TP = 'C'
          
          GATHER MEMVAR
          *- Aumento el correlativo del pago
       ELSE
          SELECT Itepec
       ENDIF
    ELSE
       *- No pudo agregar la liquidaci¢n
       SELECT Pecosa
    ENDIF
 DO Vista                    && Muestra nuevos datos
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
ON KEY LABEL F7 
ON KEY LABEL F9
PRIVATE VTEMP
PUBLIC vMes,vPart,vTipFun,Ob
 SELECT Parma
 SEEK 'CORREL'+'PECOSA'
 SELECT Pecosa
 VTEMP = RECNO()
 DO Pantalla
 SCATTER MEMVAR BLANK
 m.Numpec = '    '
 vCodPrg  = space(2)  
 vCodSub  = space(3)    
 vProyec  = space(3) 
 vSubpry  = space(2)
 vCodAct  = space(3)
 m.Fecpec  = DATE()
 m.FecDesp = vFecliq
 m.periodo = STR(YEAR(DATE())-1900,2)
 Ob = .F.
 IF YESNO('¨Es un Pecosa de Stock?')
    m.TipPec = 'S'
 ELSE
    m.TipPec = 'O'
 ENDIF      

 @  1,22 GET m.FecPec    PICTURE '@D'
 @  1,60 GET m.Periodo   PICTURE '!!'
 @  1,63 SAY '-'
 @  1,63 GET m.NumPec    PICTURE '!!!!'  valid VAL_PE() 
 @  2,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,40,7) 
 @  3,22 GET m.atte      PICTURE '@S20'   VALID !EMPTY(m.CodDep) 
 @  3,60 GET m.Fecdesp   PICTURE '@D'      
 @  4,22 GET m.CodFte    PICTURE '!!!'  VALID VAL_PARA(m.CodFte ,'CODFTE',' ',22,20) AND SIHAY()
 @  4,60 GET m.tipfun    PICTURE '!'    VALID VAL_PARA(m.tipfun,'TIPFUN',' ',60,15,2) 

 @  5,22 GET vCodPrg     PICTURE '!!'   WHEN alltrim(m.tipfun)='I'
 @  6,22 GET vCodSub     PICTURE '!!!'  WHEN alltrim(m.tipfun)='I'
 @  7,22 GET vProyec     PICTURE '!!!'  when m.tipfun='I'
 @  7,26 GET vSubPry     PICTURE '!!'   when !EMPTY(vProyec) and m.tipfun='I'
 @  8,28 GET m.destino   picture '@S50' 
 @  9,22 GET m.Observa   picture '@S40' 

 READ VALID VAL_READ()

 IF LASTKEY() # 27 
    m.codcal = m.periodo+padl(alltrim(str(month(m.fecpec),2)),2,'0') +ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+alltrim(vProyec)+alltrim(vSubPry)
    m.fecemi = date()
    SELECT Itepec
    DO Agreg_item
    DO WHILE .T.
       Ok = Trabaja_Hijo()
       IF LASTKEY() # 27
          IF YESNO('¨ Confirme el ingreso ?')
             EXIT
          ENDIF
       ELSE
          DO STANDBY WITH ' Cancelado el Ingreso ..'
          ok = .F.
          EXIT
       ENDIF
    ENDDO
       
    * verifica si estan correctos

    SELECT Itepec
    SEEK m.periodo + m.numpec + alltrim(m.codfte)
    SCAN WHILE m.Periodo = Periodo and m.Numpec = Numpec and alltrim(m.codfte)=codfte
	     IF RLOCK()
		    IF EMPTY(CODART) OR EMPTY(DESCRI) or canreq=0
	  	       DELETE NEXT 1
    		ELSE
		       REPLACE CodCal  WITH m.CodCal, TipPec with m.tippec, ESTADO WITH '50' ,candesp with canreq, fecdesp with m.fecdesp
		    ENDIF
		 ENDIF
	ENDSCAN
  
    IF Ok .AND. LASTKEY() # 27
       SELECT Pecosa
       if !empty(m.Numpec)
		   m.user = sys(0)
  		   m.user_fc = date()
	       m.user_TP = 'I'
	       m.codmay = 'X'
	       m.estado='50'
	       IF F_APPD()
              GATHER MEMVAR
           ENDIF 
        endif
    ELSE
        SELECT Itepec
        SEEK m.periodo+m.numpec + alltrim(m.codfte)
        SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec and alltrim(codfte)= codfte
             IF RLOCK()
                DELETE NEXT 1
             ENDIF
        ENDSCAN
        GO VTEMP
    ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
   GO VTEMP
 ENDIF
 UNLOCK ALL
 SELECT Pecosa
 DO Vista
 RETURN

procedure af_proy
*---------------
OB = IIF(alltrim(m.tipfun)='I',Iif(yesno('Ingresar Obra?'),.t.,.f.),.f.)
IF OB
    @  8,22 GET m.TipObr    PICTURE '!'    VALID VAL_para(m.tipobr ,'TIPOBR','C',26,40) WHEN ALLTRIM(M.TIPFUN)='I' 
    @  8,24 GET m.Codobra   PICTURE '!!!'  VALID VAL_OBRA(m.CodObra,m.periodo+Alltrim(vCodprg)+alltrim(vcodsub)+alltrim(vProyec)+alltrim(vSubPry),'m.CodObra',' ',22,40) WHEN ALLTRIM(M.TIPFUN)='I' 
    READ
endif    
SHOW GETS
Return       


PROCEDURE VAL_PE
*---------------
m.NumPec = Padl(alltrim(m.NumPec),4,'0')
return

PROCEDURE SIHAY
*--------------
SEEK m.periodo+m.NumPec +alltrim(m.Codfte)
if found()
   do standby with 'El Pecosa ya esta registrado'
   return .f.
endif   
return

PROCEDURE VALATTE
*----------------
SELECT Cuadro
SEEK  m.Periodo+m.Coddep

if found()
   m.atte = IIF(EMPTY(m.atte) or m.atte='No se Registra Solicitante..,revise',Cuadro.Atte,m.atte)
else
   m.atte = 'No se Registra Solicitante..,revise'
endif

SELECT Pecosa
return

PROCEDURE Trabaja_Hijo
*---------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELE ITEPEC
 SEEK m.periodo+m.numpec +alltrim(m.codfte)  &&quitar luego 

 IF !FOUND()
     DO Agreg_item
 ENDIF

 IF m.TipPec = 'S'
    BROWSE NOREFRESH NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY m.periodo+m.numpec+alltrim(m.codfte) FIELD ;
     CodArt      : H= 'C¢digo' :V=VAL_ARTC(codArt) AND antr() :F ,;
     Descri      : H= 'Descripci¢n' :40 :W=.F.,;
     Coduni      : H= 'Uni' :W=.F. :4 ,;
     Canreq      : H= 'Cantd' :P='99,999.99' :W=!EMPTY(CodArt) ,;
     costot      : H= 'Total' :P='999,999.99' :v=divide():f,;
     cosmed      : h= 'Precio ' :P='999,999.999' :v=MULTIPLI():f
 ELSE
     BROWSE NOREFRESH NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY m.periodo+m.numpec+alltrim(m.codfte) FIELD ;
     CodArt      : H= 'C¢digo' :V=VAL_ARTC(codArt) AND antr() :F ,;
     Descri      : H= 'Descripci¢n' :40 :W=.F.,;
     Coduni      : H= 'Uni' :W=.F. :4 ,;
     Canreq      : H= 'Cantd' :P='99,999.99' :W=!EMPTY(CodArt) ,;
     costot      : H= 'Total' :P='999,999.99' :v=divide():f,;
     preuni      : h= 'Precio ' :P='999,999.999' :v=MULTIPLI():f
 ENDIF     
     
UNLOCK ALL

 ON KEY LABEL F5
 ON KEY LABEL F8
 ON KEY LABEL F10

 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Pecosa

 RETURN .T.

procedure antr
*-------------
vAnt = itepec.Canreq
return .t.

PROCEDURE Valor
*--------------
sele itecn
seek m.Periodo+alltrim(m.Coddep)+ALLTRIM(ITEPEC.CodArt)
vPer = Trimestre(m.Fecpec)
vDispon = Itecn.Nec_&vPer - Itecn.Ped_&vPer

If Itepec.Canreq > vDispon + vant
   do standby with IIF(vdispon+vant=0,'Ya no Existe Art¡culos para este Trimestre','Ud. s¢lo puede pedir m ximo '+ALLTRIM(str(vdispon+VANT,5))+' para este Trimestre')
*  sele itepec
*  return .f.
* else
endif
  if rlock()
     replace Ped_&vper with Ped_&vper + ItePec.Canreq - vant
  endif
*endif
sele itepec
return .t.


PROCEDURE Anula
*---------------
 SELECT Pecosa
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR ‚ste Pecosa ?')
 vEstado = .t.
 SELECT Itepec
 SEEK m.periodo+m.numpec + alltrim(m.codfte)
 SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec and alltrim(codfte)= codfte
      IF ESTADO='30' or estado='20' 
         DO STANDBY WITH 'La Pecosa ya tiene generada O/C o S/C,no se puede anular'
         vEstado = .f.            
         EXIT
 	 ENDIF
 ENDSCAN
 if vestado
    IF velimina 
	    SELECT Itepec
	    SEEK m.periodo+m.numpec + alltrim(m.codfte)
	    SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec and alltrim(codfte)= codfte
		 if rlock()
		    replace estado with '99' 
         endif
	    endscan
   		sele pecosa
	    REPLACE ESTADO WITH '99' ,FECVER WITH DATE()
    endif
 endif   
 sele pecosa    
 DO Vista
 
 UNLOCK
 RETURN

PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON
SELECT Pecosa
VCON = RECNO()
SCATTER MEMVAR
vNumPec = m.Periodo+m.NumPec +alltrim(m.Codfte)
SET RELATION TO periodo + NUMpec+CODFTE INTO ITEpec
set filt to Periodo+Numpec+Codfte=vNumpec
SET SKIP TO ITEpec
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"LisPec1",' Pe.co.sa ',2
endif
set skip to
set filter to
SET RELATION TO 
SELECT PECOSA
GO VCON
DO VISTA
RETURN


PROCEDURE Agreg_Item
*-----------------
IF F_appd()
  REPLACE Periodo WITH m.Periodo ,;
          NumPec  WITH m.NumPec ,;
          Estado  WITH '00' ,;
          CodCal  WITH m.CodCal ,;
		  CodFte  with m.Codfte ,;	
          Tipfun  WITH m.Tipfun
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Elimi_Item
*-------------------
SELECT ItePec
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif
return

PROCEDURE Corri_Item
*-----------------
REPLACE CodCal  WITH m.CodCal
RETURN .T.

PROCEDURE LISTA
*--------------
ON KEY LABEL F7 
ON KEY LABEL F9
select PECOSA
vtemp =recno()
SET RELATION TO PERIODO+NUMPEC+CODFTE INTO ITEPEC
SET SKIP TO ITEPEC
if eof()
   do standby with vmens08
   return
else
   DO LISPEC
endif
select PECOSA
SET RELATION TO
SET FILT TO
DO VISTA
RETURN

PROCEDURE LisPec
*---------------
vOrde = ORDER()
vrec  = RECNO()
DEFINE WINDOW LIS FROM 0,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado Pecosas' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vToPec,vToMes,vToFue,vTodep,vOrden,vtiplis,vTipRep
vNumpec = SPAC(4)
vFte    = SPAC(3)
vCodmes = SPAC(2)
vPeriod = SPAC(2)
vCoddep = SPAC(6)
vCodFte = SPAC(3)

@ 01,01 SAY "Todas las Pecosas : " GET vToPec  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToPec,2,22)  and assig()
@ 02,01 SAY "           Pecosa : "
@ 02,22 GET vNumpec  PICTURE '!!!!' WHEN vToPec = 2  &&VALID Padl(alltrim(vNumpec),4,'0')
@ 02,27 get vPeriod  Picture '!!'   WHEN vToPec = 2 
@ 02,30 SAY '-'
@ 02,31 GET vFte     PICTURE '!!!' VALID VAL_PARA(vfte,'CODFTE','C') AND ValPecO() WHEN vToPec = 2

@ 04,01 SAY "  Todos las Meses : " GET vTomes  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTomes,5,22)  WHEN vToPec = 1
@ 05,01 SAY "              Mes : "
@ 05,22 GET vCodmes  PICTURE '!!'  VALID VAL_PARA(vCodMes,'FECMES','C') WHEN vToPec = 1 AND vTomes = 2

@ 07,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTofue,8,22)  WHEN vToPec = 1
@ 08,01 SAY "           Fuente : "
@ 08,22 GET vCodFte  PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE','C')  WHEN vToPec = 1 and vTofue =2

@ 10,01 SAY "Todas las Dependc : " GET vToDep  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTodep,11,22)  WHEN vToPec = 1
@ 11,01 SAY "      Dependencia : "
@ 11,22 GET vCodDep  PICTURE '!!!!!!' VALID VAL_PARA(vCodDep,'CODDEP','C')  WHEN vToPec = 1 and vTodep =2

@ 13,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Dependencia;Emision'  WHEN vToPec = 1

@ 16,01 SAY "           Estado : " GET vTipLis  FUNCTION '^ Todos;Pendientes;Atendidos;Liquidados'  WHEN vToPec = 1

@ 19,01 SAY "           Estado : " GET vTipRep  FUNCTION '^ Resumido;Detallado'  WHEN vToPec = 1

@ 22,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1 AND LASTKEY()#27
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,Periodo+NumPec,IIF(vOrden=2,CodDep,DTOS(FECemi))) TO (vInd) ;
      FOR IIF(vToPec=1,.T.,Periodo+NumPec+CodFte = vPeriod+vNumpec+vFte) .AND. IIF(vTipLis=1,.T.,iif(vTipLis=2,Estado = '00',iif(vTipLis=3,Estado = '40',Estado='50')))
   SET FILTER TO iif(vToMes=1,.t.,IIF(vTipLis=4,month(fecdesp),month(fecpec))=val(vCodMes)) and iif(vToMes=1,.t.,month(fecpec)=val(vCodMes)) AND iif(vToFue=1,.t.,Codfte=alltrim(vCodfte)) and ;
		 		 iif(vToDep=1,.t.,CodDep=alltrim(vCodDep)) 
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipLis=1,' en General ',IIF(vTipLis=2,' Pendientes ',' Atendidos '))
   IF !EOF()
      if vtopec = 1
         if vTiprep = 1
	         do reporte with 2,"LisPecX",' Pe.co.sa ',1,.F.,.T.
	     else
	         do reporte with 2,"LisPec",' Pe.co.sa ',1,.F.,.T.
	     endif    
	  else
	     do reporte with 2,"LisPec1",' Pe.co.sa '
	  endif   
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILT TO
   CLOSE INDEX
   ERASE (VIND)
ENDIF
SELECT PECOSA
SET ORDE TO (VORDE)
GO TOP
GO VREC
RETURN

PROCEDURE ASSIG
*--------------
vNumpec = PECOSA.NUMPEC
vPeriod = pecosa.PERIODO
vfte    = pecosa.codfte
RETURN  .T.


PROCEDURE VALPECO              && Revisi¢n de BD en browse
*--------------
SELE PECOSA
IF EOF()
    DO standby WITH Vmens08
    RETURN
ENDIF
vNumpec=Padl(alltrim(vNumpec),4,'0')
SEEK '95'+vNumPec +alltrim(vFte)
IF !FOUND()
     SET FILTER TO Codfte = alltrim(vFte)	
	 SET RELATION TO PERIODO+NUMPEC INTO ITEPEC
	 SET SKIP TO ITEPEC
	 vtemp = RECNO()
	 HIDE MENU mMenu
	 ACTIVATE SCREEN
	 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	 DO Logos WITH Rotulo1,vTempo
	 ON KEY LABEL F10 KEYBOARD CHR(23)
	 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	 NumPec :H=' N§ ' ,;
	 Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
	 Codcal :H='Calendario' ,;
	 FecPec :H='Fecha' ,;
	 Coddep :H='DEP',;
	 Itepec.CanReq : H='Cantidad':P='99,999',;
	 itepec.Descri :H='Detalle '
	 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	 DO Logos WITH Rotulo1,vTempo
	 SELE PECOSA
	 SET RELATION TO
	 set filter to
ENDIF
vNumpec = Numpec
vFte    = Codfte
SHOW MENU mMenu
ON KEY LABEL F10
SELE PECOSA
RETURN

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  ON KEY LABEL F7 
  ON KEY LABEL F9
  ON KEY LABEL F4  
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  ON KEY LABEL F7 
  ON KEY LABEL F9
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    WINDOW wind_4
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

function valpec
*-----------------
parameter vnumpec,alis
alis = alias()
private vfun
vfun = .t.
m.numpec=padl(alltrim(str(vnumpec,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif
SELECT Parma
SEEK 'CORREL' + 'PECOSA'
REPLACE NumEnt WITH NumEnt + 1
select (alis)
return vfun

function valult
*--------------
parameter vrec
vrec = recno()
select pecosa
set orde to pecosa1
go bott
vnumpec = val(numpec)+1
m.numpec=padl(alltrim(str(vnumpec,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif

function repasa
*--------------
parameter vrec,vfun
vfun = .t.
vrec = recno()
select pecosa
set orde to pecosa1
go 328
numr = 482
do while .t.
   if val(numpec)=numr
      numr = numr + 1
      skip
      loop
   else 
      exit   
   endif   
enddo

m.numpec=padl(alltrim(str(numr,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif

SELECT Parma
SEEK 'CORREL' + 'PECOSA'
REPLACE NumEnt WITH NumR 

return vfun




FUNCTION VALART
*--------------
PARAMETERS _Cod
DC=ALIAS()
PRIVATE XX,YY,ZZ,vFun
vFun = .F.
SELEC ITEART
vTemp = RECNO()
SEEK 'B'+ALLTRIM(ITEPEC.CODART)
IF FOUND() AND !EMPTY(ITEPEC.CODART)
   SELECT Itecn
   seek m.Periodo+alltrim(m.Coddep)+itepec.Codart
   if found()
      SELECT itepec
         IF RLOCK()
          REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri,;
          TIPCDR  WITH 'S'
          ENDIF
      vFun = .T.
   else
      do standby with 'Este producto no est  registrado en el Cuadro de Necesidades'
      SELE ITEPEC
      IF RLOCK()
        *REPLACE ITEPEC.CodArt  WITH spac(11) && CAMBIAR PARA VALIDAR LOS PROD. QUE NO ESTAN EN CN
         REPLACE TIPCDR WITH 'N'
      ENDIF
      vFun = .T.
  endif
ELSE
   SELE itepec
   ZZ=VAL_PARA(CodArt,'CODGEB','C')
   IF LASTKEY()=27
      RETURN .T.
   ENDIF
   IF ZZ
      XX = Val_Art(_Cod,.F.)
      IF XX
         YY = Val_ArtDet(SUBSTR(Alltrim(PRODU.CODART),2,6),.F.)
         IF YY
            SELECT Itecn
            seek m.Periodo+alltrim(m.Coddep)+itepec.Codart
            if !found()
               do standby with 'Este producto no est  registrado en el Cuadro de Necesidades'
               SELE ITEPEC
               if Rlock()
                   REPLACE ;
                   coduni  WITH Iteart.coduni,;
                   preuni  WITH Iteart.preuni,;
                   descri  WITH Iteart.descri,;
                   TIPCDR WITH 'N'
               endif
              vFun = .T.
            else
               SELECT Itepec
               if rlock()
                  REPLACE ;
                   coduni  WITH Iteart.coduni,;
                   preuni  WITH Iteart.preuni,;
                   descri  WITH Iteart.descri,;
                   TIPCDR WITH 'S'
               endif
               vFun = .T.
            endif
         ELSE
           if f_lock(1)
               REPLACE ITEPEC.codart  WITH spac(11)
           endif
           vFun = .F.
        ENDIF
      ELSE
        if f_lock(1)
            REPLACE ITEPEC.codart  WITH spac(11)
        endif
        vFun = .F.
      ENDIF
   ELSE
      if f_lock(1)
        REPLACE ITEPEC.codart  WITH spac(11)
      endif
      vFun = .F.
   ENDIF
ENDIF
ON KEY
ON KEY LABEL F5  DO Agreg_item
ON KEY LABEL F8  DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELEC itepec

RETURN vFun

PROCEDURE Trimestre
*------------------
parameter vFecha

DO CASE
   CASE MONTH(vFecha) = 1  OR MONTH(vFecha) = 2   OR  MONTH(vFecha) = 3
        vTrim = '1'
   CASE MONTH(vFecha) = 4  OR MONTH(vFecha) = 5   OR  MONTH(vFecha) = 6
        vTrim = '2'
   CASE MONTH(vFecha) = 7  OR MONTH(vFecha) = 8   OR  MONTH(vFecha) = 9
        vTrim = '3'
   CASE MONTH(vFecha) = 10 OR MONTH(vFecha) = 11  OR  MONTH(vFecha) = 12
        vTrim = '4'
ENDCASE
return vtrim

FUNCTION XVal_ArtC  && Articulos
*------------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo
_campo = VARREAD()
ORD=ORDER()
mAlias = ALIAS()
SELECT IteArt
GO TOP
_OldWnd = WOUTPUT()
v_Fun=.f.
IF !mEdita
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
ELSE
   IF EMPTY(xcod)
      SET ORDER TO 2
      ACTIVATE SCREEN
      ON KEY LABEL F10 KEYBOARD CHR(23)
      ON KEY LABEL F8 DO BorrDet
      ON KEY LABEL F5 DO Agr2Det
      ON KEY LABEL F2 DO FunBusDet
      DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
      BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
        CodArt   :H='C¢digo' :W=.F. ,;
        Descri   :H='Nombre':70  ,;
        CodUni   :H='Unidad':7   
      vORD = RECNO()  
      GO TOP
      SCAN WHILE EMPTY(DESCRI)
           IF RLOCK()
              DELETE NEXT 1
           ENDIF    
      ENDSCAN  
      GO TOP
      GO vord
      ON KEY LABEL F10
      ON KEY LABEL F8
      ON KEY LABEL F5
      ON KEY LABEL F2
      RELEASE WINDOW _BusArt
      SET ORDER TO 1
      
      IF Lastkey()=27
         V_FUN = .f.
      ELSE
         xcod = CodArt
         IF mMsg
            @ _x,_y SAY Descri
         ENDIF
         SELECT (mAlias)
         IF !_tipo
            REPLACE &_campo WITH  xcod
         ENDIF
         v_fun = .T.
      ENDIF
   ENDIF
ENDIf
IF v_Fun
   SELECT itePEC
   IF RLOCK()
      REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri
      IF ITEART.CODART='00.000.000'
          REPLACE canreq with -1
      endif    
   ENDIF
endif
SELECT (mAlias)
SET ORDE TO (ORD)
ON KEY LABEL F5  DO Agreg_item
ON KEY LABEL F8  DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
RETURN v_fun

FUNCTION Val_ArtC  && Articulos
*------------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo
_campo = VARREAD()
ORD=ORDER()
mAlias = ALIAS()
SELECT IteArt
GO TOP
_OldWnd = WOUTPUT()
v_Fun=.f.
v_ent =.f.

IF !mEdita
   SET ORDER TO ITEART3
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
   v_ent = found()
ENDI
   IF EMPTY(xcod) OR !V_ENT
      SET ORDER TO ITEART2
      GO TOP
      ACTIVATE SCREEN
      ON KEY LABEL F10 KEYBOARD CHR(23)
      ON KEY LABEL F2 DO FunBusDet
      DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
      BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
        CodArt   :H='C¢digo'    :w=.f. ,;
        Descri   :H='Nombre':70   ,;
        CodUni   :H='Unidad':7   
      ON KEY LABEL F10
      ON KEY LABEL F2
      RELEASE WINDOW _BusArt
      SET ORDER TO 1

      IF Lastkey()=27
         V_FUN = .f.
         v_ent = .f.
      ELSE
         xcod = CodArt
         xDes = Descri
         xuni = coduni
         IF mMsg
            @ _x,_y SAY Descri
         ENDIF
         SELECT (mAlias)
         IF !_tipo
            REPLACE &_campo WITH  xcod &&,itepec.descri with xdes ,itepec.coduni with xuni
         ENDIF
         v_fun = .T.
         v_ent = .t.
      ENDIF
   ENDIF
*ENDIF
SELECT itepec
IF RLOCK()
   REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri

ENDIF

SELECT (mAlias)
SET ORDE TO (ORD)
ON KEY LABEL F5  DO Agreg_item
ON KEY LABEL F8  DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
IF !V_ENT
   RETURN v_FUN
ELSE
   RETURN v_ENT
ENDIF   

PROCEDURE DIVIDE
*---------------
IF m.TipPec = 'S'
   REPLACE COSMED WITH costot/canREQ
ELSE   
   REPLACE PREUNI WITH costot/canREQ
ENDIF
RETURN .T.   

PROCEDURE MULTIPLI
*-----------------
IF m.TipPec = 'S'
   REPLACE COSTOT WITH cosmed*canREQ
ELSE   
   REPLACE COSTOT WITH preuni*canREQ
ENDIF
RETURN .T.   
