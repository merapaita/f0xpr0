*--------------------------------------------------------------------------
* AdjCot.Prg
* Adjudica Solicitudes de Cotizaciones a Proveedor
* Estado de la cotizacion:
*   '00' Emitida   Este es el que se registra en el Solicitud de Cotizaci¢n
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: Luis Castillo Dur n
*--------------------------------------------------------------------------
*- Abriendo Archivos
PARAMETER vOpcion
SET EXCL OFF
USE Parmae   IN 1   order tag  Parmae1  ALIAS Parma
IF vOpcion = 1
   USE Solcot   IN 2   order tag Solcot1 ALIAS Solcot
   USE AuxCot   IN 7   order tag Auxcot1 ALIAS Auxcot
ELSE
   USE Solcot   IN 2   order tag SolCot2 ALIAS Solcot
   USE AuxCot   IN 7   order tag Auxcot2 ALIAS Auxcot
ENDIF
USE Itesc    IN 3   order tag  Itesc1   ALIAS Itesc
USE Artmae   IN 4   order tag  Artmae1  ALIAS Produ
USE Itepec   IN 5   order tag  ItePec2  ALIAS Itepec
USE Promae   IN 6   order tag  Promae1  ALIAS Promae
USE AuxCot   IN 7   order tag  Auxcot1  ALIAS Auxcot
USE Solser   IN 8   order tag  Solser2  ALIAS Solser
USE USUARIO  IN 0  ORDER TAG USUARIO1 ALIAS USU
SET MEMO TO 112
Public just1,just2,just3,just4,just5,just6,Obs1,Obs2,Obs3,Obs4,Obs5,Obs6
*STORE ' ' TO just1,just2,just3,just4,just5,just6,Obs1,Obs2,Obs3,Obs4,Obs5,Obs6

*- Mensajes de aviso al usuario
Vmens01 = ' Cuadro Comparativo de Cotizaciones: REGISTRO '
Vmens02 = ' Registro del Cuadro Comparativo de Cotizaciones '
Vmens04 = 'Dicho Solicitud de Cotizaci¢n no fue encontrada'
Vmens05 = 'No existe Solicitud de Cotizaci¢n anterior'
Vmens06 = 'No existe Solicitud de Cotizaci¢n siguiente'
Vmens07 = '¨ Desea ELIMINAR ‚ste Cuadro Comparativo de Cotizaciones ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta Solicitud de Cotizaci¢n ha sido anulada'
Vmens10 = 'La Solicitud de Cotizaci¢n ya fue atendida'
Vmens11 = 'La Solicitud de Cotizaci¢n ha sido devuelta'

SELECT Solcot
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
ON KEY LABEL F5 DO VISTA_PROV
*- Inicia proceso
HIDE POPUP ALL
DO Inicia                    && Define ventanas, men£s, t¡tulos
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Precios Netos ' ;
 COLOR SCHEME 10

 DEFINE WINDOW Wind_9 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' ®Cuadro Comparativo de Cotizaci¢nes¯       ®F7¯ Justificaci¢n/Obs ' ;
 COLOR SCHEME 10

 DEFINE WINDOW Wind_8 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' ®Cuadro Comparativo de Cotiz.¯     ®F7¯ Justificaci¢n,Observaci¢n' ;
 COLOR SCHEME 10

 DEFINE WINDOW jusobs FROM 10,02 TO 15,77  DOUBLE ;
 TITLE ' Acta de Otorgamiento  '

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_1
 CLEAR

 @  1, 2 SAY " N£mero Solicitud : "
 @  1,40 SAY "            Fecha : "

RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Solcot
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 SCATTER MEMVAR

 @ 0,60 SAY IIF( m.Estado= '00','Pendiente',IIF( m.Estado = '20','CON COTIZ.',IIF(m.Estado='99','Anulada ',IIF(m.Estado='50','Atendido','        '))))
 @ 1,22 SAY m.NumSc
 @ 1,60 SAY m.FecSc
 @  2,40 SAY iif(empty(m.numccc),'                   ','Cuadro Comparativo:')
 @  2,60 SAY iif(empty(m.numccc),'    ',m.numccc)

 DO PROVEEDOR
 DO VISTA_HIJO
 RETURN


PROCEDURE VISTA_HIJO
*-------------------
HIDE POPUP ALL
SELECT IteSc
IF vOpcion = 1
   BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo + m.numsc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   CodArt      : H= 'C¢digo' :W=.F. ,;
   NUMORD      : H= 'OR' :W=.F. ,;
   NUMPEC      : H= 'Pecs':W=.F. ,;
   DesPed      : H= 'Descripci¢n' :40 :W=.F. ,;
   UniMed      : H= 'UniMd'  :W=.F. :4 ,;
   Cansol      : H= 'Cant' :P='9,999.999':W=.F. ,;
   CodPrvX     : H= 'Prov' :W=.f.
else
   BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo + m.numsc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   ZZ=MLINE(DeTALLE,1)  : H= 'Descripci¢n' :70 :W=.F. ,;
   CodPrvX     : H= 'Prov' :W=.f.
endif
SELE SOLCOT
RETURN


PROCEDURE VISTA_PROV
*-------------------
 do case
    case estado = '70'
         DO standby with ' La Solicitud ha sido Devuelta '
         return
    case estado = '99'
         DO standby with ' La Solicitud est  Anulada '
         return
 endcase
hide popup all
SELECT IteSc
IF vOpcion=1
	BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOEDIT KEY m.Periodo + m.numsc  ;
	   WINDOW Wind_9 ;
	   FIELDS ;
	   CodArt      : H= 'C¢digo' ,;
	   DesPed      : H= 'Descripci¢n' :46 ,;
	   UniMed      : H= 'UniMd'  :5 ,;
	   Cansol      : H= 'Cant' :P='9,999.999' ,;
	   CodPrvX     : H= 'Prov' :W=.f.,;
	   A='³' :H='*' :W=.F.,;
	   PreUni1     : H= 'Unitario1'  :P='9,999,999.999',;
	   Valtot1     : H= 'Total 1'  :P='9,999,999.99',;
	   B='³' :H='*' :W=.F.,;
	   PreUni2     : H= 'Unitario2'  :P='9,999,999.999',;
	   ValTot2     : H= 'Total 2'  :P='9,999,999.999',;
	   C='³' :H='*' :W=.F.,;
	   PreUni3     : H= 'Unitario3'  :P='9,999,999.999',;
	   ValTot3     : H= 'Total 3'  :P='9,999,999.999',;
	   D='³' :H='*' :W=.F.,;
	   PreUni4     : H= 'Unitario4'  :P='9,999,999.999',;
	   ValTot4     : H= 'Total 4'  :P='9,999,999.999',;
	   E='³' :H='*' :W=.F.,;
	   PreUni5     : H= 'Unitario5'  :P='9,999,999.999',;
	   ValTot5     : H= 'Total 5'  :P='9,999,999.999',;
	   F='³' :H='*' :W=.F.,;
	   PreUni6     : H= 'Unitario6'  :P='9,999,999.999',;
	   Valtot6     : H= 'Total 6'  :P='9,999,999.999'
ELSE
	BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOEDIT KEY m.Periodo + m.numsc  ;
	   WINDOW Wind_9 ;
	   FIELDS ;
	   CodArt      : H= 'C¢digo' ,;
	   DesSC       : H= 'Descripci¢n' :46 ,;
	   UniMed      : H= 'UniMd'  :5 ,;
	   Cansol      : H= 'Cant' :P='9,999.999' ,;
	   CodPrvX     : H= 'Prov' :W=.f.,;
	   A='³' :H='*' :W=.F.,;
	   PreUni1     : H= 'Unitario1'  :P='9,999,999.999',;
	   Valtot1     : H= 'Total 1'  :P='9,999,999.99',;
	   B='³' :H='*' :W=.F.,;
	   PreUni2     : H= 'Unitario2'  :P='9,999,999.999',;
	   ValTot2     : H= 'Total 2'  :P='9,999,999.99',;
	   C='³' :H='*' :W=.F.,;
	   PreUni3     : H= 'Unitario3'  :P='9,999,999.999',;
	   ValTot3     : H= 'Total 3'  :P='9,999,999.99',;
	   D='³' :H='*' :W=.F.,;
	   PreUni4     : H= 'Unitario4'  :P='9,999,999.999',;
	   ValTot4     : H= 'Total 4'  :P='9,999,999.99',;
	   E='³' :H='*' :W=.F.,;
	   PreUni5     : H= 'Unitario5'  :P='9,999,999.999',;
	   ValTot5     : H= 'Total 5'  :P='9,999,999.99',;
	   F='³' :H='*' :W=.F.,;
	   PreUni6     : H= 'Unitario6'  :P='9,999,999.999',;
	   Valtot6     : H= 'Total 6'  :P='9,999,999.99'
ENDIF

SELE SOLCOT
DO VISTA
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE SOLCOT
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO PERIODO+NUMSC INTO ITESC
 SET SKIP TO ITESC
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 IF vOpcion=1
 	BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	 NumSc :H=' N§ ' ,;
	 FecSc :H='Fecha' ,;
	 gg=iif(empty(itesc.codprvx),'....',itesc.codprvx) :H='PROVE' ,;
	 hh=iif(empty(itesc.codprvx),'....',itesc.preunix) :H='PreUni' :P='9,999,999.999',;
	 itesc.Cansol :H='Cantid' :P='9,999.999',;
	 itesc.desped :H='Articulo ' :35 ,;
	 itesc.unimed :H='Unid'
 ELSE
 	BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	 NumSc :H=' N§ ' ,;
	 FecSc :H='Fecha' ,;
	 gg=iif(empty(itesc.codprvx),'....',itesc.codprvx) :H='PROVE' ,;
	 hh=iif(empty(itesc.codprvx),'....',itesc.preunix) :H='PreUni' :P='9,999,999.999',;
	 QQ=MLINE(itesc.deTALLE,1)  :H='Servicio ' :56 
 ENDIF


 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 SET RELA TO
 ON KEY LABEL F10
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp    = RECNO()
 vPeriodo = RIGHT(DTOC(DATE()),2)
 vNum_sc  = 0
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero Solicitud : ' GET vPeriodo PICTURE '!!'
 @ 1,32 SAY '-' GET vNum_sc PICTURE '9999' VALID vBusca()
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_sc) .or. LASTKEY()=27
    RETURN
 ELSE
   SEEK  vPeriodo + vNum_sc
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
vNum_sc=Padl(alltrim(str(vNum_sc,4)),4,'0')
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
    DO Pantalla
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
   DO Pantalla
   DO Vista
 ENDIF
 RETURN


PROCEDURE Corri
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 *****
 SELECT Solcot
 do case
    case estado='00'
         DO standby with ' La Solicitud aun no Registra Proveedores '
         return
    case estado='20'
         DO standby with ' La Solicitud no esta Adjudicada '
         return
    case estado = '70'
         DO standby with ' La Solicitud ha sido Devuelta '
         return
    case estado = '99'
         DO standby with ' La Solicitud est  Anulada '
         return
 endcase
 DO Pantalla
 SCATTER MEMVAR
 @ 1,22 GET m.NumSc   DISABLE
 @ 1,60 GET m.FecSc   DISABLE
 DO PROVEEDOR
 DO WHILE .T.
     Ok = Trabaja_Hijo()
     IF LASTKEY() # 27
        IF YESNO('¨ Confirme el ingreso ?')
			DO AgrJusyObs
			EXIT
        ENDIF
     ELSE
        IF YESNO('¨ Cancela el ingreso ?')
           ok = .F.
           EXIT
        ENDIF
     ENDIF
  ENDDO
  IF Ok .AND. LASTKEY() # 27
     m.estado='50'
     ACTIVATE WINDOW STANDBY
     @ 1,1 SAY ' N§ Cuadro Comparativo -> : ' get m.numccc DISABLE
     read
     =INKEY(0)
     DEACTIVATE WINDOW STANDBY 
     SELECT Solcot
     m.user = vUser_ID
  	 m.user_fc = date()
	 m.user_TP = 'C'
     GATHER MEMVAR
  ENDIF

 UNLOCK ALL
 SELECT solcot
 DO Vista
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT Solcot
 do case
    case estado='00'
         DO standby with ' La Solicitud aun no Registra Proveedores '
         return
    case estado='50'
         DO standby with ' La Solicitud ha sido Atendida '
         return
    case estado = '70'
         DO standby with ' La Solicitud ha sido Devuelta '
         return
    case estado = '99'
         DO standby with ' La Solicitud est  Anulada '
         return
 endcase
 DO Pantalla
 SCATTER MEMVAR
 @ 1,22 GET m.NumSc   DISABLE
 @ 1,60 GET m.FecSc   DISABLE
 DO PROVEEDOR
 DO WHILE .T.
     Ok = Trabaja_Hijo()
     IF LASTKEY() # 27
        IF YESNO('¨ Confirme el ingreso ?')
			DO AgrJusyObs
			EXIT
		ENDIF
     ELSE
       DO STANDBY WITH ' Cancelado el Ingreso ..'
       ok = .F.
       EXIT
     ENDIF
  ENDDO
  IF Ok .AND. LASTKEY() # 27
     m.estado='50'
     SELE PARMA
     SEEK 'CORRELCCC'
     = valCCC ( Parma.NumEnt + 1 )
     ACTIVATE WINDOW STANDBY
     @ 1,1 SAY ' N§ Cuadro Comparativo -> : ' get m.numccc DISABLE
     read
     =INKEY(0)
     DEACTIVATE WINDOW STANDBY 
     SELECT Solcot
     SEEK m.periodo+m.numsc
     m.user = vUser_ID
  	 m.user_fc = date()
	 m.user_TP = 'I'
     if found()
        GATHER MEMVAR
     else   
        IF F_appd()
           GATHER MEMVAR
        ENDIF
        UNLOCK   
     endif
     SELE PARMA
     SEEK 'CORRELCCC'
     REPLA NUMENT WITH NUMENT + 1
     SELEC SOLCOT
  ENDIF

 UNLOCK ALL
 SELECT solcot
*ON KEY LABEL F4
*@ 20,00 SAY WINTITLE(' ')
 DO Vista
 RETURN

PROCEDURE AgrJusyObs
*---------------
SET CONFIRM OFF
DEFINE WINDOW W_Just FROM 03,10 TO 22,69 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

ACTIVATE WINDOWS W_Just

@ 01,05 SAY "Ingrese Justificaci¢n"
@ 02,01 GET m.Just1
@ 03,01 GET m.Just2
@ 04,01 GET m.Just3
@ 05,01 GET m.Just4
@ 06,01 GET m.Just5
@ 07,01 GET m.Just6

@ 09,05 SAY "Ingrese Observaci¢n"
@ 10,01 GET m.Obs1 FUNCTION "S40"
@ 11,01 GET m.Obs2 FUNCTION "S40"
@ 12,01 GET m.Obs3 FUNCTION "S40"
@ 13,01 GET m.Obs4 FUNCTION "S40"
@ 14,01 GET m.Obs5 FUNCTION "S40"
@ 15,01 GET m.Obs6 FUNCTION "S40"

READ VALID val_read()

DEACTIVATE WINDOWS W_Just
RELEASE    WINDOWS W_Just
SET CONFIRM ON

RETURN

PROCEDURE AgrObs
*---------------
RETURN

PROCEDURE Trabaja_Hijo
*---------------------
PRIVATE VFUN
 ACTIVATE SCREEN
 VFUN=.T.
 PUBLIC VPRV
 HIDE MENU mMenu
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° F10->Terminar°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°' 
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELECT ITESC
 if vOpcion=1
    BROWSE ;
    NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo + m.numsc ;
    WINDOW Wind_9 ;
    FIELDS;
    CodArt      : H= 'C¢digo' :W=.F.,;
    NUMORD      : H= 'OR' :W=.F. ,;
    NUMPEC      : H= 'Pecs':W=.F. ,;
    DesPed      : H= 'Descripci¢n' :42 :W=.F.,;
    UniMed      : H= 'UniMd'  :4 :W=.F.,;
    Cansol      : H= 'Cant' :P='9,999.999':W=.F. ,;
    NumPrv      : H= 'P'          :P='X' :V=ESCOGE():F ,;
    NomproX = Val_prv(CodPrvx):F :H=' PROVEEDOR ' :30 :W=.F. ,;
    A='³' :H='*' :W=.F.,;
    PreUni1     : H= 'Unitario1'  :P='9,999,999.999' :W=.F.,;
    Valtot1     : H= 'Total 1'  :P='9,999,999.99':W=.F.,;
    B='³' :H='*' :W=.F.,;
    PreUni2     : H= 'Unitario2'  :P='9,999,999.999':W=.F.,;
    ValTot2     : H= 'Total 2'  :P='9,999,999.99':W=.F.,;
    C='³' :H='*' :W=.F.,;
    PreUni3     : H= 'Unitario3'  :P='9,999,999.999':W=.F.,;
    ValTot3     : H= 'Total 3'  :P='9,999,999.99':W=.F.,;
    D='³' :H='*' :W=.F.,;
    PreUni4     : H= 'Unitario4'  :P='9,999,999.999':W=.F.,;
    ValTot4     : H= 'Total 4'  :P='9,999,999.99':W=.F.,;
    E='³' :H='*' :W=.F.,;
    PreUni5     : H= 'Unitario5'  :P='9,999,999.999':W=.F.,;
    ValTot5     : H= 'Total 5'  :P='9,999,999.99':W=.F.,;
    F='³' :H='*' :W=.F.,;
    PreUni6     : H= 'Unitario6'  :P='9,999,999.999':W=.F.,;
    Valtot6     : H= 'Total 6'  :P='9,999,999.99' :W=.F.
 else
    BROWSE ;
    NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo + m.numsc ;
    WINDOW Wind_9 ;
    FIELDS;
    SS=MLINE(DeTALLE,1)      : H= 'Descripci¢n' :70 :W=.F.,;
    NumPrv      : H= 'P'          :P='X' :V=ESCOGE():F ,;
    NomproX = Val_prv(CodPrvx):F :H=' PROVEEDOR ' :30 :W=.F. ,;
    A='³' :H='*' :W=.F.,;
    PreUni1     : H= 'Unitario1'  :P='9,999,999.999' :W=.F.,;
    Valtot1     : H= 'Total 1'  :P='9,999,999.99':W=.F.,;
    B='³' :H='*' :W=.F.,;
    PreUni2     : H= 'Unitario2'  :P='9,999,999.999':W=.F.,;
    ValTot2     : H= 'Total 2'  :P='9,999,999.99':W=.F.,;
    C='³' :H='*' :W=.F.,;
    PreUni3     : H= 'Unitario3'  :P='9,999,999.999':W=.F.,;
    ValTot3     : H= 'Total 3'  :P='9,999,999.99':W=.F.,;
    D='³' :H='*' :W=.F.,;
    PreUni4     : H= 'Unitario4'  :P='9,999,999.999':W=.F.,;
    ValTot4     : H= 'Total 4'  :P='9,999,999.99':W=.F.,;
    E='³' :H='*' :W=.F.,;
    PreUni5     : H= 'Unitario5'  :P='9,999,999.999':W=.F.,;
    ValTot5     : H= 'Total 5'  :P='9,999,999.99':W=.F.,;
    F='³' :H='*' :W=.F.,;
    PreUni6     : H= 'Unitario6'  :P='9,999,999.999':W=.F.,;
    Valtot6     : H= 'Total 6'  :P='9,999,999.99' :W=.F.
 endif
 ON KEY LABEL F10
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Solcot
RETURN VFUN

PROCEDURE escoge
*---------------
private vPrv,vPre,vKei
vPrv='SOLCOT.codprv'+numprv
vPre='Preuni'+numprv
vKei = ITESC.PERIODO+ITESC.NUMSC+ITESC.CODFTE+ITESC.CODART+ITESC.NUMORD+ITESC.NUMPEC
IF NUMPRV $ '12345' AND &vPrv#'    '
   REPLACE CodPrvX WITH &vPrv, ;
           PreUniX WITH &vPre
ELSE
   REPLACE CodPrvX WITH '    ',NumPrv WITH ' ',PreUniX WITH 0
ENDIF

IF vOpcion = 1
   SELECT ITEPEC
   SET ORDER TO ITEPEC11
   SEEK vkei
   IF FOUND()
      IF RLOCK()
         REPLACE CodPrv WITH Itesc.codprvx,PreUni WITH Itesc.preunix
      ENDIF
   ELSE
     IF !EMPTY(ITESC.NUMPRV)
        DO STANDBY WITH 'Error, no grab¢ en Pecosa..'
     ENDIF   
   endif  
   SET ORDE TO ITEPEC2
ELSE
   SELECT SOLSER
   SEEK m.Periodo + Itesc.Numsc
   IF FOUND()
      IF RLOCK()
          REPLACE CodPrv WITH Itesc.codprvx ,;
              VALTOT WITH Itesc.preunix
      ENDIF
      UNLOCK
   ELSE
     DO STANDBY WITH 'Error..'
   ENDIF
ENDIF
SELE ITESC
ON KEY LABEL F7 DO JUSOBS
return .t.

PROCEDURE JUSOBS
*---------------
IF !Empty(Itesc.NumPrv)
	vNum=alltrim(Itesc.NumPrv)
	ACTIVATE WINDOW JUSOBS
	@ 0,1 SAY 'PROVEEDOR N§ '+VNUM+':'
	@ 1,1 SAY 'Justificaci¢n :' get m.just&vNum PICTURE '@S56'
	@ 2,1 SAY '  Observaci¢n :' get m.obs&vNum  PICTURE '@S56'
	read
	DEACTIVATE WINDOW JUSOBS
endif	
ON KEY LABEL F7
RETURN

PROCEDURE XACTUALIZA
*------------------
* ACTUALIZAR EL ARCHIVO ITEPEC CON CODPRVX Y PREUNIX
PRIVATE ZX
zx=alias()
IF vOpcion = 1

   SELECT ITEPEC
   SET ORDER TO ITEPEC11
   SEEK ITESC.PERIODO+ITESC.NUMSC+ITESC.CODFTE+ITESC.CODART+ALLTRIM(ITESC.NUMORD)
   IF FOUND()
*     SCAN FOR ITEPEC.CODART=ITESC.CODART AND ITEPEC.NUMSC=ITESC.NUMSC AND ITEPEC.CODFTE=ITESC.CODFTE AND ITEPEC.PERIODO=ITESC.PERIODO AND IIF(!EMPTY(ITESC.NUMORD),ALLTRIM(ITEPEC.NUMORD)=ALLTRIM(ITESC.NUMORD),.T.)
           IF RLOCK()
              REPLACE CodPrv WITH Itesc.codprvx ,;
                PreUni WITH Itesc.preunix
           ENDIF
*     ENDSCAN
      SET ORDE TO ITEPEC2
   ELSE
     IF !EMPTY(ITESC.NUMPRV)
        DO STANDBY WITH 'Error, no grab¢ en Pecosa..'
     ENDIF   
   endif  
ELSE
   SELECT SOLSER
   SEEK m.Periodo + Itesc.Numsc
   IF FOUND()
      IF RLOCK()
          REPLACE CodPrv WITH Itesc.codprvx ,;
              VALTOT WITH Itesc.preunix
      ENDIF
      UNLOCK
   ELSE
     DO STANDBY WITH 'Error..'
   ENDIF
ENDIF
sele &zx
return .t.

PROCEDURE BORRAPEC
*------------------
* ACTUALIZAR EL ARCHIVO ITEPEC
sele itepec
set filt to itepec.codart=itesc.codart and itepec.numsc=itesc.numsc
SET ORDE TO 1
GO TOP
scan
  if rlock()
     REPLACE CodPrv WITH '    ' ,;
             PreUni WITH 0
  endif
endSC
SET ORDE TO 2
return .t.

PROCEDURE AGREGAOC
*------------------
* ACTUALIZAR EL ARCHIVO ITEOC
ZX=alias()
SELECT Parma
SEEK 'CORREL'+'ORDCOM'
vCorr = Parma.NumEnt + 1
SELECT ITEPEC
SET FILTER TO ITEPEC.CODART = ITESC.CODART AND ITEPEC.NUMSC = ITESC.NUMSC
SET ORDER TO 1
GO TOP
SCAN
   vCanreq=0
   vCa1 = CodArt + CodCad
   vCa2 = CodArt + CodCad
   DO WHILE  vCa1 = vCa2
      vCanreq=vCanreq+Canreq
      SKIP
      vCa1 = CodArt + CodCad
   ENDDO
   SKIP -1
   DO AGREG_ITEM
ENDSCAN
SCAN
  IF RLOCK()
     REPLACE CodPrv WITH Itesc.codprvx ,;
             PreUni WITH Itesc.preunix
  ENDIF
ENDSCAN
SET ORDE TO 2
sele &zx
return .t.


PROCEDURE costo
*--------------
if rlock()
   replace valtot&vN with cansol*preuni&vN
endif
return .t.

PROCEDURE Elimi
*--------------
SELE SOLCOT
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 DO CASE
    case estado='00'
         DO standby with ' La Solicitud aun no Registra Proveedores '
         return
    case estado='20'
         DO standby with ' La Solicitud NO ha sido Atendida '
         return
    case estado = '70'
         DO standby with ' La Solicitud ha sido Devuelta '
         return
    case estado = '99'
         DO standby with ' La Solicitud est  Anulada '
         return
 ENDCASE

 velimina = YESNO('¨ Desea ELIMINAR Adjudicaci¢n de ‚sta Solicitud ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
    DO BORRAPEC

    SELE ITESc
      SCAN FOR itesc.numsc=solcot.numsc
           replace numprv with ' ',codprvx with '    ',preunix with 0, SOLCOT.estado with '20'
      ENDSCAN

   SELE SOLCOT
   DO Vista
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Agreg_Item
*-----------------
sele itesc
IF F_appd()
   REPLACE NumSc  WITH m.NumSc ,;
           CodArt WITH Itepec.CodArt ,;
           CodCad WITH Itepec.CodCad ,;
           CanSol WITH vCanreq ,;
           UniMed WITH Itepec.Coduni ,;
           DesPed WITH Itepec.Descri
   RETURN .T.
ENDIF
RETURN .F.


PROCEDURE Lista
*--------------
select SolCot
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   DO LISADJ
*  SET RELATION TO PERIODO+NumSc INTO Itesc
*  SET SKIP TO Itesc
*  SET FILTER TO ESTADO='50'
*  do reporte with 2,"LisAdj",' Proveedores Adjudicados '
*  SET RELATION TO
*  SET FILTER TO
endif
select SolCot
go vtemp
DO VISTA
RETURN


PROCEDURE LisAdj
*---------------
vTemo = RECNO()
DEFINE WINDOW LIS FROM 5,15 TO 19,65 FLOAT DOUBLE TITLE 'Listado Adjudicaci¢n' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipLis
vCli = SPAC(4)
vAn = space(2)

@ 01,01 SAY "Total Solicitudes : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22) AND IIF(VTOCLI=2,ASSIG(),.T.)
@ 03,01 SAY "        Solicitud : "

@ 03,22 GET vCli    WHEN VTOCLI=2  PICTURE '!!!!' 
@ 03,26 SAY "."
@ 03,27 GET vAn   PICTURE '!!'  WHEN VTOCLI=2  VALID ValSol()

@ 06,01 SAY "      Listado por : " GET vTipLis  FUNCTION '^ Art¡culo;Otorgamiento'

@ 12,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
	ACTIVATE WINDOW STANDBY
	@ 01,04 SAY 'Espere un momento........'
	SET RELATION TO PERIODO+NumSc INTO Itesc
	SET SKIP TO Itesc
	SET FILTER TO ESTADO='50' AND IIF(vTocli=1,.t.,NumSc=vCli)
	GO TOP
	DEACTIVATE WINDOW STANDBY
	vTitulo=IIF(vTipLis=1,' por Art¡culo ',' por Acta de Otorgamiento ')
	IF !EOF()
		IF vOpcion=1
			if vTipLis=1
				do reporte with 2,"LisAdj",' Proveedores Adjudicados '
			 else
				do reporte with 2,"LisAdj1",' Acta de Otorgamiento '
			endif
		 ELSE
			if vTipLis=1
				do reporte with 2,"LisAdjS",' Proveedores Adjudicados '
			 else
				do reporte with 2,"LisAdj2",' Acta de Otorgamiento '
			endif
		ENDIF
	 ELSE
		DO STANDBY WITH VMENS08
	ENDIF
	SET FILTER TO
	SELE SOLCOT
	SET RELATION TO
ENDIF

RETURN

PROCEDURE ASSIG
*--------------
vCli = SOLCOT.NUMSC
vAn = SOLCOT.PERIODO
RETURN  .T.


PROCEDURE ValSol             && Revisi¢n de BD en browse
*--------------
SELECT SOLCOT
 SET RELATION TO PERIODO+NUMSC INTO ITESC
 SET SKIP TO ITESC
 Vtemp = RECNO()
 ON KEY LABEL F10 KEYBOARD CHR(23)
 seek van+vcli
 if !found()
    IF vopcion = 1
    	BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	    NumSc  :H=' N§ ' ,;
	    FecSc  :H='Fecha' ,;
	    TipDoc :H='DOC' ,;
	    ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
	    itesc.desped :H='Articulo ':36 ,;
	    itesc.unimed :H='Unid' ,;
	    itesc.Cansol :H='Cantid'
	ELSE
    	BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	    NumSc  :H=' N§ ' ,;
	    FecSc  :H='Fecha' ,;
	    TipDoc :H='DOC' ,;
	    ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
	    DD=MLINE(itesc.deTALLE,1)  :H='Servicio ':36 ,;
	    itesc.unimed :H='Unid' ,;
	    itesc.Cansol :H='Cantid'
	ENDIF
    vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo
    IF LASTKEY()=27
      GOTO Vtemp
    ENDIF
 endif
 VCLI=NUMSC
 VAN =PERIODO
 SELECT SOLCOT  
 ON KEY LABEL F10
 SET RELATION TO
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE WINDOW wind_0
  RELEASE WINDOW wind_1
  RELEASE WINDOW wind_c1
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


function valprv
*--------------
private xx, vfun
vfun = .f.
m.codprv&vN = iif( empty(m.codprv&vN),m.codprv&vN,padl(alltrim(m.codprv&vN),4,'0'))
xx = val_prv( m.codprv&vN,.t.,vCod+2,27)
if xx
   return .t.
endif
return vfun

function valsC
*-----------------
parameter vnumSc
private vfun
vfun = .t.
m.numSc =padl(alltrim(str(vnumSc,4)),4,'0')
if m.numSc  = '0000' or empty(m.numSc)
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

PROCEDURE PROVEEDOR
*------------------

if !empty(m.Codprv1)
 @  3, 2 SAY "   Proveedor N§ 1 :  "
 @ 3,22 SAY m.codprv1
 @ 3,27 SAY val_prv( m.codprv1)
else
 @ 3,2 say SPac(77)
endif

if !empty(m.Codprv2)
 @  4, 2 SAY "   Proveedor N§ 2 :  "
 @ 4,22 SAY m.codprv2
 @ 4,27 SAY val_prv( m.codprv2)
else
 @ 4,2 say spac(77)
endif

if !empty(m.Codprv3)
 @  5, 2 SAY "   Proveedor N§ 3 :  "
 @ 5,22 SAY m.codprv3
 @ 5,27 SAY val_prv( m.codprv3)
else
 @ 5,2 say spac(77)
endif

if !empty(m.Codprv4)
 @  6, 2 SAY "   Proveedor N§ 4 :  "
 @ 6,22 SAY m.codprv4
 @ 6,27 SAY val_prv( m.codprv4)
else
 @ 6,2 say spac(77)
endif
if !empty(m.Codprv5)
 @  7, 2 SAY "   Proveedor N§ 5 :  "
 @ 7,22 SAY m.codprv5
 @ 7,27 SAY val_prv( m.codprv5)
else
 @ 7,2 say spac(77)
endif
if !empty(m.Codprv6)
 @  8, 2 SAY "   Proveedor N§ 6 :  "
 @ 8,22 SAY m.codprv6
 @ 8,27 SAY val_prv( m.codprv6)
else
 @ 8,2 say spac(77)
endif
RETURN
*-----------*-----------*

PROCEDURE BUSCAR
*---------------
RETURN .T.
vNum=alltrim(Itesc.NumPrv)
just&vNum=Itesc.actjusx
 obs&vNum=Itesc.observx
return ' '

FUNCTION VALCCC
*--------------
parameter vcodCCC
private vfun
vfun = .t.
m.numccc=padl(alltrim(str(vcodCCC,4)),4,'0')
if m.numccc = '0000' or empty(m.numCCC)
   vfun = .f.
endif
return vfun


PROCEDURE VUSUA
***************
PARAMETER CSYS
PRIVATE ALI
ALI = ALIAS()
VKEY = ALLTRIM(CSYS)
VFUN = ' '
IF !EMPTY(VKEY)
    SELE USU
    SEEK VKEY
    VFUN = IIF(FOUND(),NOMBRE,'   ')
    SELE (ALI)
ENDIF    
RETURN VFUN

