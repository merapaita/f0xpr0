*--------------------------------------------------------------------------
* SolCot.Prg
* Registra las Solicitudes de Cotizaciones
* Estado del Solicitud de cotizacion:
*   '00' Emitida   Este es el que se registra en el Solicitud de Cotizaci¢n
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: Luis Castillo Dur n
*--------------------------------------------------------------------------
*- Abriendo Archivo
PARAMETER vOpcion
SET EXCL OFF
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
IF vOpcion = 1
  USE Solcot   IN 2   order tag Solcot1 ALIAS Solcot
  USE AuxCot   IN 6   order tag Auxcot1 ALIAS Auxcot
ELSE
  USE Solcot   IN 2   order tag SolCot2 ALIAS Solcot
  USE AuxCot   IN 6   order tag Auxcot2 ALIAS Auxcot
ENDIF
USE Itesc    IN 3   order tag Itesc1   ALIAS Itesc
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE Itepec   IN 5   order tag ItePec1  ALIAS Itepec
USE Promae   IN 7   ORDER TAG promae1  ALIAS Promae
USE Solser   IN 8   order tag Solser1  ALIAS Solser
USE IteArt   IN 9   order tag Iteart3  ALIAS Iteart
USE Pecosa   IN 10  order tag Pecosa1  ALIAS Pecosa
USE USUARIO  IN 19  ORDER TAG USUARIO1 ALIAS USU
SET MEMOWIDTH TO 56

*- Mensajes de aviso al usuario
Vmens01 = ' Solicitud de Cotizaciones : REVISION '
Vmens02 = 'Registro de Solicitud de Cotizaci¢n'
Vmens04 = 'Dicho Solicitud de Cotizaci¢n no fue encontrada'
Vmens05 = 'No existe Solicitud de Cotizaci¢n anterior'
Vmens06 = 'No existe Solicitud de Cotizaci¢n siguiente'
Vmens07 = '¨ Desea ANULAR ‚sta Solicitud de Cotizaci¢n ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta Solicitud de Cotizaci¢n ha sido anulada'
Vmens10 = 'La Solicitud de Cotizaci¢n ya fue atendida'
Vmens11 = 'La Solicitud de Cotizaci¢n ha sido devuelta'

vesc = SET('ESCAPE')
SET ESCAPE OFF

SELECT Solcot
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

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

PROCEDURE VISTA_DETA
*------------------
if vOpcion = 1
   DO VIS_DET
else
   DO VIS_DET1
endif

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10
                                   *09
 DEFINE WINDOW Wind_1 FROM 00,00 TO 07,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 08,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle:S/C    ®F2¯ Muestra Proveedores   ®F9¯ Detalle Item ' COLOR SCHEME 10
 
 DEFINE WINDOW Wind_2a FROM 08,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle:S/C    ®F9¯ Agregar Detalle   ®Esc¯ Sale ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 11,05 TO 21,74 DOUBLE ;
 TITLE ' Registro de Proveedores ' COLOR SCHEME 10 ;
 FOOTER '®F2¯ Muestra Proveedores'
 
 DEFINE WINDOW Wind_4 FROM 20,50 TO 22,78 ;
 COLOR SCHEME 10

 DEFINE WINDOW Wind_5 FROM 08,15 TO 23,79 DOUBLE ;
 TITLE 'Detalle:S/C    ' COLOR SCHEME 10

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
 ACTIVATE WINDOW Wind_1
 CLEAR
 @  1, 2 SAY " N£mero Solicitud :"
 @  1,40 SAY "            Fecha :"
 @  3, 2 SAY "       Referencia :"
 @  4, 2 SAY "          Destino :"
 RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Solcot
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ON KEY LABEL F2  DO vista_aux
 ACTIVATE WINDOW Wind_1
 SCATTER MEMVAR

 @  0,60 SAY IIF( m.Estado= '00','Pendiente',IIF( m.Estado = '20','Con Cotiz',IIF(m.Estado='99','Anulada  ',IIF(m.Estado='50','Atendido ','         '))))
 @  1,22 say m.Periodo
 @  1,24 SAY '-'
 @  1,25 say m.NumSc
 @  1,60 SAY m.FecSc
 @  2,40 SAY iif(empty(m.numccc),'                   ','Cuadro Comparativo:')
 @  2,60 SAY iif(empty(m.numccc),'    ',m.numccc)
 if vOpcion = 2
    * @  4,22 SAY val_para(m.CodDep,'CODDEP','D',22,45,7)
    @  4,22 SAY val_para(m.CodDep,'CODDEP','D',22,45,7)    
 endif
 @  3,22 SAY m.Observa             && PRUEBA
 @  4,22 SAY m.Lugar
 
if vOpcion = 1
   DO VIS_HIJO
else
   DO VIS_HIJO1
endif

RETURN


PROCEDURE VIS_HIJO
*-----------------
hide popup all
SELECT IteSc
ON KEY LABEL F9 DO VISTA_DETA
IF vOpcion=1
   BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.Numsc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   numpec      : H= 'Pecs',;
   codfte      : H= 'Fte',;
   CodArt      : H= 'C¢digo',;
   Cansol      : H= 'Cant.' :P='999,999.999',;
   unimed      : H= 'Uni'      :W=.F. :5,;
   Desped      : H= 'Descripci¢n' :58 :W=.F.
ELSE
   BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.Numsc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   numpec      : H= 'Pecs',;
   codfte      : H= 'Fte',;
   CodArt      : H= 'C¢digo',;
   Cansol      : H= 'Cant.' :P='999,999.999',;
   unimed      : H= 'Uni'      :W=.F. :5,;
   XX=MLINE(DeTALLE,1)  : H= 'Descripci¢n' :58 :W=.F.
ENDIF
SELE SOLCOT

RETURN


PROCEDURE VIS_HIJO1
*------------------
hide popup all
ON KEY LABEL F2  DO vista_aux
SELECT IteSc
ON KEY LABEL F9  DO VISTA_DETA
IF vOpcion=1
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.Numsc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   numss       : H= 'Sol.Ser.',;
   codfte      : H= 'Fte',;   
   Codcad      : H= 'Cod.Cadena' :W=.f.,;
   Desped      : H= 'Descripci¢n' :W=.F.
ELSE
*MODIFY MEMO DETALLE NOEDIT WINDOW WIND_2
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.Numsc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   numss       : H= 'Sol.Ser.',;
   codfte      : H= 'Fte',;   
   Codcad      : H= 'Cod.Cadena' :W=.f.,;
   XX=MLINE(DeTALLE,1) : H= 'Descripci¢n' :60 :W=.F.
ENDIF
SELE SOLCOT
RETURN


PROCEDURE VIS_DET
*-----------------
hide popup all
SELECT IteSc
ON KEY LABEL F9 DO OBSERVA
IF vOpcion=1
   BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.Numsc ;
   WINDOW Wind_2A;
   FIELDS;
   numpec      : H= 'Pecs',;
   codfte      : H= 'Fte',;
   CodArt      : H= 'C¢digo',;
   Cansol      : H= 'Cant.' :P='999,999.999',;
   unimed      : H= 'Uni'      :W=.F. :5,;
   Desped      : H= 'Descripci¢n' :58 :W=.F.
ELSE
   BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.Numsc ;
   WINDOW Wind_2 ;
   FIELDS;
   numpec      : H= 'Pecs',;
   codfte      : H= 'Fte',;
   CodArt      : H= 'C¢digo',;
   Cansol      : H= 'Cant.' :P='999,999.999',;
   unimed      : H= 'Uni'      :W=.F. :5,;
   XX=MLINE(DeTALLE,1)  : H= 'Descripci¢n' :58 :W=.F.
ENDIF
SELE SOLCOT
ON KEY LABEL F9  DO vista_det
DO VISTA
RETURN


PROCEDURE VIS_DET1
*-------------------
hide popup all
ON KEY LABEL F2  DO vista_aux
SELECT IteSc
ON KEY LABEL F9 DO OBSERVA
IF vOpcion=1
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.Numsc  ;
   WINDOW Wind_2 ;
   FIELDS;
   numss       : H= 'Sol.Serv.',;
   codfte      : H= 'Fte',;   
   Codcad      : H= 'Cod.Cadena' :W=.f.,;
   Desped      : H= 'Descripci¢n' :W=.F.
ELSE
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.Numsc ;
   WINDOW Wind_2 ;
   FIELDS;
   numss       : H= 'Sol.Serv.',;
   codfte      : H= 'Fte',;   
   Codcad      : H= 'Cod.Cadena' :W=.f.,;
   XX=MLINE(DeTALLE,1) : H= 'Descripci¢n' :60 :W=.F.
ENDIF
SELE SOLCOT
ON KEY LABEL F9  DO vista_det
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
ON KEY LABEL F2
SELECT SOLCOT
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
 IF vopcion=1
	 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	 NumSc  :H=' N§ ' ,;
	 FecSc  :H='Fecha' ,;
	 TipDoc :H='DOC' ,;
	 ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
	 itesc.desped :H=IIF(vOpcion=1,'Articulo ','Servicio ') :36 ,;
	 itesc.unimed :H='Unid' ,;
	 itesc.Cansol :H='Cantid'
 ELSE
 	 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	 NumSc  :H=' N§ ' ,;
	 FecSc  :H='Fecha' ,;
	 TipDoc :H='DOC' ,;
	 ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
	 CC=MLINE(ITesc.deTALLE,1) :H=IIF(vOpcion=1,'Articulo ','Servicio ') :36 ,;
	 itesc.unimed :H='Unid' ,;
	 itesc.Cansol :H='Cantid'
 ENDIF
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 ON KEY LABEL F2
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
 ON KEY LABEL F2
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
   * El Solicitud de Cotizaci¢n ha sido devuelto
   DO STANDBY WITH Vmens11
   RETURN
 ENDIF
 IF Estado = '50'
   * El Solicitud de Cotizaci¢n ya tiene O/C
   DO STANDBY WITH Vmens10
   *RETURN
 ENDIF
 *****

 SELECT Solcot
 SCATTER MEMVAR
 ACTIVATE WINDOW Wind_1
 DO PANTALLA
 @  1,22 GET m.Periodo    picture '!!'  DISABLE
 @  1,24 SAY '-'
 @  1,25 GET m.NumSc     picture '!!!!'  DISABLE
 @  1,60 GET m.FecSc     PICTURE '@D'
 @  3,22 GET m.Observa
 @  4,22 GET m.Lugar
 READ VALID Val_Read()
   
 IF LASTKEY() # 27
    DO WHILE .T.
       OK = Corrije_Hijo()
       DO WHILE .T.
          OH=AUXCOT()
          IF !YESNO('¨ + Proveedores?')
             EXIT
          ENDIF
       ENDDO
       IF LASTKEY() # 27
          IF YESNO('¨ Conforme la modificaci¢n ?')
             EXIT
          ENDIF
       ELSE
          IF YESNO('¨ Cancela la modificaci¢n ?')
             ok = .F.
             EXIT
          ENDIF
       ENDIF
    ENDDO

    IF Ok .AND. LASTKEY() # 27
       IF vOpcion = 1
          SELECT ITEPEC
  	      SET ORDE TO ITEPEC11
          SEEK m.periodo+m.numsc 
          SCAN while LLave+periodo+numsc = 'û'+m.periodo+m.numsc &&and estado = '00'
	          IF RLOCK()
    	         REPLACE NumsC with m.NumsC,Estado with '20'
        	  ENDIF
 	      ENDSCAN
	      set order to ITEPEC1
	    ELSE
	      SELECT SOLSER
	      IF RLOCK()
	         REPLACE NumsC with m.NumsC,Estado with '20'
	      ENDIF
	    ENDIF
	    unlock
	    SELECT ITESC
	    IF vOpcion = 2
    	   oh = detalle()
	    ENDIF	  
        SELECT Solcot
		m.user = sys(0)
  		m.user_fc = date()
	    m.user_TP = 'C'
        
        GATHER MEMVAR
    ELSE
        SELECT solcot
    ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT solcot
 DO Vista
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
SELECT Parma
SEEK 'CORRELSOLCOT'
ON KEY LABEL F2
SELECT Solcot
DO Pantalla

SCATTER MEMVAR BLANK
m.periodo = str(year(date()) - 1900,2)
= valSc (Parma.NumEnt+1)
m.FecSC   = date()
m.Estado  = '00'

@ 1,22 GET m.Periodo   picture '!!' DISABLE
@ 1,24 SAY '-'
@ 1,25 GET m.NumSc     PICTURE '!!!!' DISABLE &&valid val_sc()

@ 1,60 GET m.FecSc     PICTURE '@D'
@ 3,22 GET m.Observa
@ 4,22 GET m.Lugar
 
 READ VALID Val_Read()
 IF LASTKEY() # 27
    DO WHILE .T.
 	   ACTIVATE WINDOW STANDBY 
 	   @ 1,14 SAY "Espere un momento ... " COLOR W*
       Ok = Trabaja_Hijo()
       IF LASTKEY() # 27 AND OK
          IF YESNO('¨ Confirme el ingreso ?')
             IF vOPCION=2
                DO AGREG_SER
             ENDIF
             EXIT
          ENDIF
       ELSE
          DO STANDBY WITH ' Cancelado el Ingreso ..'
             ok = .F.
             EXIT
       ENDIF
    ENDDO
    SELECT Solcot
    IF Ok .AND. LASTKEY() # 27
       *- Aumento el correlativo
       SELECT Parma
       SEEK 'CORREL' + 'SOLCOT'
       REPLACE NumEnt WITH NumEnt + 1
       IF vOpcion = 1
          SELECT ITEPEC
          SET ORDE TO ITEPEC11
          SEEK m.periodo+m.numsc 
          SCAN while LLave+periodo+numsc = 'û'+m.periodo+m.numsc and estado = '00'
           IF RLOCK()
              REPLACE NumsC with m.NumsC,Estado with '20'
           ENDIF
          ENDSCAN
          set orde to ITEPEC1
          M.Tipdoc = 'PEC'
       ELSE
          SELECT SOLSER
          IF RLOCK()
             REPLACE NumsC with m.NumsC,Estado with '20'
          ENDIF
          M.Tipdoc = 'S/S'
       ENDIF
       unlock
       SELECT Solcot
       m.tipact = iif(vOpcion=1,'B','S')
       SELECT ITESC
       IF vOpcion = 2
       	  oh = detalle()
       ENDIF	  
       SELECT Solcot       
       IF !empty(m.NumSc)
	      m.user = sys(0)
  	      m.user_fc = date()
	      m.user_TP = 'I'
	      GATHER MEMVAR
	   ENDIF
       DO WHILE .T.
          OH=AUXCOT()
          IF !YESNO('¨ + Proveedores?')
             EXIT
          ENDIF
       ENDDO            
       SELECT Solcot
    ELSE
       SELECT ITESC
       seek m.Periodo+m.NumSc
       SCAN WHILE Itesc.Periodo=m.Periodo and IteSc.NumSc = m.NumSc
         if rlock()
            delete next 1
         endif   
       ENDSCAN
       unlock
       if vOpcion = 1
	      select itepec
	      set orde to 11
    	  SCAN WHILE periodo+numsc = m.periodo+m.numsc
    	    if rlock()          
          	    REPLACE LLave with ' ' ,numsc with spac(4)
          	endif    
          ENDSCAN
          unlock
          set order to 1
       else
       	  select solser
          SEEK m.periodo+m.numsc 
          if rlock()          
           	*REPLACE LLave with ' ' ,numsc with spac(4)
           	REPLACE numsc with spac(4)
          endif 	
          unlock
        endif	 	  
       SELECT solcot       	  
    ENDIF
ELSE
   DO STANDBY WITH 'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT solcot
DO Vista
RETURN


PROCEDURE Trabaja_Hijo
*---------------------
private vfun
vFun = .t.
ACTIVATE SCREEN
HIDE MENU mMenu
ON KEY LABEL F10 KEYBOARD CHR(23)
DO CASE
    CASE vOpcion = 1
	   	 ON KEY LABEL F2 DO BUSPEC    
    	 ON KEY LABEL F11 DO MARCA
       	 ON KEY LABEL F12 DO DESMARCA
         vTempo = '°°°°° ®F2¯ Busca Pec °°° ®F11¯ Marca û °°° ®F12¯ Desmarca û °°° ®F10¯ Sale °°°°°'
         DO Logos WITH Rotulo1,vTempo
         SELECT ITEPEC
         SET ORDER TO ITEPEC1
         set filt to estado = '00'
		 SET RELATION TO Periodo+NumPec INTO Pecosa 
		 DEACTIVATE WINDOW Standby
         BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2  FIELD ;
         numord      : H= 'Od':W=.F. ,;
         llave       : H= 'û' :W=.F. ,;
         CodArt      : H= 'C¢digo'  :W=.F.,;
         NumPec      : H= 'Doc'  :W=.F.,;
         Codfte      : H= 'Fte'  :w=.f. ,;
         Descri      : H= 'Descripci¢n' :30 :W=.F.  ,;
         Coduni      : H= 'Uni'      :W=.F. :5,;
         Canreq      : H= 'Cantidad' :P='9,999.999' :W=.f. ,;
         X=ROUND(CanReq*PreUni,5)    :H='Total'  :P='999,999.99' :W=.F.
         
         IF LASTKEY()#27
            SET ORDE TO ITEPEC11
            SEEK m.Periodo+m.NumSc
	        SCAN while LLave+periodo+numsc = 'û'+m.periodo+m.numsc AND ESTADO='00' 
    	        DO AGREG_ITEM
	            SELE ITEPEC
	        ENDSCAN
	        SET ORDE TO itepec1
         endif
         SET RELATION TO
		 set filter to         
    CASE vOpcion = 2
         vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
         DO Logos WITH Rotulo1,vTempo
         SELECT SOLSER
         vOrd = ORDER()
         SET ORDER TO Solser3
         GO  TOP
         IF !EOF()
		 	DEACTIVATE WINDOW Standby
		    BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2  FIELD ;
                NUMSS    :H='N§ S/S'  :W=.F.,;
                FECSS    :H='Fecha'  :W=.F.,;
                ATTE     :H='Con Atencion a:' :W=.F. ,;
                CODDEP=val_para(CodDep,'CODDEP','D') :15 :H='Dependencia':W=.F. ,; 
                ss=mline(DEStino,1)   :H='Destino':W=.F.
         ELSE
            DO STANDBY WITH 'No existe Solicitudes a Cotizar'
            vFun = .f.
         ENDIF
         
         SELECT SOLSER
         SET ORDER TO (vOrd)
ENDCASE
if lastkey()=27
   vfun = .f.
endif   
ON KEY LABEL F2 
ON KEY LABEL F10
ON KEY LABEL F11 
ON KEY LABEL F12 
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
SELECT Solcot
RETURN vfun

PROCEDURE BUSPEC
*---------------
ACTIVATE WINDOW WIND_4
VNUMPEC='0000'
@ 0,1 say 'Ingrese N§ Pecosa =>' GET VNUMPEC PICTURE '!!!!' 
READ
DEACTIVATE WINDOW WIND_4
SEEK m.PERIODO+ALLTRIM(vNUMPEC)
RETURN

Procedure val_sc
*---------------
private ads,ord
ads=recno()
ord=order()
set orde to solcot1
seek m.periodo+m.numsc
ok1 = found()
set orde to solcot2
seek m.periodo+m.numsc
ok2 = found()
set orde to ord
go ads
if ok1 or ok2
   return .F.
endif
return .T.

PROCEDURE Marca
*--------------
REPLACE llave with 'û',numsc with m.numsc
return

PROCEDURE DesMarca
*--------------
REPLACE llave with ' ',numsc with spac(4)
return

PROCEDURE TODESMAR
*-----------------
RECS=RECNO()
GO TOP 
SCAN 
  REPLACE LLave with ' ' ,numsc with spac(4)
ENDSCAN
GO TOP
GO RECS
RETURN

PROCEDURE TODMARCA
*-----------------
vPerPec = Periodo+NumPec
RECX=RECNO()
GO TOP
SCAN FOR Periodo+NumPec = vPerPec
  REPLACE llave with 'û',numsc with m.numsc
  vPerPec = Periodo+NumPec 
ENDSCAN
GO TOP
GO RECX
RETURN

PROCEDURE Corrije_Hijo
*---------------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO Agreg_c
ON KEY LABEL F8  DO Elimi_C
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT ITESC
if vOpcion = 1
       BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2 key m.Periodo + m.NumSc FIELD ;
       NumOrd      : H= 'Od' :W=.F.,;
       CodArt      : H= 'C¢digo'  :W=.F.,;
       DesPed      : H= 'Descripci¢n' :50 :W=.F.  ,;
       Unimed      : H= 'Uni'      :W=.F. :3,;
       Cansol      : H= 'Cantidad' :P='9,999.999' :W=.f.
else
    BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2 key m.Periodo + m.NumSc FIELD ;
       Codcad      : H= 'Cadena Fun.' :W=.f.,;
       Desped      : H= 'Descripci¢n' :W=.F.
endif
 
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
SELECT Solcot
 if lastkey()=27
    return .f.
 endif
RETURN

PROCEDURE agreg_C
*-----------------
OQ=trabaja_hijo()
IF vOPCION=2
   DO AGREG_SER
ENDIF

ON KEY LABEL F10 KEYBOARD CHR(23)
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
return OQ

 
PROCEDURE ELIMI_C
*-----------------
PRIVATE vFun,vKey,vAlias
vAlias = alias()
vfun = .t.
IF vOpcion = 1
	SELE ITEPEC
	SET ORDE TO ITEPEC11
	GO TOP
	seek iteSc.periodo+iteSc.numsC+iteSC.codfte+iteSC.codart &&+ITESC.NUMORD
	if found()
	  IF RLOCK()
	   	REPLACE Llave with ' ',estado with '00',;
   	        NUMSC WITH SPACE(4)
	  ENDIF
	else
	  do standby with 'Advertencia:No es ubicado la Pecosa,Revise'
	  vFun=.f.
	ENDif
	set order to Itepec1
else
	select solser
    SEEK itesc.periodo+itesc.numsc 
    if found()
	    if rlock()          
    	   REPLACE ESTADO with '00' ,numsc with spac(4)
	    endif 	
   	else
		do standby with 'Advertencia:No es ubicado la S/S,Revise'
		vFun=.f.
	ENDif
    unlock
endif	
SELECT ITEsc
IF RLOCK() and vfun
   DELE NEXT 1
ENDIF
select (valias)
RETURN 


PROCEDURE AuxCot
*---------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°F10->Continua °°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO Agreg_Cot
 ON KEY LABEL F8  DO Elimi_Cot
 ON KEY LABEL F10 KEYBOARD CHR(23)
 cc=alias()
 SELECT AUXCOT
 SEEK  m.Periodo + m.NumSc

 IF !FOUND()
    DO AGREG_COT
 ENDIF
 BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_3 key m.Periodo + m.NumSc FIELD ;
     CodPrv      : H= 'C¢digo'     :V=Valprv():F ,;
     NomPro      : H= 'Proveeedor' :W=.F.
 ON KEY LABEL F5
 ON KEY LABEL F8
 ON KEY LABEL F10
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 sele &cc
 retur


PROCEDURE VISTA_AUX
*---------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°F10->Continua °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 AS=ALIAS()
 SELECT AUXCOT
 SEEK  m.Periodo + m.NumSc
 IF FOUND()
     BROWSE NOAPPEND NOEDIT NODELETE NOCLEAR NOMENU WINDOW Wind_3 key m.Periodo + m.NumSc FIELD ;
     CodPrv      : H= 'C¢digo'     :W=.F. ,;
     NomPro      : H= 'Proveeedor' :W=.F.
 ELSE
    DO STANDBY WITH 'No se encuentra Registrado Proveedores'
 ENDIF
 ON KEY LABEL F10
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SELECT &AS
 DO VISTA
 RETURN


PROCEDURE vista_aux1
*---------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°F10->Continua °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 ON KEY LABEL F10 KEYBOARD CHR(23)
 cc=alias()
 SELECT AUXCOT
 SEEK  m.Periodo + m.NumSc
 ACTIVATE WINDOW WIND_3
 IF FOUND()
 BROWSE noclear norefresh nooptimize noedit key m.Periodo + m.NumSc FIELD ;
     CodPrv      : H= 'C¢digo'  :W=.F. ,;
     NomPro      : H= 'Proveeedor' :W=.F.
 ELSE
    DO  STANDBY WITH ' No  tiene registrado Proveedores '
 ENDIF
 ON KEY LABEL F10
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 sele &cc
 DEACTIVATE WINDOW WIND_3
 return

PROCEDURE Agreg_Cot
*-----------------
IF F_appd()
  REPLACE NumSC  WITH m.NumSC , periodo with m.periodo,tipact with iif(vOpcion=1,'B','S')
  RETURN .T.
ENDIF
RETURN .F.


PROCEDURE Elimi_Cot
*-------------------
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif
return

function valprv
*--------------
private xx, vfun
vfun = .f.
codprv = iif( empty(codprv),codprv,padl(alltrim(codprv),4,'0'))
xx = val_cot( codprv,.t.)
ON KEY LABEL F10 KEYBOARD CHR(23)
if xx
   replace codprv with promae.codprv ,  nompro with promae.nompro
   return .t.
endif
return vfun

PROCEDURE Anula
*--------------
 SELE SOLCOT
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR ‚sta SOLICITUD ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
	SELE ITEPEC
    SET ORDE TO ITEPEC11
    GO TOP
    VKEY = iteSc.periodo+iteSc.numsC+iteSC.codfte
    seek VKEY
    
    if found()
       SCAN WHILE ITEPEC.PERIODO+ITEPEC.NUMSC+ITEPEC.CODFTE = VKEY
       IF RLOCK()
          rEPLACE Llave with ' ',estado with '00' &&,NUMSC WITH SPACE(4)
  	   ENDIF
  	   UNLOCK
  	   
  	   ENDSC
	else
	   do standby with 'Advertencia:No es ubicado la Pecosa,Revise'
	   vFun=.f.
	ENDif
	
    * SELE ITESC
    * SCAN FOR TIPART+SUBSTR(CODART,1,6)=SUBSTR(M.CODART,1,7)
    *   DO standby WITH "Espere un Momento,Por Favor..."
    *   DO Elimi_Item
    * ENDSCAN
   SELE SOLCOT
   IF  RLOCK()
       REPLACE ESTADO WITH '99' ,FECVER WITH DATE()
   ENDIF
   DO Vista
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Agreg_Item
*-----------------
AS=ALIAS()
sele itesc
IF F_appd()
  IF vOpcion=1
	  REPLACE NumSc  WITH m.NumSc ,;
    	      Periodo WITH m.Periodo ,;
        	  CodArt WITH Itepec.CodArt ,;
	          CodCad WITH Itepec.CodCad ,;
    	      CanSol WITH Itepec.Canreq ,;
        	  UniMed WITH Itepec.Coduni ,;
	          DesPed WITH Itepec.Descri ,;
    	      Perpec with Itepec.Periodo ,;
	          NumPec with itepec.Numpec,;
        	  Numord with itepec.Numord,;
        	  Codfte with itepec.Codfte,;
        	  TipDoc with 'PEC',;
      	     Detalle with itepec.Observa
     	     
    ELSE
	  REPLACE NumSc  WITH m.NumSc ,;
    	      Periodo WITH m.Periodo ,;
        	  CodArt WITH Itepec.CodArt ,;
	          CodCad WITH Itepec.CodCad ,;
    	      CanSol WITH vCanreq ,;
        	  UniMed WITH Itepec.Coduni ,;
	          DesSC  WITH Itepec.Descri ,;
    	      Perpec with Itepec.Periodo ,;
	          NumPec with itepec.Numpec,;
        	  Codfte with itepec.Codfte,;
        	  TipDoc WITH 'S/S'        	  
    ENDIF
  RETURN .T.
SELE &AS
ENDIF
SELE &AS
RETURN .F.


PROCEDURE Agreg_SER
*-----------------
sele itesc
IF F_appd()
  	REPLACE NumSc   WITH m.NumSc ,;
    	      Periodo WITH m.Periodo ,;
        	  CodCad  WITH solser.CodCad ,;
    	      numSS   with solser.Numss,;
        	  perSS   with solser.Periodo ,;
	          Codfte  with solser.Codfte,;
    	      Detalle with solser.detalle,;
    	      TipDoc  WITH 'S/S'
    RETURN .T.
ENDIF
RETURN .F.


PROCEDURE XMarca
*-----------------
DO CASE
   CASE llave='*'
        if rlock()
           REPLACE llave WITH '*', estado with '20'
        ENDIF
        RETURN .T.
   CASE llave=' '
        if rlock()
           REPLACE llave WITH ' ', estado with '  '
        ENDIF
        RETURN .T.
ENDCASE
RETURN .F.


PROCEDURE Lista
*--------------
ON KEY LABEL F2
SELECT SolCot
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   DO LISSOL
endif
select SolCot
go vtemp
DO VISTA
RETURN


PROCEDURE LisSol
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE 'Listado Solicitud de Cotizaciones' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE spac(25) to vOferta,vFoPag,vPlaEnt,vGaran
vCli = SPAC(4)
vAn  = Spac(2)
@ 01,01 SAY "     Solicitud N§ : "
@ 01,22 GET vCli  PICTURE '!!!!' VALID ASSIG()
@ 01,26 SAY "."
@ 01,27 GET vAn   PICTURE '!!'   VALID ValSol()

@ 03,01 SAY "         Condiciones : "
@ 04,01 SAY "Validez de la Oferta : "   GET  vOferta  PICTURE "@S20"
@ 05,01 SAY "       Forma de Pago : "   GET  vFoPag   PICTURE "@S20"
@ 06,01 SAY "    Plazo de Entrega : "   GET  vPlaEnt  PICTURE "@S20"
@ 07,01 SAY "            Garant¡a : "   GET  vGaran   PICTURE "@S20"

@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   SELE AUXCOT
   SET FILTER TO NumSc = vCli and Periodo = van and iif(vOpcion=1,tipact='B',tipact='S')
   SET RELATION TO PERIODO+NUMSC INTO SOLCOT
   SET RELATION TO PERIODO+NUMSC INTO ITESC  ADDI
   DEACTIVATE WINDOW STANDBY
   SET SKIP TO ITESC
   SET MEMO TO 60
   GO TOP
   IF !EOF()
      if vOpcion = 1
         do reporte with 2,"LisScB",' Solicitud de Cotizaciones Bienes'
      else
        do reporte with 2,"LisScoS",' Solicitud de Cotizaciones Servicios'
      endif
   ELSE
      DO STANDBY WITH VMENS08
   ENDIF
   SET FILTER TO
   SET RELATION TO
   SELE SOLCOT
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
	IF vOpcion=1
	    BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
    	NumSc  :H=' N§ ' ,;
	    FecSc  :H='Fecha' ,;
    	TipDoc :H='DOC' ,;
	    ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
    	itesc.desped :H=IIF(vOpcion=1,'Articulo ','Servicio ') :36 ,;
	    itesc.unimed :H='Unid' ,;
    	itesc.Cansol :H='Cantid'
    ELSE
	    BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
    	NumSc  :H=' N§ ' ,;
	    FecSc  :H='Fecha' ,;
    	TipDoc :H='DOC' ,;
	    ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
    	xx=mline(itesc.detalle,1)  :H=IIF(vOpcion=1,'Articulo ','Servicio ') :36 ,;
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
  ON KEY LABEL F2
  ON KEY LABEL F5
  ON KEY LABEL F8
  ON KEY LABEL F10
  SET ESCAPE &vesc
  RELEASE WINDOW wind_0
  RELEASE WINDOW wind_1
  RELEASE WINDOW wind_c1
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

function valsC
*-------------
parameter vnumSc
private vfun
vfun = .t.

m.numSc =padl(alltrim(str(vnumSc,4)),4,'0')
if m.numSc  = '0000' or empty(m.numSc)
   vfun = .f.
else
	sele solcot
	append blank
	IF F_APPD()
		repla periodo with m.periodo,numsc with numsc, estado with m.estado
	endif
	unlock
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


PROCEDURE XTrabaja_Hijo
*---------------------
 vFun = .t.
 ACTIVATE SCREEN
 HIDE MENU mMenu
 ON KEY LABEL F10 KEYBOARD CHR(23)
 DO CASE
    CASE vOpcion = 1
         vTempo = '°°°°°°° [*] Selecciona°°°°° [Space Bar] Desselecciona °°°°°° F10->Terminar°°°°°°'
         DO Logos WITH Rotulo1,vTempo
         SELECT ITEPEC
         BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2  FIELD ;
         llave       : H= 'C' :V=MARCA() AND LLAVE$'* ' ,;
         CodArt      : H= 'C¢digo'  :W=.F.,;
         NumPec      : H= 'Doc'  :W=.F.,;
         Descri      : H= 'Descripci¢n' :25 :W=.F.  ,;
         Coduni      : H= 'Uni'      :W=.F. :3,;
         Canreq      : H= 'Cantidad' :P='99,999.999' :W=.f. ,;
         PreUni      : H= 'Costo'    :P='9,999,999.999' :W=.f. ,;
         X=ROUND(CanReq*PreUni,5)    :H='Total'  :P='9,999,999.99' :W=.F.

         SET FILTER TO LLAVE='*'
         GO TOP

         SCAN
            vCanreq=0
            vCa1 = CodArt
            vCa2 = CodArt
            DO WHILE  vCa1 = vCa2
               vCanreq=vCanreq+Canreq
               SKIP
               vCa1 = CodArt
            ENDD
            SKIP -1
            DO AGREG_ITEM
            SELE ITEPEC
         ENDSCAN
         SET FILT TO
    CASE vOpcion = 2
         vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
         DO Logos WITH Rotulo1,vTempo
         SELECT SOLSER
         SET FILTER TO ESTADO='00'
         GO  TOP
         IF !EOF()
             BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2  FIELD ;
                NUMSS    :H='N§ S/S'  :W=.F.,;
                FECSS    :H='Fecha'  :W=.F.,;
                CODCAD   :H='Cod.Cadena' :W=.F. ,;
                xCODDEP=val_para(CodDep,'CODDEP','D') :15 :H='Dependencia':W=.F. ,;
                DESSS    :H='Descripci¢n':W=.F.
         ELSE
            DO STANDBY WITH 'No existe Solicitudes a Cotizar'
            vFun = .f.
         ENDIF
         SELECT SOLSER
         SET FILT TO
 ENDCASE

 ON KEY LABEL F10
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Solcot
 RETURN vfun

FUNCTION DETALLE
*---------------
vAlias = ALIAS()
SELE ITESC
SET MEMOWIDTH TO 56
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("OBSERVA")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle Solicitud Servicio ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("OBSERVA")
   ACTIVATE WINDOW OBSERVA SAME
ELSE
   ACTIVATE WINDOW OBSERVA NOSHOW
ENDIF

MODIFY MEMO DETALLE WINDOW OBSERVA

IF !WVISIBLE("OBSERVA")
   ACTIVATE WINDOW OBSERVA
ENDIF

RELEASE WINDOW OBSERVA
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba el OBSERVA '
ENDIF
SELECT (vAlias)
RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
SELE ITESC
IF !WEXIST("OBSERVA")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle Solicitud de Servicio ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("OBSERVA")
   ACTIVATE WINDOW OBSERVA SAME
ELSE
   ACTIVATE WINDOW OBSERVA NOSHOW
ENDIF

MODIFY MEMO DETALLE NOEDIT WINDOW OBSERVA

IF !WVISIBLE("OBSERVA")
   ACTIVATE WINDOW OBSERVA
ENDIF
RELEASE WINDOW OBSERVA
RETURN .T.

FUNCTION val_Cot   && VALIDA PROVEEDORES cotizaciones
*---------------
PARAMETER xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE medita, mmsg, malias, v_fun, _oldwind,_campo

medita = (PARAMETERS()>=2)
mmsg   = (PARAMETERS()=4) .AND.  _tipo

_campo = VARREAD()

malias = ALIAS()
SELECT promae
_oldwnd = WOUTPUT()

ON KEY LABEL F10
IF !medita
	SEEK xcod
	v_fun = IIF(FOUND(),nompro,"")
ELSE
	IF EMPTY(xcod)
		SET ORDE TO PROMAE2
     	ON KEY LABEL ENTER KEYBOARD CHR(23)
		ON KEY LABEL f2  DO prvbusC
		DEFINE WINDOW _xx FROM 3,3 TO 22,77
		BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona    ®F2¯ Busca   ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codprv   :H='C¢digo'     ,;
			nompro   :H='Nombre':45  ,;
			dirpro   :H='Direccci¢n' :25
		ON KEY LABEL f10
		ON KEY LABEL f2
		RELEASE WINDOW _xx
		SET ORDER TO promae1
		IF !EMPTY(_oldwnd)
			ACTIVATE WINDOW &_oldwnd
		ENDIF

		IF LASTKEY()=27
			v_fun = .F.
		ELSE
			xcod = codprv
			IF mmsg
				@ _x,_y SAY nompro
			ENDIF
			SELECT (malias)
			IF !_tipo
				REPLACE &_campo WITH  xcod
			ENDIF
			v_fun = .T.
		ENDIF
	ELSE
		SEEK xcod
		IF mmsg .AND. FOUND()
			@ _x,_y SAY nompro
		ENDIF
		v_fun = FOUND()
	ENDIF
ENDIF
SELECT (malias)
IF V_FUN
   SELECT AUXCOT
   IF RLOCK()
      replace codprv with promae.codprv ,  nompro with promae.nompro
   ENDIF   
ENDIF   
ON KEY LABEL ENTER
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN v_fun

FUNCTION prvbusC
*--------------
IF escolor
	DEFINE POPUP _mm FROM 16,54 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP _mm FROM 16,54 COLOR SCHEME c_popup
ENDIF
*ON KEY LABEL F10
DEFINE BAR 1 OF _mm PROMPT ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT ' Busqueda por \<Nombre '
DEFINE BAR 3 OF _mm PROMPT ' Busqueda por \<RUC    '
ON SELECTION POPUP _mm DEACTIVATE POPUP
orden = ORDER()
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
	CASE BAR()=1
		ACTIVATE WINDOW _funbus
		_cod = '0000'
		@01,02 SAY 'C¢digo: ' GET _cod PICTURE '!!!!'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 1
			SEEK ALLTRIM(_cod)
		ENDIF
	CASE BAR()=2
		ACTIVATE WINDOW _funbus
		_cod = SPACE(40)
		@01,02 SAY 'Nombre: ' GET _cod PICTURE '@S30'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 2
			SEEK UPPER( ALLTRIM(_cod) )
		ENDIF
	CASE BAR()=3
		ACTIVATE WINDOW _funbus
		_cod = SPACE(8)
		@01,02 SAY 'RUC: ' GET _cod PICTURE '@S8'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 3
			SEEK ALLTRIM(_cod)
		ENDIF
	OTHERWISE
		RELEASE POPUP _mm
		SET ORDER TO (orden)
*	 	ON KEY LABEL F10 &&KEYBOARD CHR(23)
		RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
IF !FOUND()
	DO standby WITH 'Proveedor no encontrado'
ENDIF
*ON KEY LABEL F10 &&KEYBOARD CHR(23)
RETURN

PROCEDURE VUSUA
***************
PARAMETER CSYS
PRIVATE ALI
ALI = ALIAS()
VKEY = ALLTRIM(CSYS)
SELE USU
SEEK VKEY
VFUN = NOMBRE
SELE (ALI)
RETURN VFUN

FUNCTION Observa
*---------------
vAlias = ALIAS()
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle Pecosa ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO DETALLE WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF

RELEASE WINDOW Observa
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci¢n '
ENDIF

RETURN .T.

FUNCTION VisObs
*--------------
vAlias = ALIAS()
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,18 TO 20,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle Pecosa ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO DETALLE NOEDIT WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
RETURN .T.

PROCEDURE SIHAY
*--------------
PRIVATE AS
AS=RECNO()
SEEK m.periodo+m.NumSc 
if found() AND RECNO()#AS
   do standby with 'La Solicitud de Cotiz. ya esta registrada'
   return .f.
endif   
return
