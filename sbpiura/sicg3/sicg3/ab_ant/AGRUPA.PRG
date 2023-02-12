*--------------------------------------------------------------------------
* Agrupa.Prg
* Registra los vales que se emiten en cada dependencia
* Estado de la factura
*   '00' Emitida     ** 	Este es el que se registra en el pecosa
*   '20' Con Correlativo
*   '50' Con O/C
*   '70' Devuelta
*   '99' Anulada
* Autor: LCD
*--------------------------------------------------------------------------
*- Abriendo Archivos

USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE Factura  IN 2   order tag Factura1 ALIAS Factura
USE vale     IN 3   order tag vales1   ALIAS vale
USE Regveh   IN 4   order tag RegVeh1  ALIAS vehi
USE calen    IN 5   order tag calen3   ALIAS calen
USE Pecosa   IN 6   order tag Pecosa1  ALIAS Pecosa
USE ItePec   IN 7   order tag ItePec1  ALIAS Itepec
USE Artmae   IN 8   order tag Artmae1  ALIAS Produ
USE IteArt   IN 9   order tag IteArt1  ALIAS Iteart
USE Promae   IN 10  order tag Promae1  ALIAS Promae
USE Maepre   IN 11  order tag Maepre1  ALIAS maepre

on key label F4 do imprimir
ON KEY LABEL F9 DO VISTA_DET

*- Mensajes de aviso al usuario
Vmens01 = ' Fichas : REVISION '
Vmens02 = 'Registro de fichas'
Vmens04 = 'Dicho ficha no fue encontrado'
Vmens05 = 'No existe ficha anterior'
Vmens06 = 'No existe ficha siguiente'
Vmens07 = '¨ Desea Anular ‚sta ficha ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta ficha ha sido anulada'
Vmens10 = 'La ficha ya est  Atendida'
Vmens11 = 'La ficha ha sido devuelta'
Vmens12 = 'El ficha ya tiene Pecosa'

SELECT Factura
GO BOTTOM

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular           Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 12,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 13,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: Vales         ®F9¯ Detalle : Item         ®F4¯ Imprime ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,66 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nular '    AT 24,54
*DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD anula  OF mMenu DO anula
*ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_1
 CLEAR
 @  1, 2 SAY "              Mes :"
 @  1,40 SAY "            Fecha :" 
 @  2, 2 SAY "           Fuente :"
 @  3, 2 SAY "       Tipo Gasto :"
 @  4, 2 SAY "      Dependencia :"
 @  5, 2 SAY "           C¢digo :"
 @  6, 2 SAY "         Producto :"
 @  7, 2 say "              Del :"
 @  7,40 say "               Al :"
 @  8, 2 say "     Documento N§ :"
 @  8,40 say "      Total Vales :"
 @  9, 2 say "  Total Requerido :"
 @  9,40 say "  Precio Unitario :"
 @ 10, 2 SAY "          Destino :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Factura
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF

 ACTIVATE WINDOW Wind_1
 SCATTER MEMVAR
 =val_CODCAD(ALLT(m.codcad),m.periodo,'C') 

@  1,22 SAY m.Nummes
@  1,60 SAY m.Fecfac
@  2,22 SAY Val_Para(m.CodFte,'CODFTE','V',22,40)

@  4,22 SAY val_para(m.CodDep,'CODDEP','V',22,49,7)

@  5,22 SAY m.codart
@  6,22 SAY m.descri
 
@  7,22 SAY m.fecini
@  7,60 SAY m.fecfin
@  8,22 SAY m.numdoc
@  8,60 SAY m.totvales picture '999'
@  9,22 SAY m.Canreq picture '999,999.999'
@  9,60 SAY m.preuni picture '999,999.999'
@ 10,22 SAY m.Destino

DO VISTA_HIJO
&& do total
RETURN

PROCEDURE TOTAL
*--------------
IF m.valtot # 0
	ACTIVATE WINDOW WIND_3
	@ 0,0 SAY m.valtot picture '999,999.999'
else	
    DEACTIVATE WINDOW WIND_3
endif	
return

PROCEDURE VISTA_HIJO
*-------------------
hide popup all
SELECT vale
SET ORDER TO VALES1
GO TOP
BROWSE ; 
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY alltrim(m.nummes) &&+alltrim(m.codart)+m.coddep TIMEOUT 0.0001 ;
   WINDOW Wind_2 FIELDS ;
   codval      : H= 'Vale' ,;
   fecval      : H= 'Fecha' ,;
   codprv      : H= 'Provd' ,;
   Codpla      : H= 'Placa' ,;
   Canreq      : H= 'Canreq' :p='9,999.999' ,;
   valtot      : H= 'Total'  :p='999,999.99' ,;
   xx=val_para(codchf,'CODCHF','D',22,60)  : H= 'Chofer'   
   SELE factura
RETURN

PROCEDURE VISTA_det
*------------------
hide popup all
ON KEY LABEL F9 
SELECT vale
SET ORDER TO VALES1
GO TOP
BROWSE ; 
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.nummes+m.codart+m.coddep ;
   WINDOW Wind_2 ;
   FIELDS;
   codval      : H= 'Vale' ,;
   fecval      : H= 'Fecha' ,;
   codprv      : H= 'Provd' ,;
   Codpla      : H= 'Placa' ,;
   Canreq      : H= 'Canreq' :p='9,999.999' ,;
   valtot      : H= 'Total' :p='999,999.99' ,;
   xx=val_para(codchf,'CODCHF','D',22,60)  : H= 'Chofer'   

SELE factura
ON KEY LABEL F9 DO VISTA_DET
DO VISTA
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE FACTURA
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO nummes+codart+coddep INTO VALE
 SET SKIP TO VALE
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 fecFac :H='Fecha' ,;
 Coddep :H='DEP',;
 CODART :H='Art¡culo',;
 vale.codval      : H= 'Vale' ,;
 vale.fecval      : H= 'Fecha' ,;
 vale.codprv      : H= 'Provd' ,;
 vale.Codpla      : H= 'Placa' ,;
 vale.Canreq      : H= 'Canreq' :p='9,999.999' ,;
 vale.valtot      : H= 'Total' :p='999,999.99' ,;
 xx=val_para(vale.codchf,'CODCHF','D',22,60)  : H= 'Chofer'  :25,;
 VALTOT :H='Total'
 
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SELE factura
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
vNum_pec = '    '
vCod_fte = '  '
ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero Pecosa: ' GET vPeriodo PICTURE '!!'
 @ 1,27 SAY '-' GET vNum_Pec PICTURE '!!!!' VALID vBusca()
 @ 1,33 SAY '-' GET vcod_fte PICTURE '!!' VALID VAL_PARA(vCod_Fte ,'CODFTE','C',33,20)
READ
DEACTIVATE WINDOW standby

 IF EMPTY(vNum_pec) .or. LASTKEY()=27
 	ON KEY LABEL F9 DO vista_det
    RETURN
 ELSE
    SEEK vPeriodo + vNum_pec &&+ alltrim(vcod_fte)
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
vnum_pec=padl(alltrim(vnum_pec),4,'0')
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
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
IF Estado = '99'
   * Anulada
   DO STANDBY WITH Vmens09
   RETURN
ENDIF
IF Estado = '50'
   * El pecosa ya tiene O/C
   DO STANDBY WITH Vmens12
   RETURN
ENDIF
DEACTIVATE WINDOW WIND_3
SELECT Factura
VTEMP = RECNO()
DO Pantalla
SCATTER MEMVAR 

@  1,22 GET m.Nummes PICTURE '!!' VALID val_para(m.Nummes,'FECMES',' ',22,50) AND ING_FEC() DISABLE
@  1,60 GET m.Fecfac 
@  4,22 GET M.Coddep PICTURE '!!!!!!' VALID val_para(m.CodDep,'CODDEP',' ',22,49,7) DISABLE
@  5,22 get m.codart picture '!!!!!!!!!!' VALID val_art(m.Codart,.T.)
@  6,22 get m.descri picture '@S56'  
@  7,22 get m.fecini 
@  7,60 get m.fecfin
@  8,22 GET m.numdoc 
@  9,60 get m.preuni  picture '999,999.999'
@ 10,22 GET m.destino picture '@S56' 

READ VALID VAL_READ()
 
IF LASTKEY() # 27 && Si se presion¢ Esc, no graba
   DO WHILE .T.
      Ok = corrije_Hj()
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
   
   IF Ok .AND. LASTKEY() # 27
      * verifica si estan correctos
      SELECT VALE
  	  SET ORDER TO VALEs2
	  SEEK ALLTRIM(m.nummes)+ALLTRIM(m.codart)+ALLTRIM(m.coddep)
	  vTotal = 0
	  vReq = 0
	  vIng = 0
	  SCAN FOR Nummes = alltrim(m.Nummes) and Codart = alltrim(m.codart) and Coddep=alltrim(m.coddep)
	       if flag='û'
		      IF RLOCK()
		         replace preuni with m.preuni, valtot with preuni*candesp,estado with '50'
		         ving = ving + 1
			  ENDIF
  	          vTotal = vtotal + vale.valtot
	          vreq =vreq + vale.candesp
		   endif   
	  ENDSCAN
	  SET ORDE TO VALES1
	  SELECT factura
	  m.valtot  = vReq*m.preuni
	  m.canreq  = vreq
	  m.totvales = ving
      m.user    = sys(0)
  	  m.user_fc = date()
	  m.user_TP = 'C'
	  if ving>0
        gather memvar
	  endif
    ELSE
       *- No pudo agregar la liquidaci¢n
       SELECT fACTURA
    ENDIF
endif    
DO Vista                    && Muestra nuevos datos
UNLOCK
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
DEACTIVATE WINDOW WIND_3
SELECT Factura
VTEMP = RECNO()
DO Pantalla

SCATTER MEMVAR BLANK
M.FECFAC = DATE()

@  1,22 GET m.Nummes PICTURE '!!' VALID val_para(m.Nummes,'FECMES',' ',22,50) AND ING_FEC()
@  1,60 GET m.Fecfac
@  2,22 GET m.CodFte PICTURE '!!' VALID VAL_PARA(m.CodFte ,'CODFTE',' ',22,30)
 
@  4,22 GET M.Coddep PICTURE '!!!!!!' VALID val_para(m.CodDep,'CODDEP',' ',22,49,7)

*@  5,22 get m.codart picture '!!!!!!!!!!' VALID val_art(m.Codart,.T.)
*@  6,22 get m.descri picture '@S56'  
*@  7,22 get m.fecini
*@  7,60 get m.fecfin
*@  8,22 GET m.numdoc 
*@  9,60 get m.preuni  picture '999,999.999'
*@ 10,22 GET m.destino picture '@S56' 

READ VALID VAL_READ()

IF LASTKEY() # 27 
   DO WHILE .T.
      Ok = Trabaja_Hj()
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
       
   IF Ok .AND. LASTKEY() # 27
        * verifica si estan correctos
	    SELECT VALE
  	    SET ORDER TO VALEs2
	    SEEK ALLTRIM(m.nummes)+ALLTRIM(m.codart)+ALLTRIM(m.coddep)
	    vTotal = 0
	    vReq = 0
	    vIng = 0
	    SCAN FOR Nummes = alltrim(m.Nummes) and Codart = alltrim(m.codart) and Coddep=alltrim(m.coddep)
	         if flag='û'
		        IF RLOCK()
		           replace estado with '50',preuni with m.preuni, valtot with preuni*candesp
		           ving = ving + 1
			    ENDIF
  	            vTotal = vtotal + vale.valtot
	            vreq =vreq + vale.candesp
			 endif   
		ENDSCAN
		SET ORDE TO VALES1
		SELECT factura
		m.valtot  = vReq*m.preuni
		m.canreq  = vreq
		m.estado  = '00'
		m.totvales = ving
        m.user    = sys(0)
  		m.user_fc = date()
	    m.user_TP = 'I'
	    if ving>0
	       if f_appd()
	          gather memvar
	       endif   
	    endif
    ELSE
       DO STANDBY WITH 'Proceso cancelado'
       SELE factura
    ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
   SELE factura
ENDIF
DO Vista
RETURN


PROCEDURE ING_FEC
*----------------
xmes=alltrim(m.nummes)
ymes=padl(alltrim(  str((val(m.nummes)+1),2))  ,2,'0')
m.fecini = ctod('01-&xmes-97')
m.fecfin = ctod('01-&ymes-97') - 1
return

PROCEDURE Trabaja_Hj
*-------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°F11->Marca°°°°°°°°°°°°°°F12->Desmarca°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F11 DO Marca
 ON KEY LABEL F12 DO Desmarca
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELE vale
 set orde to vales1
 GO TOP
 SET FILT TO BETWEEN(fecval,m.fecini,m.fecfin)
 BROWSE NOREFRESH NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY ALLTRIM(m.nummes)+ALLTRIM(m.codart)+ALLTRIM(m.coddep) FIELD ;
     FLAG :H='OK' :W=.F.,;
	 CODVAl :H=' N§ ' :W=.F. ,;
	 Fecval :H='Fecha'  :W=.F.,;
	 codpla :H='Placa' :W=.F. ,;	 
	 CanReq : H='Cantid':P='99,999.99' :W=.F.,;
	 CanDesp: H='Despac':P='99,999.99'  ,;
	 valtot :H='Valor' ,;
 	 prv=val_prv(codprv) :H='Proveedor' :32 :w=.f.
 set filter to 
 ON KEY LABEL F11
 ON KEY LABEL F12
 ON KEY LABEL F10

 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Vale
 set orde to vales1
RETURN 


PROCEDURE Corrije_Hj
*-------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°F5->Agrega°°°°°°°°°°°°°°F12->Desmarca°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 on key label f5 do AGREG_VAL
 on key label f12 do desmarca
 SELE vale
 set orde to vales1
 GO TOP
 SET FILT TO BETWEEN(fecval,m.fecini,m.fecfin)
 BROWSE NOREFRESH NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY ALLTRIM(m.nummes)+ALLTRIM(m.codart)+ALLTRIM(m.coddep) FIELD ;
     FLAG :H='OK' :W=.F.,;
	 CODVAl :H=' N§ ' :W=.F. ,;
	 Fecval :H='Fecha'  :W=.F.,;
	 codpla :H='Placa' :W=.F. ,;	 
	 CanReq : H='Cantid':P='99,999.99' :W=.F.,;
	 CanDesp: H='Despac':P='99,999.99'  ,;
	 valtot :H='Valor' ,;
 	 prv=val_prv(codprv) :H='Proveedor' :32 :w=.f.
 set filter to 
 ON KEY LABEL F10
 ON KEY LABEL F12
 ON KEY LABEL F5
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Vale
 set orde to vales1
RETURN 

PROCEDURE AGREG_VAL
*-------------------
OQ=trabaja_hj()
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°[Enter] Continua°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
RETURN


PROCEDURE Anula
*---------------
 SELECT FACTURA
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 if YESNO('¨ Desea ANULAR ‚sta Ficha ?')
      SELECT VALE
  	  SET ORDER TO VALEs1
	  SEEK ALLTRIM(m.nummes)+ALLTRIM(m.codart)+ALLTRIM(m.coddep)
	  vTotal = 0
	  vReq = 0
	  vIng = 0
	  SCAN FOR Nummes = alltrim(m.Nummes) and Codart = alltrim(m.codart) and Coddep=alltrim(m.coddep)
           IF RLOCK()
		      replace preuni with 0, valtot with 0, estado with '00', flag with  ' '
		   ENDIF
	  ENDSCAN
	  SET ORDE TO VALES1
	  SELECT factura
	  IF RLOCK()
	     DELETE NEXT 1
	  ENDIF   
 endif   
 sele factura   
 DO Vista
 
 UNLOCK
 RETURN

PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON
SELE VALE
SET ORDE TO VALES5
SELECT FACTURA
VCON = RECNO()
SCATTER MEMVAR
vNumc = ALLTRIM(m.nummes)+ALLTRIM(m.codart)+ALLTRIM(m.coddep)
SET RELATION TO nummes+codart+coddep INTO vale
set filt to nummes+codart+coddep=vNumc
SET SKIP TO vale
if eof()
   do standby with vmens08
   return
else
   if !yesno('¨Reporte detallado?')
      do reporte with 2,"agrupa",' Agrupa Vales ',1
   else
      do reporte with 2,"agrupat",' Agrupa Vales Detallado ',1
   endif   
endif
set skip to
set filter to
SET RELATION TO 
SELECT factura
GO VCON
DO VISTA
RETURN


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
vFte    = SPAC(2)
vCodmes = SPAC(2)
vPeriod = SPAC(2)
vCoddep = SPAC(6)
vCodFte = SPAC(2)

@ 01,01 SAY "Todas las Pecosas : " GET vToPec  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToPec,2,22)  and assig()
@ 02,01 SAY "           Pecosa : "
@ 02,22 GET vNumpec  PICTURE '!!!!' WHEN vToPec = 2  &&VALID Padl(alltrim(vNumpec),4,'0')
@ 02,27 get vPeriod  Picture '!!'   WHEN vToPec = 2 
@ 02,30 SAY '-'
@ 02,31 GET vFte     PICTURE '!!' VALID VAL_PARA(vfte,'CODFTE','C') AND ValPecO() WHEN vToPec = 2

@ 04,01 SAY "  Todos las Meses : " GET vTomes  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTomes,5,22)  WHEN vToPec = 1
@ 05,01 SAY "              Mes : "
@ 05,22 GET vCodmes  PICTURE '!!'  VALID VAL_PARA(vCodMes,'FECMES','C') WHEN vToPec = 1 AND vTomes = 2

@ 07,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTofue,8,22)  WHEN vToPec = 1
@ 08,01 SAY "           Fuente : "
@ 08,22 GET vCodFte  PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE','C')  WHEN vToPec = 1 and vTofue =2

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
SEEK '97'+vNumPec &&+alltrim(vFte)
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
  RELEASE    WINDOW wind_2
  RELEASE    WINDOW wind_3
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
parameter vrec,vfun,vali,AS
vfun = .t.
vrec = recno()
vali = alias()
select pecosa
set orde to pecosa1
SEEK '970482'
AS = RECNO()
go as
numr = 0482
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
else
   IF F_APPD() 
     replace periodo with m.periodo,numpec with m.numpec
     gh = recno()
   ENDIF 
   UNLOCK
endif

SELECT Parma
SEEK 'CORREL' + 'PECOSA'
REPLACE NumEnt WITH NumR 
sele (vali)
return vfun


FUNCTION XVALART
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
          tipcdr  WITH 'S'
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
         IF YY and m.Tipdoc='S'
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

MODIFY MEMO OBSERVa WINDOW OBSERVA

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

MODIFY MEMO OBSERVa NOEDIT WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
RETURN .T.

FUNCTION Val_Art  && Articulos
*---------------
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
   SET ORDER TO ITEART5
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
   v_ent = found()
ELSE
   IF EMPTY(xcod) 
      SET ORDER TO ITEART6
      GO TOP
      ACTIVATE SCREEN
      ON KEY LABEL F10 KEYBOARD CHR(23)
      ON KEY LABEL F2 DO FunBusDet
      ON KEY LABEL F5
      ON KEY LABEL F8
      DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
      BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
        CodArt   :H='C¢digo'    :w=.f. ,;
        Descri   :H='Nombre':70   ,;
        CodUni   :H='Unidad':7   
      ON KEY LABEL F10
      ON KEY LABEL F2
      RELEASE WINDOW _BusArt
      SET ORDER TO 3

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
   ELSE
   SET ORDER TO ITEART5
   SEEK xcod
   v_ent = found()
   v_FUN = found()
   ENDIF
ENDIF
SELECT VALE
M.CODART = ITEART.CODART
M.DESCRI = ITEART.DESCRI
M.UNIMED = ITEART.CODUNI
SELECT (mAlias)
SET ORDE TO (ORD)
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL

*IF !V_ENT
   RETURN v_FUN
*ELSE
*   RETURN v_ENT
*ENDIF   

PROCEDURE Marca
*--------------
REPLACE flag with 'û'
return

PROCEDURE DesMarca
*-----------------
REPLACE flag with ' ',estado with '00'
return
