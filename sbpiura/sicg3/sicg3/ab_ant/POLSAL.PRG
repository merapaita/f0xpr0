*--------------------------------------------------------------------------
* PolSal.Prg
* Registra las Polizas de Salida que emiten en Abastecimiento
* Estado :
*   '00' Emitida     ** 	Este es el que se registra en el 
*   '50' Liquidada
*   '99' Anulada
*--------------------------------------------------------------------------
*- Abriendo Archivos
Set excl off
PUBLIC vperiodo,vcodfte

USE OrdCom   IN 1   order tag OrdCom2  ALIAS Orden
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE Pecosa   IN 5   order tag Pecosa1  ALIAS Pecosa
USE Itepec   IN 6   order tag ItePec1  ALIAS Itepec
USE Promae   IN 8   order tag Promae1  ALIAS Promae
USE calen    IN 9   order tag calen1   ALIAS calen
USE Iteart   IN 10  order tag iteart1  ALIAS iteart
USE Poliza   IN 11  order tag poliza2  ALIAS poliza
USE Itepol   IN 12  order tag itepol2  ALIAS itepol
USE Asiaut   IN 13  order tag asiaut1  ALIAS asig
USE Parkar   IN 14  order tag parkar1  ALIAS parkar
USE Cuentas	 IN 20	ORDER TAG Cuentas1 ALIAS Cuen
USE IteUsuOp IN 0 ORDER TAG IteUsuOp1 ALIAS SubOp

*USE IteOc    IN 2   order tag IteOc1   ALIAS IteOc
*USE AuxCot   IN 7   order tag AuxCot1  ALIAS Auxcot

*- Mensajes de aviso al usuario
Vmens01 = ' P¢lizas de Salida : REVISION '
Vmens02 = 'Registro de P¢liza de Salida'
Vmens04 = 'Dicho P¢liza no fue encontrado'
Vmens05 = 'No existe P¢liza anterior'
Vmens06 = 'No existe P¢liza siguiente'
Vmens07 = '¨ Desea Liquidar ‚sta P¢liza ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta P¢liza ha sido anulado'
Vmens10 = 'La P¢liza ya est  Atendido'
Vmens11 = 'La P¢liza ha sido devuelto'
Vmens12 = 'La P¢liza ya tiene O/C'

ON KEY LABEL F7 DO VISTA_COR
SELECT Poliza
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
* DEFINE WINDOW CLAVE FROM 18,30 TO 22,60 DOUBLE SHADOW COLOR SCHEME 7

 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Elimina  Ingresa            Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 09,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 10,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: P¢liza    ®F9¯ Detalle : Item  ®F7¯ Verifica Precio' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,65 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD Anula   OF mMenu PROMPT 'An\<Ula   '    AT 24,54
*DEFINE PAD liqui   OF mMenu PROMPT 'Li\<Quida '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD Anula  OF mMenu DO Anula
*ON SELECTION PAD liqui  OF mMenu DO liqui
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_1
 CLEAr
 @  1, 2 SAY "          Periodo :"
 @  2, 2 SAY "          Emisi¢n :"
 @  3, 2 SAY "    N£mero P¢liza :"
 @  4, 2 SAY "         Fte.Fto. :"
*@  5, 2 SAY " Fecha Liquidaci¢n:" 
 @  5, 2 SAY "         Vigencia :"
 @  6, 2 SAY "         Analisis :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Poliza
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 ON KEY LABEL F9 DO vista_det
 SCATTER MEMVAR

 @  0,60 SAY vEstpol(m.Estado)
 @  1,22 SAY m.periodo 
 @  2,22 SAY m.FecEmi
 @  3,22 SAY m.NumPol
 @  4,22 SAY val_para(m.Codfte,'CODFTE','V',22,20,4)
*@  5,22 SAY m.fecliq 
 @  5,22 SAY m.fecini 
 @  5,37 SAY m.fecfin
 @  6,22 SAY val_para(m.analisis,'ANALIS','V',22,15,3)
 DO VISTA_HIJO
 DO TOTAL
IF !vFlag$'J*'
	DO SubOpc
ENDIF
 RETURN

PROCEDURE VISTA_HIJO
*-------------------
hide popup all
SELECT ItePol
GO TOP
do case
   case ALLTRIM(m.analisis) = 'G'
	   BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo+m.NumPol TIMEOUT 0.001 ;
	   WINDOW Wind_2 ;
	   FIELDS;
	   DesPro : h='D' :1,;   
	   TipRef : h='REF' ,;
	   PerRef : h='Pr'  ,;
	   NumRef : h='N§Rf',;
	   fecref : h='FecRef',;
	   Codgen : h='GG' ,;
	   Descri : h='Definici¢n':36 ,;
	   ValTot : h='Total'
   case ALLTRIM(m.analisis) = 'C'   
	   BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo+m.NumPol TIMEOUT 0.001 ;
	   WINDOW Wind_2 ;
	   FIELDS;
	   DesPro : h='P':1,;   
	   TipRef : h='REF' ,;
	   PerRef : h='Pr'  ,;
	   NumRef : h='N§Rf',;
	   Fecref : h='FecRef',;
	   Codgen : h='GG' ,;
	   CODCLA : H='Clf' ,;
	   Descri : h='Definici¢n':33 ,;
	   ValTot : h='Total'
   case ALLTRIM(m.analisis) = 'A'   
       SEEK m.Periodo+m.NumPol
	   BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo+m.NumPol TIMEOUT 0.001 ;
	   WINDOW Wind_2 ;
	   FIELDS;
	   DesPro : h='P':1,;   
	   TipRef : h='REF' ,;
	   PerRef : h='Pr'  ,;
	   NumRef : h='N§Rf',;
	   Codcad : h='Cadena',;	   
	   Fecref : h='FecRef',;
	   Codgen : h='GG' ,;
	   CODCLA : H='Clf' ,;
	   coddet : H='Det' ,;	   
	   Descri : h='Definici¢n':29 ,;
	   ValTot : h='Total'
  ENDCASE   
  SELE Poliza
RETURN



PROCEDURE VISTA_COR
*------------------
hide popup all
SELECT ItePol
ON KEY LABEL F7
GO TOP
do case
   case ALLTRIM(m.analisis) = 'G'
	   BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.Periodo+m.NumPol ;
	   WINDOW Wind_2 ;
	   FIELDS;
	   DesPro : h='D' :1 :W=.F.,;   
	   TipRef : h='REF' :W=.F.,;
	   PerRef : h='Pr' :W=.F. ,;
	   NumRef : h='N§Rf':W=.F.,;
	   fecref : h='FecRef':W=.F.,;
	   Codgen : h='GG' :W=.F.,;
	   Descri : h='Definici¢n':36 :W=.F.,;
	   ValTot : h='Total'
   case ALLTRIM(m.analisis) = 'C'   
	   BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH  KEY m.Periodo+m.NumPol ;
	   WINDOW Wind_2 ;
	   FIELDS;
	   DesPro : h='P':1 :W=.F.,;   
	   TipRef : h='REF':W=.F. ,;
	   PerRef : h='Pr':W=.F.,;
	   NumRef : h='N§Rf':W=.F.,;
	   Fecref : h='FecRef' :W=.F.,;
	   Codgen : h='GG':W=.F. ,;
	   CODCLA : H='Clf':W=.F. ,;
	   Descri : h='Definici¢n':33 :W=.F.,;
	   ValTot : h='Total'
   case ALLTRIM(m.analisis) = 'A'   
	   BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH  KEY m.Periodo+m.NumPol ;
	   WINDOW Wind_2 ;
	   FIELDS;
	   DesPro : h='P':1 :W=.F.,;   
	   TipRef : h='REF':W=.F. ,;
	   PerRef : h='Pr' :W=.F. ,;
	   NumRef : h='N§Rf' :W=.F.,;
	   Fecref : h='FecRef':W=.F.,;
	   Codgen : h='GG' :W=.F.,;
	   CODCLA : H='Clf':W=.F. ,;
	   coddet : H='Det':W=.F. ,;	   
	   Descri : h='Definici¢n':29 :W=.F. ,;
	   ValTot : h='Total'
  ENDCASE   
  
  select itepol 
  SEEK m.periodo+m.numpol
  vTotalz = 0
  scan while itepol.periodo+itepol.numpol=m.periodo+m.numpol
       vTotalz = vtotalz + valtot
  endscan          
  SELE Poliza
  REPLACE VALTOT WITH VTOTALZ
  
  ON KEY LABEL F7 DO VISTA_COR
  DO VISTA
RETURN



PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
@ 0,0 SAY m.Valtot picture '9,999,999.99'
return


PROCEDURE VISTA_DET
*------------------
hide popup all
ON KEY LABEL F9 
ON KEY LABEL F4 DO AgrIGV
SELECT ItePol
GO TOP
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH  KEY m.Periodo+m.NumPol ;
   WINDOW Wind_2 ;
   FIELDS;
   DesPro : h='D' :1,;   
   TipRef : h='REF' ,;
   PerRef : h='Pr'  ,;
   NumRef : h='N§Rf',;
   fecref : h='FecRef',;
   Codgen : h='GG' ,;
   Descri : h='Definici¢n':36 ,;
   IGV	  : H= 'IGV' ,;
   ValTot : H= 'Total' ,;
   dcuenta: H= 'Al Debe' :V=Val_Cta('D') :F,;
   hcuenta: H= 'Al Haber':V=Val_Cta('H') :F

   SELE Poliza
   ON KEY LABEL F9 DO vista_det   
   ON KEY LABEL F4
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE Poliza
private vtemp
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF LASTKEY()=27
    sele poliza
    DO VISTA
    RETURN
 ENDIF     
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumPol :H=' N§ ' ,;
 FecEmi :H='Fecha' ,;
 Est = IIF(Estado= '00','Pend',IIF(Estado = '51','Cont','    ')) :H='Estd' ,;
 Codfte :H='Fte ' ,;
 FecIni :H='Del :' ,;
 FecFin :H=' al :' ,;
 Fecemi :H='Emision',;
 Valtot :H='Total' :P='9,999,999.99'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SELE Poliza
*SET RELA TO
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
vNum_pol = '    '
ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero P¢liza: ' GET vPeriodo PICTURE '!!'
 @ 1,27 SAY '-' GET vNum_Pol PICTURE '!!!!' &&LID vBusca()
READ
DEACTIVATE WINDOW standby

 IF EMPTY(vNum_pol) .or. LASTKEY()=27
    RETURN
 ELSE
    SEEK vPeriodo + ALLTRIM(vNum_pol) 
    IF !FOUND()
       DO standby WITH Vmens04
       GOTO Vtemp
    ELSE
       DO Vista
    ENDIF
 ENDIF
RETURN


PROCEDURE Anter
*--------------
SELE Poliza
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
SELE Poliza
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

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
PRIVATE VRE
DO PANTALLA
SELECT Poliza

vPeriodo = RIGHT(DTOC(DATE()),2)
vNumMes  = space(2)
vCodFte  = space(2)
vRe=recno()

SCATTER MEMVAR BLANK
store date() to m.fecini,m.fecfin,m.fecemi
m.periodo=substr(dtoc(date()),9,2)
= REPASA()
* @  1,22 SAY vPeriodo
  @  1,22 say m.periodo
 @  2,22 GET m.FecEmi
 @  3,22 GET m.NumPol valid vnumpol()  DISABLE
 @  3,22 SAY M.NUMPOL COLOR "GBR+/R"
 @  4,22 GET m.Codfte picture '!!' valid val_para(m.Codfte,'CODFTE',' ',22,20,4)
*@  5,22 GET m.fecliq  DISABLE
 @  5,22 GET m.fecini
 @  5,37 GET m.fecfin
 @  6,22 GET m.analisis picture '!'  valid val_para(m.analisis,'ANALIS',' ',22,15,3)
 READ VALID VAL_READ()
*--------------------------------
IF LASTKEY()<>27
	selec parma
	seek "CORRELPOLSAL"
	CORREL=NUMENT+1
	REPLACE NUMENT WITH CORREL
	
endif
SELEC POLIZA
*--------------------------------
 IF LASTKEY() # 27
    DO WHILE .T.
       Ok = Ingresa_H()
       IF LASTKEY() # 27 AND OK
         IF YESNO('¨ Confirme el ingreso ?')
            EXIT
         ENDIF
       ELSE
         DO STANDBY WITH ' Cancelado el Ingreso ..'
         OK = .F.
         EXIT
       ENDIF
   ENDDO

   IF Ok .AND. LASTKEY() # 27
      select itepol 
      SEEK m.periodo+m.numpol
      vTotalz = 0
      scan while itepol.periodo+itepol.numpol=m.periodo+m.numpol
           vTotalz = vtotalz + valtot
           sele pecosa
           seek m.periodo+itepol.numref+m.codfte
           if rlock()
**ojo        *replace pecosa.estado with '51'
           endif   
           UNLOCK
      endscan          
      SELECT Poliza
      m.valtot = vtotalz
      m.tippol = 'S'
      m.Estado = '00'
      if f_appd()
        gather memvar
      endif
   ELSE
      select itepol 
      SEEK m.periodo+m.numpol
      if Found()
      scan while itepol.periodo+itepol.numpol=m.periodo+m.numpol
           if rlock()
              dele next 1
           endif
      endscan          
      endif
      SELE POLIZA
      IF !EOF()
        GO VRE
      ENDIF
      DO STANDBY WITH ' No se pudo crear la Poliza '   
   ENDIF    
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    SELE POLIZA
    GO VRE
 ENDIF
 UNLOCK ALL
 SELECT Poliza
 DO Vista
 RETURN


PROCEDURE ingresa_H
*------------------
private ok,ok1
ACTIVATE SCREEN
HIDE MENU mMenu
ON KEY LABEL F4 DO AgrIGV
ACTIVATE WINDOW standby
xcolor = '*' + SET('COLOR')
@ 01,03 SAY 'Espere un momento ... Reporte en proceso ...' COLOR &xcolor

ok = pasa()
OK1=oK
*OK1=PASAPM()  		&& Tiene que ver con Revajas

DEACTIVATE WINDOW standby
vTempo = '°°°°°°°°°°Presione ®F10¯ para salir grabando o  ®Esc¯ para cancelar°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
if (ok or ok1)
   SELE ITEPOL
   SEEK m.periodo+m.numpol
   do case
      case ALLTRIM(m.analisis) = 'G'
	  BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY m.periodo+m.numpol FIELD ;
	      DesPro      : H= iif(m.tippol='E','Des','Prc') ,;
	      TipRef      : H= 'Ref'   :W=.F. ,;
	   	  Perref      : H= 'Mes'   :W=.F. ,;
	      Numref      : H= 'N§O/C' :W=.F. ,;
		  Codcad	  : H= 'Cadena':W=.F. ,;
	      Codgen      : H= 'GG'    :W=.F. ,;
	      Descri      : H= 'Descripci¢n' :w=.f. :35 ,;
	      IGV		  : H= 'IGV' ,;
	      VALTOT      : H= 'Total' ,;
	      dcuenta     : H= 'Al Debe' :V=Val_Cta('D') :F,;
	      hcuenta     : H= 'Al Haber':V=Val_Cta('H') :F
      case ALLTRIM(m.analisis) = 'C'
	  BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY m.periodo+m.numpol FIELD ;
	      DesPro      : H= iif(m.tippol='E','Des','Prc') ,;
	      TipRef      : H= 'Ref'   :W=.F. ,;
	   	  Perref      : H= 'Mes'   :W=.F. ,;
	      Numref      : H= 'N§O/C' :W=.F. ,;
		  Codcad	  : H= 'Cadena':W=.F. ,;
	      Codgen      : H= 'GG'    :W=.F. ,;
	      Codcla      : H= 'Clf'   :W=.f. ,;
	      Descri      : H= 'Descripci¢n' :w=.f. :32 ,;
	      IGV		  : H= 'IGV' ,;
	      VALTOT      : H= 'Total' ,;
	      dcuenta     : H= 'Al Debe' :V=Val_Cta('D') :F,;
	      hcuenta     : H= 'Al Haber':V=Val_Cta('H') :F
      case ALLTRIM(m.analisis) = 'A'
	  BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY m.periodo+m.numpol FIELD ;
	      DesPro      : H= iif(m.tippol='E','Des','Prc') ,;
	      TipRef      : H= 'Ref'   :W=.F. ,;
	   	  Perref      : H= 'Mes'   :W=.F. ,;
	      Numref      : H= 'N§O/C' :W=.F. ,;
		  Codcad	  : H= 'Cadena':W=.F. ,;
	      Codgen      : H= 'GG'    :W=.F. ,;
	      Codcla      : H= 'Clf'   :W=.f. ,;	      
	      coddet      : H= 'Det'   :W=.f. ,;
	      Descri      : H= 'Descripci¢n' :w=.f. :29 ,;
	      IGV		  : H= 'IGV' ,;
	      VALTOT      : H= 'Total' ,;
	      dcuenta     : H= 'Al Debe' :V=Val_Cta('D') :F,;
	      hcuenta     : H= 'Al Haber':V=Val_Cta('H') :F
   endcase           	      
   ON KEY LABEL F10
   ACTIVATE SCREEN
   vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
   DO Logos WITH Rotulo1,vTempo
   SHOW MENU mMenu
   SELECT Poliza
else   
   DO STANDBY WITH ' No existe Registros a Procesar ...'
endif
ON KEY LABEL F4
RETURN (OK or Ok1)


PROCEDURE AgrIgv
*---------------
IF EMPTY(igv)
	REPLACE Igv WITH ValTot - ROUND(ValTot/1.18,2)
 ELSE
	REPLACE Igv WITH 0
ENDIF
RETURN 

PROCEDURE pasa
*-------------
private vfun,malias
malias=alias()
SELECT itepec
set relation to periodo+numpec+codfte into pecosa 
INDEX ON PERIODO+NUMPEC+CODFTE+CODART TO PP FOR codfte=alltrim(m.Codfte) and BETWEEN(FecDesp,m.FecIni,m.FecFin) AND ITEPEC.ESTADO='5'
*&&AND ITEPEC.tipPEC#'B' and ITEPEC.tipfun = alltrim(m.tipfun)
GO TOP
IF EOF()
   vfun = .f.
else
  DO CASE
     CASE ALLTRIM(m.analisis)='G'
		  SCAN
*		   vPart1 = left(ItePEC.CodArt,2)+numPEC+codfte
*		   vPart2 = left(ItePEC.CodArt,2)+numPEC+codfte
    	   vTot = 0
*		   DO WHILE  vPart1 = vPart2
		      vTot = vTot + (Preuni*candesp)*(100-PctDto)/100*(100+pctIgv)/100
**		      vTot = vTot + iif(COSMED#0,cosmed*candesp,Preuni*candesp)
*	          SKIP
*	          vPart1 = left(ItePEC.CodArt,2)+numPEC+codfte
*		   ENDDO
*           SKIP -1
		   DO AGREG_pol
		   SELE ITEPEC
	     ENDSCAN
         vfun = .t. 
     CASE ALLTRIM(m.analisis)='C'
		  SCAN
*		   vPart1 = left(ItePEC.CodArt,6)+numPEC+codfte
*		   vPart2 = left(ItePEC.CodArt,6)+numPEC+codfte
    	   vTot = 0
*		   DO WHILE  vPart1 = vPart2
		      vTot = vTot + (Preuni*candesp)*(100-PctDto)/100*(100+pctIgv)/100
*	          SKIP
*	          vPart1 = left(ItePEC.CodArt,6)+numPEC+codfte
*		   ENDDO
*           SKIP -1
		   DO AGREG_pol
		   SELE ITEPEC
	     ENDSCAN
         vfun = .t. 
    CASE ALLTRIM(m.analisis)='A'
		SCAN 
*			vPart1 = left(ItePEC.CodArt,10)+numPEC+codfte
*			vPart2 = left(ItePEC.CodArt,10)+numPEC+codfte
			vTot = 0
*			DO WHILE  vPart1 = vPart2
				vTot = vTot + (Preuni*candesp)*(100-PctDto)/100*(100+pctIgv)/100
*				vTot = vTot + iif(COSMED#0,cosmed*candesp,Preuni*candesp)
*				SKIP
*				vPart1 = left(ItePEC.CodArt,10)+numPEC+codfte
*			ENDDO
*			SKIP -1
			DO AGREG_pol
			SELE ITEPEC
		ENDSCAN
		vfun = .t.
	ENDCASE
  vfun = .t. 
endif
select (malias)
return vfun

 
PROCEDURE pasaPM
*---------------
private vfun,malias
malias=alias()
SELECT itepec
set relation to periodo+numpec+codfte into parkar
INDEX ON PERIODO+NUMPEC+CODFTE+CODART TO PP FOR codfte=alltrim(m.Codfte) and BETWEEN(parkar.FecDesp,m.FecIni,m.FecFin) AND ITEPEC.ESTADO='5' &&and parkar.tippec='P'
GO TOP
IF EOF()
  *DEACTIVATE WINDOW STANDBY
  *DO STANDBY WITH ' No existe Registros a Procesar '
   vfun = .f.
else   
 DO CASE 
     CASE ALLTRIM(m.analisis)='G'
		  SCAN 
		   vPart1 = left(ItePEC.CodArt,2)+numPEC+codfte
		   vPart2 = left(ItePEC.CodArt,2)+numPEC+codfte
    	   vTot = 0
		   DO WHILE  vPart1 = vPart2
		      vTot = vTot + costot*IIF(ITEPEC.TIPREB='-',-1,1)
	          SKIP
	          vPart1 = left(ItePEC.CodArt,2)+numPEC+codfte
		   ENDDO
           SKIP -1
		   DO AGREG_pm
		   SELE ITEPEC
	     ENDSCAN
         vfun = .t. 
     CASE ALLTRIM(m.analisis)='C'
		  SCAN 
		   vPart1 = left(ItePEC.CodArt,6)+numPEC+codfte
		   vPart2 = left(ItePEC.CodArt,6)+numPEC+codfte
    	   vTot = 0
		   DO WHILE  vPart1 = vPart2
		      vTot = vTot + costot*IIF(ITEPEC.TIPREB='-',-1,1)
	          SKIP
	          vPart1 = left(ItePEC.CodArt,6)+numPEC+codfte
		   ENDDO
           SKIP -1
		   DO AGREG_pm
		   SELE ITEPEC
	     ENDSCAN
         vfun = .t. 
    CASE ALLTRIM(m.analisis)='A'
		  SCAN 
		   vPart1 = left(ItePEC.CodArt,10)+numPEC+codfte
		   vPart2 = left(ItePEC.CodArt,10)+numPEC+codfte
    	   vTot = 0
		   DO WHILE  vPart1 = vPart2
		      vTot = vTot + costot*IIF(ITEPEC.TIPREB='-',-1,1)
	          SKIP
	          vPart1 = left(ItePEC.CodArt,10)+numPEC+codfte
		   ENDDO
           SKIP -1
		   DO AGREG_pm
		   SELE ITEPEC
	     ENDSCAN
         vfun = .t. 
  ENDCASE
  vfun = .t. 
endif
select (malias)
return vfun

PROCEDURE Agreg_Pol
*------------------
private as
AS=ALIAS()
sele itepol
IF F_appd()
   REPLACE  Periodo  with m.Periodo ,;
   			NumPol with m.Numpol,;
            TipPol with 'S',;
            TipRef with 'PEC',;
            Tippec with pecosa.tippec,;
            Numref with itepec.Numpec,;
            Codcad with itepec.codcad,;
            Codgen with LEFT(itepec.codart,2),;
            Codcla with substr(itePEC.codart,4,3),;
            Coddet with substr(itePEC.codart,8,3),;
   			DESCRI WITH IIF(ALLTRIM(m.analisis)='G',substr(val_para('B'+alltrim(LEFT(itePec.codart,2)),'CODGEB','D',22,60),4,100),IIF(ALLTRIM(m.analisis)='C',buscart(),VAL_ARTC('B'+ITEPEC.codArt))),;
            Valtot with vtot,;
            Destino with pecosa.destino,;
            estado with '00',;
            coddep with pecosa.coddep,;
            fecref with pecosa.fecdesp,;
            Despro WITH '1' ,;
            NumOc with itepec.numoc
   UNLOCK
   DO UBICTA
   SELE &AS
   RETURN .T.
ENDIF
SELE &AS
RETURN .F.


PROCEDURE Agreg_Pm
*-----------------
private as
AS=ALIAS()
sele itepol
IF F_appd()
   REPLACE  Periodo  with m.Periodo ,;
   			NumPol with m.Numpol,;
            TipPol with 'S',;
            TipRef with 'PMK',;
            Tippec with parkar.tippec,;
            Numref with itepec.Numpec,;
			Codcad with itepec.codcad,;            
            Codgen with LEFT(itepec.codart,2),;
            Codcla with substr(itePEC.codart,4,3),;
            Coddet with substr(itePEC.codart,8,3),;            
   			DESCRI WITH IIF(ALLTRIM(m.analisis)='G',substr(val_para('B'+alltrim(LEFT(itePec.codart,2)),'CODGEB','D',22,60),4,100),IIF(ALLTRIM(m.analisis)='C',buscart(),VAL_ARTC('B'+ITEPEC.codArt))),;
            Valtot with vtot,;
            Destino with parkar.destino,;
            estado with '00',;
            coddep with parkar.coddep,;
            fecref with parkar.fecdesp,;
            Despro WITH '3' ,;
            NumOc with itepec.numoc
   UNLOCK            
   DO UBICTA
   SELE &AS
   RETURN .T.
ENDIF
SELE &AS
RETURN .F.


PROCEDURE vNumpol
*---------------
m.numpol=padl(alltrim(m.numpol),4,'0')
SEEK m.periodo+m.numpol
if !found()
   retur .t.
else
   do standby with 'La poliza ya se encuentra registrada..!'
   return .f.
endif   


PROCEDURE Elimi
*--------------
SELECT POLIZA
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
IF Estado = '50'
   DO standby with 'La Poliza ya esta Liquidada'
   return
ELSE
	IF Estado = '51'
       DO standby with 'La Poliza ya esta Contabilizada'
       return
	endIF
ENDIF	
PRIVATE OK
*iK=VE_PASSW('CHINA')
*IF OK
    velimina = YESNO('¨ Desea Eliminar ‚sta Poliza ?')
    IF vElimina 
       SELECT Itepol
       SEEK m.periodo+m.numpol
	   SCAN WHILE m.Periodo = Periodo and m.Numpol = Numpol 
           *IF RLOCK()
               delete next 1
            *ENDIF
       ENDSCAN    
       SELE Poliza
       IF RLOCK()
          delete next 1
       ENDIF
       IF EOF()
          SKIP -1
       ENDIF   
	ENDIF    
	DO VISTA
	UNLOCK ALL
*ENDIF
RETURN
      
PROCEDURE Anula
*--------------
nreg = RECNO()
IF Estado = "99"
	DO standby WITH "La poliza ya esta Anulada"
	DO VISTA
	RETURN
ENDIF
IF YesNo("¨Desea Anular esta Poliza?")
	SELE ItePol
	SEEK Poliza.Periodo+ Poliza.NumPol
	IF FOUND()
		REPLACE ESTADO WITH "99" WHILE Poliza.Periodo+Poliza.NumPol = ItePol.Periodo+ItePol.NumPol 
	ENDIF
	SELE Poliza
	REPLACE ESTADO WITH "99" 
	DO Vista
	DO standby WITH Vmens09
ENDIF
RETURN

PROCEDURE Liqui
*---------------
SELECT POLIZA
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 do case
    case Estado = '50'
         DO standby with 'La Poliza ya est  Liquidada'
         return
    case Estado = '51'
         DO standby with 'La Poliza ya est  Contabilizada'
         return
 endcas        
 vliquida = YESNO('¨ Desea LIQUIDAR ‚sta Poliza ?')
 IF vLiquida 
    ACTIVATE WINDOWS WIND_1
*   @ 5,22 get m.fecliq
*   READ
*   REPLACE FECLIQ WITH M.FECLIQ

    SELECT Itepol
    SEEK m.periodo+m.numpol
	SCAN WHILE m.Periodo = Periodo and m.Numpol = Numpol 
        IF RLOCK()
           REPLACE ESTADO WITH '50'
        ENDIF
    
        sele pecosa
        seek m.periodo+itepol.numref+m.codfte
        IF RLOCK()
         * replace pecosa.estado with '50'
        ENDIF  
    ENDSCAN    
    SELE Poliza
    IF RLOCK()
       REPLACE ESTADO WITH '50'
    ENDIF
    DO Vista
 ENDIF
 UNLOCK ALL
RETURN


PROCEDURE LISTA
*-------------
PRIVATE VTEMP

*-Cierra archivos que no se est‚n usando en ese momento
USE IN 9  
USE IN 13 
USE IN 14 

select POLIZA
vtemp =recno()
SET RELATION TO PERIODO+NUMPOL INTO ITEPOL
SET SKIP TO ITEPOL

IF EOF()
   DO standby WITH vmens08
   RETURN
 ELSE
   DO LISPOL WITH vTemp
ENDIF

*- abre archivos

USE calen    IN 9   order tag calen1   ALIAS calen
USE Asiaut   IN 13  order tag asiaut1  ALIAS asig
USE Parkar   IN 14  order tag parkar1  ALIAS parkar


select POLIZA
SET RELATION TO
SET FILT TO
*GO TOP
DO VISTA
RETURN


PROCEDURE LisPol
*---------------
PARAMETERS vReg
PRIVATE VORDE
vOrde = ORDER()
vrec  = RECNO()
DEFINE WINDOW LIS FROM 2,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Polizas' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vToPol,vToMes,vToFue,vTodep,vOrden,vtiplis
vNumpol = SPAC(4)
vFte    = SPAC(2)
vCodmes = SPAC(2)
vCoddep = SPAC(6)
vCodFte = SPAC(2)
vtiprep = 1
@ 01,01 SAY "Todas las Polizas : " GET vToPol  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToPol,3,22) 
@ 03,01 SAY "           Poliza : "
@ 03,22 GET vNumpol  PICTURE '!!!!' WHEN vToPol = 2  VALID vnumpol1()

@ 05,01 SAY "      Pecosa tipo : " get vTiprep FUNCTION '^ Todos;Compra;Stock'  WHEN vToPol = 2

@ 08,01 SAY "  Todos las Meses : " GET vTomes  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTomes,6,22)  WHEN vToPol = 1
@ 09,01 SAY "              Mes : "
@ 09,22 GET vCodmes  PICTURE '!!'  VALID VAL_PARA(vCodMes,'FECMES','C') WHEN vToPol = 1 AND vTomes = 2

@ 11,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTofue,9,22)  WHEN vToPol = 1
@ 12,01 SAY "           Fuente : "
@ 12,22 GET vCodFte  PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE','C')  WHEN vToPol = 1 and vTofue =2

@ 14,01 SAY "           Estado : " GET vTipLis  FUNCTION '^ Todos;Pendientes;Atendidos'  WHEN vToPol = 1

@ 18,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1 AND LASTKEY()#27
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON Periodo+Itepol.NumRef+itepol.Codgen TO (vInd) ;
      FOR IIF(vToPol=1,.T.,NumPol+CodFte = vNumpol+alltrim(vFte)) .AND. IIF(vTipLis=1,.T.,iif(vTipLis=2,Estado = '00',Estado = '50' )) and ;
                 iif(vToMes=1,.t.,month(fecemi)=val(vCodMes))  
   SET INDEX TO (VIND)

   SET FILTER TO TIPPOL='S' AND iif(vToFue=1,.t.,Codfte=alltrim(vCodfte)) AND iif(vtiprep=1,.t.,iif(vtiprep=2,itepol.tippec='O',itepol.tippec='S')) &&and iif(!empty(itepol.tippec),iif(vtiprep=1,.t.,iif(vtiprep=2,itepol.tippec='C',Itepol.tippec='S')),.t.)    &&AND itepol.valtot>0
   SELE ITEPOL                    
   GO TOP
   SELE POLIZA

   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipLis=1,' en General ',IIF(vTipLis=2,' Pendientes ',' Atendidos '))
   IF !EOF()
      if vTopol = 2 
      	 do case
      	    case alltrim(analisis)='G' 
				do reporte with 2,"polsal1",' Poliza de Salida',1,.F.,.T.
            case alltrim(analisis)='C' 
                do reporte with 2,"polsal_c",' Poliza de Salida',1,.F.,.T.
            case alltrim(analisis)='A'
                *do reporte with 2,"polsalaa",' Poliza de Salida' ,1,.F.,.T.            
                *do reporte with 2,"polsal_a",' Poliza de Salida' ,1,.F.,.T.
				do reporte with 2,"polsal1",' Poliza de Salida',1,.F.,.T.
         endcase      	          	    
         *do reporte with 2,"polsal1",' Poliza de Salida'
	  else
	     do REPORTE with 2,"Polsal",' Poliza de Salida ',1,.F.,.T.
	  endif   
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILT TO
   CLOSE INDEX
*  ERASE (VIND)
ENDIF
SELECT Poliza
SET ORDE TO (VORDE)
GO TOP
GO VREC
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_2
  RELEASE    WINDOW wind_3
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


PROCEDURE vNumpol1
*-----------------
IF !EMPTY(vnumpol)
	vnumpol=padl(alltrim(vnumpol),4,'0')
	SEEK m.periodo+vnumpol
	if found()
	   retur .t.
	else
	   do standby with 'La poliza NO se encuentra registrada'
	   return .f.
	endif   
ELSE
	vnumpol=m.numpol
	RETURN .T.
ENDIF


function valpec
*-----------------
parameter vnumpec
private vfun
vfun = .t.
m.numpec=padl(alltrim(str(vnumpec,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif
return vfun


FUNCTION Val_ArtC  && Articulos
*------------------
PARAMETER xcod
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
_campo = VARREAD()
mAlias = ALIAS()
SELECT IteArt
GO TOP
_OldWnd = WOUTPUT()
v_Fun='*'

SEEK xcod
v_fun = IIF(FOUND(),Descri,"*")
   SELECT itepol
   IF RLOCK()
      REPLACE descri  WITH Iteart.descri
   ENDIF
SELECT (mAlias)

RETURN v_fun



 FUNCTION BuscArt
 *----------------
 private vfun
 as=alias()
 sele produ
 set orde to 1
 seek 'B'+LEFT(ITEPEC.CODART,6)
 if !found()
    vFun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
 else
    vfun=UPPER(produ.descri)
 endif
 sele (as)
 return vfun


PROCEDURE UBICTA
*---------------
Private as
as = alias()
select asig
seek 'P/S'+'1  '+'B'+LEFT(itepec.codart,6)

SELECT ITEPOL
if found()
   replace ITEPOL.DCuenta with asig.dcuenta, ITEPOL.HCuenta with asig.hcuenta
else   
   replace ITEPOL.DCuenta with '          ', ITEPOL.HCuenta with '          '
endif   
select (aS)
RETURN

PROCEDURE BUSCOC
*---------------
PRIVATE ALI,VFUN
ALI=ALIAS()
VK = ITEPOL.NUMOC
SELE ORDEN
SEEK VK
IF FOUND()
   VFUN = VK+'.'+PADL(ALLTRIM(STR(MONTH(FECDESP),2)),2,'0')
ELSE
   VFUN = VK+'.'+'??'
ENDIF
RETURN VFUN

Function repasa
*--------------
vfun = .t.
vrec = recno()
vali = alias()
** abrimos el maestro de parametros para traer numero
SELECT Parma
SEEK 'CORRELPOLSAL'
NewPol=NumEnt+1
m.numpol=padl(alltrim(STR(Newpol)),4,'0')
*REPLACE NumEnt WITH NumR 
sele (vali)
return vfun

FUNCTION Val_Cta
*---------------
PARAMETERS cTipo
xField = IIF(cTipo='D','DCuenta','HCuenta')
IF !EMPTY(&xField)
	IF seek(&xField,'Cuen')
		*
	 ELSE
		REPLACE &xField WITH SPACE(10)
	ENDIF
	xCta = Val_Fun('Cuen','Cuenta',"Cuenta+' '+Descri",&xField,2)
	IF Cuen.Detalle#'S'
		=STANDBY('La Cuenta No es de Detalle. Por Favor Intente de Nuevo')
		RETURN .F.
	ENDIF
ENDIF
RETURN .T.

PROCEDURE SubOpc
*---------------
PRIVATE cAlias,Cmod
cAlias = ALIAS()
SELE SubOp
cCtrlOp = ''
cMod = "04"
SEEK vUsuCla+PADL(SistCtrl,2,'0')+cMod

IF FOUND()
	SCAN WHILE vUsuCla+PADL(SistCtrl,2,'0')+cMod = ALLTRIM(SubOp.User)+SubOp.Sistema+SubOp.Modulo
		cCtrlOp = cCtrlOp + SubOp.Opcion
	ENDSCAN
ENDIF

set skip of PAD Revis   of mMenu !'A' $cCtrlOp
set skip of PAD Busca   of mMenu !'B' $cCtrlOp
set skip of PAD Anter   of mMenu !'C' $cCtrlOp
set skip of PAD Proxi   of mMenu !'D' $cCtrlOp
set skip of PAD Elimi   of mMenu !'E' $cCtrlOp
set skip of PAD Ingre   of mMenu !'F' $cCtrlOp
set skip of PAD Anula   of mMenu !'G' $cCtrlOp
set skip of PAD Lista   of mMenu !'H' $cCtrlOp

SELE (cAlias)

RETURN

