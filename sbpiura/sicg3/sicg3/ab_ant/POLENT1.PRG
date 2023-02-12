*--------------------------------------------------------------------------
* PolEnt.Prg
* Registra las Polizas de Entrada que emiten en Abastecimiento
* Estado :
*   '00' Emitida     ** 	Este es el que se registra en el 
*   '50' Liquidada
*   '51' Contabilizada
*--------------------------------------------------------------------------
*- Abriendo Archivos
SET EXCL OFF
USE OrdCom   IN 1   order tag OrdCom1  ALIAS Orden
USE IteOc    IN 2   order tag IteOc1   ALIAS IteOc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE NOTALM   IN 5   order tag NOTALM1  ALIAS NOTA
USE Itepec   IN 6   order tag ItePec1  ALIAS Itepec
USE AuxCot   IN 7   order tag AuxCot1  ALIAS Auxcot
USE Promae   IN 8   order tag Promae1  ALIAS Promae
USE ITEALM   IN 9   order tag ITEALM1  ALIAS ITEALM
USE Iteart   IN 10  order tag iteart1  ALIAS iteart
USE Poliza   IN 11  order tag poliza1  ALIAS poliza
USE Itepol   IN 12  order tag itepol1  ALIAS itepol
USE itepar   in 13  order tag itepar1  ALIAS ITEPAR          
USE maepre   IN 14   order tag maepre1  ALIAS maepre
USE Asiaut   IN 15  order tag asiaut1  ALIAS asig
USE Parkar   IN 16  order tag parkar1  ALIAS parkar
USE AstOrd	 IN 19	ORDER TAG AstOrd1  ALIAS AstOrd
USE Cuentas	 IN 20	ORDER TAG Cuentas1 ALIAS Cuen
USE IteUsuOp IN 0 ORDER TAG IteUsuOp1 ALIAS SubOp

*- Mensajes de aviso al usuario
Vmens01 = ' P¢lizas de Entrada: REVISION '
Vmens02 = 'Registro de P¢liza de Entrada'
Vmens04 = 'Dicho P¢liza no fue encontrado'
Vmens05 = 'No existe P¢liza anterior'
Vmens06 = 'No existe P¢liza siguiente'
Vmens07 = '¨ Desea Anular ‚sta P¢liza ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta P¢liza ha sido anulado'
Vmens10 = 'La P¢liza ya est  Atendido'
Vmens11 = 'La P¢liza ha sido devuelto'
Vmens12 = 'La P¢liza ya tiene O/C'

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
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Elimina Ingresa            Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 09,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 10,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: P¢liza         ®F9¯ Detalle : Item ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,65 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD Anula   OF mMenu PROMPT 'An\<ula  '    AT 24,54
*DEFINE PAD liqui   OF mMenu PROMPT 'li\<Quida'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD Anula  OF mMenu DO Anula
 *ON SELECTION PAD liqui  OF mMenu DO liquida
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_1
 CLEAr
 
 @  1, 2 SAY "        * Periodo :"
 @  2, 2 SAY "          Emisi¢n :"
 @  3, 2 SAY "    N£mero P¢liza :"
 @  4, 2 SAY " F.Financiamiento :"
 @  5, 2 SAY "   N£mero de Gu¡a :"
 @  6, 2 SAY "         Vigencia :"
 @  7, 2 SAY "         Analisis :"
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
 @  4,22 SAY val_para(m.Codfte,'CODFTE','V',22,30)
 @  5,22 SAY m.numgui
 @  6,22 SAY m.fecini 
 @  6,36 SAY m.fecfin
 @  7,22 SAY val_para(m.analisis,'ANALIS','V',22,15,3)
 
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
	   DesPro : h='P':1,;   
	   TipRef : h='REF' ,;
	   PerRef : h='Pr'  ,;
	   NumRef : h='N§Rf',;
	   Fecref : h='FecRef',;
	   Codgen : h='GG' ,;
	   Descri : h='Definici¢n':36 ,;
	   Igv    : H='IGV':P='99,999.99',;
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
	   Igv    : H='IGV':P='99,999.99',;
	   ValTot : h='Total'
   case ALLTRIM(m.analisis) = 'A'   
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
	   Igv    : H='IGV':P='99,999.99',;
	   ValTot : h='Total'
  ENDCASE   
   SELE Poliza   
RETURN


PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
@ 0,0 SAY m.Valtot picture '9,999,999.99'
return


PROCEDURE VISTA_DET
*------------------
hide popup all
ON KEY LABEL F4 DO AgrIGV
ON KEY LABEL F9 
ON KEY LABEL F10 KEYBOARD(CHR(23))
SELECT ItePol
GO TOP
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.Periodo+m.NumPol ;
   WINDOW Wind_2 ;
   FIELDS;
   DesPro : h='P':1,;   
   TipRef : h='REF' ,;
   PerRef : h='Pr'  ,;
   NumRef : h='N§Rf',;
   Fecref : h='FecRef',;   
   Codgen : h='GG' ,;
   Descri : h='Definici¢n':36 ,;
   Igv    : H='IGV':P='99,999.99',;
   ValTot : h='Total',;
   dcuenta : H= 'Al Debe' :V=Val_Cta('D') :F,;
   hcuenta : H= 'Al Haber':V=Val_Cta('H') :F
   SELE Poliza
 ON KEY LABEL F9 DO vista_det   
 ON KEY LABEL F10 

SELE ItePol
SEEK m.Periodo+m.NumPol
IF FOUND()
	m.ValTot = 0
	SCAN WHILE m.Periodo=Periodo AND m.NumPol=NumPol
		m.Valtot = m.ValTot + ValTot
	ENDSCAN
ENDIF
SELE Poliza
REPLACE Valtot WITH m.ValTot
DO AsiOrd

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
* SET RELATION TO Periodo+NumPol INTO ITEPol
* SET SKIP TO ITEPol
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
 
 &&Itepol.codgen : H='GG',;
 itepol.Descri : H='Detalle '
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SELE Poliza
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
vNum_pol = '0000'
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
vRe=recno()
SCATTER MEMVAR BLANK
store date() to m.fecini,m.fecfin,m.fecemi
m.periodo = RIGHT(DTOC(DATE()),2)
=REPASA()
@  1,22 SAY m.periodo 
@  2,22 GET m.FecEmi
@  3,22 GET m.NumPol valid vnumpol() DISABLE
@  3,22 SAY m.NumPol COLOR "GBR+/R"
@  4,22 GET m.Codfte picture   '!!' valid val_para(m.Codfte,'CODFTE',' ',22,30)
@  5,22 get m.numgui
@  6,22 GET m.fecini 
@  6,36 GET m.fecfin
@  7,22 GET m.analisis picture '!'  valid val_para(m.analisis,'ANALIS',' ',22,15,3)
READ VALID VAL_READ()

*------------------------------------------------------------
* Acualiza el correlativo de las Polizas de Entrada en PARMAE

IF LASTKEY()<>27
	selec parma
	seek "CORRELPOLENT"
	CORREL=NUMENT+1
	REPLACE NUMENT WITH CORREL
	
endif
SELEC POLIZA
*--------------------------------------------------------------
 
IF LASTKEY() # 27
	DO WHILE .T.
		Ok = Ingresa_H()
		IF LASTKEY() # 27 AND OK
			IF YESNO('¨ Confirme el ingreso ?')
				EXIT
			ENDIF
		 ELSE
			DO STANDBY WITH ' Cancelado el Ingreso ...'
			OK = .F.
			EXIT
		ENDIF
	ENDDO
	
	IF Ok .AND. LASTKEY() # 27
		select itepol
		SEEK m.periodo+m.numpol
		vTotalz = 0
		scan while itepol.periodo+itepol.numpol=m.periodo+m.numpol
			vTotalz = vtotalz + valtot &&-VALANUL()
			sele orden
			seek m.periodo+itepol.numref+m.codfte
			if rlock()
**ojo			*replace orden.estado with '51'
			endif
			UNLOCK
		endscan
		SELECT Poliza
		m.valtot = vtotalz
		m.tippol = 'E'
		m.Estado = '00'
		if f_appd()
			gather memvar
		endif
		
		select itepol
		SEEK m.periodo+m.numpol
		IF FOUND()
			xNumR = ItePol.NumRef
			SELE Orden
			SEEK m.periodo+xNumR
			REPLACE DocRef WITH "Pol: "+m.NumPol
		ENDIF
		DO AsiOrd
	 ELSE
		select itepol
		SEEK m.periodo+m.numpol
		if Found()
			scan while itepol.periodo+itepol.numpol=m.periodo+m.numpol
				if rlock()
					dele next 1
				endif
			endscan
			UNLOCK
		endif
		SELE POLIZA
		DO STANDBY WITH ' No se pudo crear la Poliza '
		IF !EOF()
			GO VRE
		ENDIF
	ENDIF
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
	SELECT POLIZA
	GO VRE
ENDIF
SELECT Poliza
DO Vista
RETURN



PROCEDURE ingresa_H
*------------------
private ok
ON KEY LABEL F4 DO AgrIGV
ACTIVATE SCREEN
HIDE MENU mMenu
ACTIVATE WINDOW standby
xcolor = '*' + SET('COLOR')
@ 01,06 SAY 'Espere un momento ... Reporte en proceso' COLOR &xcolor
ok=pasa()

DEACTIVATE WINDOW standby
vTempo = '°°°°°°°°°°Presione ®F10¯ para salir grabando o  ®Esc¯ para cancelar°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
if (ok)
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
          Igv         : H= 'IGV':P='99,999.99',;
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
	      Igv         : H='IGV':P='99,999.99',;
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
	      IGV         : H='IGV':P='99,999.99',;
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
  ok = .f.
endif
ON KEY LABEL F4
return (ok)

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
SELECT iteoc
SDX = SYS(3)+'.IDX'
set relation to periodo+numoc+codfte into orden
INDEX ON PERIODO+NUMOC+CODFTE+CODART TO (sdx) FOR codfte=alltrim(m.Codfte) and BETWEEN(orden.FecVIS,m.FecIni,m.FecFin) AND EMPTY(Orden.DocRef) AND PerPec#'LI'				&& AND (orden.ESTADO='5'or orden.estado='20') 
*&& AND iteoc.tipord#'B' AND iif(orden.estado='21' OR month(pteanu.fecpa) = val(vNummes) ,!empty(pteanu.numpa),.t.) &&AND orden.tipord#'B'
GO TOP
IF EOF()
   vfun = .f.
else  
	DO CASE 
		CASE ALLTRIM(m.analisis)='G'
			SCAN 
*				vPart1 = left(IteOc.CodArt,2)+numoc+codfte
*				vPart2 = left(IteOc.CodArt,2)+numoc+codfte
				vTot = 0
*				DO WHILE  vPart1 = vPart2
					vTot = vTot + canDESP*preuni   &&Valtot
*					SKIP
*					vPart1 = left(IteOc.CodArt,2)+numoc+codfte
*				ENDDO
*				SKIP -1
				DO AGREG_pol WITH 'O/C'
				SELE ITEOC
			ENDSCAN
			vfun = .t.
		CASE ALLTRIM(m.analisis)='C'
			SCAN
*				vPart1 = left(IteOc.CodArt,6)+numoc+codfte
*				vPart2 = left(IteOc.CodArt,6)+numoc+codfte
				vTot = 0
*				DO WHILE  vPart1 = vPart2
					vTot = vTot + canDESP*preuni   &&Valtot
*					SKIP
*					vPart1 = left(IteOc.CodArt,6)+numoc+codfte
*				ENDDO
*				SKIP -1
				DO AGREG_pol WITH 'O/C'
				SELE ITEOC
			ENDSCAN
			vfun = .t.
		CASE ALLTRIM(m.analisis)='A'
			SCAN
*				vPart1 = left(IteOc.CodArt,10)+numoc+codfte
*				vPart2 = left(IteOc.CodArt,10)+numoc+codfte
				vTot = 0
*				DO WHILE  vPart1 = vPart2
					IF ESTADO = "50"
						*vTot = vTot + canDESP*preuni1   &&Valtot
						vTot=vTot+valtot
					 else
						*vTot = vTot + canDESP*prereb   &&Valtot
						vtot=vTot+valtot
					endif
*					SKIP
*					vPart1 = left(IteOc.CodArt,10)+numoc+codfte
*				ENDDO
*				SKIP -1
				DO AGREG_pol WITH 'O/C'
				SELE ITEOC
			ENDSCAN
			vfun = .t. 
	ENDCASE
endif
select (malias)
return vfun



PROCEDURE Agreg_Pol
*-----------------
PARAMETER VTIPDOC
private as
AS=ALIAS()
sele itepol
IF VTIPDOC='O/C'
	IF F_appd()
		REPLACE Periodo  with m.Periodo ,;
				NumPol with m.Numpol,;
                TipPol with 'E',;
                TipRef with VTIPDOC,;
				Codcad with alltrim(iteoc.codcad),;
                Perref with Orden.Nummes,;
                Numref with iteoc.NumOc,;
                Codgen with LEFT(iteoc.codart,2),;
                Codcla with substr(iteoc.codart,4,3),;
                Coddet with substr(iteoc.codart,8,3),;            
                Valtot with vtot,;
                NumMeshc with ORDEN.perHC,;
                NumHc with ORDEN.numhc,;
                CodPrv with orden.codprv,;
                Destino with orden.destino,;
                estado with '00',;
                FECref with orden.fecdesp,;
                DESPRO WITH '1' ,;
                numpec with iteoc.numpec,;
			    DESCRI WITH IIF(ALLTRIM(m.analisis)='G',substr(val_para('B'+alltrim(LEFT(iteoc.codart,2)),'CODGEB','D',22,60),4,100),IIF(ALLTRIM(m.analisis)='C',buscart(),VAL_ARTC('B'+ITEOC.codArt)))
                DO UBICTA
	ELSE
	   SELE &AS
	   RETURN .T.
	ENDIF
ENDIF
UNLOCK
SELE &AS
RETURN .T.



PROCEDURE val_can
*----------------
private vfun
vfun = .t.
vResta = Canreq-Candesp
if Candesp1 > vResta
   do standby with 'Se esta exediendo en '+alltrim(str(candesp1-vResta,10))
   vfun = .f.
endif
return vfun


procedure val_fec
*----------------
if empty(fecdesp)
	if rlock()
		replace fecdesp with date()
	else
		retur .f.
	endif	
endif   
retur .t.


procedure val_fec
*------------------
if empty(fecdesp)
	if rlock()
		replace fecdesp with date()
	else
		retur .f.
	endif	
endif   
retur .t.


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

      
PROCEDURE vNumpol1
*---------------
IF !EMPTY(vNumpol)
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



PROCEDURE Elimi
*--------------
SELECT POLIZA
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
IF Estado = '50'
	DO standby with 'La P¢liza ya est  Liquidada'
	return
 ELSE        
	IF ESTADO= '51'
		DO standby with 'La P¢liza ya est  Contabilizada'
		return
	ENDIF
ENDIF
PRIVATE OK
OK=VE_PASSW('CHINA')
IF OK
	velimina = YESNO('¨ Desea Eliminar ‚sta P¢liza ?')
	IF vElimina 
       SELECT Itepol
       SEEK m.periodo+m.numpol
	   SCAN WHILE m.Periodo = Periodo and m.Numpol = Numpol
	   		SELE Orden
	   		SEEK m.Periodo+ItePol.NumRef
	   		REPLACE TipDoc WITH "  ",DocRef WITH "          ",FecVis WITH {  /  /  }
	   		SELE ItePol
            IF RLOCK()
               delete next 1
            ENDIF
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
ENDIF
RETURN

PROCEDURE Anula
*--------------
nreg = RECNO()
IF Estado = "99"
	DO standby WITH "La poliza ya esta Anulada"
	DO VISTA
	RETURN
ENDIF
IF YesNo(vMens07)
	SELE ItePol
	SEEK Poliza.Periodo+ Poliza.NumPol
	IF FOUND()
		REPLACE ESTADO WITH "99" WHILE Poliza.Periodo+Poliza.NumPol = ItePol.Periodo+ItePol.NumPol 
	ENDIF
	SEEK Poliza.Periodo+ Poliza.NumPol
	SELE Poliza
	REPLACE ESTADO WITH "99" 
	SELE Orden
	SEEK m.Periodo+ItePol.NumRef
	REPLACE TipDoc WITH "  ",DocRef WITH "          ",FecVis WITH {  /  /  }
	SELE Poliza
	DO Vista
	DO standby WITH Vmens09
ENDIF
RETURN

PROCEDURE LIQUIDA
*---------------
SELECT POLIZA
IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 do case
    case Estado = '50'
         DO standby with 'La P¢liza ya est  Liquidada'
         return
    case Estado = '51'
         DO standby with 'La P¢liza ya est  Contabilizada'
         return
 endcas        
 vLIQUIDA = YESNO('¨ Seguro de LIQUIDAR ‚sta P¢liza ?')
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
    ENDSCAN        
        sele orden
        seek m.periodo+itepol.numref+m.codfte
        IF RLOCK()
           replace orden.estado with '50'
        ENDIF  
    

    SELE Poliza
    IF RLOCK()
       REPLACE ESTADO WITH '50'
    ENDIF
    
   endif
 DO Vista
 UNLOCK ALL
 RETURN



PROCEDURE LISTA
*-------------
*-Cierra archivos que no se est‚n usando en ese momento
USE IN 5
USE IN 7
USE IN 9
USE IN 10
USE IN 14
USE IN 15
USE IN 16
USE itehc IN 17 ORDER TAG itehc1 ALIAS itehc
USE HOJCON IN 18 ORDER TAG HOJCON1 ALIAS HOJA

select POLIZA
SET FILT TO
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   DO LISPOL
endif

*-Abre archivos
USE NOTALM   IN 5   order tag NOTALM1  ALIAS NOTA
USE AuxCot   IN 7   order tag AuxCot1  ALIAS Auxcot
USE ITEALM   IN 9   order tag ITEALM1  ALIAS ITEALM
USE Iteart   IN 10  order tag iteart1  ALIAS iteart
USE maepre   IN 14  order tag maepre1  ALIAS maepre
USE Asiaut   IN 15  order tag asiaut1  ALIAS asig
USE Parkar   IN 16  order tag parkar1  ALIAS parkar
USE          IN 17
USE          IN 18

select POLIZA
SET RELATION TO
SET FILT TO
DO VISTA
RETURN



PROCEDURE LisPol
*---------------
PRIVATE VORDE
vOrde = ORDER()
vrec  = RECNO()
DEFINE WINDOW LIS FROM 3,15 TO 20,65 FLOAT DOUBLE TITLE 'Listado Polizas' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vToPol,vToMes,vToFue,vTodep,vOrden,vtiplis
vNumpol = SPAC(4)
vFte    = SPAC(2)
vCodmes = SPAC(2)
vCoddep = SPAC(6)
vCodFte = SPAC(3)

@ 01,01 SAY "Todas las Polizas : " GET vToPol  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToPol,3,22) 
@ 03,01 SAY "           Poliza : "
@ 03,22 GET vNumpol  PICTURE '!!!!' WHEN vToPol = 2  VALID vnumpol1()

@ 05,01 SAY "  Todos las Meses : " GET vTomes  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTomes,6,22)  WHEN vToPol = 1
@ 06,01 SAY "              Mes : "
@ 06,22 GET vCodmes  PICTURE '!!'  VALID VAL_PARA(vCodMes,'FECMES','C') WHEN vToPol = 1 AND vTomes = 2

@ 08,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTofue,9,22)  WHEN vToPol = 1
@ 09,01 SAY "           Fuente : "
@ 09,22 GET vCodFte  PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE','C')  WHEN vToPol = 1 and vTofue =2

@ 11,01 SAY "           Estado : " GET vTipLis  FUNCTION '^ Todos;Pendientes;Atendidos'  WHEN vToPol = 1

@ 14,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1 AND LASTKEY()#27
   SET RELATION TO PERIODO+NUMPOL INTO ITEPOL
   SET SKIP TO ITEPOL
   
   SELE ITEPOL
   SET RELATION TO NUMMESHC+NUMHC INTO HOJA     ADDI

   SELE POLIZA
   
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON Periodo+Itepol.NumRef+itepol.Codgen TO (vInd) ;
      FOR IIF(vToPol=1,.T.,NumPol+CodFte = vNumpol+alltrim(vFte)) .AND. IIF(vTipLis=1,.T.,iif(vTipLis=2,Estado = '00',Estado = '50' )) and ;
                 iif(vToMes=1,.t.,month(fecemi)=val(vCodMes))  AND iif(vToFue=1,.t.,Codfte=alltrim(vCodfte))
                 SET FILTER TO TIPPOL='E' 		
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipLis=1,' en General ',IIF(vTipLis=2,' Pendientes ',' Atendidos '))

   IF !EOF()
      if vTopol = 2 
      	 do case
      	    case alltrim(m.analisis)='G' 
      	    	do reporte with 2,"polent1",' Poliza de Entrada',1,.F.,.T.
            case alltrim(m.analisis)='C' 
                do reporte with 2,"polent_c",' Poliza de Entrada',1,.F.,.T.
            case alltrim(m.analisis)='A'
      	    	do reporte with 2,"polent1",' Poliza de Entrada',1,.F.,.T.
*                do reporte with 2,"polent_a",' Poliza de Entrada',1,.F.,.T.
         endcase      	          	    
	  else
	     do reporte with 2,"Polent",' Poliza de Entrada ',1,.F.,.T.
	  endif   
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILT TO
   CLOSE INDEX
   ERASE (VIND)
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



 FUNCTION BuscArt
 *----------------
 private vfun
 as=alias()
 sele produ
 set orde to 1
 seek 'B'+LEFT(ITEOC.CODART,6)
 if !found()
    vFun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
 else
    vfun=UPPER(produ.descri)
 endif
 sele (as)
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




PROCEDURE UBICTA
*---------------
Private as
as = alias()
select asig
seek 'P/E'+'1  '+'B'+LEFT(iteOC.codart,6) &&+iteOC.tipfun

SELECT ITEPOL
if found()
   replace ITEPOL.DCuenta with asig.dcuenta, ITEPOL.HCuenta with asig.hcuenta
else   
   replace ITEPOL.DCuenta with '         ', ITEPOL.HCuenta with '         '
endif   
select (aS)
RETURN

PROCEDURE VALANUL
*--------------
PRIVATE AS
VH = ITEPOL.PERIODO+ITEPOL.NUMREF
AS=ALIAS()      
SELE ORDEN
SEEK VH
IF FOUN()
   VTOT = ORDEN.ANULTOT
ELSE
   VTOT = 0
ENDIF
SELE (AS)
RETURN VTOT


FUNCTION BusCatG
*---------------
PARAMETER ClaveHc
vAlias = SELECT()
SELECT iteHc
IF SEEK(ClaveHc)
   vCatgas = LEFT(iteHc.codpart,1)
ELSE
   vCatGas= " "
ENDIF
SELECT (vAlias)
RETURN vCatGas      


PROCEDURE AsiOrd
*---------------
*SELE Parma
*SEEK 'TIPDOCO/C'
*IF !FOUND()
*	DO StandBy WITH "PARAMETRO NO INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
*	RETURN
*ENDIF
SELE Asig

SEEK "P/E"+"   "+"ASTORD"

IF !FOUND()
	DO StandBy WITH "PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
	RETURN
 ELSE
	cCtaD = DCuenta
	cCtaH = HCuenta
ENDIF

SELE AstOrd
cNumMes = PADL(MONT(M.FECINI),2,'0')
SEEK m.Periodo+cNumMes+m.NumPol+"P/E"
IF FOUND()
	FOR i = 1 TO 2
		IF f_Lock(1) OR RLOCK()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH cNumMes  ,;
					NUMREF	WITH m.NumPol   ,;
					TIPDOC	WITH "P/E"     ,;
					FECHA	WITH m.FecIni,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValTot,0),;
					MTOHAB	WITH IIF(i=2,m.ValTot,0)
			UNLOCK
			SKIP
		ENDIF
	ENDFOR
 ELSE
	FOR i = 1 TO 2
		IF f_Appd()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH cNumMes  ,;
					NUMREF	WITH m.NumPol   ,;
					TIPDOC	WITH "P/E"     ,;
					FECHA	WITH m.FecIni,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValTot,0),;
					MTOHAB	WITH IIF(i=2,m.ValTot,0)
			UNLOCK
		ENDIF
	ENDFOR
ENDIF
DEFINE WINDOW wAstOrd FROM 10,10 TO 15,70 ;
 TITLE ' ASIENTOS DE ORDEN' COLOR SCHEME 02
ACTIVATE WINDOW wAstOrd
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY '        Debe '
@ 00,34  SAY '        Haber '
@ 01,04  SAY cCtaD PICTURE '!!!!!!!!!!!'
@ 01,18  SAY m.ValTot PICTURE '99,999,999.99'
@ 02,12  SAY cCtaH PICTURE '!!!!!!!!!!!'
@ 02,34  SAY m.ValTot PICTURE '99,999,999.99'
WAIT " "
DEACTIVATE WINDOW wAstOrd
RELEASE WIND wAstOrd
RETURN

Function repasa
*--------------
vfun = .t.
vrec = recno()
vali = alias()
** abrimos el maestro de parametros para traer numero
SELECT Parma
SEEK 'CORRELPOLENT'
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
cMod = "03"
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