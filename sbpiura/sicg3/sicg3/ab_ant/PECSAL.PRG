*--------------------------------------------------------------------------
* PecSal.Prg
* Registra las salidas que emiten en cada dependencia
* Estado :
*   '00' Emitida     ** 	Este es el que se registra en el pecosa
*   '20' Con Correlativo
*   '50' Con O/C
*   '70' Devuelta
*   '99' Anulada
* Autor: LCD
* Soporte y Actualizaci¢n : GUSWALEN
*--------------------------------------------------------------------------
*- Abriendo Archivos
parameter SISTEMA
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE calen    IN 2   order tag calen1   ALIAS calen
USE Pecosa   IN 3   order tag Pecosa1  ALIAS Pecosa
USE ItePec   IN 4   order tag ItePec1  ALIAS Itepec
USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE IteArt   IN 6   order tag IteArt1  ALIAS Iteart
USE KARDEX   IN 7   order tag KARDEX1  ALIAS KARDEX
USE cdrnec   IN 8   order tag Cdrnec1  ALIAS cuadro
USE itecn    IN 9   order tag itecn3   ALIAS itecn
USE astpat   IN 12  ORDER TAG Astpat37 ALIAS astpat
IF sistema='1'
	USE maepre   IN 10  order tag maepre1  ALIAS maepre
 ELSE
	USE maepre   IN 10  order tag maepre3  ALIAS maepre
ENDIF	
USE itepar   IN 11  order tag itepar1  ALIAS ITEPAR
USE iteoc    IN 13  order tag iteoc9   ALIAS iteoc
USE USUARIO  IN 20  order tag usuario1 ALIAS USU
USE STOCK    IN 21  ORDER TAG STOCK1   ALIAS STOCKALI
USE IteUsuOp IN 0 ORDER TAG IteUsuOp1 ALIAS SubOp

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

ON KEY LABEL F11 DO CORRI

SELECT Pecosa
GO BOTTOM
ON KEY LABEL F2 DO FECREP
ON KEY LABEL F7 DO VISTA_ING   
*ON KEY LABEL F4 DO IMPRIMir

PUBLIC VECES

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
Public vtotp
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  liQuida  Ingresa           Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 13,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Pecosa     ± ®F9¯ Detalle:Item ± ®F7¯ Coloca precio ± ®F4¯ Imprime' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,63 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10
 
 DEFINE WINDOW Wind_5 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE '± ®F11¯ Liquidaci¢n Parcial ± ®F12¯ Habilitaci¢n Parcial ±' COLOR SCHEME 10


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT 'li\<Quida'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nular '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO liquida
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD anula  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_1
CLEAR
@  1, 2 SAY "            Fecha :"
@  1,40 SAY "    N£mero Pecosa :"
@  2, 2 SAY "      Dependencia :"
@  3, 2 SAY "      Liquidaci¢n :"

@  4, 2 SAY "       Cadena Fun.:"
@  5, 2 SAY " F.Financiamiento :"
 
@  6, 2 SAY "          Funci¢n :"
@  7, 2 SAY "         Programa :"
@  8, 2 SAY "      Subprograma :"
@  9, 2 SAY "  Activ./Proyect. :"
 
@ 10, 2 SAY "          Destino :"
@ 11, 2 SAY "     Obs. Almacen :"
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Pecosa
IF EOF()
   DO Pantalla
   RETURN
ENDIF
ACTIVATE WINDOW Wind_1

ON KEY LABEL F4 DO Imprimir
ON KEY LABEL F9 DO vista_det
SCATTER MEMVAR
=val_CODCAD(ALLT(m.codcad),m.periodo,'C') 

IF USer_tp$'E' 
	@  1,22 SAY m.FecPec
	@  1,60 SAY m.periodo
	@  1,63 SAY m.NumPec
	DO STANDBY with 'Pecosa Elaborando por '+user
	IF !BOF()
		SKIP -1
		DO VISTA
	ENDIF   
 ELSE
	@  0,00 SAY IIF(m.user_tp='C',PADC("®Corregido por "+ IIF(!EMPTY(m.User_CR),ALLTRIM(m.User_CR),ALLTRIM(m.User))+'¯',79,' '),PADC("®Elaborado por "+ ALLTRIM(m.User)+'¯',79,' '))
	@  0,02 SAY IIF(m.TipPec='S','Pecosa Stock     ',IIF(m.TipPec='O','Pecosa Compra    ',IIF(m.TipPec='B','Pecosa Bayovar',IIF(m.TipPec='T','Pecosa Transporte','Pecosa Caja Chica')))) COLOR SCHEME 02 
	@  0,60 SAY vEstPec(m.Estado) COLOR SCHEME 02
	@  1,22 SAY m.FecPec
	@  1,60 SAY m.periodo
	@  1,63 SAY m.NumPec
	
	@  2,22 SAY val_para(m.CodDep,'CODDEP','A',22,40,7)
	@  3,22 say m.fecdesp
	
	@  4,22 SAY val_codcad(m.codcad,m.periodo,'D',22,30)
	@  5,22 SAY val_para(m.Codfte,'CODFTE','D',22,30)
	
	@  6,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
	@  7,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
	@  8,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
	@  9,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)
	
	@ 10,22 SAY m.destino picture '@S56'
	@ 11,22 SAY m.ObsAlma picture '@S50'
	*@ 11,22 SAY m.Observa && PRUEBA
	
	DO VISTA_HIJO
	
	IF m.estado='50'
		do total
	 else
		deactivate WINDOW WIND_3
	endif
ENDIF
IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN
 
procedure calcula
*---------------- 
select itepec
seek m.periodo+m.numpec
vTotal = 0
scan while itepec.periodo=m.periodo and itepec.numpec=m.numpec
     vTotal = vTotal + itepec.preuni*itepec.candesp*IIF(ITEPEC.ESTADO='50' OR !EMPTY(FECDESP),1,0)
endscan
seek m.periodo+m.numpec
SELECT PECOSA
return vtotal

procedure calcula_S
*------------------ 
select itepec
seek m.periodo+m.numpec
vTotal = 0
scan while itepec.periodo=m.periodo and itepec.numpec=m.numpec
     vTotal = vTotal + itepec.cosmed*itepec.candesp*IIF(ITEPEC.ESTADO='50' OR !EMPTY(FECDESP),1,0)
endscan
seek m.periodo+m.numpec
SELECT PECOSA
return vtotal

PROCEDURE VISTA_HIJO
*-------------------
hide popup all
SELECT ItePec
VECES = 1
*SET RELATION TO PERIODO+NUMOC+NEWFTE+CODART+NUMORD INTO ITEOC
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.numpec TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   EsX=buscoc() :H=' ' :W=.F. ,;
   Ess=IIF(TIPPEC='S','Stock',verestdo()) :H='Estado' :W=.F. ,;
   Descri       : H= 'Descripci¢n' :33 :W=.F.  ,;
   Coduni       : H= 'Uni'      :W=.F. :3 ,;
   Canreq       : H= 'Pedido' :P='99,999.99' :W=!EMPTY(CodArt) ,;
   Candesp      : H= 'Atendido' :P='99,999.99',;
   aa=iif(preuni=0,cosmed,PREUNI)   : h= 'Precio ' :P='9,999,999.9999',;
   XX=IIF(EMPTY(FecDesp),'        ',FecDesp)   : H= 'Fec Aten' 
  SELE PECOSA
RETURN

PROCEDURE buscoc
****************
PRIVATE vMens
VECES=1
vMens='*'
DO CASE
   CASE Estado= '50' and Tippec='S'
        vMens='û'
   CASE Estado= '30' 
        alis = alias()
        vkey = PERIODO+NUMPEC+NEWFTE+CODART+NUMORD
        select iteoc
        seek vkey
        vMens=iif(found(),iif(estado='50','û',iif(estado='95','R','')),'î') 
        select (alis)
   CASE Estado= '50' and Tippec#'S'
        alis = alias()
        vkey = PERIODO+NUMPEC+NEWFTE+CODART+NUMORD
        select iteoc
        seek vkey
        if found() 
        	if estado#'50'
        	   If estado='9'
        	      *do standby with 'Item (÷) N§ '+alltrim(itepec.numord)+' est  Anulada o Rebajada'
        	   else   
	              *do standby with 'Item (÷) N§ '+alltrim(itepec.numord)+' no est  Liquidada la O/C'
	           endif
	           vmens = 'R'
	        else
	           vmens = 'û'
	        endif
	    else    
	      vMens='?'
	    endif    
        select (alis)
   CASE Estado= '99'
        vMens= ''
   CASE Estado= '00'
        vMens= ''
ENDCASE            
return vMens   




PROCEDURE VERESTDO
******************
PRIVATE vMens
vmens= '---'
DO CASE
   CASE Estado= '00'
        vMens='Pendte'
   CASE Estado= '10'
        vMens='Progra'
   CASE Estado= '20'
        vMens="SC"+NumSc 
   CASE Estado= '30'
        vMens="OC"+NumOc 
   CASE Estado= '50'
        vMens="OC"+NumOc
   CASE Estado= '99'
        vMens= 'Anulad'
ENDCASE            
return vMens   

PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON
SELECT Pecosa
VCON = RECNO()
SCATTER MEMVAR
fdbf = sys(3)+'.DBF'
fidx = sys(3)+'.IDX'
copy stru to (fdbf)
use (fdbf) in 5 alias def exclu
select def
index on periodo+numpec to (fidx)
append blank
gather memvar
SET RELATION TO periodo + numpec INTO itepec
SET SKIP TO ITEPEC
IF EOF()
	DO STANDBY WITH vmens08
	RETURN
 ELSE
	DIMENSION aFecha(200)
	AFECHA={  /  /  }
	I = 0
	SCAN
		vFecha  = ItePec.fecdesp
		lDescarta = .F.
		FOR J = 1 to I
			IF aFecha(j) = vFecha
				lDescarta = .T.
				EXIT
			ENDIF
		ENDFOR
		IF !lDescarta
			I = I + 1
			aFecha(i) = vFecha
		ENDIF
	ENDSCAN
	
	DEFINE WINDOW W_Fecha FROM 05,10 TO 15,60 DOUBLE;
		TITLE Vmens01 COLOR SCHEME 10
		
	ACTIVATE WINDOW W_Fecha
	nFecha = 1
	
	@ 01,01 SAY "Selecciones Fecha :" GET nfecha FROM aFecha SIZE 6,14 DEFAULT AfECHA(1)  COLOR SCHEME 5
	READ
	
	RELEASE WINDOW W_Fecha
	
	SELE ItePec
	SET FILTER TO ItePec.fecdesp=AFecha(nFecha)
	GO TOP
	SELE AstPat
	SET FILTER TO Periodo=Def.Periodo AND NumMes=PADL(ALLTRIM(STR(MONT(AFecha(nFecha)))),2,'0') AND NumRef=NumPec AND fecha=AFecha(nFecha) AND TipDoc='PEC'
	SELE Def
	DO REPPRG WITH "PECOSARep",' ORDEN DE COMPRA'
*	DO reporte with 2,"Despach1",' Pe.co.sa ',2
ENDIF
SET RELATION TO
SELE ASTPAT
SET FILTER TO
SELE ItePec 
SET FILTER TO
SELECT PECOSA
GO VCON
DO VISTA
RETURN

PROCEDURE PECOSARep
*------------------
PARAMETER _desti
private nReg,cNum
IF _desti=2
	SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINT
Impri    = .F.
xColumna = SPACE(7)
GO TOP
@ 00,00 SAY CHR(27)+CHR(64)
*SCAN
	STORE 0 TO pagina, linea, vSuma,vNum
	nReg = RECNO()
	SELE ItePec
	SCAN WHILE Def.NumPec = ItePec.NumPec
		IF pagina = 0 .OR. linea > 60
			DO Titulo
		ENDIF
		DO CASE
	 		CASE linea < 39
				vNum = vNum + 1
				@ LINEA,00  SAY CHR(15)
				@ LINEA,01  SAY PADL(ALLTRIM(STR(VNUM,2)),2,'0')
				@ LINEA,04  SAY CHR(179)
				@ LINEA,05  SAY ItePec.CodArt
				@ LINEA,15  SAY CHR(179)
				@ LINEA,16  SAY itepec.canreq
				@ LINEA,25  SAY CHR(179)
				@ LINEA,26  SAY LEFT(ALLTRIM(ITEPEC.DEScri),50)
*				@ LINEA,26  SAY ALLTRIM(ITEPEC.DEScri) + IIF(EMPTY(MLINE(ItePEC.observa,1)),' ','')		&&' - '+ALLTRIM(MLINE(ItePEC.observa,1))
				@ LINEA,70  SAY CHR(179)
				@ LINEA,71  SAY CHR(179)
				@ LINEA,73  SAY itepec.numoc
				@ LINEA,78  SAY CHR(179)
				@ LINEA,79  SAY itepec.candesp		PICTURE "@Z 99,999.99"
				@ LINEA,88  SAY CHR(179)
				@ LINEA,89  SAY ItePec.fecdesp
				@ LINEA,99  SAY CHR(179)
				@ LINEA,100 SAY ItePec.CodUni		PICTURE "@TI"
				@ LINEA,111 SAY CHR(179)
				@ LINEA,112 SAY ItePec.PreUni		PICTURE "@Z 99,999.9999"
				@ LINEA,123 SAY CHR(179)
				@ LINEA,124 SAY ROUND(itepec.preuni*itepec.candesp,3)	PICTURE "@Z 9,999,999.999"
				@ LINEA,137 SAY CHR(179)
				vSuma = vSuma + ROUND(itepec.preuni*itepec.candesp,3)
				linea = linea + 1
			CASE linea >= 39
				@ linea,1 say Chr(18)
				@ linea,05 say repli('-',80)
				linea = linea + 1
				@ linea,50 say 'S U B T O T A L   S/.'
				@ Linea,71 say vSuma PICTURE '999,999.99'
				linea = linea + 2
				DO SUMARIO
				DO TITULO
				SKIP -1 IN ItePec
				@ linea,01 say chr(18)
				@ linea,52 say 'V I E N E N  S/.'
				@ linea,71 say vSuma PICTURE '999,999.99'
				linea = linea + 1
			OTHER
		ENDCASE
		
		IF !EMPTY(ALLTRIM(RIGHT(ITEPEC.DEScri,50)))
			@ linea,26 say SUBSTR(ITEPEC.DEScri,51,50)
			linea = linea + 1
		ENDIF
		
 		IF !EMPTY(ITEPec.OBSERVA)
			for xx = 1 to MEMLINES(ITEPec.OBSERVA)
				@ linea,26 say MLINE(ItePec.observa,xx)
				linea = linea + 1
				if linea >= 39
					@ linea,1 say chr(18)
					@ linea,05 say repli('-',80)
					linea = linea + 1
					@ linea,56 say '    V A N   S/.'
					@ Linea,71 say vSuma PICTURE '999,999.99'
					linea = linea + 2
					DO SUMARIO
					DO TITULO
					@ linea,01 say chr(18)
					@ linea,52 say 'V I E N E N  S/.'
					@ linea,71 say vSuma PICTURE '999,999.99'
					@ LINEA,79 SAY CHR(15)
					linea = linea + 1
				endif
			endfoR
		ENDI    
	ENDSCAN
	
	@ linea,1 say chr(18)
	@ linea,05 say repli('Í',80)
	linea = linea + 1
	@ linea,56 say 'T O T A L   S/.'
	@ Linea,71 say vSuma PICTURE '999,999.999'
	linea = linea + 1
	@ Linea,71 say 'ÍÍÍÍÍÍÍÍÍÍÍÍ'
	linea = linea + 1
	
	**  ASIENTOS PATRIMONIALES
	
	IF linea >= 37
		@ linea,05 say repli('-',80)
		linea = linea + 1
		DO SUMARIO
		DO TITULO
		linea = linea + 1
	ENDIF
	
	SELE AstPat
	GO TOP
	lINEA = Linea + 2
	@ LINEA,0   SAY CHR(18)
	@ LINEA,04  SAY PADC("CUENTAS PATRIMONIALES",47)
	lINEA = Linea + 1
	@ LINEA,04  SAY PADC("CUENTAS",10)
	@ LINEA,18  SAY PADC("DEBE",13)
	@ LINEA,34  SAY PADC("HABER",13)
	lINEA = Linea + 1
	@ LINEA,04  SAY repli('~',47)
	Linea = Linea + 1
	SCAN			&& WHILE Def.NumPec = ItePec.NumPec
		IF linea > 60
			DO Titulo
		ENDIF
		DO CASE
			CASE linea < 42
				@ Linea,04  SAY CodCta   PICTURE '!!!!!!!!!!'
				@ Linea,18  SAY IIF(TipCta='D',MtoDeb,0)   PICTURE '99,999,999.99'
				@ Linea,38  SAY IIF(TipCta='H',MtoHab,0)   PICTURE '99,999,999.99'
				linea = linea + 1
			CASE linea >= 42
*				@ linea,05 say repli('-',80)
				linea = linea + 1
				DO SUMARIO
				DO TITULO
				linea = linea + 1
				SKIP -1
			OTHER
		ENDCASE
	ENDSCAN
*	@ linea,1 say chr(18)
*	@ linea,05 say repli('Í',80)
*	linea = linea + 1
*	@ linea,56 say 'T O T A L   S/.'
*	@ Linea,71 say vSuma PICTURE '999,999.999'
*	linea = linea + 1
*	@ Linea,71 say 'ÍÍÍÍÍÍÍÍÍÍÍÍ'
*	linea = linea + 1
	
	GO nReg
*	do coDific
	do sumario
	SELE Def
*ENDSCAN
SET DEVICE TO SCREEN
SET PRINTER TO

RETURN

PROCEDURE Titulo
*---------------
pagina = pagina  + 1
vTitulo = ' PEDIDO - COMPROBANTE DE SALIDA '
@ 0,01  SAY CHR(15)
@ 0,02  SAY ALLTRIM(cia)
@ 0,62  SAY CHR(18)+CHR(14)
@ 0,66  SAY "PEDIDO N§ " + Def.numpec+'.'+Def.periodo +CHR(27)+'H'+CHR(15)
@ 0,93  SAY 'P g.'
@ 0,98  SAY pagina   PICTURE '####'
@ 1,01  SAY CHR(18)+CHR(14)
@ 1,04  SAY VTITULO++CHR(27)+'H'+CHR(18)+CHR(15)
@ 1,40  SAY "Tipo:"+IIF(Def.tippec='S','Stock',iif(Def.tippec='O','Compra',IIF(Def.tippec='T','Transporte','Referencia')))
@ 1,53  SAY PADL(ALLTRIM(STR(MONT(DATE()))),2,"0")+"/"+ALLTRIM(STR(YEAR(DATE())))
@ 2,01  SAY "Dependencia Solicitante:"
@ 2,26  SAY val_para(DEF.coddep,'CODDEP','D',22,60)
@ 2,77  SAY "Lugar y Fecha:"
@ 2,92  SAY fecha(DEF.FECPEC)
@ 3,0   SAY 'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 4,00  SAY "³Solicito entregar a :"+Def.Atte
@ 4,110 SAY "DEPENDENCIA :"+Def.CodDep
@ 4,132 SAY "³"
@ 5,00  SAY "³CodDestino a:"+ALLTRIM(DEF.destino)+' '+',los siguientes art¡culos:'
@ 5,132 SAY "³"
@ 6,00  SAY 'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
@ 7,0   SAY 'ÚÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 8,0   SAY '³Rgl³ C¢digo   ³Cantidad ³              Descripci¢n                   ³³N§ O/C³DesPacho ³   Fecha  ³  Unidad   ³    P.U.   ³  T O T A L  ³'
@ 9,0   SAY 'ÀÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
linea = 10
RETURN


PROCEDURE sumario
*----------------
@ 43,0  SAY CHR(15)
@ 43, 1 SAY 'Fte. Financiamiento:'
@ 43,22 SAY ALLTRIM(VAL_PARA(PECOSA.CODFTE,'CODFTE','D',22,60))
@ 44,06 SAY IIF(!EMPTY(observa),'OBSERVACIONES :',' ')
@ 44,22 SAY LEFT(obsAlma,67)
@ 44,99 SAY 'ÚÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿'
@ 45,22 SAY SUBSTR(obsAlma,68,67)
@ 45,99 SAY '³Cadena³Func.³Prog.³Subpr.³Act./Proy.³'
@ 46,22 SAY SUBSTR(obsAlma,135,67)
@ 46,99 SAY 'ÃÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´'
@ 47,99 SAY CHR(179)
@ 47,101 SAY CodCad
@ 47,106 SAY CHR(179)
@ 47,108 SAY alltrim(maepre.codfun)
@ 47,112 SAY CHR(179)
@ 47,114 SAY alltrim(maepre.codprg)
@ 47,118 SAY CHR(179)
@ 47,120 SAY alltrim(maepre.codspr)
@ 47,125 SAY CHR(179)
@ 47,127 SAY alltrim(maepre.actpry)
@ 47,136 SAY CHR(179)
@ 48,06 SAY 'Elaborado por: ' + alltrim(VUSUA(IIF(User_Tp='I',USER,USER_Cr)))
@ 48,99 SAY 'ÀÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ'
@ 49,01 SAY '__ Equipo de Oficina ........ S/.          __ Gastos de Operaci¢n ......... S/.            __ Bienes en Deposito ..... S/.'
@ 50,01 SAY '__ Equipo de Transporte ..... S/.          __ Reconstrucci¢n de Equipos.... S/.            __ Pedido en Transito ..... S/.'
@ 51,01 say '__ Maquinaria y Equipo ...... S/.          __ Construcciones en Curso...... S/.            __                  ....... S/.'

@ 54,03 SAY 'ÚÄ¿                            ÚÄ¿                             ÚÄ¿                             ÚÄ¿'
@ 55,03 SAY '³1³                            ³2³                             ³3³                             ³4³'
@ 56,03 SAY 'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ    ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ     ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ     ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ'
@ 57,03 say '         Solicitante               Jefe de Abastecimiento             Jefe de Almacen           Recib¡ comforme:    /  /'
@ 59, 1 say chr(15)+VAL_BOY(PADL(ALLTRIM(STR(MONTH(PECOSA.FECPEC),2)),2,'0'),'DERNI¥','D')
linea = 1
@ 60, 1 SAY CHR(12)
RETURN


PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
IF m.tippec='S'
	@ 0,0 SAY calcula_s() picture '9,999,999.99'
else
	@ 0,0 SAY calcula() picture '9,999,999.99'
endif	
return



PROCEDURE VISTA_ING
*-------------------
IF ESTADO#'50' OR tippec#'S'
   DO STANDBY WITH 'El Pe.Co.Sa. no esta liquidada o no es de Stock'
   do vista
   retur
endif   
hide popup all
VALOR=0
ON KEY LABEL F7
SELECT ItePec
BROWSE ;
   KEY m.periodo+m.numpec  ;
   WINDOW Wind_2 ;
   FIELDS;
   EsX=IIF( Estado= '00',' ',IIF( Estado = '20'," ",IIF(Estado='99','x',IIF(Estado='50','û',' ')))) :H=' ' :W=.F. ,;
   Ess=IIF(TIPPEC='S','Stock',IIF( Estado= '00','Pendte',IIF( Estado = '10',"Progra",IIF( Estado = '20',"SC"+NumSc,IIF(Estado='99','Anulad',IIF(Estado='50',"OC"+NumOc,"OC"+NumOc)))))) :H='Estado' :W=.F. ,;   
   Descri      : H= 'Descripci¢n' :33 :W=.F.  ,;
   Coduni      : H= 'Uni'      :W=.F. :3 ,;
   Canreq      : H= 'Pedido' :P='99,999.99' :W=.F. ,;
   Candesp     : H= 'Atendido' :P='99,999.99' :W=.F.,;
   costot      : H= 'Total' :P='99,999.99' :v=divide():f,;
   cosmed      : h= 'Precio ' :P='99,999.999' :v=MULTIPLI():f,;
   XX=IIF(EMPTY(FecDesp),'        ',FecDesp)   : H= 'Fec Aten' :W=.F.
   
   SELE PECOSA
ON KEY LABEL F7 DO VISTA_ING   
DO VISTA
RETURN



PROCEDURE DIVIDE
*---------------
IF CANDESP#0
   REPLACE COSMED WITH costot/CANDESP
ELSE   
   REPLACE COSMED WITH 0
ENDIF
RETURN .T.   



PROCEDURE MULTIPLI
*-----------------
IF CANDESP#0
   REPLACE COSTOT WITH cosMED*CANDESP
ELSE   
   REPLACE COSTOT WITH 0
ENDIF
RETURN .T.   



PROCEDURE VISTA_DET
*------------------
hide popup all
ON KEY LABEL F9 DO OBSERVA
SELECT ItePec
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.numpec ;
   WINDOW Wind_2 ;
   FIELDS;
   EsX=IIF( Estado= '00',' ',IIF( Estado = '20'," ",IIF(Estado='99','x',IIF(Estado='50','û',' ')))) :H=' ' :W=.F. ,;
   Ess=IIF(TIPPEC='S','Stock',IIF( Estado= '00','Pendte',IIF( Estado = '10',"Progra",IIF( Estado = '20',"SC"+NumSc,IIF(Estado='99','Anulad',IIF(Estado='50',"OC"+NumOc,"OC"+NumOc)))))) :H='Estado' :W=.F. ,;   
   Descri      : H= 'Descripci¢n' :34 :W=.F.  ,;
   Coduni      : H= 'Uni'      :W=.F. :3 ,;
   Canreq      : H= 'Pedido' :P='99,999.99' :W=!EMPTY(CodArt) ,;
   Candesp     : H= 'Atendido' :P='99,999.99',;
   qq=IIF(TIPPEC='S',cosmed,preuni)   : H= 'Preuni' :P='99,999.99',;
   aa=iif(preuni=0,cosmed*CANDESP,PREUNI*CANDESP)  : h= 'Precio Total ' :P='9,999,999.9999',;
   XX=IIF(EMPTY(FecDesp),'        ',FecDesp)   : H= 'Fec Aten' 
   SELE PECOSA
 ON KEY LABEL F9 DO vista_det   
 DO VISTA
RETURN

FUNCTION Observa
*---------------

vAlias = ALIAS()
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F9
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle Pecosa ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
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
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci¢n '
ENDIF

ON KEY LABEL F9 DO Observa
RETURN .T.


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE PECOSA
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO PERIODO+NUMPEC INTO ITEPEC
 SET SKIP TO ITEPEC
 IF escolor
    DEFINE POPUP Busmenu FROM 15,50  SHADOW COLOR &L_COL
 ELSE
    DEFINE POPUP Busmenu FROM 15,50  COLOR SCHEME C_POPUP
 ENDIF
 DEFINE BAR 1 OF Busmenu PROMPT ' ordenado por:  \<N£mero      '
 DEFINE BAR 2 OF Busmenu PROMPT ' ordenado por:  \<Dependencia '
 ON SELECTION POPUP Busmenu DEACTIVATE POPUP
 ACTIVATE POPUP Busmenu
 DO CASE
     CASE BAR()=1
          set orde to pecosa1
     CASE BAR()=2
          set orde to pecosa3
 ENDCASE     
 IF LASTKEY()=27
    sele pecosa
    SET ORDE TO PECOSA1
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
 NumPec :H=' N§ ' ,;
 Est = IIF(Itepec.Estado= '00','Pend',IIF(Itepec.Estado = '10',"Progra",IIF(Itepec.Estado = '30',itepec.NumOc,IIF(Estado='99','Anul',IIF(Itepec.Estado='50','Aten','S/Ct'))))) :H='Estd' ,;
 Codfte :H='Fte ' ,;
 FecPec :H='Fecha' ,;
 Coddep :H='Dep',;
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
 set orde to PECOSA1
 SET RELA TO
 DO Vista
 RETURN


PROCEDURE XRevis              && Revisi¢n de BD en browse
*--------------
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
 CodFte :H='Fte',;
 Est = vestOC(Estado) :H='ESTD' :4 ,;
 FecPec :H='Fecha' ,;
 Coddep :H='DEP',;
 Itepec.CanReq : H='Cantidad':P='99,999',;
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
vtemp    = RECNO()
vPeriodo = RIGHT(DTOC(DATE()),2)
vNum_pec = '    '
vCod_fte = '   '
ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero Pecosa: ' GET vPeriodo PICTURE '!!'
 @ 1,27 SAY '-' GET vNum_Pec PICTURE '!!!!' VALID vBusca()
*@ 1,33 SAY '-' GET vcod_fte PICTURE '!!!' VALID VAL_PARA(vCod_Fte ,'CODFTE','C',33,20)
READ
DEACTIVATE WINDOW standby

 IF EMPTY(vNum_pec) .or. LASTKEY()=27
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
RETURN

PROCEDURE vBusca
*---------------
vnum_pec=padl(alltrim(vnum_pec),4,'0')
retur .t.

PROCEDURE Anter
*--------------
SELE PECOSA
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
SELE PECOSA
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



PROCEDURE LIQUIDA
*----------------
 SELECT Pecosa
 vRe=recno()
 SCATTER MEMVAR
IF M.ESTADO='50'
   DO STANDBY WITH 'El Pe.co.sa. ya est  liquidada'
   do vista
   return
endif   
 IF M.ESTADO#'40'
    DO STANDBY WITH 'Todav¡a no esta atendido'
    do vista
    return
 endif   

ACTIVATE WINDOW STANDBY
 @ 1,5 SAY "Fecha de despacho:" get m.fecdesp
 read VALID VAL_READ()
DEACTIVATE WINDOW STANDBY

IF LASTKEY()#27
	m.ESTADO = iif(m.estado='51','51','50')
    gather memvar
 ENDIF	
 DO Vista
 RETURN


PROCEDURE Corri
*--------------
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
 
SELECT PECOSA
ACTIVATE WINDOW Wind_1
SCATTER MEMVAR
 
@ 3,22 get m.fecdesp
read VALID VAL_READ()
 
IF LASTKEY() # 27
   SELE PECOSA
   GATHER MEMVAR
ELSE
   DO STANDBY WITH 'Proceso cancelado'
ENDIF

SELECT PECOSA
DO PANTALLA
DO Vista
RETURN
  

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
SELECT Pecosa
if estado='50'
   do standby with 'El Pe.co.sa ya est  liquidada'
   do VISTA
   retu
endif

DO CASE
	CASE TIPPEC='O'  AND estado='10'
		do standby with 'El Pe.co.sa no est  atendido'
		retu
	CASE TIPPEC='B'  AND estado='10'
		do standby with 'El Pe.co.sa no est  atendido'
*................
* no se utiliza
*		do CONTI
*................
		retu
*	CASE estado='00'
*		do standby with 'El Pe.co.sa no est  Programado'
*		do vista
*		retu
ENDCASE

ACTIVATE WINDOW Wind_1

vRe=recno()
SCATTER MEMVAR

@ 11,22 GET m.ObsAlma FUNCTION 'S50'

READ

IF LASTKEY() # 27
	DO WHILE .T.
		Ok = Trabaja_Hijo()
		IF LASTKEY() # 27 AND OK
			IF YESNO('¨ Confirme el ingreso ?')
				DO WSTOCKX
				EXIT
			ENDIF
		 ELSE
			DO STANDBY WITH ' Cancelado el Ingreso ...'
			OK = .F.
			EXIT
		ENDIF
	ENDDO
	
	IF Ok .AND. LASTKEY() # 27
		SELE ITEPEC
		* set orde to ITEPEC5
		SET FILTER TO ESTADO #'99'
		SET RELA TO PERIODO+NUMPEC INTO PECOSA
		seek m.Periodo+m.NumPec
		vIg = 0
		vEs = 0
		vfec=itePEC.fecdesp
		ENTRO=.F.
		SCAN WHILE PERIODO+NUMPEC =m.Periodo + m.NumPec
			IF Itepec.CanDesp1=0
				vEs = vEs + 1
			 ELSE
				SELE ITEpec
				IF RLOCK()
					REPLACE CANDESP WITH CANDESP+CANDESP1,ESTADO WITH IIF(CANDESP=CANREQ,'50','00'),CANDESP1 WITH 0
				ENDIF
				
				IF (candesp < canreq)
					if estado # '50'
						vIg = vIg + 1
					endif
				endif
			ENDIF
			ENTRO = .T.
		ENDSCAN
		SELECT pecosa
		
		**********
		ENTRO = .T.
		***********
		
		seek m.Periodo+m.NumPec
		IF ENTRO
			if vIg = 0 AND vEs = 0
				m.ESTADO = iif(m.estado='51','51','40')
			endif
			m.User_Tp  = 'C'
			m.User_Cr = vUser_Id
			m.User_Fc = m.Fecha
			gather memvar
		 ELSE
			DO STANDBY WITH ' No se pudo Atender este Pe.co.sa. '
		ENDIF
	ENDIF
	
	SELE ItePec
	SEEK m.Periodo+m.NumPec
	****
	
	DIMENSION aFec(200,2)
	
	AFEC=''
	I = 0
	SCAN WHILE Periodo+NumPec = m.Periodo+m.NumPec
		vFecha  = ItePec.fecdesp
		vMonto  = 0
		lDescarta = .F.
		FOR J = 1 to I
			IF aFec(j,1) = vFecha
				aFec(j,2) = aFec(j,2)+ItePec.CanDesp*ItePec.PreUni
				lDescarta = .T.
				EXIT
			ENDIF
		ENDFOR
		IF !lDescarta AND !EMPTY(vFecha)
			I = I + 1
			aFec(i,1) = vFecha
			aFec(i,2) = ItePec.CanDesp*ItePec.PreUni
		ENDIF
	ENDSCAN
	nTot = i
	
	i=0

	FOR i = 1 TO nTot
		gFec = aFec(i,1)
		iIgv = IIF(DTOC(gFec,1)>'20110228',1.18,1.19)
		aFec(i,2) = ROUND((aFec(i,2))/iIgv,2)
	ENDFOR
	
	
	DIMENSION aFec(I,2)
	
	i=0
	j=0
	xTmp ={  /  /  }
	FOR i = 1 to ntot-1
		FOR j = I+1 TO nTot
			IF aFec(i,1)>aFec(j,1)
				xTmp    = aFec(i,1)
				aFec(i,1) = aFec(j,1)
				aFec(j,1) = xTmp
				
				xTmp    = aFec(i,2)
				aFec(i,2) = aFec(j,2)
				aFec(j,2) = xTmp
			ENDIF
		ENDFOR
	ENDFOR
	
	FOR i = 1 TO nTot
		SELE AstPat
		IF !SEEK(m.Periodo+PADL(ALLTRIM(STR(MONT(aFec(i,1)))),2,'0')+m.numpec+'PEC'+DTOC(aFec(i,1),1))
			DO AgrIte1 WITH aFec(i,1), aFec(i,2)
		ENDIF
	ENDFOR
		
	****
	SELE ItePec
	SEEK m.Periodo+m.NumPec
	
	DO IngAp
	
	SELE ITEPEC
	set orde to ITEPEC1
	SET RELATION TO
	SET FILTER TO
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT Pecosa
go vRe
do pantalla
DO Vista
RETURN


PROCEDURE Trabaja_Hijo
*---------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°°°Presione ®F10¯ para salir grabando o  ®Esc¯ para cancelar°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F9  DO Obs_Item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 ON KEY LABEL F11 do liqparc
 ON KEY LABEL F12 do nliqparc
 SELE ITEPEC
 set rela to periodo+numpec into pecosa
 SET ORDE TO ITEPEC1
 SET FILTER TO ESTADO #'99'
 SEEK m.periodo+m.numpec
 if FOUND()
     if pecosa.tippec='S'
	   BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_5 KEY m.periodo+m.numpec FIELD ;
       EsX=IIF( Estado= '00',' ',IIF( Estado = '20'," ",IIF(Estado='99','x',IIF(Estado='50','û',' ')))) :H=' ' :W=.F. ,;
	   Descri      : H= 'Descripci¢n' :35 :W=.F.  ,;
	   Coduni      : H= 'Uni'      :W=.F. :3 ,;
	   Canreq      : H= 'Pedido' :P='99,999.999' :W=.F.,;
	   Candesp     : H= 'Desp.' :P='99,999.999' :w=estado#'50' :v=val_can():f ,;
	   FecDesp     : H= 'Atendido'  :W=estado#'50' and CANDESP>0 ,;
	   PreUni      : H= 'PreUni' :W=estado#'50' :P= '9,999,999.9999'
   else
      BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_5 KEY m.periodo+m.numpec FIELD ;
       EsX=IIF( Estado= '00',' ',IIF( Estado = '20'," ",IIF(Estado='99','x',IIF(Estado='50','û',' ')))) :H=' '  ,;
	   Ess=IIF( Estado= '00','Pendte',IIF( Estado = '10',"Progra",IIF( Estado = '20',"SC"+NumSc,IIF(Estado='99','Anulad',IIF(Estado='50','Atendi',"OC"+NumOc))))) :H='Estado' :W=.F. ,;
	   Descri      : H= 'Descripci¢n' :35 :W=.F.  ,;
	   Coduni      : H= 'Uni'      :W=.F. :3 ,;
	   Canreq      : H= 'Pedido' :P='99,999.999' :W=.F.,;
	   Candesp     : H= 'Desp.' :P='99,999.999' :w=estado#'50' :v=val_can():f ,;
	   FecDesp     : H= 'Atendido'  :W=estado#'50' and CANDESP>0 ,;
	   PreUni      : H= 'PreUni' :w=estado#'50' :P= '9,999,999.9999'

*		Candesp1    : H= 'X Desp.' :P='99,999.99' :w=estado#'50' :v=val_can():f ,;
*		Candesp1    : H= 'X Desp.' :P='99,999.99' :w=estado#'50' :v=val_can():f ,;
*		and TipPec='S'
	endif
   
   ON KEY LABEL F9
   ON KEY LABEL F10
   ON KEY LABEL F11
   ON KEY LABEL F12

   SET FUNCTION  F12 TO
   set filter TO
   ACTIVATE SCREEN
   vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
   DO Logos WITH Rotulo1,vTempo
   SHOW MENU mMenu
   SELECT Pecosa
   RETURN .T.
 else
   do standby with 'No se tiene articulos con Orden de Compra'
   set filter TO
   ACTIVATE SCREEN
   vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
   DO Logos WITH Rotulo1,vTempo
   SHOW MENU mMenu
   SELECT Pecosa
   RETURN .F.
endif
return


PROCEDURE ingap   && F5
*--------------
PRIVATE cNum
USE cuentas  IN 0  ORDER TAG cuentas1     ALIAS cuenta
SELECT astpat
SET FILTER TO Periodo+numref+TipDoc = m.periodo+m.numpec+'PEC'

ON KEY LABEL F5 DO agrite
ON KEY LABEL F8 DO eliite
ON KEY LABEL F10 KEYBOARD CHR(23)

BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 FIELDS;
	PERIODO,;
	Fecha :H='Desp.':V=Valf() :F,;
	NUMREF:F,;
	codcta :H='Cuenta':v= val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2) :F,;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :p='99,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='99,999,999.99'

*KEY m.periodo+cNum+m.numpec+'PEC'

*SEEK m.Periodo+cNum+m.numpec+'PEC'

GO TOP

STORE 0 TO vdebe, vhaber

SCAN WHILE Periodo=m.Periodo and numref=m.NumPec AND TipDoc='PEC'		&&nummes=cNum AND 
	vdebe  = vdebe  + IIF(tipcta='D',mtodeb,0)
	vhaber = vhaber + IIF(tipcta='H',mtohab,0)
ENDSCAN

SET FILTER TO 

IF vdebe#vhaber
	DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF


USE IN Cuenta

DEACTIVATE WINDOW wind_12
RETURN

FUNCTION Valf
*------------
REPLACE NumMes WITH PADL(MONTH(Fecha),2,'0')
RETURN .T.

PROCEDURE ValNM
*--------------
REPLACE NumMes WITH PADL(MONT(Fecha),2,'0')
RETURN

PROCEDURE agrite
*---------------
SELECT astpat
FOR I = 1 TO 2
	IF f_appd()
		REPLACE periodo WITH m.periodo,;
				numref WITH m.numpec,;
				tipdoc WITH 'PEC',;
				TipCta WITH IIF(I=1,'D','H')

*				nummes WITH PADL(MONT(ItePec.FecDesp),2,'0')
*				fecha  WITH itepec.FecDesp


	ENDIF
ENDFOR
RETURN

PROCEDURE agrite1
*----------------
PARAMETERS xFec,xMto
PRIVATE i
SELECT astpat
FOR i = 1 TO 2
	IF f_appd()
		REPLACE periodo WITH m.periodo,;
				nummes WITH PADL(MONT(xFec),2,'0'),;
				numref WITH m.numpec,;
				tipdoc WITH 'PEC',;
				fecha  WITH xfec,;
				TipCta WITH IIF(i=1,'D','H'),;
				MtoDeb WITH IIF(i=1,xMto,0),;
				MtoHab WITH IIF(i=2,xMto,0)
	ENDIF
ENDFOR
RETURN

PROCEDURE eliite
*---------------
SELECT astpat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN

PROCEDURE Obs_Item
*-----------------
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F9
DEFINE WINDOW Detalle FROM 03,12 TO 20,67 FLOAT NOCLOSE SHADOW DOUBLE TITLE ALLTRIM(Descri) FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1

IF WVISIBLE("Detalle")
	ACTIVATE WINDOW Detalle SAME
 ELSE
	ACTIVATE WINDOW Detalle NOSHOW
ENDIF

MODIFY MEMO Observa WINDOW Detalle
IF !WVISIBLE("Detalle")
	ACTIVATE WINDOW Detalle
ENDIF
RELEASE WINDOW Detalle

ON KEY LABEL F9 DO Obs_Item

RETURN .T.


PROCEDURE CONFIRMA
*-----------------
SELECT ITEPEC
alis = alias()
*vkey = ALLTRIM(PERIODO+NUMPEC+NEWFTE+CODART+NUMORD)
vkey = ALLTRIM(PERIODO+NUMPEC+CODART)
select iteoc
seek vkey
if found() and estado='50' 
   vOk = .t. 
else
   DO STANDBY WITH 'La O/C no est  liquidada..'
   vOk = .f. 
endif
select (alis)
return vok


PROCEDURE FECREP
*---------------
SET FUNCTION F3 TO DTOC(FECDESP)
RETURN FECDESP

procedure liqparc
*----------------
if itepec.candesp=itepec.canreq
   replace estado with '50'
endif   

procedure nliqparc
*-----------------
do case
   case orden='û'
        replace estado with '30'
   case orden=' ' and llave='û'
        replace estado with '20'
   other     
        replace estado with '00'
endcase        


PROCEDURE val_can
*----------------
private vfun
vfun = .t.
*vResta = Canreq-Candesp
*if Candesp > vResta
if Candesp > Canreq
	do standby with 'Se esta exediendo en '+alltrim(str(candesp-vResta,10))
	vfun = .f.
endif
return vfun


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



PROCEDURE Agreg_Item
*-------------------
IF F_appd()
  REPLACE Periodo WITH m.Periodo ,;
          NumPec  WITH m.NumPec ,;
          Estado  WITH m.Estado
 *KEYBOARD CHR(15)
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
*-------------------
REPLACE CodCad  WITH m.CodCad
RETURN .T.


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
 SEEK m.periodo+m.numpec &&+ alltrim(m.codfte)
 SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec 		&&and alltrim(codfte)= codfte
      IF ESTADO='30' or estado='20' 
         DO STANDBY WITH 'La Pecosa ya tiene generada O/C o S/C,no se puede anular'
         vEstado = .f.            
         EXIT
 	 ENDIF
 ENDSCAN
if vestado
    IF velimina 
	    SELECT Itepec
	    SEEK m.periodo+m.numpec &&+ alltrim(m.codfte)
	    SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec 		&& and alltrim(codfte)= codfte
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


PROCEDURE LISTA
*--------------
select PECOSA
vtemp =recno()
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
STORE 1  TO vToPec,vToMes,vToFue,vTodep,vOrden,vtiplis,vTipPec,vtop
vNumpec = SPAC(4)
vFte    = SPAC(2)
vCodmes = SPAC(2)
vCoddep = SPAC(6)
vCodFte = SPAC(2)
vfecini = date()
vfecfin = date()
VPP     = SPAC(2)
@ 01,01 SAY "Todas las Pecosas : " GET vToPec  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToPec,2,22) 
@ 02,01 SAY "           Pecosa : "
@ 02,22 GET vNumpec  PICTURE '!!!!' WHEN vToPec = 2  &&VALID Padl(alltrim(vNumpec),4,'0')
@ 02,27 SAY '-'
@ 02,29 GET vFte     PICTURE '!!' VALID VAL_PARA(vfte,'CODFTE','C')  WHEN vToPec = 2

@ 04,01 SAY "  Todos las Meses : " GET vTomes  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTomes,5,22)  WHEN vToPec = 1
@ 05,01 SAY "              Mes : "
@ 05,22 GET vCodmes  PICTURE '!!'  VALID VAL_PARA(vCodMes,'FECMES','C') WHEN vToPec = 1 AND vTomes = 2

@ 06,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTofue,7,22)  WHEN vToPec = 1
@ 07,01 SAY "           Fuente : "
@ 07,22 GET vCodFte  PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE','C')  WHEN vToPec = 1 and vTofue =2

@ 08,01 SAY "Todas las Dependc : " GET vToDep  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTodep,9,22)  WHEN vToPec = 1
@ 09,01 SAY "      Dependencia : "
@ 09,22 GET vCodDep  PICTURE '!!!!!!' VALID VAL_PARA(vCodDep,'CODDEP','C')  WHEN vToPec = 1 and vTodep =2

@ 11,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Dependencia;Emision;Fuente'  WHEN vToPec = 1

@ 14,01 SAY "           Estado : " GET vTipLis  FUNCTION '^ Todos;Emitidos;Atendidos;Liquidados;Anulados'  WHEN vToPec = 1
@ 17,01 say "      Tipo Pecosa : " GET vTipPec FUNCTION '^ Todos;Compra;Stock;Transporte;Referencial;C.Chica'
* @ 17,35 say " Tipo :" get vtipfun picture '!' valid vtipfun$' IF'
@ 20,01 SAY "           Fechas : " GET vFecIni picture '@D'  when vtipLIS = 4 &&OR   VTIPLIS = 5
@ 20,32                            GET vFecFin picture '@D'  when vtipLIS = 4 &&OR   VTIPLIS = 5
@ 21,01 SAY "       Mes Propym : " GET vPP PICTURE '@D'  when vtipLIS = 5  &&&OR   VTIPLIS = 5 &&AQUI
@ 22,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1 AND LASTKEY()#27
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   SET RELATION TO PERIODO+NUMPEC INTO ITEPEC
   
   INDEX ON IIF(vOrden=1,ITEPEC.Periodo+ITEPEC.NumPec+ITEPEC.Codfte,IIF(vOrden=2,CodDep,IIF(vOrden=3,DTOS(FECemi),codfte))) TO (vInd) ;
      FOR IIF(vToPec=1,.T.,NumPec+CodFte = vNumpec+alltrim(vFte)) .AND. IIF(vTipLis=1,.T.,iif(vTipLis=2,Estado = '00',iif(vTipLis=3,Estado='30' or Estado='40',iif(vtiplis =4,Estado='50',Estado = '99'))))
*      FOR IIF(vToPec=1,.T.,NumPec+CodFte = vNumpec+alltrim(vFte)) .AND. IIF(vTipLis=1,.T.,iif(vTipLis=2,Estado = '00',iif(vtiplis= 3 ,Estado='30' or Estado='40',iif(vtiplis =4,Estado='50',Estado = '99' )))) 
   SET FILTER TO iif(vToMes=1,.t.,month(fecpec)=val(vCodMes)) and ;
				 iif(vTop=1,.t.,ITEPEC.mes=val(vpp)) and ;
                 iif(vToFue=1,.t.,itepec.Codfte=alltrim(vCodfte)) and ;
		 		 iif(vToDep=1,.t.,CodDep=alltrim(vCodDep)) and ;
		 		 iif(vTiplis=4,BETWEEN(fecdesp,vFecini,vFecfin),.T.) AND ;
		 		 iif(vTipPec=1,.t.,iif(vtippec=2,tippec='O',iif(vtippec=3,tippec='S',iif(vtippec=4,tippec='R',tippec='C'))));
		 		 iif(vTiplis=5,between(MES,VPP),.T.) AND ;  
		 		 iif(vTipPec=1,.t.,iif(vtippec=2,tippec='O',iif(vtippec=3,tippec='S',iif(vtippec=4,tippec='R',tippec='C'))))  &&AQUI

   SET INDEX TO (VIND)
   count all to vtotp
   SET SKIP TO ITEPEC
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipLis=1,' en General ',IIF(vTipLis=2,' Pendientes ',' Atendidos '))
   vTippo =iif(vTipPec=1,' ',iif(vtippec=2,'Pecosa por Compra',iif(vtippec=3,'Pecosa por Stock','Pesosa por Transporte')))    
   IF !EOF()
      if vtopec = 2 
	     do reporte with 2,"Pecsal1",' Pe.co.sa '
	  else
	     do case
	        case vtiplis=4
        	     do reporte with 2,"Pecsalx",' Pe.co.sa ',1,.F.,.T.
	        case vtiplis=5
        	     do reporte with 2,"PROPYM",' Pe.co.sa ',1,.F.,.T.
	        case vtiplis#4
	             do reporte with 2,"Pecsal",' Pe.co.sa ',1,.F.,.T.
	     endcase        
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



PROCEDURE QLista
*--------------
select PECOSA
vtemp =recno()
SET RELATION TO PERIODO+NUMPEC INTO ITEPEC
SET SKIP TO ITEPEC
if eof()
   do standby with vmens08
   return
endif
if !eof()
   do reporte with 2,"LisPec",' Pe.co.sa s '
else
   do standby with 'Archivo Vac¡o '
endif
select PECOSA
SET RELATION TO
go vtemp
DO VISTA
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  ON KEY LABEL F12
  ON KEY LABEL F3
  ON KEY LABEL F2
  ON KEY LABEL F4
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    WINDOW wind_3
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


function valpec
*--------------
parameter vnumpec
private vfun
vfun = .t.
m.numpec=padl(alltrim(str(vnumpec,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif
return vfun

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.

XX = Val_ArtDet(_Cod,.F.)
IF XX
  SELECT Itepec
  REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri
  vFun = .T.
ENDIF
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

return .t.

PROCEDURE VUSUA
*--------------

PARAMETER CSYS
PRIVATE ALI
ALI = ALIAS()
VKEY = ALLTRIM(CSYS)
SELE USU
SEEK VKEY
VFUN = NOMBRE
SELE (ALI)
RETURN VFUN

PROCEDURE VMES
*--------------

M= IIF(MES='03','MARZO',IIF(MES='04','ABRIL',IIF(MES='05','MAYO',IIF(MES='06','JUNIO',IIF(MES='07','JULIO',IIF(MES='08','AGOSTO',IIF(MES='09','SETIEMBRE',IIF(MES='10','OCTUBRE',IIF(MES='11','NOVIEMBRE','DICIEMBRE')))))))))


PROCEDURE WSTOCKX
*------------------
SAVALI=ALIAS()
BUSPEC=NUMPEC
*m.periodo=periodo
vfecvis=FECPEC
SELEC ITEPEC
SELEC STOCKALI &&
SEEK m.periodo+"DESP"+BUSPEC
IF !FOUND()
	SELEC ITEPEC
	SEEK m.periodo+BUSPEC
	DO WHILE !EOF() .AND. PERIODO=m.periodo .AND. NUMPEC=BUSPEC
		Periw = Periodo
		Codaw = CodArt
		Numow = Numpec
		Codfw = Codfte
		Canrw = Canreq
		Preuw = Preuni
		fecow = Fecoc
    	
		SELEC STOCKALI
		APPEND BLANK
		REPLACE Periodo with Periw
		REPLACE CodArt  with Codaw
		REPLACE Tipdoc  with "DESP"
		REPLACE Numdoc  with Numow
		REPLACE Fuente  with Codfw
		REPLACE Cantidad with Canrw
		REPLACE Cosmed   with Preuw
		REPLACE Fechamov with vfecvis
		REPLACE Tipomov with "S"
		selec itepec
		skip
	ENDDO
 ELSE &&
	SCAN WHILE Periodo=m.Periodo AND Tipdoc="DESP" AND Numdoc=BUSPEC
		IF RLOCK()
			DELETE NEXT 1
		ENDIF
	 EndScan
	 *---------------------------------
	 SELEC ITEPEC
  	 SEEK m.periodo+BUSPEC
	 DO WHILE !EOF() .AND. PERIODO=m.periodo .AND. NUMPEC=BUSPEC
    	Periw = Periodo
	    Codaw = CodArt
    	Numow = Numpec
	    Codfw = Codfte
    	Canrw = Canreq
	    Preuw = Preuni
    	fecow = Fecoc
    
		SELEC STOCKALI
		APPEND BLANK
		REPLACE Periodo with Periw
		REPLACE CodArt  with Codaw
		REPLACE Tipdoc  with "DESP"
		REPLACE Numdoc  with Numow
		REPLACE Fuente  with Codfw
		REPLACE Cantidad with Canrw
		REPLACE Cosmed   with Preuw
		REPLACE Fechamov with vfecvis
		REPLACE Tipomov with "S"
		selec itepec
		skip
ENDDO
*---------------------------------

ENDIF &&
selec (SAVALI)
return 

PROCEDURE SubOpc
*---------------
PRIVATE cAlias,cMod
cAlias = ALIAS()
SELE SubOp
cCtrlOp = ''
cMod = "05"

SEEK vUsuCla+PADL(SistCtrl,2,'0')+cMod

IF FOUND()
	SCAN WHILE vUsuCla+PADL(SistCtrl,2,'0')+cMod = ALLTRIM(SubOp.User)+SubOp.Sistema+SubOp.Modulo
		cCtrlOp = cCtrlOp + SubOp.Opcion
	ENDSCAN
ENDIF

set skip of PAD Revis of mMenu !'A' $cCtrlOp
set skip of PAD Busca of mMenu !'B' $cCtrlOp
set skip of PAD Anter of mMenu !'C' $cCtrlOp
set skip of PAD Proxi of mMenu !'D' $cCtrlOp
set skip of PAD Corri of mMenu !'E' $cCtrlOp
set skip of PAD Ingre of mMenu !'F' $cCtrlOp
set skip of PAD Anula of mMenu !'G' $cCtrlOp
set skip of PAD Lista of mMenu !'H' $cCtrlOp

SELE (cAlias)

RETURN
