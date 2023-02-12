*--------------------------------------------------------------------------
* Regval.Prg
* Registra los VALES que se emiten 
* Estado del VALE:
*   '00' Emitida     ** 	Este es el que se registra en el Vale
*   '20' Con Correlativo
*   '50' Con O/C
*   '70' Devuelta
*   '99' Anulada
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE vale     IN 2   order tag vales1   ALIAS vale
USE Pecosa   IN 3   order tag Pecosa1  ALIAS Pecosa
USE ItePec   IN 4   order tag ItePec1  ALIAS Itepec
USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE IteArt   IN 6   order tag IteArt5  ALIAS Iteart
USE cdrnec   IN 7   order tag Cdrnec1  ALIAS cuadro
USE itecn    IN 8   order tag itecn3   ALIAS itecn
USE Promae   IN 9   order tag Promae1  ALIAS Promae
USE maepre   IN 10  order tag maepre1  ALIAS maepre
USE itepar   in 11  order tag itepar1  ALIAS ITEPAR          
USE Regveh   IN 12  order tag RegVeh1  ALIAS vehic

on key label F4 do imprimir

*- Mensajes de aviso al usuario
Vmens01 = ' Vales : REVISION '
Vmens02 = 'Registro de Vale'
Vmens04 = 'Dicho Vale no fue encontrado'
Vmens05 = 'No existe Vale anterior'
Vmens06 = 'No existe Vale siguiente'
Vmens07 = '¨ Desea Anular ‚ste Vale ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Vale ha sido anulado'
Vmens10 = 'El Vale ya est  Atendido'
Vmens11 = 'El Vale ha sido devuelto'
Vmens12 = 'El Vale ya tiene Pecosa'

SELECT vale
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular   Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_4 FROM 04,01 TO 21,77 DOUBLE ;
 TITLE 'Relaci¢n de Art¡culos'     COLOR SCHEME 10

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
 ACTIVATE WINDOW Wind_0
 CLEAR
 
 @  1, 2 SAY "          Periodo :"
 @  1,40 say "              Mes :"
 @  2, 2 SAY "       Fecha Vale :"
 @  3, 2 SAY "      N£mero Vale :"
 @  4, 2 SAY "        Proveedor :"
 @  5, 2 SAY "         Veh¡culo :"
 @  6, 2 SAY "     Tipo de Vale :"

 @  7, 2 SAY "  Cadena de Gasto :"
 @  8, 2 SAY " F.Financiamiento :" 
 
 @ 11, 2 SAY "   Detalle Pedido :"
 @ 12, 2 SAY "            Valor :"
 @ 13, 2 SAY "           Chofer :"
 @ 14, 2 SAY "          Oficina :"
 
 @ 16, 2 SAY "     Pe.Co.Sa. N§ :"
 @ 17, 2 SAY "       Factura N§ :"
 
 * 18, 2 SAY "  Orden de Compra :"
 * 18,40 SAY "  Hoja de Control :"
 * 19, 2 SAY " Comprobante Pago :"
 * 19,40 SAY "           Cheque :" 
 
 @ 18, 2 SAY "          Destino :"
 @ 19, 2 SAY "   Doc.Referencia :"
 @ 20, 2 SAY "    Observaciones :"	  
 
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Vale
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR
 =val_CODCAD(ALLT(m.codcad),m.periodo,'C')
  
 @  1,22 SAY m.periodo
 @  1,60 SAY val_para(m.Nummes,'FECMES','D',22,20)
 @  2,22 SAY m.Fecval
 @  3,22 SAY m.Codval
 @  4,22 SAY IIF(m.codprv='0000',m.nomprv,val_prv(m.Codprv))+SPAC(5)
 @  5,22 SAY m.Codpla  
 @  6,22 SAY val_para(m.TipVal,'TIPVAL','D',22,30)

 @  7,22 SAY val_codcad(m.codcad,m.periodo,'D',22,40)
 @  8,22 SAY Val_Para(m.CodFte,'CODFTE','D',22,40)
* @  8,22 SAY iif(!empty(m.codfte),Val_Para(m.CodFte,'CODFTE','D',22,50),space(25))

 IF alltrim(m.Tipval)$'PC'
    @  9, 2 SAY "  Cantidad Pedido :"
    @ 10, 2 SAY "    C¢digo Pedido :"
    @ 10,40 SAY "    Unidad Medida :" 
  	@  9,22 clear to  9,78
  	@  9,22 SAY m.Canreq picture '9,999,999.999'
	@ 10,22 SAY m.codart
	@ 10,60 SAY m.unimed 
 else
    @  9, 2 SAY "         Servicio :"
	@  9,22 SAY val_para(m.CODSER,'CODSER','D',22,50)
	@ 10,02 clear to 10,78
 endif	
 @ 11,22 SAY m.descri picture "@S56"
 @ 12,22 SAY m.valtot
 @ 13,22 SAY val_para(m.codCHF,'CODCHF','D',22,50) 
 @ 14,22 SAY val_para(m.coddep,'CODDEP','D',22,50) 
 
 @ 16,22 SAY m.numpec
 @ 17,22 SAY m.serfac+'-'+m.numfac
 
 * 18,22 SAY m.numoc
 * 18,60 SAY m.numhc
 * 19,22 SAY m.numcp
 * 19,60 SAY m.numchq
 
 @ 18,22 SAY m.Destino picture "@S56"
 @ 19,22 SAY m.docref  picture "@S56"
 @ 20,22 SAY m.observa picture "@S56"
 @ 21,00 SAY PADC(' ° ®F4¯ Imprime ° ',79,' ') COLOR W+/B 
RETURN



PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE vale
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 CODVAl :H=' N§ ' ,;
 Fecval :H='Fecha' ,;
 prv=val_prv(codprv) :H='Proveedor' :32,;
 codpla :H='Placa' ,;
 Coddep :H='Oficina',;
 CanReq : H='Cantid':P='99,999.99',;
 vArt = val_artQ(codart) :H='Detalle ' :25,;
 valtot :H='Valor'
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
vtemp    = RECNO()
*VPERIODO = '97'
vPeriodo = RIGHT(DTOC(DATE()),2)
VNUMMES  = space(2)
vNumval  = space(4)

ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero Vale: ' GET vPeriodo PICTURE '!!'
 @ 1,26 SAY '-' GET vNummes PICTURE '!!'
 @ 1,31 SAY '-' GET vNumVal PICTURE '!!!!' VALID vBusca()
READ
DEACTIVATE WINDOW standby

IF EMPTY(vNumval) .or. LASTKEY()=27
   RETURN
ELSE
   SEEK vPeriodo + vNummes + vNumval
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
vnumval=padl(alltrim(vnumval),4,'0')
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

*****

SELECT VALE
SCATTER MEMVAR
=val_CODCAD(ALLT(m.codcad),m.periodo,'C')

ACTIVATE WINDOW Wind_0
DO PANTALLA
@  1,22 GET m.periodo Disable
@  1,60 GET m.nummes  Disable 
@  2,22 GET m.Fecval  
@  3,22 GET m.codval  disable
@  4,22 GET m.CodPrv  PICTURE '!!!!' VALID iif(m.codprv='0000',.t.,Val_prv( m.CodPrv,.T.))
@  4,27 get m.nomprv  picture '@S50' &&when m.codprv='0000'
@  5,22 GET m.Codpla  picture '!!!!!!!' VALID val_pla(m.CodPla,'C')
@  6,22 GET m.TipVal picture '!'  valid val_para(m.TipVal,'TIPVAL',' ',22,15)

@  7,22 GET m.codcad picture '!!!!' valid val_codcad(m.codcad,m.periodo,' ',22,30)
@  8,22 get m.codfte picture '!!' valid Val_Para(m.CodFte,'CODFTE',' ',22,30) 
READ

IF alltrim(m.Tipval)$'PC'
    @  9, 2 SAY "  Cantidad Pedido :"
    @ 10, 2 SAY "    C¢digo Pedido :"
    @ 10,40 SAY "    Unidad Medida :" 
  	@  9,22 GET m.Canreq picture '9,999,999.999'
	@ 10,22 get m.codart picture '!!!!!!!!!!' VALID val_artQ(m.Codart,.f.) WHEN !alltrim(m.Tipval)$'S'
	@ 10,60 get m.unimed WHEN !alltrim(m.Tipval)$'S'
 else
    @  9, 2 SAY "         Servicio :"
    @  9,22 get m.codser picture '!!!!!' valid val_para(m.CODSER,'CODSER',' ',22,50,6) and asser() WHEN alltrim(m.Tipval)$'S'
	@ 10,02 clear to 10,78
 endif	
@ 11,22 get m.descri picture "@S56" 
@ 12,22 get m.valtot  

@ 13,22 get m.cODCHF PICTURE '!!!' VALID val_para(m.codCHF,'CODCHF',' ',22,50) 
@ 14,22 get m.coddep picture '!!!!!!' valid val_para(m.coddep,'CODDEP',' ',22,40,7) 
 
@ 16,22 get m.numpec  &&disable
@ 17,22 get m.serfac  &&disable
@ 17,24 SAY '-'
@ 17,25 get m.numfac  &&disable
@ 18,22 get m.Destino picture "@S56"
@ 19,22 get m.docref  picture "@S56"
@ 20,22 get m.observa picture "@S56"

READ VALID VAL_READ()
    
IF LASTKEY() # 27 && Si se presion¢ Esc, no graba
   GATHER MEMVAR
ELSE
   *- No pudo agregar la liquidaci¢n
    SELECT Vale
ENDIF
DO Vista                    && Muestra nuevos datos
UNLOCK
RETURN

procedure asser
*--------------
*m.descri =  parma.descri
return .t.


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
SELECT Vale
VTEMP = RECNO()
DO Pantalla
SCATTER MEMVAR BLANK
m.Periodo = RIGHT(DTOC(DATE()),2)
m.fecval = date()
m.codprv=space(4)
m.codcad=space(4)
m.codfte=space(2)

@  1,22 GET m.periodo
@  1,60 GET m.NumMes  PICTURE '!!' VALID Val_para(m.Nummes,'FECMES',' ',60,10,3) 
@  2,22 GET m.Fecval  VALID ASIG_VAL()
@  3,22 GET m.codval  VALID VAL_VAL()
@  4,22 GET m.CodPrv  PICTURE '!!!!' VALID iif(m.codprv='0000',.t.,Val_prv( m.CodPrv,.T.) AND ASPRV())
@  4,27 get m.nomprv  picture '@S50' &&when m.codprv='0000'
@  5,22 GET m.Codpla  picture '!!!!!!!' VALID val_pla(m.CodPla,'C')
@  6,22 GET m.TipVal picture '!' valid val_para(m.TipVal,'TIPVAL',' ',22,15)

@  7,22 GET m.codcad picture '!!!!' valid val_codcad(m.codcad,m.periodo,' ',22,30)
@  8,22 GET m.codfte picture '!!'   valid val_para(m.codfte,'CODFTE',' ',22,30)

READ

IF alltrim(m.Tipval)$'PC'
    @  9, 2 SAY "  Cantidad Pedido :"
    @ 10, 2 SAY "    C¢digo Pedido :"
    @ 10,40 SAY "    Unidad Medida :" 
  	@  9,22 GET m.Canreq picture '9,999,999.999'
	@ 10,22 get m.codart picture '!!!!!!!!!!' VALID val_artQ(m.Codart,.f.) WHEN !alltrim(m.Tipval)$'S'
	@ 10,60 get m.unimed WHEN !alltrim(m.Tipval)$'S'
 else
    @  9, 2 SAY "         Servicio :"
    @  9,22 get m.codser picture '!!!!!' valid val_para(m.CODSER,'CODSER',' ',22,50,6) and asser() WHEN alltrim(m.Tipval)$'S'
	@ 10,02 clear to 10,78
 endif	

@ 11,22 get m.descri  picture "@S56"
@ 12,22 get m.valtot  

@ 13,22 get m.cODCHF PICTURE '!!!' VALID val_para(m.codCHF,'CODCHF',' ',22,50) 
@ 14,22 get m.coddep picture '!!!!!!' valid val_para(m.coddep,'CODDEP',' ',22,40,7) 

@ 18,22 get m.Destino picture "@S56"
@ 19,22 get m.docref  picture "@S56"
@ 20,22 get m.observa picture "@S56" 

READ VALID VAL_READ()

IF LASTKEY() # 27 
   * verifica si estan correctos
   SELECT Vale
   IF F_APPD()
      m.estado='00'
      m.CanDESP = m.Canreq
      GATHER MEMVAR
   ENDIF 
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
   GO VTEMP
   SELE Vale
 ENDIF
DO Vista
RETURN

procedure asPRV
*--------------
M.NOMPRV=PROMAE.NOMPRO
RETURN .T.


procedure asig_val
*-----------------
SET FILTER TO NUMMES=ALLTRIM(M.NUMMES)
go bottom
v1=val(codval)
m.codval= padl(alltrim(str(v1+1,4)),4,'0')
SET FILT TO 
return


PROCEDURE VAL_VAL
*----------------
m.codval = padl(alltrim(m.codval),4,'0')
seek m.periodo+m.nummes+m.codval
if found()
   do standby with 'N£mero ya registrado'
   return .f.   
endif
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
 velimina = YESNO('¨ Desea ANULAR ‚ste Vale ?')
 vEstado = .t.
 SELECT Itepec
 SEEK m.periodo+m.numpec &&+ alltrim(m.codfte)
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
	    SEEK m.periodo+m.numpec &&+ alltrim(m.codfte)
	    SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec and alltrim(codfte)= codfte
		 if rlock()

         endif
	    endscan
   		sele pecosa
    endif
 endif   
 sele pecosa    
 DO Vista
 
 UNLOCK
 RETURN

PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON
SELECT VALE
VCON = RECNO()
SCATTER MEMVAR
vNumVAL = m.Periodo+m.Nummes+m.Codval
SEEK vnumval
if !found()
   do standby with vmens08
else
  set filt to periodo=m.periodo and codval=m.codval and nummes=alltrim(m.nummes)
  if m.tipval="S"
     do reporte with 2,"Regvals",' Pe.co.sa ',1
  else   
     do reporte with 2,"Regval",' Pe.co.sa ',1
  endif   
  set filt to 
endif
SELECT vale
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
REPLACE CodCad  WITH m.CodCad
RETURN .T.

PROCEDURE LISTA
*--------------
ON KEY LABEL F7 
ON KEY LABEL F9
select VALE

vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   DO LISVAL
endif
select VALE
DO VISTA
RETURN


PROCEDURE LisVAL
*---------------
vOrde = ORDER()
vrec  = RECNO()
DEFINE WINDOW LIS FROM 0,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado Vales' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vToPec,vToMes,vToArt,vTodep,vOrden,vtiplis,vTipRep,vTotip,vToPla,vRepo,vToCho
vCodmes = SPAC(2)
vCoddep = SPAC(6)
vCodart = SPAC(10)
vCodTip = SPAC(1)
vCodPla = SPAC(7)
vCodCho = SPAC(3)

@ 01,01 SAY "     Tipo Reporte : " GET vRepo   FUNCTION '^ General;Control;Equipo Mec'  

@ 04,01 SAY " Todos las Meses? : " GET vTomes  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTomes,5,22)  
@ 05,01 SAY "              Mes : "
@ 05,22 GET vCodmes  PICTURE '!!'  VALID VAL_PARA(vCodMes,'FECMES','C') WHEN vTomes = 2

@ 06,01 SAY "  Todas Oficinas? : " GET vToDep  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTodep,7,22)
@ 07,01 SAY "          Oficina : "
@ 07,22 GET vCodDep  PICTURE '!!!!!!' VALID VAL_PARA(vCodDep,'CODDEP','C')  WHEN vTodep =2

@ 08,01 SAY " Todos Articulos? : " GET vToArt  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToArt,9,22)
@ 09,01 SAY "         Art¡culo : "
@ 09,22 GET vCodArt  PICTURE '!!!!!!!!!!' VALID FBUT() WHEN vToArt =2

@ 10,01 SAY " Todos los Tipos? : " GET vTotip  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToTip,11,22)
@ 11,01 SAY "             Tipo : "
@ 11,22 GET vCodTip  PICTURE '!' VALID val_para(vCodTip,'TIPVAL',' ',22,15) WHEN vTotip = 2

@ 12,01 SAY "Todas las Placas? : " GET vTopla  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTopla,13,22)
@ 13,01 SAY "            Placa : "
@ 13,22 GET vCodPla  PICTURE '!!!!!!!' VALID val_pla(vCodPla,'C') WHEN vTopla = 2

@ 14,01 SAY "  Todos Choferes? : " GET vTocho  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTocho,15,22)
@ 15,01 SAY "           Chofer : "
@ 15,22 GET vCodCho  PICTURE '!!!' VALID val_para(vcodcho,'CODCHF',' ',22,50) WHEN vTocho = 2

@ 16,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Vale;Dependencia;Placa;Fecha'  
@ 19,01 SAY "           Estado : " GET vTipLis  FUNCTION '^ Todos;Pendientes;Atendidos' 
@ 22,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1 AND LASTKEY()#27
   SELECT VALE
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,Periodo+codval,IIF(vOrden=2,CodDep,IIF(vOrden=3,Codpla,FecVal))) TO (vInd) ;
   FOR      IIF(vTipLis=1,.T.,iif(vTipLis=2,Estado='00',Estado='50')) and ;
            iif(vTopla=1,.t.,Codpla=alltrim(vCodpla))
   set filt to ;
            iif(vToArt=1,.t.,Codart=alltrim(vCodart)) and ;
		    iif(vToDep=1,.t.,CodDep=alltrim(vCodDep)) and ;
		    iif(vToMes=1,.t.,month(fecval)=val(vCodMes)) and ; 
		    iif(vToTip=1,.t.,Tipval=alltrim(vCodtip)) and ;
		    iif(vTocho=1,.t.,codchf=alltrim(vCodcho)) 		    
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipLis=1,'Vales en General ',IIF(vTipLis=2,' Vales Pendientes ',' Vales Atendidos '))
   vMes = iif(vtomes=1,' ',"Mes de :"+alltrim(val_para(alltrim(vcodmes),'FECMES','D',22,60)))
   IF !EOF()
      do case 
         case vRepo = 1
              do reporte with 2,"LisVal",' Vales '
         case vRepo = 2
              do reporte with 2,"LisVal1",' Vales '
         case vRepo = 3     
              *do reporte with 2,"LisVal",' Vales '
      endcase   
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILT TO
   CLOSE INDEX
   ERASE (VIND)
ENDIF
SELECT vale
GO TOP
GO VREC
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
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


PROCEDURE Validar
*----------------
private vfun
vAli = Alias()
vRec = Recno()
vKey = 'B'+m.CodArt
sele iteart
seek vkey
vFun = .t.
if found() 
   do standby with 'El C¢digo ya Existe..'
   vFun = .f.
endif
select vali
return vFun


FUNCTION Val_ArtQ  && Articulos
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
   v_fun = found()
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
 *  RETURN v_ENT
*ENDIF   


PROCEDURE FBUT
*-------------
SELE ITEART
SET ORDE TO iteart3
SEEK VCODART
IF !FOUND() OR EMPTY(VCODART)
    SET ORDE TO iteart2
	DO FUNBUSDET
	BROWSE WINDOW Wind_4 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     Iteart.CodArt  :H='Articulo':10 ,;
     iteart.Descri  :H='Descripci¢n' :35,;
     Produ.CodArt  :H='Det':7,;
     Produ.Descri  :H='Detalle'
	SET ORDER TO iteart1
	SET RELA TO
ENDIF	
VCODART = ITEART.CODART
RETURN .T.
