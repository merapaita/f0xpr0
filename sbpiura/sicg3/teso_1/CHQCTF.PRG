* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ Chqctf.Prg  11/08/96                                L: 0464  ³	
* ³ Registro de Cheques Certificados                             ³
* ³ AUTORES :  Julio Cruz Ortiz  (Versi¢n Inicial)   		     ³
* ³            Ing. Federico Montero Valdiviezo REGION GRAU      ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

PUBLIC UlSec, UlDia
UlSec = 0
UlDia = 1
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Cheques Certificados'
vmens02 = 'Revisi¢n de Extractos ingresados  '+cia
vmens03 = 'Digite n£mero de Cuenta que desea :'
vmens04 = 'Dicha Cuenta no fue encontrada.'
vmens05 = 'No existe Cuenta anterior'
vmens06 = 'No existe Cuenta siguiente'
vmens07 = '¨ Est  seguro que desea ANULAR esta Cuenta ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Detalle de Cheque Certificado         [F9] Ver detalle  '
vmens10 = '¨ Est  seguro que desea ANULAR esta l¡nea ?'
vmens11 = 'Cuenta est  ANULADA. No puede ser procesada'
*- Abre archivos y se posiciona en £ltimo registro

USE Parmae   IN  1 ORDER TAG Parmae1         ALIAS Parma
USE Cajas    IN  2 ORDER TAG cajas1          ALIAS caja
USE ChqCert  IN  3 ORDER TAG chqcert1        ALIAS Chqcert
USE BcoCta   IN  4 ORDER TAG BcoCta1         ALIAS Bcocta

bloquea  = .t.
agrega   = .t.
select BcoCta
SET EXACT OFF
go bottom

*- Variables de trabajo (registro a trabajar)

scatter memvar blank          && Crea variables en blanco
PUBLIC vc,VUSER_ID,wuser_id
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
vc=space(1)
DO  inicia                     && DEFINE ventanas, men£s, t¡tulos
hide popup all
*- Inicia proceso
DO  pantalla                   && Muestra pantalla inicial
DO  vista
*- Activa men£ mientras vEn_accion es .T.
store .t. to ven_accion
DO  while ven_accion
   activate screen
   activate menu mMenu
endDO 
ON KEY LABEL F9
DO  fin_opcion
show popup menu
return

procedure inicia              && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente           Ingresa           Listar  Termina '
 DO  Logos WITH Rotulo1,vTempo

 DEFINE window wind_f0 from 00,00 to 12,79 double ;
 title vmens01 color scheme 10

 DEFINE window wind_f1 from 00,00 to 23,79 double ;
 title vmens02 color scheme 10

 DEFINE window wind_f2 from 13,00 to 23,79 double ;
 title vmens09 color scheme 10

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 13,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_C1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_C2 FROM 13,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT '\aNula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 *ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
* ON SELECTION PAD anula  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO Lista
 ON SELECTION PAD termi  OF mMenu DO termi
 ACTIVATE SCREEN
return


procedure pantalla            && Pinta m scara de datos
*-----------------
activate window wind_f0
clear
@  1, 2 say "                Mes:"
@  2, 2 say "             Cuenta:"
@  3, 2 say "              Banco:"
RETURN


procedure vista               && Coloca valores de BD en variables y pinta datos
*--------------
activate window wind_f0
xSalFin = 0
select BcoCta
if eof()
   do pantalla
   return
endif
scatter memvar
@  1,24 say m.Mes
@  1,30 SAY Val_ParaC(m.Mes,"FECMES")
@  2,24 say m.CodCtc
=vctc()
DO vista_hijo
return


procedure vista_hijo
*-------------------
select Chqcert
SET ORDER TO Chqcert1
seek m.Mes+ALLTRIM(m.CodCtc)
BROWSE ;
   NOAPPEND NOEDIT NODELETE NOMENU NOREFRESH TIMEOUT 0.005 noclear;
   window wind_f2 key m.Mes+ALLTRIM(m.CodCtc) fields;
   Fecha ,;
   NumChq  ,;
   Valchq  :h='Monto':p='@Z 999,999,999,999.99'

ON KEY LABEL F9 DO Vis_hijo
return

procedure revis               && Revisi¢n de BD en browse
*--------------
select BcoCta
if eof()
   do standby with vmens08
   return
endif
vtemp = recno()
   hide menu mMenu
   activate screen
   vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
   do logos with rotulo1,vtempo
   on key label f10 keyboard chr(23)
   browse window wind_f1 noedit noappend nodelete nomenu fields;
     Mes    :H="Mes":P="!!",;
     CodCtc :H="Cuenta",;
     s2=Val_fun('Caja','CodCtc','descri',ALLTRIM(CodCtc)):H="Descripci¢n",;
     s3=Val_ParaC(Banco,"BANCOS") :H="Banco",;
     FecSal :H="Fec.Ini.",;
     SalIni :H="Sal.Ini" :P="@X 999,999,999,999.99",;
     SalFin :H="Sal.Fin" :P="@X 999,999,999,999.99"
   vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
   do logos with rotulo1,vtempo

SELECT BcoCta
if lastkey()=27
   goto vtemp
else
   scatter memvar
endif
show menu mMenu
on key label f10
do vista
return


procedure busca               && Realiza b£squeda directa
*--------------
select BcoCta
if eof()
   do standby with vmens08
   return
endif
vtemp = recno()
activate window standby
store SPACE(2)        TO vbusca1
STORE SPACE(14)       TO vBusca2
@ 1,3 say " Mes  :" get vbusca1    pict '@!'
@ 2,3 say "Cuenta:" get vbusca2    pict '@!'
read
deactivate window standby
if LASTKEY()=27
   goto vtemp
else
   seek vbusca1+vBusca2
   if !found()
      do standby with vmens04
      goto vtemp
   else
      scatter memvar
      do vista
   endif
endif

return


procedure anter
*--------------
select BcoCta
if eof()
   do standby with vmens08
   return
endif
if !bof()
   skip -1
endif

if bof()
   go top
   do standby with vmens05
else
   do vista
endif
return


procedure proxi
*--------------
select BcoCta
if eof()
   do standby with vmens08
   return
endif
if !eof()
   skip
endif
if eof()
   do standby with vmens06
   go bottom
else
   do vista
endif
return


PROCEDURE Ingre
*--------------
select BcoCta
if eof() 
   do standby with vmens08
   return
endif
if m.estado = '99'
   do standby with vmens11
   return
endif
select BcoCta
do fox_lock with 1, bloquea
if bloquea
   ACTIVATE WINDOW Wind_F0
   @  1,24 say m.Mes
   @  1,30 SAY Val_ParaC(m.Mes,"FECMES")
   @  2,24 say m.CodCtc 
   =vctc()
*  @  3,24 say Val_ParaC(m.Banco,"BANCOS"," ",24)
   IF LASTKEY()=27
      RETURN
   ENDIF
   ON KEY
   IF LASTKEY() <> 27           && Si se NO presion¢ EsC
      do trabaja_hijo
   ELSE
      RETURN
   ENDIF
   DO vista                     && Muestra nuevos datos
ENDIF
UNLOCK ALL
return


PROCEDURE trabaja_hijo
*---------------------
hide menu mMenu
activate screen
do logos with rotulo1,'°°°°F5->Agregar°°°°°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°°°°°F10->Terminar°°°°'
on key label f5  do agreg_item
on key label f8  do elimi_item
on key label f10 keyboard chr(23)

SELECT chqcert
SET ORDER TO chqcert1
IF EOF()
	IF F_Appd()
   		REPLACE NumMes with m.Mes ,;
           		CodCtc WITH ALLTRIM(m.CodCtc),;
           		usuario WITH wuser_id
	ENDIF
ENDIF	

SEEK m.Mes+ALLTRIM(m.CodCtc)
BROWSE ;
   noappend nodelete nomenu ;
   window wind_f2 key m.Mes+ALLTRIM(m.CodCtc) fields;
   fecha ,;
   NumChq :H="Num.Doc." :P="@!" ,;
   valchq :H="Monto" :P="@Z 999,999,999,999.99" 
SEEK m.Mes+ALLTRIM(m.CodCtc)
SCAN WHILE nummes=ALLT(m.mes) AND codctc=ALLT(m.codctc)
     REPLACE periodo WITH RIGHT(STR(YEAR(chqcert.fecha)),2)
ENDSCAN     
activate screen
do logos with rotulo1,' Ingresa  conPedido  Corrige  Anula  Emite  enLote  Stocks  Revisa      Termina '
show menu mMenu
on key
return


procedure agreg_item
*-------------------
SELECT chqcert
IF F_Appd()
   REPLACE NumMes with m.Mes ,;
           CodCtc WITH ALLTRIM(m.CodCtc),;
           usuario WITH wuser_id
ENDIF
UNLOCK ALL
RETURN


procedure elimi_item
*-------------------
do fox_lock with 1, bloquea
if bloquea
   store yesno(vmens10) to velimina
   if velimina
      select chqcert
      delete next 1
   endif
endif
return

procedure Lista
*--------------

 SELECT BcoCta
 vRecc1 = RECNO()

 *- Determinaci¢n de rango de reporte
 DEFINE WINDOW _Rango FROM 5,10 TO 11,70 COLOR SCHEME 10 TITLE " Ctas a Reportar "
 ACTIVATE WINDOW _Rango
 CLEAR
 STORE SPACE(14) TO vCta1, vCta2
 @ 1, 1 SAY "    Mes : " GET vMes1    DEFAULT m.mes PICTURE "!!" VALID Val_ParaC(vMes1,"FECMES"," ",18)
 @ 3, 1 SAY " Cuenta : " GET vCodCtc  DEFAULT m.codctc PICTURE '!!!!!!!!!!!!!!';
 	VALID Val_Fun('Caja','CodCtc','codctc+descri',vCodctc,1,3,26,'descri',27)
 READ VALID Val_read()
 
 IF LASTKEY() = 27
 	RELEASE WINDOW _Rango
 	RETURN
 ENDIF
 
 RELEASE WINDOW _Rango
 vMes1 = ALLTRIM(vMes1)

 SELECT BcoCta
 
 SELECT chqcert
 SET ORDER TO chqcert1
 SET FILTER TO Codctc = vCodctc AND NumMes = vmes1
 SET RELATION TO Nummes+ALLTRIM(CodCtc) INTO BcoCta
 
 GO TOP
 SubTit = "  "
  
 IF !FOUND()
    SET RELATION OFF INTO BcoCta
 	SELECT chqcert
 	SET FILTER TO
 	SELECT BcoCta  	
 	GO vRecc1
    DO standby WITH " No hay datos para dicha cuenta "
    DO vista
    RETURN
 ENDIF
 DO Reporte WITH 2,"CHQCERT","Extracto de Bancos",1,.F.,.T.
 SET RELATION OFF INTO BcoCta
 SELECT chqcert
 SET FILTER TO
 SELECT BcoCta 
 GO vRecc1
 UNLOCK ALL
 DO Vista

RETURN


FUNCTION NoCtaI
*--------------
PRIVATE vFun, vRec
   vRec = IIF(EOF(),-1,RECNO())
   SEEK m.Mes+ALLTRIM(m.CodCtc)
   
   vFun = !FOUND()
   
   IF vRec<0
      GO BOTTOM
   ELSE
      GO vRec
   ENDIF
   
RETURN vFun


procedure termi
*--------------
ven_accion = .f.
deactivate menu
return


procedure fin_opcion
*-------------------
close data
release    window wind_f0
release    window wind_f1
release    window wind_f2
release    menu   mmenu
ON KEY LABEL F9
return

procedure vis_hijo
*--------------------
SELECT chqcert
SEEK m.Mes+ALLTRIM(m.CodCtc)
BROWSE ;
   NOAPPEND NOEDIT NODELETE NOMENU NOREFRESH noclear;
   window wind_f2 key m.Mes+ALLTRIM(m.CodCtc) fields;
   fecha ,;
   NumChq  :H="num.Doc.",;
   Valchq  :h='Cargo':p='@Z 999,999,999,999.99'
  
return

FUNCTION vctc
*------------
Vali=ALIAS()
SELE Caja
SEEK ALLT(m.codctc)
IF FOUND()
   m.codban =  Caja.Banco
   @  3,24 SAY Val_paraD(m.codban,'BANCOS','V',26,26)
ENDIF
SELE (vali)
RETURN   