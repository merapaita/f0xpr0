* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ ExtBco.PRG     30/03/96                             L: 0714  ³	
* ³ Registro de Extractos Bancarios                              ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

PUBLIC UlSec, UlDia
UlSec = 0
UlDia = 1
*- Abre archivos y se posiciona en £ltimo registro

USE parmae    in 1  ORDER parmae1                            alias parma
USE BcoCta1   in 2  ORDER BcoCta1                            alias BcoCta
USE DetCta1   in 3  ORDER DetCta2                            alias DetCta
USE Cajas     in 4  ORDER Cajas1                             alias Caja

*- Mensajes de aviso al usuario
vmens01 = 'Registro de Extractos Bancarios'
vmens02 = 'Revisi¢n de Extractos ingresados  '+cia
vmens03 = 'Digite n£mero de Cuenta que desea :'
vmens04 = 'Dicha Cuenta no fue encontrada.'
vmens05 = 'No existe Cuenta anterior'
vmens06 = 'No existe Cuenta siguiente'
vmens07 = '¨ Est  seguro que desea ANULAR esta Cuenta ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Detalle de Extracto Bancario            [F9] Ver detalle  '
vmens10 = '¨ Est  seguro que desea ANULAR esta l¡nea ?'
vmens11 = 'Cuenta est  ANULADA. No puede ser procesada'

bloquea  = .t.
agrega   = .t.
select BcoCta
go bottom

*- Variables de trabajo (registro a trabajar)

scatter memvar blank          && Crea variables en blanco
PUBLIC W_TIPCTC,vc,VUSER_ID,wuser_id
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
vc=space(1)
DO  inicia                     && Define ventanas, men£s, t¡tulos
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO  Logos WITH Rotulo1,vTempo

 define window wind_f0 from 00,00 to 12,79 double ;
 title vmens01 color scheme 10

 define window wind_f1 from 00,00 to 23,79 double ;
 title vmens02 color scheme 10

 define window wind_f2 from 13,00 to 23,79 double ;
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
@  4, 2 say "Fecha Saldo Inicial:"
@  5, 2 say "      Tipo de Saldo:"
@  6, 2 say "      Saldo Inicial:"
@  7, 2 say "        Total Cargo:"
@  8, 2 say "        Total Abono:"
@  9, 2 say "        Saldo Final:"
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
vestado = Val_ParaC(m.Estado,"ESTADO")
@  0,60 say space(18)
@  0,(76-len(vestado))   say vestado
@  1,24 say m.Mes
@  1,30 SAY Val_ParaC(m.Mes,"FECMES")
@  2,24 say m.CodCtc
@  3,24 say m.Banco
@  3,30 SAY Val_ParaC(m.Banco,"BANCOS")
@  4,24 say m.FecSal
@  5,24 say IIF(SalIni<0,"SD","SA") &&SD=SALDO DEUDOR, SA=ACREEDOR
@  6,24 SAY SPACE(20)
@  6,24 say SalIni PICTURE "@X 999,999,999,999.99"
@  7,24 SAY SPACE(20)
@  7,24 say TotCar PICTURE "@X 999,999,999,999.99"
@  8,24 SAY SPACE(20)
@  8,24 say TotAbo PICTURE "@X 999,999,999,999.99"
xSalFin = SalIni - TotCar + TotAbo
@  9,24 SAY SPACE(20)
IF xsalfin >= 0
	@  9,24 say xSalFin PICTURE "@X 999,999,999,999.99" COLOR SCHEME 02
ELSE	
	@  9,24 say xSalFin PICTURE "@X 999,999,999,999.99" COLOR SCHEME 05
ENDIF	
DO vista_hijo
return


procedure vista_hijo
*-------------------
select DetCta
SET ORDER TO DetCta5
seek m.Mes+ALLTRIM(m.CodCtc)
BROWSE ;
   NOAPPEND NOEDIT NODELETE NOMENU NOREFRESH TIMEOUT 0.005 noclear;
   window wind_f2 key m.Mes+ALLTRIM(m.CodCtc) fields;
   DesRef=val_ParaC(TipDoc,"MOVBCO") :h="Referencia" :23,;
   CanCar  :h='Cargo':p='@Z 999,999,999,999.99',;
   CanAbo  :h='Abono':p='@Z 999,999,999,999.99',;
   FecBco   :H="FecBco"
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
   BROWSE WINDOW wind_f1 noedit noappend nodelete nomenu fields;
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


procedure corri
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
   ULSEC=M.NUMEXT
   @  1,24 say m.Mes
   @  1,30 SAY Val_ParaC(m.Mes,"FECMES")
   @  2,24 say m.CodCtc
   @  4,24 SAY Val_paraD(m.Banco,'BANCOS','V',26,26)
*  @  3,30 SAY Val_ParaC(m.Banco,"BANCOS")
   @  4,24 GET m.FecSal
   @  5,24 say IIF(SalIni<0,"SD","SA")
   @  6,24 GET m.SalIni PICTURE "@X 999,999,999,999.99"
   @  7,24 SAY m.TotCar PICTURE "@X 999,999,999,999.99"
   @  8,24 SAY m.TotAbo PICTURE "@X 999,999,999,999.99"
   @  9,24 SAY m.SalFin PICTURE "@X 999,999,999,999.99"
   m.usuario=wuser_id
   READ
   SELE CAJA
   SEEK ALLTRIM(M.CODCTC)
   W_TIPCTC=CAJA.TIPO
   m.fecha=DATE()
   SELE BCOCTA
   IF LASTKEY()=27
      RETURN
   ENDIF
   on key

   if lastkey() <> 27           && Si se NO presion¢ EsC
      do trabaja_hijo
      gather memvar
      M.NUMEXT=ULSEC
      select BcoCta
      gather memvar
   else
   
      RETURN
   endif
   do vista                     && Muestra nuevos datos

endif
unlock all
return

PROCEDURE Ingre               && Crea nuevo registro en BD
*--------------
activate window wind_f0
select BcoCta
DO pantalla
scatter memvar blank
*- Aqu¡ coloca valores por omisi¢n (default)
@  1,22 clear to  1,28
@  2,22 clear to  8,78
   @  1,24 GET m.Mes    VALID  Val_ParaC(m.Mes,"FECMES"," ",30)
   @  2,24 GET m.CodCtc VALID  Val_fun('Caja','CodCtc','codctc+descri',m.CodCtc,1,2,24) .AND. NoCtaI() .AND. vctc()
*   @  3,24 GET m.Banco  VALID  Val_ParaC(m.Banco,"BANCOS"," ",30)
   @  4,24 GET m.FecSal
   @  5,24 say IIF(m.SalIni<0,"SD","SA")
   @  6,24 GET m.SalIni PICTURE "@X 999,999,999,999.99"
   @  7,24 SAY m.TotCar PICTURE "@X 999,999,999,999.99"
   @  8,24 SAY m.TotAbo PICTURE "@X 999,999,999,999.99"
   @  9,24 SAY m.SalFin PICTURE "@X 999,999,999,999.99"
   m.usuario=wuser_id
   m.fecha  =DATE()
   READ VALID Val_Read()
IF LASTKEY() # 27
   SELE CAJA
   SEEK ALLTRIM(M.CODCTC)
   W_TIPCTC=CAJA.TIPO
    vAnula = .F.
    SELECT BcoCta
    IF F_Appd()
       GATHER MEMVAR
       DO Agreg_item
       SELECT DetCta
       SCATTER MEMVAR
       DO WHILE .T.
          DO Trabaja_Hijo 
          M.NUMEXT=ULSEC
          select BcoCta
          gather memvar
          IF LASTKEY() = 27
             IF YESNO( '¨ Cancela el Ingreso ?' )
                vAnula = .T.
                EXIT
             ENDIF
          ELSE
             IF YESNO( '¨ Est n correctos los datos ?')
                EXIT
             ENDIF
          ENDIF
       ENDDO
       IF vAnula
          DO Anula
       ENDIF
    ELSE
       GO BOTTOM
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 
UNLOCK ALL
DO VISTA
RETURN

PROCEDURE trabaja_hijo
*---------------------
SELECT Parma
SEEK "VARIOS"+"ANOACT"
Agno = NumEnt
hide menu mMenu
activate screen
do logos with rotulo1,'°°°°F5->Agregar°°°°°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°°°°°F10->Terminar°°°°'

on key label f5  do agreg_item
on key label f8  do elimi_item
on key label f10 keyboard chr(23)

SELECT DetCta
SET ORDER TO DetCta5
SEEK m.Mes+ALLTRIM(m.CodCtc)
BROWSE ;
   noappend nodelete nomenu ;
   window wind_f2 key m.Mes+ALLTRIM(m.CodCtc) fields;
   Sec    :H="Sec" :P="999999"  :F :R,;
   TipDoc :V=val_ParaC(TipDoc,"EXTBCO","TipDoc") .AND. CarAbo() :h="TipDoc" :F,;
   Certi  :H='Certificado' :W=Tipdoc='CHQ' :V=Certi$'SN',;
   Tipo   :H="Mov.":P="!" :W=Tipdoc = 'OTR' OR Tipdoc = 'EXT',;
   ss=val_ParaC(TipDoc,"EXTBCO") :H="Descripci¢n" :W = EMPTY(Tipdoc):R :20,;
   Concepto :W= Tipdoc='C/I' OR TIPDOC='N/C' OR TIPDOC='N/A' , ;
   NumChq :H="Num.Doc." :P="@!" :W=Tipdoc='CHQ',;
   Veces  :H="N§D‚pos." :P="@!" :W=Tipdoc='B/D',;
   Unidad :H="Monto Unit." :v=vermto() :P="@Z 999,999,999,999.99" :W=Tipdoc='B/D',;
   CanCar :H="Cargo" :v=vermto() :P="@Z 999,999,999,999.99" :W=Tipo="C" :F,;
   CanAbo :H="Abono" :v=vermto() :P="@Z 999,999,999,999.99" :W=Tipo="A"  :F,;
   FecBco :H="FecBco"
*...JCL...* :V=YEAR(DATE())=YEAR(fecbco)

SELECT DetCta
SET ORDER TO DetCta5
GO TOP
SEEK m.Mes+ALLTRIM(m.CodCtc)
m.SalFin = m.SalIni
STORE 0 TO m.TotCar, m.TotAbo
SCAN WHILE ALLT(m.mes) = ALLT(mes)  .AND. ALLTRIM(m.CodCtc)=ALLTRIM(CodCtc)
	m.SalFin = m.SalFin + CanAbo - CanCar 
	m.TotCar = m.TotCar + CanCar
	m.TotAbo = m.TotAbo + CanAbo
    IF RLOCK()
       REPLACE tipctc WITH w_tipctc
    ENDIF
ENDSCAN
SET ORDER TO DetCta1
FLUSH

SELECT BcoCta
gather memvar
activate screen
do logos with rotulo1,' Ingresa  conPedido  Corrige  Anula  Emite  enLote  Stocks  Revisa      Termina '
show menu mMenu

on key
UNLOCK ALL
return


procedure agreg_item
*-------------------
SELECT DetCta
ULSEC=ULSEC+1
vTipdoc = Tipdoc
vCancar = Cancar
vCanabo = Canabo
vMonto  = Monto
vTipo   = Tipo
vfecBco = FecBco
Vnumchq = Numchq
IF F_Appd()
   REPLACE Mes with m.Mes ,;
           CodCtc WITH ALLTRIM(m.CodCtc),;
           Sec    WITH ULSec,;
           Tipdoc WITH vTipdoc,;
           Monto  WITH vMonto,;
           CanCar WITH vcancar,;
           CanAbo WITH vcanabo,;
           FecBco WITH vfecbco
ENDIF
UNLOCK ALL
RETURN


procedure elimi_item
*-------------------
do fox_lock with 1, bloquea
if bloquea
   store yesno(vmens10) to velimina
   if velimina
      select DetCta
      delete next 1
   endif
endif
UNLOCK ALL
return

procedure anula
*--------------
parameters vanula
vanula = iif( parameters() = 0, 1, vanula )
vanula = 2
select BcoCta
if eof() .or. !EMPTY(estado)
   do standby with vmens08
   return
endif
if m.estado = '99'
   do standby with vmens11
   return
endif
store yesno(vmens07) to velimina
select BcoCta
if velimina
   bloquea = .t.
   do fox_lock with 1,bloquea
   if bloquea
      ** Habilita el pedido previo a la guia
      select DetCta
      seek m.Mes+ALLTRIM(m.CodCtc)
      scan while m.Mes=Mes .AND. ALLTRIM(m.CodCtc)=ALLTRIM(CodCtc)
         bloquea = .t.
         do fox_lock with 1,bloquea
         if bloquea
            if vanula = 2
               delete next 1
            else
               replace estado with '99'
            endif
         endif
      endscan
   endif
   unlock all
endif

SELECT BcoCta
if vAnula=2
   DELETE
ELSE
   REPLACE Estado WITH '99'
   replace usuario WITH wuser_id
ENDIF
GO BOTTOM

do vista
return


procedure Lista
*--------------
 SELECT Parma
 SEEK "VARIOS"+"ANOACT"
 vAgno = NumEnt

 SELECT BcoCta
 vRecc1 = RECNO()

 *- Determinaci¢n de rango de reporte
 DEFINE WINDOW _Rango FROM 5,10 TO 11,70 COLOR SCHEME 10 TITLE " Ctas a Reportar "
 ACTIVATE WINDOW _Rango
 CLEAR
* STORE "01"      TO vMes1, vMes2
 STORE SPACE(14) TO vCta1, vCta2
 @ 1, 1 SAY "    Mes : " GET vMes1    DEFAULT m.mes PICTURE "!!" VALID Val_ParaC(vMes1,"FECMES"," ",18)
* @ 2, 1 SAY "        Hasta : " GET vMes2    PICTURE "!!" VALID Val_ParaC(vMes2,"FECMES"," ",25) .AND. vMes2>=vMes1
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
 
 SELECT DetCta
 SET ORDER TO DetCta5
 SET FILTER TO Codctc = vCodctc AND Mes = vmes1
* Tmp = SYS(3) + '.Idx'
 
 SET RELATION TO Mes+ALLTRIM(CodCtc) INTO BcoCta
 
 GO TOP
 SubTit = "  "
  
 IF !FOUND()
    SET RELATION OFF INTO BcoCta
 	SELECT Detcta
 	SET FILTER TO
 	SELECT BcoCta  	
 	GO vRecc1
    DO standby WITH " No hay datos para dicha cuenta "
    DO vista
    RETURN
 ENDIF
 
 DO Reporte WITH 2,"RepBol3","Extracto de Bancos",1,.F.,.T.
 
 SET RELATION OFF INTO BcoCta
 SELECT Detcta
 SET FILTER TO
 SELECT BcoCta 
 GO vRecc1
 UNLOCK ALL
 DO Vista

 IF .F.
 
  SELECT DetCta
  SEEK BcoCta.Mes+ALLTRIM(BcoCta.CodCtc)
 
  IF !FOUND()
     DO standby WITH " Cuenta sin detalle "
     CLOSE DATA
     RETURN
  ENDIF

 SET FILTER TO Mes=BcoCta.Mes .AND. ALLTRIM(CodCtc)=ALLTRIM(BcoCta.CodCtc)
 DO Reporte WITH 2,"RepBol2","Extracto de Bancos"

 SET FILTER TO
 
 SELECT BcoCta
 GO vRecc1
 
 UNLOCK ALL
 
 DO Vista
 
ENDIF

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


FUNCTION VerMto
*--------------
   IF CanCar>0
      REPLACE Tipo WITH "C", Monto WITH CanCar
   ELSE
      IF CanAbo>0
         REPLACE Tipo WITH "A", Monto WITH CanAbo
      ENDIF
   ENDIF
   REPLACE Monto WITH IIF(Tipo="C",ABS(CanCar)*-1,ABS(CanAbo))
RETURN .T.

FUNCTION CarAbo
*--------------
IF ALLTRIM(Tipdoc) # 'OTR'  AND ALLTRIM(Tipdoc) # 'EXT'
   IF Tipo#LEFT(Parma.DescriAux,1)
      IF LEFT(Parma.DescriAux,1)="C"
         REPLACE CanAbo WITH 0
      ELSE
         REPLACE CanCar WITH 0
      ENDIF
   ENDIF
   REPLACE Tipo WITH LEFT(Parma.DescriAux,1)
ENDIF
RETURN .T.
 
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
select DetCta
seek m.Mes+ALLTRIM(m.CodCtc)
BROWSE ;
   NOAPPEND NOEDIT NODELETE NOMENU NOREFRESH noclear;
   window wind_f2 key m.Mes+ALLTRIM(m.CodCtc) fields;
   DesRef=val_ParaC(TipDoc,"MOVBCO") :h="Referencia" :23,;
   Certi   :H='Certificado',;
   NumChq  :H="num.Doc.",;
   CanCar  :h='Cargo':p='@Z 999,999,999,999.99',;
   CanAbo  :h='Abono':p='@Z 999,999,999,999.99',;
   FecBco   :H="FecBco"
RETURN  

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