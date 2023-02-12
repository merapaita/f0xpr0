***************************************************
*:
*:        Program: C:\Nichos\Fuentes\nichos.prg
*:
*:         System: Modulo de Control de Nichos
*:         Author: A.S. Marco Rodriguez Abad
*:      Copyright: 
*:  Last modified: 17/10/2004
*:
***************************************************
clear
close all
clear all

worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .t.,.f. )

SET PROCEDURE TO LibSis

IF !FILE( 'C:\WINDOWS\IsDiskIn.Bin')
	DO STANDBY WITH ' COPIA NO AUTORIZADA '
	RETURN
ENDIF

DO fox_ambi

*******************    OJO PARA CONTROL DEL SISTEMA
dFecSis = DATE()
*******************


ruta  ='..\DataMig'
rutaBk='..\BACKUP'
rutapr='..\DATAPR'

SET PATH TO &ruta

IF worker
	IF yesno("Datos de Prueba?")
		SET PATH TO &rutapr
	ENDIF
ENDIF

m.FecSis = DATE()		&& OJO SOLO MIENTRAS NO TRABAJA SelPer()
*IF SET("PATH")=ruta
*	IF SelPer()
*	 ELSE
*		RETURN
*	ENDIF
*ENDIF

DO CtrlDB WITH 1

*DO EstMorCl		&& para verificar la morosidad de los clientes

*-- Clave de acceso

IF !FILE( '&ruta\USUARIO.CDX')
	DO STANDBY WITH ' Error en INDEXA '
	RETURN
ENDIF

*public vcoddep,vconex,vmaq,vllave,vusurec,vflag,vnumdep
PUBLIC vUser_Id,vNombre,vSistema,vConex,vMaq,vLlave,vFlag,vUsuCla

vConex = alltrim(left(sys(0),8))
vMaq   = alltrim(right(sys(0),2))
vLlave = alltrim(sys(2))

USE Usuario ORDER TAG Usuario1  ALIAS usua
IF !Clave()
	CLOSE DATA
	DO STANDBY WITH '!! Acceso Denegado !!',10,15
	DO ctrldb WITH 2
	IF Worker
		CANCEL
	ENDIF
	QUIT
ENDIF

vUser_Id  = Usuario
vNombre  = alltrim(nombre)
vFlag    = flag
*vUsuCla  = ALLTRIM(Usua.Clave)


vUsuCla = chrtran(ALLTRIM(vuser_id),'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                           'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')

USE

**-- Definiciones para el Men£ --**
*---------------------------------*
USE Parmae ORDER TAG Parmae1 ALIAS Parma
SEEK "EMPRES"
IF !EOF()
	cia  = alltrim(Descri)			&& abreviatura de la empresa
	detaw  = alltrim(Descriaux)	    && detalle del nombre de la empresa
	direw  = alltrim(Descriau2)   	&& detalle de la direccion de empresa
	telew  = Codigoaux		    	&& nro. de telefono de la empresa
	rucw   = NumEnt
	IF SEEK("RUCEMP01")
		rucw = ALLTRIM(DescriAux)
	 ELSE
		rucw = "           "
	ENDIF
endif

use
subc = " "
desa = " M.R.A "
titu = "   Sistema de Control de Nichos "

**-- Colores
escolor = iscolor()
if escolor
   set color to ,n/w,b
   c_panta = 8
   c_borde = 10
   c_popup = 4  &&15
   c_texto = "N/W"
   c_fondo = 1
else
   c_panta = 8
   c_borde = 7
   c_popup = 3
   c_texto = "N+/W"
   c_fondo = 1
endif

SET MESSAGE TO 23 CENTER

@  1, 0,23,79  box "°°°°°°°°°" color scheme c_fondo
@  1, 1,23,24  box "±±±±±±±±±" color scheme c_fondo

spac = (80 -(len(cia)+len(titu)+10))/2
rotulo1 = cia + space(spac) + titu + spac(spac) + space(11)

usuarios = '® USER: '+vnombre+' ¯'
fecha    = dtoc(date()) +space(2)
rotulo2  = padr(desa,17,' ')+padc(usuarios,46,' ')+padl(fecha,17,' ')
do logos with rotulo1,rotulo2,50

save screen to principal

* ERROR TRAPPING SYSTEM
if worker
   on error
 else
   on error do fox_errs with program()
endif

save screen to pantalla

set clock on
*store .t. to ven_sistema
*store .f. to esc_tecla

= sys(2002,1)

*--- El men£ principal ---**
l_col  ="W+/W,N/W,N/W,GR+/B, R+/B, W+/B , W+/W, N+/N, GR+/B, R+/B "

** þÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄþ
** º        N i c h o s        º
** þÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄþ

IF escolor
	DEFINE POPUP menu FROM 5,2 SHADOW COLOR &L_COL
 ELSE
	DEFINE POPUP menu FROM 5,2 COLOR SCHEME C_POPUP
ENDIF

*DEFINE BAR 1 OF menu PROMPT ' \<Ingresos      ' MESSAGE ''
DEFINE BAR 2 OF menu PROMPT ' \<Registros     ' MESSAGE ''
DEFINE BAR 3 OF menu PROMPT '\-'                MESSAGE ''
*DEFINE BAR 3 OF menu PROMPT ' \<Reportes      ' MESSAGE ''
DEFINE BAR 4 OF menu PROMPT ' \<Tablas        ' MESSAGE ''
DEFINE BAR 5 OF menu PROMPT ' \<Catalogos     ' MESSAGE ''
DEFINE BAR 6 OF menu PROMPT ' \<mantenimiento ' MESSAGE ''
DEFINE BAR 7 OF menu PROMPT ' \<Salir          '

*- Ingresos
if escolor
	define popup Pop_10 from 5,22  shadow color &l_col
else
	define popup Pop_10 from 5,22  color scheme c_popup
endif

DEFINE BAR 1  OF  Pop_10 PROMPT ' \<Ventas           ' MESSAGE ''
DEFINE BAR 2  OF  Pop_10 PROMPT ' \<Pag. A Cta.      ' MESSAGE ''
DEFINE BAR 3  OF  Pop_10 PROMPT ' \<Pag. Subvenciones' MESSAGE ''

*- Registros
if escolor
	define popup Pop_11 from 5,22  shadow color &l_col
else
	define popup Pop_11 from 5,22  color scheme c_popup
endif

DEFINE BAR 1  OF  Pop_11 PROMPT ' \<Difuntos       ' MESSAGE '' 
DEFINE BAR 2  OF  Pop_11 PROMPT ' \<Ocup. futuras  ' MESSAGE '' 
DEFINE BAR 3  OF  Pop_11 PROMPT ' \<Traslados      ' MESSAGE '' 
DEFINE BAR 4  OF  Pop_11 PROMPT ' \<Transferencia  ' MESSAGE '' 

*- Reportes
IF escolor
	DEFINE POPUP pop_15 FROM 5,27 SHADOW color &l_col
 ELSE
	DEFINE POPUP pop_15 FROM 5,27 color SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  pop_15 PROMPT '\<Caja           ' message ''
DEFINE BAR 2  OF  pop_15 PROMPT '\<Creditos       ' message ''
DEFINE BAR 3  OF  pop_15 PROMPT '\<Subvenciones   ' message ''
DEFINE BAR 4  OF  pop_15 PROMPT '\<Kardex         ' message ''


*- Tablas
if escolor
	define popup Pop_12 from 6,22  shadow color &l_col
else
	define popup Pop_12 from 6,22  color scheme c_popup
endif

DEFINE BAR 1  OF  Pop_12 PROMPT ' \<Cementerios ' MESSAGE '' 
DEFINE BAR 2  OF  Pop_12 PROMPT ' \<Cuarteles   ' MESSAGE '' 
DEFINE BAR 3  OF  Pop_12 PROMPT ' \<Mousoleos   ' MESSAGE '' 
DEFINE BAR 4  OF  Pop_12 PROMPT '\-             ' MESSAGE '' 
DEFINE BAR 5  OF  Pop_12 PROMPT ' \<Clientes    ' MESSAGE '' 
DEFINE BAR 6  OF  Pop_12 PROMPT ' \<Avales      ' MESSAGE '' 

*- Catalogos
if escolor
   define popup Pop_13 from 7,22 shadow color &l_col
else
   define popup Pop_13 from 7,22 color scheme c_popup
endif
define bar 1  of  Pop_13 prompt ' \<Correlativos  ' message ''

*- Mantenimiento
if escolor
   define popup Pop_14 from 8,22 shadow color &l_col
else
   define popup Pop_14 from 8,22 color scheme c_popup
endif
define bar 1  of  Pop_14 prompt ' \<Indexar Archivos   ' message ''
define bar 2  of  Pop_14 prompt " \<Backup's Archivos  " message ''
define bar 3  of  Pop_14 prompt " \<Dep. Backup's      " message ''
define bar 4  of  Pop_14 prompt ' \<Impresi¢n Archivos ' message ''
define bar 5  of  Pop_14 prompt '\-                   ' message ''
define bar 6  of  Pop_14 prompt ' \<Usuarios           ' message ''

ON SELECTION POPUP Menu DO Act_Menus

IF !vFlag$'J*'
	DO ABREPAS
ENDIF

**- La sombra del principal
save screen to principal
activate popup menu
do while empty(prompt())
	activate popup menu
enddo

*-Cerrar todo...
on key
return
*-- Fin  del Programa Principal

procedure act_menus
*------------------
choice=bar()
do case
	CASE choice = 1    && Ingresoss
		on selection popup Pop_10 do  Menu_10
		activate popup Pop_10
	CASE choice = 2    && Registros
		on selection popup Pop_11 do  Menu_11
		activate popup Pop_11
	CASE choice = 3    && Reportes
		on selection popup Pop_15 do  Menu_15
		activate popup Pop_15
	CASE choice = 4    && Tablas
		on selection popup Pop_12 do  Menu_12
		activate popup Pop_12
	CASE choice = 5    && Catalogos
		on selection popup Pop_13 do  Menu_13
		activate popup Pop_13
	case choice = 6    && Mantenimiento
		on selection popup Pop_14 do  Menu_14
		activate popup Pop_14
	case choice = 7    && Salida
		do salmenu
endcase

restore screen from principal
return

procedure menu_10
*------------------
choice=bar()
do case
	CASE choice = 1		&& Ventas
		DO Ventas
	CASE choice = 2		&& Pagos a Cuenta
		DO PagACta
	CASE choice = 3		&& Recibos
		DO PagSub
endcase

restore screen from principal
SHOW POPUP Pop_10,Menu
return

procedure menu_11
*------------------
choice=bar()
do case
	CASE choice = 1		&& Difuntos
		DO Difuntos
	CASE choice = 2		&& Ocup. Futuras
		DO OcupFut
	CASE choice = 3		&& Traslados
		DO Traslado
	CASE choice = 4		&& Transferencias
		DO Trans
endcase

restore screen from principal
SHOW POPUP Pop_11,Menu
return

procedure menu_15  && Reportes
*----------------
choice=bar()
do case
case choice = 1 && Caja
	do RepCaj
case choice = 2 && Creditos
	do RepCre
case choice = 3 && Subvenciones
	do RepSub
case choice = 4 && Subvenciones
	do KarCre
endcase
if !escolor
*	restore screen from principal
	on selection popup pop_15 do menu_15
endif
restore screen from principal
show popup pop_15,menu
hide window all
return

procedure menu_12
*------------------
choice=bar()
do case
	CASE choice = 1		&& Cementerios
		DO Cement
	CASE choice = 2		&& Cuarteles
		DO Cuartel
	CASE choice = 3		&& Mausoleos
		DO Mausoleo
	CASE choice = 5		&& Clientes
		DO CatCli
	CASE choice = 6		&& Clientes
		DO CatAvl
endcase

restore screen from principal
SHOW POPUP Pop_12,Menu
return

procedure menu_13
*------------------
choice=bar()
DO CASE
	CASE choice = 1    && Correlativos
		DO manpar1 WITH "Correlativos","CORREL","Codigo","C¢digo Aux.","Descripci¢nn","","","","Correlativo"
ENDCASE

restore screen from principal
SHOW POPUP Pop_13,Menu
return

procedure menu_14
*------------------
choice=bar()
DO CASE
	CASE choice = 1    && Indexar
		DO Indexa
	CASE choice = 2    && BackUps
		DO BackUp
	CASE choice = 3    && Depuracion de BackUps
		DO DepBk
	CASE choice = 4    && Impresion de Archivos
		DO ImpArc
	CASE choice = 6    && Usuarios
		DO CtrlAcc WITH vUser_Id
ENDCASE

restore screen from principal
SHOW POPUP Pop_14,Menu
return

procedure salmenu
*----------------
store  yesno('¨ Estamos terminando la sesi¢n ?') to in_secoemsa
if in_secoemsa
	DO CtrlDB WITH 2
	set color to
	close data
	hide popup all
	if worker
		clear
		set help on
		on key
		set exclu off
		cancel
	 else
		restore screen from principal
		do clrscr
		quit
	endif
 else
	return
endif


procedure abrepas
*----------------
USE IteUsuOp ORDER TAG IteUsuOp2

*-Ingresos
set skip of bar 1  of pop_10 !SEEK(vUsuCla+"0001")
set skip of bar 2  of pop_10 !SEEK(vUsuCla+"0010")
set skip of bar 3  of pop_10 !SEEK(vUsuCla+"0019")

* No acceso a Ingresos
vopcion = .t.
for i = 1 to 3
	if !skpbar('pop_10',i)
		vopcion = .f.
		exit
	endif
endfor
set skip of bar 1 of menu vopcion

*-Registros
set skip of bar 1  of pop_11 !SEEK(vUsuCla+"0028")
set skip of bar 2  of pop_11 !SEEK(vUsuCla+"0037")
set skip of bar 3  of pop_11 !SEEK(vUsuCla+"0046")
set skip of bar 4  of pop_11 !SEEK(vUsuCla+"0055")

* No acceso a Registros
vopcion = .t.
for i = 1 to 4
	if !skpbar('pop_11',i)
		vopcion = .f.
		exit
	endif
endfor
set skip of bar 2 of menu vopcion

*-Reportes
set skip of bar 1  of pop_15 !SEEK(vUsuCla+"0064")
set skip of bar 2  of pop_15 !SEEK(vUsuCla+"0065")
set skip of bar 3  of pop_15 !SEEK(vUsuCla+"0066")
set skip of bar 4  of pop_15 !SEEK(vUsuCla+"0067")

* No acceso a Registros
vopcion = .t.
for i = 1 to 4
	if !skpbar('pop_15',i)
		vopcion = .f.
		exit
	endif
endfor
set skip of bar 3 of menu vopcion

*-Tablas
set skip of bar 1  of pop_12 !SEEK(vUsuCla+"0068")
set skip of bar 2  of pop_12 !SEEK(vUsuCla+"0077")
set skip of bar 3  of pop_12 !SEEK(vUsuCla+"0086")
set skip of bar 5  of pop_12 !SEEK(vUsuCla+"0095")
set skip of bar 6  of pop_12 !SEEK(vUsuCla+"0104")

* No acceso a Tablas
vopcion = .t.
for i = 1 to 6
	if !skpbar('pop_12',i)
		vopcion = .f.
		exit
	endif
endfor
set skip of bar 4 of menu vopcion

*-Catalogos

set skip of bar 1  of pop_13 !SEEK(vUsuCla+"0113")

* No acceso a Catalogos
vopcion = .t.
for i = 1 to 1
	if !skpbar('pop_13',i)
		vopcion = .f.
		exit
	endif
endfor
set skip of bar 5 of menu vopcion

*-Mantenimiento
set skip of bar 1  of pop_14 !SEEK(vUsuCla+"0122")
set skip of bar 2  of pop_14 !SEEK(vUsuCla+"0123")
set skip of bar 3  of pop_14 !SEEK(vUsuCla+"0124")
set skip of bar 4  of pop_14 !SEEK(vUsuCla+"0125")
set skip of bar 6  of pop_14 !SEEK(vUsuCla+"0126")

* No acceso a Mantenimiento
vopcion = .t.
for i = 1 to 6
	if !skpbar('pop_14',i)
		vopcion = .f.
		exit
	endif
endfor
set skip of bar 6 of menu vopcion


USE IN IteUsuOp

RETURN


*PROCEDURE abrepas
*----------------
PARAMETERS cMenu
FOR i = 1 TO CNTBAR(cMenu)
	SET SKIP OF BAR i OF Menu .f.
ENDFOR
SET SKIP OF BAR i OF Menu .f.
RETURN 

FUNCTION Salta
*-------------
PARAMETERS cBar
lBorra = .F.
IF !USED("IteU")
	USE IteUsu IN 0 ORDER TAG IteUsu1 ALIAS IteU
	lBorra = .T.
ENDIF
SELE IteU
mRet = !SEEK(vUsuario+vSistema+cBar)
IF FOUND()
	cClamod = ALLTRIM(ClaMod)
ENDIF
IF lBorra
	USE
ENDIF
RETURN mRet

