* LISPROC.PRG   Programa donde se almacenan los procedimientos a utilizar en los programas


*******************
procedure direccion
*******************

clear
use s:\sici\catastro\inscripc orde iinscrip in 0 share alia ii
use s:\sici\catastro\rzcalle orde rzcall in 0 share alia calle
use s:\sici\catastro\rlurba orde rlur01 in 0 share alia urba
use s:\sici\catastro\conexion orde iconexi6 in 0 share alia cone

@14,10 say "Colocando Dirección..." COLOR RGB(0,0,128,192,192,192)

sele dd
GO TOP
do while !eof()
   i=inscrinro
   ccc=precalle
   sele cone
   seek i
   cocod=spac(15)
   if !eof()
	   cocod=IIF(SUBST(premuni,1,3)='   ','NO HAY',allt(premuni))
   endif
   sele ii
   seek i
   n=''
   uc=0
   if !eof()
      *** Aquí armamos la dirección del usuario
	  *** para la urbanización STR(regcod,2)+STR(loccod,2)+STR(urbcod,3)
      *** para la calle STR(regcod,2)+STR(zoncod,2)+STR(calcod,4)
	  u=STR(preregion,2)+STR(clilocali,2)+STR(cliurbcob,3)
	  c=STR(preregion,2)+STR(clizona,2)+STR(ccc,4)
	  nro=allt(str(clinro))
	  sele urba
	  seek u
	  urba=''
	  tipur=iif(urbtip='CER','',urbtip)
	  uc=urbcod
	  if !eof()
	     urba=allt(tipur)+' '+allt(urbdes)
	  endif
	  sele calle
	  seek c
	  cal=''
	  if !eof()
	     if substr(caldes,1,5)<>'     '
		     cal=allt(caltip)+' '+allt(caldes)+' '+nro
		 endif
	     if cocod<>'NO HAY'
		     cal=cal+' - '+cocod
		 endif
	  endif
      n=cal+'/'+urba
      sele dd
      repla direccion with n
   endif
   sele dd
   skip
enddo

sele calle
use
sele urba
use
sele ii
use
sele cone
use
sele dd
*-----
return
*-----

****************
proce tcodcattar
****************

clear
use s:\sici\catastro\uniduso orde iuniduso in 0 share alia uni
use S:\sici\catastro\categ   orde cate01   in 0 share alia cate
use S:\sici\catastro\sbcate  orde 1        in 0 share alia subcate    &&STR(ambcod,2)+STR(codcattar,2)+STR(codscatta,2)

@16,10 say "Localizando Categoría y Subcategoría..." COLOR RGB(0,0,128,192,192,192)

sele dd
GO TOP
do while !eof()
   i=STR(preregion,2)+STR(prezona,2)+STR(presector,2)+STR(premzn,3)+STR(prelote,4)+STR(presublote,4)
   sele uni
   seek i
   if !eof()
      *AQUI BUSCAMOS CATEG Y SBCATE
      CATEG=STR(ambcodx,2)+STR(codcattar,2)
	  sbcateg=STR(ambcodx,2)+STR(codcattar,2)+STR(codscatta,2)
	  sele cate
	  seek categ
	  n=''
	  if !eof()
	      n=descattar   && categ
	  endif
	  sele subcate
	  seek sbcateg
	  n1=''
	  if !eof()
	      n1=desscatta  && sbcate
	  endif
      sele dd
      repla categoria with n,sbcategori with n1
*      if SUBSTR(n1,1,5)='INDUS'
 *     	REPLA PRIOR WITH 1
  *    Endif
   *   if SUBSTR(n1,1,4)='COM.'
*      	REPLA PRIOR WITH 2
 *     Endif
  *    if SUBSTR(n1,1,5)='ESTAT'
   *   	REPLA PRIOR WITH 3
    *  Endif
*      if SUBSTR(n1,1,5)='DOM.B'
 *     	REPLA PRIOR WITH 4
  *    Endif
   *   if SUBSTR(n1,1,5)='DOM.A'
    *  	REPLA PRIOR WITH 5
     * Endif
*      if SUBSTR(n1,1,3)='SOCIA'
 *     	REPLA PRIOR WITH 6
  *    Endif
   endif
   sele dd
   skip
enddo
sele uni
use
sele cate
use
sele subcate
use
*-----
return
*-----

************
proc lisprog
************
sele dd
inde on str(preregion,2)+STR(prezona,2)+STR(presector,2)+STR(premzn,3)+STR(prelote,4)+STR(presublote,4) tag catastro
count to Total_para_corte
go top
use s:\sici\catastro\docum 		orde docu01 	in 0 alias doc share   && STR(oficod,2)+STR(ofiagecod,2)+PADR(doctip,7)
use s:\sici\epsgrau\programa 	orde catastro 	in 0 alias pro share
use s:\sici\catastro\conexion 	orde iconexio 	in 0 alias cn share
use s:\sici\progacc 			orde iprogcor 	in 0 alias pg share
use s:\sici\proglin 			orde iproglin 	in 0 alias pl share      &&STR(cornroprog,8)+STR(tabacccod,2)+STR(cornrolin,6)
use s:\sici\tprog 				orde itprog 	in 0 alias tp share         &&STR(tpreregion,2)+STR(tprezona,2)+STR(tpresector,2)+STR(tpremnz,3)+STR(tprelote,4)+STR(tpresublot,4)
use s:\sici\catastro\rutrepa 	orde urutrep2 	in 0 alias rp share     &&STR(regcod,2)+STR(zoncod,2)+STR(seccod,2)+STR(rrutmzn,3)+STR(rrutlote,4)+STR(rrutste,4)
use s:\sici\catastro\ciclos 					in 0 alias cic share
sele cic
loca for faccicmes=mont(date()) .and. faccicano=year(date())
fecini=facciccor
fefina=facciccof

* Vemos el número de programa que continúa, se actualizará al momento de grabar

SELE doc
seek STR(1,2)+STR(1,2)+PADR('PROGRAM',7)
if !eof()
	NP=ultnro+1
	numregdoc=recno()
else
	wait window 'ERROR PARA OBTENER EL NÚMERO DE PROGRAMACION... SE TERMINARÁ EL APLICATIVO'
	close data
	return	
endif
@ 2,5 say 'PROGRAMA N° ' font 'ROMAN',20
@ 2,33 get np pict '##,###,###' font 'ROMAN',20
clear gets
@6,5 say 'Acción : '
acc=3
@6,18 get acc pict "###" font 'roman',12
desacc='Corte de Agua                      '
@6,23 get desacc font 'roman',12
clear gets

sele dd
DEFINE WINDOW para_corte FROM 8,1 TO 23,95 TITLE 'CÓDIGO CATASTRAL, DIRECCION Y MESES QUE ADEUDA EL CLIENTE PARA PROGRAMA DE CORTES' CLOSE FLOAT GROW SHADOW ZOOM
DEFINE WINDOW nro_cortes FROM 8,1 TO 23,95 TITLE 'NRO. DE LOS PROGRAMAS DE CORTES QUE SE ESTAN GENERANDO' CLOSE FLOAT GROW SHADOW ZOOM
BROWSE FIELDS preregion :2:H='P',prezona :2:H='D',presector :2:H='S',premzn :3:H='M',;
prelote :4:H='L',presublote :4:H='SL',direccion :H='DIRECCION',NMESDEU :H='Mes.Deud';
nomodify noappend nodelete nowait window para_cortes
@2,60 say 'Fecha Inicio de Cortes : ' get fecini pict '@d'
@3,60 say 'Fecha Final de Cortes  : ' get fefina pict '@d'
@25,5 say 'Número de Conexiones : '
@25,29 get Total_para_corte pict '###,###'
clear gets

store 0 to cdo,operadores

cdo=Total_para_corte
totalc=cdo

*@25,81 get operadores pict '####' Valid iif(operadores=0,.f.,.t.)   && cantidad de programas a obtener
operadores=1

@27,5 GET mielecc FUNCTION '*H' PICTURE 'Grabar y Generar Programas;Salir Sin Generar Programa';
 SIZE 1.5,20,1 default 'Salir'
read

if mielecc='Salir' .or. Lastkey() = 27
	Sele doc
	use
	sele pro
	use
	sele cn
	use
	sele pg
	use
	sele pl
	use
	sele tp
	use
	sele rp
	use
	sele cic
	use
   clear
   return
endif
sele dd
store 0 to i,j
nFil=4
do while i<operadores
	i=i+1
	j=j+1
	SELE PG			&& rellenamos progacc.dbf
	GO BOTT
	NP=cornroprog+1
	appen blank
	repla cornroprog with np,tabacccod with 3,corfecelab with date(),cortotal with cdo,;
	  corfecinic with fecini,corfecfin with fefina,anorefer with year(date()),;
	  mesrefer with mont(date()),oficod with 1,ofiagecod with 1,ciclorefer with 1
    									    && luego buscar de donde se saca el ciclo

	sele doc			&& rellenamos el número del programa a ejecutar
	go numregdoc
	repla ultnro with np
	ACTIVATE WINDOW NRO_CORTES
	nCol=5+((j-1)*10)
	@2,5 say 'Se están Generando los Siguientes Programas (Tome Nota de ellos): '
	if nCol<92
		@nFil,nCol get np pict '###,###'
	else
	    j=1
	    nFil=nFil+2
    	nCol=5+((j-1)*10)
    	@nFil,nCol get np pict '###,###'
	endif
	
	clear gets
	for k=1 to cdo
		sele dd
		p=preregion
		z=prezona
		s=presector
		m=premzn
		l=prelote
		sl=presublote
		if contipmul=2
			sl=0
		endif
		xcatastro=str(p,2)+STR(z,2)+STR(s,2)+STR(m,3)+STR(l,4)+STR(sl,4)
		nmd=nmesdeu
		inc=inscrinro
		acl=conestado
		cvn=convenio
		rcl=reclamo
		xcll=precalle
		xurb=preurba
		xnro=prenro
		xmun=premuni
		tnmb=clinomx
		tdeu=totdeuda
		xcat=categoria
		sele rp
		seek xcatastro
		if eof()
			wait window 'COD. CATASTRAL NO EXISTE EN ARCHIVO RUTAS DE REPARTO'+xcatastro
			secuen=9999
		else
			secuen=rsecuen
			xrutrep=rutrep     && ruta de reparta, por ahora es igual a la manzana
		endif
		
		sele pl      && rellenamos proglin.dbf
		appen blank
		repla preregiox with p,prezonx with z,presector with s,premzn with m,prelote with l,presublote with sl
		repla cornroprog with np,tabacccod with 3,cornrolin with k,corfecprog with date(),ciclofac with 1,;
		      corsecuen with secuen
		      
		sele pro	&& rellenamos programad.dbf
		appen blank
		repla preregion with p,prezona with z,presector with s,premzn with m,prelote with l,;
		      presublote with sl,inscrinro with inc,conestado with acl,nmesdeu with nmd,;
		      convenio with cvn,reclamo with rcl,fecha_prog with date(),nro_progra with np,cornrolin with k
		      
		sele cn		&& rellenamos conexion.dbf
		seek xcatastro
		if eof()
		   wait window 'Revise el Código Catastral : '+xcatastro 
		else
		   repla cliacccod with 3, flagprog with 1, fecprog with date()
		endif

*		sele tp		&& rellenamos tprog.dbf
*		appen blank
*		repla tpreregion with p,tprezona with z,tpresector with s,tpremnz with m,tprelote with l,tpresublot with sl,inscrinro with inc,tprognro with np,;
*		      tprogcorre with k,tnromeses with nmd,fecprogcor with date(),tprecalle with xcll,tpreurba with xurb,tprenro with xnro,tpremuni with xmun,;
*		      tnombre with tnmb,truta with xrutrep,tsecuen with secuen,tcodcattar with xcat,tvalordeud with tdeu
		sele dd
		skip
	endfor
enddo
Sele doc
use
sele pro
use
sele cn
use
sele pg
use
sele pl
use
sele tp
use
sele rp
use
sele cic
use
clear
WAIT WINDOW 'DEBE IR AL SICI Y CONTINUAR CON EL PROCESO DE CORTES... Presione [ENTER] '
Clear
*-----
return
*-----

**************
proc salctacte
**************
clear

use s:\sici\ctacte orde tag ictacte alias cta share in 0

@10,10 say "Obteniendo saldos de Cta. Cte. " COLOR RGB(0,0,128,192,192,192)

sele dd
go top
do while !eof()
	wins=inscrinro
	if val(wins)=0
		skip
		loop
	endif
	store 0 to wtotal
	sele cta
	seek wins
	DO WHILE inscrinro=wins
		wtotal=ctasaldo
		skip
	enddo
	sele dd
	if eof()
		exit
	endif
	repla saldcte with wtotal
  	skip
enddo 
sele cta
use
*-----
return
*-----

******************
Procedure ingrsist
******************
*--- programa que valida el ingreso al sistema y graba en variable de
*--- memoria nCodDig el código del Usuario y en cNomDig el Nombre

Set step off
SET TALK OFF
DEFINE WINDOW ejemplo FROM 2,3 TO 21,45 TITLE ' Usuarios del Sistema ' FLOAT SHADOW SYSTEM COLOR RGB(192,192,192,192,192,192)  && COLOR SCHEME 8 

*** Llena la matriz matrizciudad con datos de ciudades ***

Use s:\sici\epsgrau\Digit share in 0 Order tag Digi01
SELECT DISTINCT Str(Digcod,3)+' '+Digname FROM digit Where Val(Dignivel)=cNivUsu Or Val(Dignivel)=9 INTO ARRAY matrizciudad

SELECT digit

ACTIVATE WINDOW ejemplo

@ 1,05 SAY 'Seleccionar Usuario : '

*** Obtiene información usando la matriz predefinida ***

@ 2,05 GET elementomatriz FROM matrizciudad SIZE 11, 30 DEFAULT matrizciudad(1) WHEN refresca() VALID muestraele(matrizciudad) COLOR SCHEME 9
@ 14,15 GET aceptar FUNCTION '*t \!Aceptar' DEFAULT 1 SIZE 1, 6
READ CYCLE SHOW muestracd()  && Activa GETS

RELEASE POPUPS cdestru, cdcampo

n = 0  &&  Número de Intentos
Do While .t.
	cClave = Space(08)
	@ 16,05 Say 'Clave : ' COLOR RGB(0,0,128,192,192,192),RGB(255,255,0,0,0,128)
	@ 16,13 Get cClave Pict '@!' Color Rgb(192,192,192,192,192,192),Rgb(192,192,192,192,192,192) && w/w,w/w
	Read
	If Lastkey() = 27
		Wait window ' Abandonó Programa !!! ' Nowait
		sele digit
		use
		quit
	Endif

	If Usuok(left(elementomatriz,3),cClave)
		nCodDig = Val(left(elementomatriz,3))
		cNomDig = Digname
		cNivUsu = Dignivel
		cDpto   = Dpto
		cClave  = Clave
		cCodSici= DigCodSici
		Sele Digit
		Use
		exit
	Else
		Wait window ' Clave Errada !!! ' Nowait
		n = n + 1
		If n = 3
			Wait window ' Usuario Intruso !!! ' Nowait
			sele digit
			use
			quit
		Else
			Loop		
		Endif
	Endif
Enddo

RELEASE WINDOW ejemplo
*-----
Return
*----- 

*****************
FUNCTION refresca
*****************
*** Actualiza la información de la pantalla sin llamar a la subrutina ***
SHOW GETS OFF

*******************
FUNCTION muestraele
*******************
*** Muestra el elemento elegido ***
PARAMETER elemento


FUNCTION muestracd
*** Muestra los elementos activos de cada lista ***
*@ 13,25 SAY elementomatriz SIZE 1, 18


Function Usuok
Para pNumUsu,pClave
fRet = .f.
*Use s:\sici\epsgrau\digit share in 0 order tag Digi01  && STR(digcod,3)
Seek Str(Val(pNumUsu),3)
If Found()
	cClaEnc = xencrip(pNumUsu+Alltrim(pClave))
	If Alltrim(Clave) = cClaEnc
		fRet = .t.
	Endif

Endif
*Use
Return fRet


Function xencrip
PARAMETERS CADENA, CADE2
PRIVATE I, L, K, CADE2
L = LEN(ALLTRIM(CADENA))
CADE2 = ''
I = 1
DO WHILE I<=L
   K = L-I+1
   IF K>30
      K = K-INT(K/30)*30
   ENDIF
   CADE2 = CADE2+CHR(       256-ASC(SUBSTR(CADENA,I,1))+K-1)
   I = I+1
ENDDO
RETURN CADE2


Procedure Graba_alt
Para pInscri,phisref,pHisvalant,pHisvaldes,pDocRef,pObser
*--- Inscripción, Abrev., Valor Ant., Valor Desp., Doc.Ref., Observación
wk_ga = Select()
Use s:\sici\epsgrau\histocam share in 0
*---
Sele Histocam
Appe Blank
Repl Hisfaccod With pInscri, Hisfecha With Date(),;
	 Hishorasi With Time(), Hisusuar With cNomDig,;
	 Hisrefer  With pHisref, hisvalant With pHisvalant, hisvaldes With pHisvaldes,;
	 Hisdocum  With pDocRef, hisfecam With Date(), hisobser With pObser
Use
Select(wk_ga)
Return


FUNCTION nom_txt
PARA n1  && Dos letras del Nombre
PRIVATE cor,ncor,archivo
cor=0

ncor = Padl(cor,6,'0')

Store space(15) to archivo
archivo = n1 + ncor + '.txt'
sw1 = .t.
sw2 = .f.
do while sw1
	sw2 = file(archivo)
	If sw2
		cor = cor + 1
		ncor = Padl(cor,6,'0')
		archivo = n1 + ncor + '.txt'
	Else
		sw1 = .f.
	Endif
Enddo
RETURN(archivo)


FUNCTION nom_dbf
PARA n1  && Dos letras del Nombre
PRIVATE cor,ncor,archivo
cor=0

ncor = Padl(cor,6,'0')

Store space(15) to archivo
archivo = n1 + ncor + '.dbf'
sw1 = .t.
sw2 = .f.
do while sw1
	sw2 = file(archivo)
	If sw2
		cor = cor + 1
		ncor = Padl(cor,6,'0')
		archivo = n1 + ncor + '.dbf'
	Else
		sw1 = .f.
	Endif
Enddo
*--------------
RETURN(archivo)
*--------------


*************
proc lisprog1
*************

sele dd
inde on str(prior,1)+str(preregion,2)+STR(prezona,2)+STR(presector,2)+STR(premzn,3)+STR(prelote,4)+STR(presublote,4) tag catastro
count to Total_para_corte
go top

use s:\sici\catastro\docum orde docu01 in 0 alias doc share   && STR(oficod,2)+STR(ofiagecod,2)+PADR(doctip,7)
use s:\sici\epsgrau\programa orde catastro in 0 alias pro share
use s:\sici\catastro\conexion orde iconexio in 0 alias cn share
use s:\sici\progacc orde iprogcor in 0 alias pg share
use s:\sici\proglin orde iproglin in 0 alias pl share      &&STR(cornroprog,8)+STR(tabacccod,2)+STR(cornrolin,6)
use s:\sici\tprog orde itprog in 0 alias tp share         &&STR(tpreregion,2)+STR(tprezona,2)+STR(tpresector,2)+STR(tpremnz,3)+STR(tprelote,4)+STR(tpresublot,4)
use s:\sici\catastro\rutrepa orde urutrep2 in 0 alias rp share     &&STR(regcod,2)+STR(zoncod,2)+STR(seccod,2)+STR(rrutmzn,3)+STR(rrutlote,4)+STR(rrutste,4)
use s:\sici\catastro\ciclos in 0 alias cic share
sele cic
loca for faccicmes=mont(date()) .and. faccicano=year(date())
fecini=facciccor
fefina=facciccof

* Vemos el número de programa que continúa, se actualizará al momento de grabar

SELE doc
seek STR(1,2)+STR(1,2)+PADR('PROGRAM',7)
if !eof()
	NP=ultnro+1
	numregdoc=recno()
else
	wait window 'ERROR PARA OBTENER EL NÚMERO DE PROGRAMACION... SE TERMINARÁ EL APLICATIVO'
	close data
	return	
endif
@ 2,5 say 'PROGRAMA N° ' font 'ROMAN',20
@ 2,33 get np pict '##,###,###' font 'ROMAN',20
clear gets
@6,5 say 'Acción : '
acc=3
@6,20 get acc pict "###" font 'roman',12
desacc='Orden Corte de Agua'
@6,28 get desacc font 'roman',12
clear gets

sele dd
DEFINE WINDOW para_corte FROM 8,1 TO 23,95 TITLE 'CÓDIGO CATASTRAL, DIRECCION Y MESES QUE ADEUDA EL CLIENTE PARA PROGRAMA DE CORTES' CLOSE FLOAT GROW SHADOW ZOOM
DEFINE WINDOW nro_cortes FROM 8,1 TO 23,95 TITLE 'NRO. DE LOS PROGRAMAS DE CORTES QUE SE ESTAN GENERANDO' CLOSE FLOAT GROW SHADOW ZOOM
BROWSE FIELDS preregion :2:H='P',prezona :2:H='D',presector :2:H='S',premzn :3:H='M',;
prelote :4:H='L',presublote :4:H='SL',direccion :H='DIRECCION',NMESDEU :H='Mes.Deud';
nomodify noappend nodelete nowait window para_cortes
@2,60 say 'Fecha Inicio de Cortes : ' get fecini pict '@d'
@3,60 say 'Fecha Final de Cortes  : ' get fefina pict '@d'
@25,5 say 'Número de Conexiones : '
@25,29 get Total_para_corte pict '###,###'
clear gets

store 0 to cdo,operadores
*@25,62 get cdo pict '####' Valid iif(cdo=0,.f.,.t.)     && cantidad de registros por programa
cdo=Total_para_corte
totalc=cdo

*@25,81 get operadores pict '####' Valid iif(operadores=0,.f.,.t.)   && cantidad de programas a obtener
operadores=1

@27,5 GET mielecc FUNCTION '*H' PICTURE 'Grabar y Generar Programas;Salir Sin Generar Programa';
 SIZE 1.5,20,1 default 'Salir'
read

if mielecc='Salir' .or. lastkey()=27
	Sele doc
	use
	sele pro
	use
	sele cn
	use
	sele pg
	use
	sele pl
	use
	sele tp
	use
	sele rp
	use
	sele cic
	use
   clear
   return
endif

sele dd
store 0 to i,j
nFil=4
do while i<operadores
	i=i+1
	j=j+1
	SELE PG			&& rellenamos progacc.dbf
	GO BOTT
	NP=cornroprog+1
	appen blank
	repla cornroprog with np,tabacccod with 3,corfecelab with date(),cortotal with cdo,;
	  corfecinic with fecini,corfecfin with fefina,anorefer with year(date()),;
	  mesrefer with mont(date()),oficod with 1,ofiagecod with 1,ciclorefer with 1
    									    && luego buscar de donde se saca el ciclo

	sele doc			&& rellenamos el número del programa a ejecutar
	go numregdoc
	repla ultnro with np
	ACTIVATE WINDOW NRO_CORTES
	nCol=5+((j-1)*10)
	@2,5 say 'Se están Generando los Siguientes Programas (Tome Nota de ellos): '
	if nCol<92
		@nFil,nCol get np pict '###,###'
	else
	    j=1
	    nFil=nFil+2
    	nCol=5+((j-1)*10)
    	@nFil,nCol get np pict '###,###'
	endif
	
	clear gets
	for k=1 to cdo
		sele dd
		p=preregion
		z=prezona
		s=presector
		m=premzn
		l=prelote
		sl=presublote
		if contipmul=2
			sl=0
		endif
		xcatastro=str(p,2)+STR(z,2)+STR(s,2)+STR(m,3)+STR(l,4)+STR(sl,4)
		nmd=nmesdeu
		inc=inscrinro
		acl=conestado
		cvn=convenio
		rcl=reclamo
		xcll=precalle
		xurb=preurba
		xnro=prenro
		xmun=premuni
		tnmb=clinomx
		tdeu=totdeuda
		xcat=categoria
		sele rp
		seek xcatastro
		if eof()
			wait window 'COD. CATASTRAL NO EXISTE EN ARCHIVO RUTAS DE REPARTO'+xcatastro
			secuen=9999
		else
			secuen=rsecuen
			xrutrep=rutrep     && ruta de reparta, por ahora es igual a la manzana
		endif
		
		sele pl      && rellenamos proglin.dbf
		appen blank
		repla preregiox with p,prezonx with z,presector with s,premzn with m,prelote with l,presublote with sl
		repla cornroprog with np,tabacccod with 3,cornrolin with k,corfecprog with date(),ciclofac with 1,;
		      corsecuen with secuen
		      
		sele pro	&& rellenamos programad.dbf
		appen blank
		repla preregion with p,prezona with z,presector with s,premzn with m,prelote with l,;
		      presublote with sl,inscrinro with inc,conestado with acl,nmesdeu with nmd,;
		      convenio with cvn,reclamo with rcl,fecha_prog with date(),nro_progra with np,cornrolin with k
		      
		sele cn		&& rellenamos conexion.dbf
		seek xcatastro
		if eof()
		   wait window 'Revise el Código Catastral : '+xcatastro 
		else
		   repla cliacccod with 3, flagprog with 1, fecprog with date()
		endif

*		sele tp		&& rellenamos tprog.dbf
*		appen blank
*		repla tpreregion with p,tprezona with z,tpresector with s,tpremnz with m,tprelote with l,tpresublot with sl,inscrinro with inc,tprognro with np,;
*		      tprogcorre with k,tnromeses with nmd,fecprogcor with date(),tprecalle with xcll,tpreurba with xurb,tprenro with xnro,tpremuni with xmun,;
*		      tnombre with tnmb,truta with xrutrep,tsecuen with secuen,tcodcattar with xcat,tvalordeud with tdeu
		sele dd
		skip
	endfor
enddo
clear
Sele doc
use
sele pro
use
sele cn
use
sele pg
use
sele pl
use
sele tp
use
sele rp
use
sele cic
use
WAIT WINDOW 'DEBE IR AL SICI Y CONTINUAR CON EL PROCESO DE CORTES... Presione [ENTER] '
Clear

*-----
return
*-----

