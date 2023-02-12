* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegFac.PRG  31/10/96                             L: 0476     ³	
* ³ Registro de Facturas                                         ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.             ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Mensajes del Sistema
Vmens01 = ' Registro de Facturas - Vta. de Bases -'
Vmens02 = ' Revisi¢n de Facturas               '
Vmens03 = 'Digite N£mero de Factura que desea :'
Vmens04 = 'Dicha Factura no fue encontrada.'
Vmens05 = 'No existe Factura anterior.'
Vmens06 = 'No existe Factura siguiente.'
Vmens07 = '¨Est  seguro que desea ANULAR esta Factura?'
Vmens08 = 'No hay registros para procesar'
SET EXACT OFF
*- Apertura de files
USE Factura IN 1 ORDER TAG Factura1 ALIAS Fact
USE ParMae  IN 2 ORDER TAG ParMae1  ALIAS Parma
USE Auxil   IN 3 ORDER TAG Auxil1   ALIAS Auxil
USE ConLic  IN 4 ORDER TAG ConLic3  ALIAS ConLic
PUBLIC wuser_id,vuser_id,figv
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID = CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')

SELECT fact
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR

*- Inicia proceso

SAVE SCREEN TO OldPant
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO vista
*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

RESTORE SCREEN FROM OldPant
SHOW POPUP Menu
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN

 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW wind_10 FROM 18,44 TO 22,78 ;
	TITLE ' TOTAL ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi

 ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
  ACTIVATE WINDOW Wind_0
  CLEAR
  @  1,02 SAY "            Serie :" 
  @  2,02 SAY "          Factura :"
  @  3,02 SAY " Concurso Licitac.:"
  @  4,02 SAY "    Tipo Auxiliar :" 
  @  5,02 SAY "   Cod.  Auxiliar :" 
  @  6,02 SAY "       Se¤or (es) :"
  @  7,02 SAY "        Direcci¢n :"
  @  8,02 SAY "           R.U.C. :"
  @  9,02 SAY "    Fecha Emisi¢n :"
  @ 10,02 SAY REPLICATE('=',74) 
  @ 12,02 SAY "         Cantidad :"
  @ 13,02 SAY " Unidad de Medida :"
  @ 14,02 SAY "         Concepto :"
  @ 15,02 SAY "  Precio Unitario :"
  @ 16,02 SAY "          Importe :"
  ACTIVATE WINDOW Wind_10
  @  0,02 SAY "Subtotal:"
  @  1,02 SAY "I.G.V.  :"
  @  2,02 SAY "Total   :" 
RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
  SELECT fact

  IF EOF()
     DO Pantalla
     RETURN
  ENDIF
  ACTIVATE WINDOW Wind_0
  SCATTER MEMVAR
  @  1,22 SAY m.serie  COLOR SCHEME 02
  @  2,22 SAY m.Numero COLOR SCHEME 02
  @  3,22 SAY Val_Fun("ConLic","Conlic","LEFT(Deslic,50)",m.tipo+ALLTRIM(m.Conlic)) 
  @  4,22 SAY Val_ParaD(m.TipAux,"AUXIL ","V",22,30) 
  @  5,22 say val_Auxi(ALLT(m.CodPrv),ALLT(m.TipAux),'V')
  @  6,22 SAY m.nombre
  @  7,22 SAY m.direcc
  @  8,22 SAY m.ruc
  @  9,22 SAY m.fecfac
  @ 12,22 SAY m.cantidad  PICTURE "99"
  @ 13,22 SAY 'Unidad.' 
  @ 14,22 SAY m.concepto  PICTURE '@S50' FUNCTION "!"
  @ 15,22 SAY m.preuni  PICTURE "999,999.99"
  @ 16,22 SAY m.importe PICTURE "99,999,999.99"
  ACTIVATE WINDOW wind_10
  @ 00,15 SAY m.subtot PICTURE "99,999,999.99"
  @ 01,15 SAY m.igv    PICTURE "99,999,999.99" 
  @ 02,15 SAY m.total  PICTURE "99,999,999.99"
RETURN


PROCEDURE Revis
*--------------
SELECT fact
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 vTemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE WINDOW wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	x1=serie+'.'+numero  :H=' N§ Fact.' ,;
	fecfac,;
	nombre :H='Se¤or (es)',;
	ruc,;
	concepto ,;
	total
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
   GOTO vtemp
ENDIF
SHOW MENU mMenu
ON KEY LABEL F10
DO Vista
RETURN


PROCEDURE Busca
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF

 Vtemp = RECNO()
 DEFINE WINDOW WBusca FROM 10,12 TO 14,68 DOUBLE COLOR SCHEME 6
 ACTIVATE WINDOW WBusca

 STORE 0 TO Vbusca

 @ 0,3 SAY "# Serie : " GET vNumSer PICTURE "999" DEFAULT 001 COLOR SCHEME 7
 @ 1,3 SAY vMens03 GET Vbusca PICT '9999999' COLOR SCHEME 7
 READ
 RELEASE WINDOW WBusca

 IF EMPTY(Vbusca) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK PADL(INT(vnumSer),3,'0')+PADL(INT(vBusca),7,'0')

   IF !FOUND()
     DO standby WITH vMens04
     GOTO Vtemp
   ELSE
     DO vista
   ENDIF

 ENDIF

RETURN


PROCEDURE Anter
*--------------
 IF EOF()
   DO standby WITH vMens08
   RETURN
 ENDIF

 SELECT fact
 IF !BOF()
   SKIP -1
 ENDIF

 IF BOF()
   GO TOP
   DO standby WITH vMens05
 ELSE
   DO vista
 ENDIF

RETURN


PROCEDURE Proxi
*--------------
 IF EOF()
   DO standby WITH vMens08
   RETURN
 ENDIF

 SELECT fact
 IF !EOF()
   SKIP
 ENDIF

 IF EOF()
   DO standby WITH vMens06
   GO BOTTOM
 ELSE
   DO vista
 ENDIF

RETURN


PROCEDURE Corri
*--------------
  IF EOF()
     DO standby WITH vMens08
     RETURN
  ENDIF

  ACTIVATE WINDOW Wind_0
  SELE Parma
  SEEK 'PARFAC'+'IGV'
  figv  = Nument
  SELE Fact

  @  1,22 SAY m.serie 
  @  2,22 SAY m.Numero
  @  3,22 GET m.conlic   PICTURE "9999" VALID val_fun('ConLic','ConLic',"ConLic+' '+LEFT(Deslic,50)",m.tipo+m.conlic,1) AND vglo()
  @  4,22 GET m.TipAux   PICTURE "!!" VALID val_ParaD(ALLT(m.TipAux),"AUXIL "," ",22) 
  @  5,22 GET m.CodPrv   PICTURE "@!" VALID val_AuxiD(ALLT(m.CodPrv),ALLT(m.TipAux),'A',30)
  @  6,22 GET m.nombre   FUNCTION "!"
  @  7,22 GET m.direcc   FUNCTION "!"
  @  8,22 GET m.ruc  PICTURE "99999999"
  @  9,22 GET m.fecfac   
  @ 12,22 GET m.cantidad  PICTURE "99"
  @ 13,22 SAY 'Unidad.' 
  @ 14,22 GET m.concepto  PICTURE '@S50' FUNCTION "!"
  @ 15,22 GET m.preuni  PICTURE "999,999.99"  VALID import(m.cantidad,m.preuni,figv)
  READ VALID Val_Read()
  
  IF LASTKEY() <> 27        && Si se presion¢ Esc, no graba
     SELECT fact
     IF F_Lock(1)
        GATHER MEMVAR
     ENDIF
     UNLOCK
  ENDIF

  SELECT fact
  
  DO vista

RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
  *- Aqu¡ coloca valores por omisi¢n (default)
  ACTIVATE WINDOW Wind_0

  SELECT fact

  SCATTER MEMVAR BLANK
  m.Fecfac  = DATE()
  m.usuario = wuser_id
  m.estado  = '00'
  SELE Parma
  SEEK 'PARFAC'+'SERIE'
  m.serie= PADL(Nument,3,'0')
  SEEK 'PARFAC'+'IGV'
  figv  = Nument
  SEEK 'CORREL'+'FACTUR'
  m.Numero = PADL(NumEnt+1,7,'0')
  SELE Fact
  @  1,22 SAY m.serie 
  @  2,22 SAY m.Numero
  @  3,22 GET m.conlic   PICTURE "9999" VALID val_fun('ConLic','ConLic',"ConLic+' '+LEFT(Deslic,50)",m.conlic,1) AND vglo()
  @  4,22 GET m.TipAux   PICTURE "!!"   VALID Val_ParaD(m.TipAux,"AUXIL "," ",22,30) 
  @  5,22 GET m.CodPrv   PICTURE "@!"   VALID val_AuxiD(m.CodPrv,m.TipAux,"A",30) AND di_pr()
  @  6,22 GET m.nombre   FUNCTION "!"
  @  7,22 GET m.direcc   FUNCTION "!"
  @  8,22 GET m.ruc  PICTURE "99999999"
  @  9,22 GET m.fecfac   
  @ 12,22 GET m.cantidad  PICTURE "99"
  @ 13,22 SAY 'Unidad.' 
  @ 14,22 GET m.concepto  PICTURE '@S50' FUNCTION "!"
  @ 15,22 GET m.preuni  PICTURE "999,999.99"  VALID import(m.cantidad,m.preuni,figv)
  READ 
  IF LASTKEY() = 27
     DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
     DO VISTA
     RETURN
  ENDIF
  IF YesNo("¨Est  Correcto?")
     SELECT fact
     IF F_Appd()            && Crea y graba nuevo registro
        GATHER MEMVAR
        *- Actualiza correlativo
        SELECT Parma
        SEEK 'CORREL' + 'FACTUR'
        IF F_Lock(1)
           REPLACE NumEnt WITH NumEnt + 1
        ENDIF
     ELSE
        DO StandBy WITH " No pudo grabar archivo "
     ENDIF
     UNLOCK ALL
  ENDIF
  SELECT fact
  DO vista
RETURN


PROCEDURE Elimi
*--------------
 IF EOF()
   DO standby WITH vMens08
   RETURN
 ENDIF
 STORE YESNO(vMens07) TO vElimina
 IF vElimina
    SELECT fact
    IF F_Lock(1)
       DELETE NEXT 1
    ENDIF
 ENDIF
 DO vista
RETURN


PROCEDURE Lista
*--------------
  SELECT fact
  IF EOF()
     DO standby WITH vMens08
     RETURN
  ENDIF
  vTemp = RECNO()
  SET FILTER TO serie=m.serie AND numero=m.Numero
  DO REPORTE WITH 2,"RepFac","Reporte de Facturas",1,.F.,.T.
  SET FILTER TO
  GO vTemp
RETURN

FUNCTION import
*--------------  
PARAMETERS xcan,xpre,xigv
m.importe=ROUND(xcan*xpre,2)
m.subtot =m.importe
m.igv    =m.importe*(xigv/100)
m.total  =m.importe+m.igv 
@ 15,60 SAY m.importe PICTURE "99,999,999.99" 
ACTIVATE WINDOW wind_10
@ 00,15 SAY m.subtot PICTURE "99,999,999.99"  
@ 01,15 SAY m.igv    PICTURE "99,999,999.99"  
@ 02,15 SAY m.total  PICTURE "99,999,999.99"
RETURN

FUNCTION di_pr
*---------------
Vali=ALIAS()
vord=ORDER()
SELE Auxil
IF SEEK (ALLT(m.tipaux)+ALLT(m.codprv)) 
      m.nombre= Auxil.descri
      m.direcc= Auxil.direccion
      m.ruc   = LEFT(ALLT(Auxil.ruc),8)
ENDIF
SELE (vali)
SET ORDER TO (vord)
RETURN 


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
RETURN


PROCEDURE Fin_opcion
*-------------------
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_10
  RELEASE    MENU   mMenu
  CLOSE DATA
RETURN

FUNCTION dmes
*------------
PARAMETER fmes
DO CASE
   CASE fmes=1
        v_d='Enero'
   CASE fmes=2
        v_d='Febrero'
   CASE fmes=3
        v_d='Marzo'
   CASE fmes=4
        v_d='Abril'
   CASE fmes=5
        v_d='Mayo' 
   CASE fmes=6
        v_d='Junio'
   CASE fmes=7
        v_d='Julio'
   CASE fmes=8
        v_d='Agosto'
   CASE fmes=9
        v_d='Setiembre'  
   CASE fmes=10
        v_d='Octubre' 
   CASE fmes=11
        v_d='Noviembre'
   CASE fmes=12
        v_d='Diciembre'        
ENDCASE
RETURN v_d        

FUNCTION vglo
*------------
vAl=ALIAS()
SELE ConLic
IF SEEK(m.conlic)
   m.concepto=ConLic.Deslic
ENDIF   
SELE (VAL)
RETURN