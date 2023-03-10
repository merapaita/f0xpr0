* 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
* ? RegFac.PRG  31/10/96                             L: 0476     ?	
* ? Registro de Facturas                                         ?
* ? AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ?
* ? Adecuaci줻 :   A.S. Oswaldo Arturo Oliva Carl죒.             ?
* 突袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴槁

*- Mensajes del Sistema
Vmens01 = ' Registro de Facturas - Vta. de Bases -'
Vmens02 = ' Revisi줻 de Facturas               '
Vmens03 = 'Digite N즡ero de Factura que desea :'
Vmens04 = 'Dicha Factura no fue encontrada.'
Vmens05 = 'No existe Factura anterior.'
Vmens06 = 'No existe Factura siguiente.'
Vmens07 = '쭱st? seguro que desea ANULAR esta Factura?'
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
DO Inicia                    && Define ventanas, men즧, t죜ulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO vista
*- Activa men? mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

RESTORE SCREEN FROM OldPant
SHOW POPUP Menu
RETURN


PROCEDURE Inicia             && Crea ventanas, men즧 y t죜ulos
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


PROCEDURE Pantalla           && Pinta m쟳cara de datos
*-----------------
  ACTIVATE WINDOW Wind_0
  CLEAR
  @  1,02 SAY "            Serie :" 
  @  3,02 SAY "  Factura /Boleta :"
  @  4,02 SAY "C줰igo Descripci줻:"
  @  5,02 SAY "           R.U.C. :" 
  @  6,02 SAY "       Se쨚r (es) :" 
  @  7,02 SAY "        Direcci줻 :"
  @  8,02 SAY "    Fecha Emisi줻 :"
  @  9,02 SAY REPLICATE('=',74) 
  @ 10,02 SAY "DATOS DEL DIFUNTO "
  @ 12,02 SAY "           Nombre :" 
  @ 13,02 SAY "       Cementerio :" 
  @ 14,02 SAY "          Cuartel :"
  @ 15,02 SAY "      N? de Nicho :"
  @ 16,02 SAY REPLICATE('=',74) 
  @ 17,02 SAY "    Pago a Cuenta :"
  @ 18,02 SAY "      Valor Venta :"
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
  @  1,22 SAY m.serie     COLOR SCHEME 02
  @  3,22 SAY val_para(m.tipo,'DOCCAJ','D',22,7) 
  @  3,37 SAY m.Numero   
  @  4,22 SAY val_Para(m.TipAux,"TIPDES","D",22) 
  @  5,22 SAY m.Ruc       PICTURE "99999999"
  @  6,22 SAY m.Nombre
  @  7,22 SAY m.Direcc 
  @  8,22 SAY m.fecfac
  @ 12,22 SAY m.Nomdif   
  @ 13,22 SAY val_Para(m.CodCem,"CODCEM","D",22)  
  @ 14,22 SAY val_Para(m.DesCua,"CODCUA","D",22) 
  @ 15,22 SAY m.NumNic
  @ 17,22 SAY m.PagCue   PICTURE "999,999.99"
  @ 18,22 SAY m.Importe  PICTURE "999,999.99"  
  ACTIVATE WINDOW wind_10
  @ 00,15 SAY m.subtot PICTURE "99,999,999.99"
  @ 01,15 SAY m.igv    PICTURE "99,999,999.99" 
  @ 02,15 SAY m.total  PICTURE "99,999,999.99"
RETURN


PROCEDURE Revis
*--------------
SELECT fact
 IF EOF()
   DO standby WITH "Archivo: vac죓. No hay registros para procesar."
   RETURN
 ENDIF
 vTemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '같같같같같?Presione 췋10? para seleccionar  o  췊sc? para cancelar같같같같같같'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE WINDOW wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	x1=serie+'.'+numero  :H=' N? Fact.' ,;
	fecfac,;
	nombre :H='Se쨚r (es)',;
	ruc,;
	concepto ,;
	total
vtempo = '같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같'
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
   DO standby WITH "Archivo: vac죓. No hay registros para procesar."
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
*  DO Pantalla
  SCATTER MEMVAR
  SELE Parma
  SEEK 'PARAME'+'DESFAC'
  figv  = Nument
*  SEEK 'PARFAC'+'IGV'
*  figv  = Nument
  SELE Fact
  @  1,22 SAY m.serie 
  @  3,22 SAY val_TIPO(m.tipo,'DOCCAJ',' ',22,8) AND CORCAJ()
  @  4,22 GET m.TipAux   PICTURE "!!"  VALID val_Para(m.TipAux,"TIPDES"," ",22) 
  @  5,22 GET m.Ruc      PICTURE "99999999"
  @  6,22 GET m.Nombre
  @  7,22 GET m.Direcc 
  @  8,22 GET m.fecfac
  @ 12,22 GET m.Nomdif   
  @ 13,22 GET m.CodCem   PICTURE "!!"  VALID val_Para(m.CodCem,"CODCEM"," ",22)  
  @ 14,22 GET m.DesCua   PICTURE "!!"  VALID val_Para(m.DesCua,"CODCUA"," ",22) 
  @ 15,22 GET m.NumNic
  @ 17,22 GET m.PagCue   PICTURE "999,999.99"
  @ 18,22 GET m.Importe  PICTURE "999,999.99"  VALID import(m.importe,figv)
  READ VALID Val_Read()
 IF LASTKEY() = 27
   DO standby WITH "Proceso cancelado. No se graba ning즢 cambio."
   DO Vista
   RETURN
 ENDIF
  
  IF LASTKEY() <> 27        && Si se presion? Esc, no graba
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
  *- Aqu? coloca valores por omisi줻 (default)
  ACTIVATE WINDOW Wind_0
  SELECT fact
*  DO pantalla
  SCATTER MEMVAR BLANK
  m.Fecfac  = DATE()
  m.usuario = wuser_id
  m.estado  = '00'
  SELE Parma
  SEEK 'PARFAC'+'SERIE'
  m.serie= PADL(Nument,3,'0')
*  SEEK 'PARFAC'+'IGV'
  SEEK 'PARAME'+'DESFAC'
  figv  = Nument
  *SEEK 'CORREL'+'FACTUR'
  *m.Numero = PADL(NumEnt+1,7,'0')
  SELE Fact
  @  1,22 SAY m.serie 
  @  3,22 GET m.Tipo     PICTURE '!!'   VALID val_TIPO(m.tipo,'DOCCAJ',' ',22,7) AND CORCAJ()
* @  3,39 GET m.Numero   
  @  4,22 GET m.TipAux    PICTURE "!!"   VALID val_Para(m.TipAux,"TIPDES"," ",22) 
  @  5,22 GET m.Ruc       PICTURE "99999999"
  @  6,22 GET m.Nombre
  @  7,22 GET m.Direcc 
  @  8,22 GET m.fecfac
  @ 12,22 GET m.Nomdif   
  @ 13,22 GET m.CodCem    PICTURE "!!"   VALID val_Para(m.CodCem,"CODCEM"," ",22)  
  @ 14,22 GET m.DesCua    PICTURE "!!"   VALID val_Para(m.DesCua,"CODCUA"," ",22) 
  @ 15,22 GET m.NumNic
  @ 17,22 GET m.PagCue   PICTURE "999,999.99"
  @ 18,22 GET m.Importe  PICTURE "999,999.99"  VALID import(m.importe,figv)
  READ 
  m.periodo=RIGHT(STR(YEAR(m.fecfac),2),2)
  m.nummes=STR(MONTH(m.fecfac),2)
  IF LASTKEY() = 27
     DO standby WITH "Proceso cancelado. No se graba ning즢 cambio."
     DO VISTA
     RETURN
  ENDIF
  IF YesNo("쭱st? Correcto?")
     SELECT fact
     IF F_Appd()            && Crea y graba nuevo registro
        GATHER MEMVAR
        *- Actualiza correlativo
  *      SELECT Parma
  *      SEEK 'CORREL' + 'FACTUR'
  *      IF F_Lock(1)
  *         REPLACE NumEnt WITH NumEnt + 1
  *      ENDIF
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
PARAMETERS xpre,xigv
*m.importe=ROUND(xcan*xpre,2)
m.importe=ROUND(xpre,2)
m.subtot =m.importe
m.igv    =m.importe*(xigv/100)
m.total  =m.importe+m.igv 
@ 15,60 SAY m.importe PICTURE "99,999,999.99" 
ACTIVATE WINDOW wind_10
@ 00,15 SAY m.subtot PICTURE "99,999,999.99"  
@ 01,15 SAY m.igv    PICTURE "99,999,999.99"  
@ 02,15 SAY m.total  PICTURE "99,999,999.99"
RETURN

FUNCTION imp
*--------------  
PARAMETERS xpre,xigv
*m.importe=ROUND(xcan*xpre,2)
m.importe=ROUND(xpre,2)
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

FUNCTION CORCAJ
*---------------

DO CASE 
	CASE m.tipo ='01'
		SELE PARMA
		GO TOP
		SEEK 'DOCCAJ'+'01'
		IF FOUND()
			m.numero= PADL(NumEnt+1,7,'0')
			NUMENT=NUMENT+1
		ENDIF
	CASE m.tipo = '02'
		SELE PARMA
		GO TOP
		SEEK 'DOCCAJ'+'02'
		IF FOUND()
			m.numero =PADL(NumEnt+1,7,'0')
			NUMENT=NUMENT+1
		ENDIF
	CASE m.tipo = '03'
		SELE PARMA
		GO TOP
		SEEK 'DOCCAJ'+'03'
		IF FOUND()
			m.numero =PADL(NumEnt+1,7,'0')	
			NUMENT=NUMENT+1
		ENDIF
	CASE m.tipo = '04'
		SELE PARMA	
 		GO TOP
 		SEEK 'DOCCAJ'+'04'
		IF FOUND()
			m.numero =PADL(NumEnt+1,7,'0')
			NUMENT=NUMENT+1			
		ENDIF
	CASE m.tipo = '07'
		SELE PARMA
		GO TOP
		SEEK 'DOCCAJ'+'07'
		IF FOUND()
			m.numero =PADL(NumEnt+1,7,'0')
			NUMENT=NUMENT+1
		ENDIF
	CASE m.tipo = '08'
		SELE PARMA
		GO TOP
		SEEK 'DOCCAJ'+'08'
		IF FOUND()
			m.numero =PADL(NumEnt+1,7,'0')
			NUMENT=NUMENT+1
		ENDIF
	CASE m.tipo = '09'
		SELE PARMA
		GO TOP
		SEEK 'DOCCAJ'+'09'
		IF FOUND()
			m.numero =PADL(NumEnt+1,7,'0')				
			NUMENT=NUMENT+1
		ENDIF
	CASE m.tipo = '12'
		SELE PARMA
		GO TOP
		SEEK 'DOCCAJ'+'12'
		IF FOUND()
			m.numero =PADL(NumEnt+1,7,'0')
			NUMENT=NUMENT+1
		ENDIF
	OTHERWISE
		SELE PARMA
		GO TOP
		SEEK 'DOCCAJ'+'14'
		IF FOUND()
			m.numero =PADL(NumEnt+1,7,'0')		
			NUMENT=NUMENT+1			
		ENDIF
ENDCASE
RETURN 

FUNCTION Val_TIPO
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
      mDist = 6
    CASE PARAMETERS() = 5
      mDist = 6
  ENDCASE
  mAlias  = ALIAS()

  SELECT Parma
  SEEK Filtro+mValor
  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO Tipo = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
	  SELECT (mAlias)
       ENDIF
	   return       
    ENDIF
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(Descri,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILTER TO
  ENDIF
  mValor = Parma.Codigo
  mCuenta= Parma.DescriAu2
  mDescr = SUBSTR( Parma.Descri, 1, mLong )
  mDescriAux = SUBSTR( Parma.DescriAux, 1, mLong)
  mCan= PADL(Parma.NUMENT+1,7,'0')
*  m.numero= PADL(mcan+1,7,'0')
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici줻
      @ ROW(),mCol         SAY mValor
      @ ROW(),mCol+mdist   SAY mDescr
      @ ROW(),MCol+15      SAY mCan
      RETURN .T.
    CASE mVariable='A'   && En edici줻 SOLO DESCRIPCION
      @ ROW(),mCol SAY mDescr
      RETURN ' '
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
*    OTHERWISE            && En browse de edici줻
 *     REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE

RETURN