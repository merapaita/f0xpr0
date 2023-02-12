*--------------------------------------------------------------------------
* RegPre.Prg
* Registra Presupuestos
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE Regveh   IN 2   order tag RegVeh1  ALIAS vehic
USE calen    IN 3   order tag calen3   ALIAS calen

*- Mensajes de aviso al usuario
Vmens01 = 'Registro de Vehiculos'
Vmens02 = ' Vehiculos : REVISION '
Vmens04 = 'Dicho Vehiculos no fue encontrado'
Vmens05 = 'No existe Vehiculo anterior'
Vmens06 = 'No existe Vehiculo siguiente'
Vmens07 = '¨ Desea Anular ‚ste Vehiculo ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Vehiculo ha sido anulado'
Vmens10 = 'El Vehiculo ya est  Atendido'
Vmens11 = 'El Vehiculo ha sido Devuelto'

ON KEY LABEL F9 DO ESTADOV
SELECT vehic
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10


 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Presupuesto' ;
 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Eliminar'   AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
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
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR

 @  1, 2 SAY "         Placa N§ :"
 
 @  3, 2 SAY "            Clase :"
 @  4, 2 SAY "            Marca :"
 @  5, 2 SAY "           Modulo :"
 @  6, 2 SAY " Tipo Combustible :"
 
 @  8, 2 SAY "       Carrocer¡a :"
 @  9, 2 SAY "              A¤o :"
 @ 10, 2 SAY "            Color :"
 @ 11, 2 SAY "            Serie :"

 @ 13, 2 SAY "            Motor :"
 @ 14, 2 SAY "        Cilindros :"
 
 @ 16, 2 SAY "       Propetario :"
 @ 17, 2 SAY "        Ubicaci¢n :"
 @ 18, 2 SAY "      Responsable :" 
 
 @ 20, 2 SAY "      Observaci¢n :"


RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT vehic
STORE 0 TO vAsigna
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0

 SCATTER MEMVAR

 @  0,60 SAY IIF(m.Estado= '00','Operativo',IIF( m.Estado = '99','Inoperatv',IIF(m.Estado='20','Reparacn ','            ')))

 @  1,22 SAY m.CodPla
 
 @  3,22 SAY m.Clase
 @  4,22 SAY m.Marca
 @  5,22 SAY m.Modelo
 @  6,22 SAY m.Combus
 
 @  8,22 SAY m.Carroceria
 @  9,22 SAY m.periodo
 @ 10,22 SAY m.color
 @ 11,22 SAY m.serie
 
 @ 13,22 SAY m.Motor
 @ 14,22 SAY m.Cilindros PICT '99'
 
 @ 16,22 SAY m.Propetario
 @ 17,22 SAY m.Ubicacion
 @ 18,22 SAY m.Responsa
 @ 20,22 SAY m.Observa
 @ 21,00 SAY PADC('° ®F9¯ Cambia Estado °',79,' ') COLOR W+/B
 RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE vehic
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
 codpla  :H='Placa' ,;
 clase   :H='Clase',;
 marca   :H='Marca',;
 modelo  :H='Modelo',;
 carrOceria :H='Tipo',;
 periodo :H='A¤o',;
 color   :H='Color'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SELE vehic
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
vtemp    = RECNO()
vPLACA   = '       '
vCodFte  = '   '
vCodPrg  = '  '
vCodSubPr= '   '

DEFINE WINDOW Lis FROM 11,12 TO 17,68 DOUBLE ;
TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW STANDBY
@  1,2 say 'Ingrese el N£mero Placa : ' GET vPlaca  PICTURE '!!!!!!!' 
READ
DEACTIVATE WINDOW STANDBY
IF EMPTY(vPlaca) .or. LASTKEY()=27
    RETURN
ELSE
    SEEK vPlaca
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
 *****
  ACTIVATE WINDOW Wind_0
 SELECT vehic
 SCATTER MEMVAR
 
 @  1,22 GET m.CodPla PICTURE '!!!!!!!' &&disAble
 
 @  3,22 get m.Clase
 @  4,22 get m.Marca
 @  5,22 get m.Modelo
 @  6,22 get m.Combus
 
 @  8,22 get m.Carroceria
 @  9,22 get m.periodo
 @ 10,22 get m.color
 @ 11,22 get m.serie
 
 @ 13,22 get m.Motor
 @ 14,22 get m.Cilindros PICT '99'
 
 @ 16,22 get m.Propetario
 @ 17,22 get m.Ubicacion
 @ 18,22 get m.Responsa
 @ 20,22 GET m.Observa PICTUR '@S56'
 
 READ VALID Val_Read()

 IF LASTKEY() # 27
    SELECT vehic
    GATHER MEMVAR
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT vehic
 DO PANTALLA
 DO Vista
 RETURN

PROCEDURE VALVEH
*---------------
PRIVATE TOPE,vret
TOPE = RECNO()
SEEK m.codpla
vret = IiF(found(),.f.,.t.)
go tope
RETURN vret

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT vehic
 V=RECNO()
 DO Pantalla
 SCATTER MEMVAR BLANK
 
 @  1,22 GET m.CodPla PICTURE '!!!!!!!' VALID VALVEH()
 
 @  3,22 get m.Clase
 @  4,22 get m.Marca
 @  5,22 get m.Modelo
 @  6,22 get m.Combus
 
 @  8,22 get m.Carroceria
 @  9,22 get m.periodo
 @ 10,22 get m.color
 @ 11,22 get m.serie
 
 @ 13,22 get m.Motor
 @ 14,22 get m.Cilindros PICT '99'
 
 @ 16,22 get m.Propetario
 @ 17,22 get m.Ubicacion
 @ 18,22 get m.Responsa
 @ 20,22 GET m.Observa PICTUR '@S56'
 
 READ VALID VAL_READ()
 
 IF LASTKEY() = 27
    SELECT vehic
    DO VISTA
    RETUR
 ENDIF

 IF LASTKEY() # 27 
    IF F_Appd()
       m.Estado = '00'
       m.fecemi = date()
       IF LASTKEY() # 27
          SELECT vehic
          GATHER MEMVAR
       ELSE
         *-??
       ENDIF
    ELSE
       *- No pudo agregar la liquidaci¢n
       SELECT vehic
       GO BOTTOM
    ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT vehic
 DO Vista
 RETURN


PROCEDURE Elimi
*---------------
 SELECT vehic
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF !Estado # '  00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ELIMINAR ‚ste vehicpuesto ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
   dele next 1
   SKIP
   DO Vista
 ENDIF
 UNLOCK all
 RETURN

PROCEDURE Agreg_Item
*-----------------
IF F_appd()
  REPLACE Periodo WITH m.Periodo ,;
          NumPec  WITH m.NumPec ,;
          Estado  WITH m.Estado ,;
          CodCal  WITH m.CodCal
 *KEYBOARD CHR(15)
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Elimi_Item
*-------------------
SELECT vehic
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif
return

PROCEDURE Corri_Item
*-----------------
REPLACE CodCal  WITH m.CodCal
RETURN .T.

PROCEDURE Lista
*--------------
select vehic
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"REGVEH",' Veh¡culos '
endif
SELE vehic
GO vtemp
do Vista
RETURN

PROCEDURE ValAP
*--------------
SELECT vehic
 vTem=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 *SET RELATION TO PERIODO+NUMOC INTO ITEOC
 *SET SKIP TO ITEOC
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 tipfun  :H='Tp' ,;
 codprg  :H='Prg',;
 codsubpr :H='SubPrg',;
 codact  :H='Act',;
 codproy :H='Proy',;
 descri :H='Obs'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 VCLI=CodPrg
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
 GOTO Vtemp
RETURN .t.


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  ON KEY LABEL F9
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
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

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.

XX = Val_ArtDet(_Cod,.F.)
IF XX
  SELECT vehic
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

PROCEDURE  PROGRAMA
*-----------------
 as=alias()
 publi t01,t02,t03,t04,t05,t06,t07,t08,t09,t10
 sele parma
 set filt to Tipo='CODPRG'
 k=0
 scan
   k=k+1
   v=padl(k,2,'0')
   t&v=parma.Descri
 endscan
 sele (as)
 go top
return .t.

procedure estadov
*----------------
DO CASE
   CASE ESTADO = '00' &&OPERATIVO
       REPLACE ESTADO WITH '20'
   CASE ESTADO = '20' &&REPARATIVO
       REPLACE ESTADO WITH '99'
   CASE ESTADO = '99' &&INOPERATIVO
       REPLACE ESTADO WITH '00'
ENDCAS
DO VISTA       
RETURN

