**-----------------------------------
** SALAUX.PRG
** Mantenimiento de Saldos por Auxiliar 
** Autor : UDEP
** Fecha : 28/12/95
**-----------------------------------

PUBLIC vdescrip,mvalor,Valias,vFormato
vFormato='salaux'
*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08,Vmens09,Vmens10,Vmens11
Vmens01 = ' Saldos Iniciales '
Vmens02 = 'Saldos Iniciales'
Vmens03 = 'Digite c¢digo de Auxiliar que desea :'
Vmens04 = 'Dicha Auxiliarno fue encontrado.'
Vmens05 = 'No existe Auxiliar anterior.'
Vmens06 = 'No existe Auxiliar siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR ‚sta Saldo?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = ' Listado de Saldos por Tipo de Auxiliar '
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
USE Salaux   IN 1 ORDER TAG Salaux2  ALIAS Salaux
USE Cuentas  IN 2 ORDER TAG Cuentas1 ALIAS Cuen
USE Parmae   IN 3 ORDER TAG Parmae1  ALIAS parma
USE Auxil    IN 4 ORDER TAG Auxil1   ALIAS Auxi

SELE Salaux
SET RELATION TO TIPO+CODIGO INTO Auxi
SET RELATION TO CUENTA      INTO Cuen ADDITIVE

SELECT parma
SEEK "MESANO"+"ACTUAL"
_actmes = LEFT(descri,6)
vmes = VAL(SUBSTR(_actmes,5,2))
vano = VAL(SUBSTR(_actmes,3,2))
 IF vmes = 1
   vano = vano - 1
   vmes = 12
 ELSE
   vmes = vmes - 1
 ENDIF

SELECT Salaux
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo
 
 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

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
 @  3, 2 SAY "              Tipo:"
 @  4, 2 SAY "            C¢digo:"
 @  5, 2 SAY "       Descripci¢n:" 
 @  6, 2 SAY "               Mes:"
 @  7, 2 SAY "               A¤o:"
 @  8, 2 SAY "            Cuenta:"
 @  9, 2 SAY "  Movimiento  Debe:"
 @ 10, 2 SAY "  Movimiento Haber:"
 @ 11, 2 SAY " Sal. Acumul. Debe:"
 @ 12, 2 SAY " Sal. Acumul.Haber:"
RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT Salaux
 SCATTER MEMVAR
 @  3,22 SAY m.Tipo      PICTURE '!!'
 @  4,22 SAY m.Codigo    PICTURE '!!!!!!'
 @  5,22 SAY Auxi.descri
 @  6,22 SAY m.Mes       PICTURE '99'
 @  7,22 SAY m.Ano       PICTURE '99'
 @  8,22 SAY m.Cuenta    PICTURE '!!!!!!!!!!'
 @  9,22 SAY m.DSumaNa   PICTURE '999,999,999,999.999'
 @ 10,22 SAY m.HSumaNa   PICTURE '999,999,999,999.999'
 @ 11,22 SAY m.DSaldoNa  PICTURE '999,999,999,999.999'
 @ 12,22 SAY m.HSaldoNa  PICTURE '999,999,999,999.999'
 *SELECT Salaux
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*-------------
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
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     Tipo     :H='Tipo',;
     Codigo   :H='C¢digo  ',;
     Mes      :H='Mes',;
     Ano      :H='A¤o',;
     Cuenta   :H='Cuenta',;
     DSaldoNa :H='Saldo Debe S/.'  :P='999,999,999,999.999',;
     HSaldoNa :H='Saldo Haber S/.' :P='999,999,999,999.999' 
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
 Vtemp = RECNO()
 ACTIVATE WINDOW standby
 
 
 STORE SPACE(02) TO Vbusca1
 STORE SPACE(06) TO Vbusca2
 STORE 0         TO Vbusca3
 STORE 0         TO Vbusca4
 STORE SPACE(10) TO Vbusca5
 
 @ 0,3  SAY 'Codigo    :'       
 @ 0,16 GET Vbusca1 PICTURE '!!' COLOR SCHEME 1
 @ 0,19 GET Vbusca2 PICTURE '!!!!!!' COLOR SCHEME 1
 @ 1,3  SAY 'Mes / A¤o :'  
 @ 1,16 GET Vbusca3 PICTURE  '99' COLOR SCHEME 1
 @ 1,19 GET Vbusca4 PICTURE  '99' COLOR SCHEME 1
 @ 2,3  SAY 'Cuenta    :'
 @ 2,16 GET Vbusca5 PICTURE  '!!!!!!!!!!' COLOR SCHEME 1
 READ
 DEACTIVATE WINDOW standby
 IF Vbusca1 = SPACE(02) .OR. LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK Vbusca1+Vbusca2+STR(Vbusca3,2)+STR(Vbusca4,2)+Vbusca5
    IF .NOT. FOUND()
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
 ACTIVATE WINDOW Wind_0
 Bloquea = .T.
 DO Fox_Lock WITH 1, Bloquea
 nReg=RECNO()
 IF Bloquea
    @  3,22 SAY m.Tipo      PICTURE '!!'
    @  4,22 SAY m.Codigo    PICTURE '!!!!!!'
    @  5,22 SAY Auxi.descri
    @  6,22 SAY m.Mes       PICTURE '99'
    @  7,22 SAY m.Ano       PICTURE '99'
    @  8,22 SAY m.Cuenta    PICTURE '!!!!!!!!!!'
    @  9,22 GET m.DSumaNa   PICTURE '999,999,999,999.999'
    @ 10,22 GET m.HSumaNa   PICTURE '999,999,999,999.999'
    @ 11,22 GET m.DSaldoNa  PICTURE '999,999,999,999.999'
    @ 12,22 GET m.HSaldoNa  PICTURE '999,999,999,999.999'
   READ
   IF LASTKEY() <> 27          && Si se presion¢ Esc, no graba
      GATHER MEMVAR
   ENDIF
   GOTO nReg
   DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR BLANK
 m.Mes = vmes
 m.Ano = vano
 @  3,22 GET m.Tipo      PICTURE '!!' VALID Val_para(m.tipo,'AUXIL ',' ',22,25) 
 @  4,22 GET m.Codigo    PICTURE '!!!!!!' VALID Val_aux(m.Codigo,m.Tipo,' ',22,25) 
 @  5,22 SAY '                                       '
 @  6,22 GET m.Mes       PICTURE '99'
 @  7,22 GET m.Ano       PICTURE '99'
 @  8,22 GET m.Cuenta    PICTURE '!!!!!!!!!!'
 READ
 SELECT Salaux
 IF LASTKEY() = 27 .OR. m.Cuenta=SPACE(10)
    DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
    GO BOTTOM
 ELSE
    SEEK  m.Codigo + STR(m.Mes,2) + STR(m.Ano,2)+ m.cuenta
    IF FOUND()
       DO standby WITH "Auxiliar ya est  registrada. Proceda a corregir datos."
       DO Vista
       DO Corri
    ELSE    
       @  2,22 CLEAR TO 23,78
    @  3,22 SAY m.Tipo      PICTURE '!!'
    * @  3,33 SAY Val_para(m.tipo,'AUXIL ','m.tipo','D',3,10) 
    @  4,22 SAY m.Codigo    PICTURE '!!!!!!'
    @  5,22 SAY Auxi.descri
    @  6,22 SAY m.Mes       PICTURE '99'
    @  7,22 SAY m.Ano       PICTURE '99'
    @  8,22 SAY m.Cuenta    PICTURE '!!!!!!!!!!'
    @  9,22 GET m.DSumaNa   PICTURE '999,999,999,999.999'
    @ 10,22 GET m.HSumaNa   PICTURE '999,999,999,999.999'
    @ 11,22 GET m.DSaldoNa  PICTURE '999,999,999,999.999'
    @ 12,22 GET m.HSaldoNa  PICTURE '999,999,999,999.999'
       
       READ
       IF LASTKEY() <> 27 .AND. yesno("Confirme el ingreso")
          IF F_APPD()
             GATHER MEMVAR
             UNLOCK
          ENDIF
       ENDIF
   ENDIF
ENDIF

DO Vista
RETURN


PROCEDURE Elimi
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 STORE YESNO(Vmens07) TO vElimina
 IF vElimina
    Bloquea = .T.
    DO Fox_Lock WITH 1, Bloquea
    IF Bloquea
       DELETE NEXT 1
       SELECT Salaux
       IF !BOF()
          SKIP -1
       ENDIF
    ENDIF
    UNLOCK
 ENDIF
 DO Vista
 RETURN


PROCEDURE Lista
*--------------
DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE ' Listado de Saldos Iniciales ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vLista,vAuxiliar
STORE SPACE(02) TO vTipo
STORE SPACE(06) TO vCodigo
STORE SPACE(10) TO vCuenta
vCuenta='0000000000'
vmes = 0
vano = 0

IF LASTKEY()=27
   RETURN
ENDIF

@ 01,01 SAY     "Auxiliar/ X Cuenta: " 
@ 00,22 GET vAuxiliar FUNCTION '^ Todos los Auxiliares;De un Auxiliar;De una Cuenta' 
READ


IF vAuxiliar=3
   @ 03,01 SAY "       Cuenta (02): " GET vCuenta PICTURE '@Z' WHEN vAuxiliar=3;
     VALID  Val_Fun("Cuen","Cuenta","Cuenta+' '+Descri",vcuenta,1,3,22)
ELSE
	@ 03,01 SAY "Auxiliar/ Reportar: " GET vTipo WHEN vAuxiliar=2 VALID Val_para(vtipo,'AUXIL ','C',4,10)
	@ 03,26 GET vCodigo  WHEN  vAuxiliar=2  
ENDIF	

@ 06,01 SAY "            Saldos: " 
@ 05,22 GET vLista FUNCTION '^ Todos los Saldos;De un Mes' 
@ 09,01 SAY "Mes/A¤o a Reportar: "
@ 09,22 GET vmes  WHEN vlista=2 PICTURE '99' VALID vmes>=1 AND vmes<=12
@ 09,26 GET vAno WHEN vlista=2 PICTURE '99' 
@ 10,01 SAY "          Formato : " GET Forma FUNCTION '^ Formato I ;Formato II'  DEFAULT 1 valid cambia_for(forma) ;
  when (vlista=1 AND vAuxiliar=1 ) or (vlista=2 AND vAuxiliar=1)
@ 13,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
vind   = SYS(3) + '.IDX'
nReg=RECNO()

IF OKCANCEL = 1
   DO ESPERA WITH 1,"Espere un momento; Procesando Informaci¢n...!"
   SELE SALAUX
   INDEX ON STR(MES,2)+STR(ANO,2)+CUENTA+TIPO+CODIGO TO (vind) 
   SELECT Salaux
   DO CASE
      CASE vlista=1 AND vAuxiliar=1 
            GO TOP 
           * Todo el Archivo
            DO ESPERA WITH 2
            DO REPORTE WITH 2,vformato,vMens09
      CASE  vlista=2 AND vAuxiliar=1 
            * De un Mes pero de Todos los Auxiliares
            SET FILTER TO Salaux.mes=vmes AND Salaux.ano=vano
            GO TOP 
            DO ESPERA WITH 2
            DO REPORTE WITH 2,vformato,vMens09
      CASE  vlista=1 AND vAuxiliar=2 
            * de un auxiliar pero todos los Meses 
            SET FILTER TO Salaux.tipo=vtipo AND Salaux.Codigo=Vcodigo 
            GO TOP 
            DO ESPERA WITH 2
            DO REPORTE WITH 2,"Salaux1",vMens09
      CASE  vlista=2 AND vAuxiliar=2
            SET FILTER TO Salaux.tipo=vtipo AND Salaux.Codigo=Vcodigo AND Salaux.mes=vmes AND Salaux.ano=vano
            GO TOP 
            DO ESPERA WITH 2
            
            DO REPORTE WITH 2,"Salaux1",vMens09
      CASE  vlista=1 AND vAuxiliar=3
      
            SET ORDER TO Salaux4
            SET FILTER TO SUBSTR(Salaux.Cuenta,1,2)=SUBSTR(vCuenta,1,2)
            GO TOP 
            DO ESPERA WITH 2
   		    DO REPORTE WITH 2,"Salaux2",vMens09
            
      CASE  vlista=2 AND vAuxiliar=3
            SET ORDER TO Salaux4
            SET FILTER TO SUBSTR(Salaux.Cuenta,1,2)=SUBSTR(vCuenta,1,2) AND Salaux.mes=vmes AND Salaux.ano=vano
            GO TOP 
        	DO Calacu
            DO ESPERA WITH 2
            DO REPORTE WITH 2,"Salaux4",vMens09
  ENDCASE    
 ENDIF
 SELE SALAUX
 SET INDEX TO
 ERASE (VIND) 
 IF vlista=2 
    SET FILTER TO 
 ENDIF   
 
 SELECT Salaux
 SET ORDER TO Salaux2
 GO nReg
 DO Vista
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
  RELEASE    MENU   mMenu
  CLOSE DATABASE
  DO Logos WITH Rotulo1, Rotulo2
  RETURN

FUNCTION busca_aux
*-----------------
* Valias=ALIAS()
SELE Auxi
* POR QUE EL CAMPO ES DE 14
SEEK ALLTRIM(m.tipo)+ALLTRIM(m.Codigo)
IF FOUND()
   vdescrip=Auxi.Descri
   SELE Salaux
   RETURN vdescrip
ELSE
   SELE Salaux
   RETURN ' ' 
ENDIF
RETURN      



FUNCTION Vrelle
*----------------
vcuenta = PADL(ALLTRIM(vcuenta),10,'0')
RETURN .T.

FUNCTION RetMes
*--------------
PARAMETERS xmes
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) 


FUNCTION cambia_for
*------------------
PARAMETER forma
IF forma=1
   vformato='saldauxi'
ENDIF
IF forma=2
   vformato='salaux'
ENDIF
RETURN .T.


   
PROCEDURE CALACU
*-----------------

filaux  =SYS(03)+'.DBF'
vindCSal=SYS(03)+'.IDX' 
SELE 10
CREATE TABLE (filaux);
  (Tipo      C(002),;
   Codigo    C(006),;
   mes       N(002),;
   ano       N(002),;
   Cuenta    C(010),;
   ADSumana  N(15,3),;
   AHSumana  N(15,3),;
   DSumana   N(15,3),;
   HSumana   N(15,3),;
   DSaldona  N(15,3),;
   HSaldona  N(15,3))
   
USE IN 5

USE (filaux) IN 10 ALIAS CSalaux   EXCLUSIVE
*---------------------------------------
* Pasa los Valores al Archivo Calculado 
*---------------------------------------
SELE SalAux
GO TOP


SCAN 
  SELE CSalaux
      APPEND BLAN
	  REPLACE CSalaux.Tipo     WITH  Salaux.Tipo    
	  REPLACE CSalaux.Codigo   WITH  Salaux.Codigo        
	  REPLACE CSalaux.mes      WITH  Salaux.mes          
	  REPLACE CSalaux.ano      WITH  Salaux.ano          
	  REPLACE CSalaux.Cuenta   WITH  Salaux.Cuenta      
	  REPLACE CSalaux.DSumana  WITH  Salaux.DSumana     
 	  REPLACE CSalaux.HSumana  WITH  Salaux.HSumana     
	  REPLACE CSalaux.DSaldona WITH  Salaux.DSaldona   
      REPLACE CSalaux.HSaldona WITH  Salaux.HSaldona   
  SELE SalAux
ENDSCAN

SELE SalAux
SET FILTER TO 
SET ORDER TO TAG Salaux5

SELE  CSalAux
INDEX ON STR(MES,2)+STR(ANO,2)+CUENTA+TIPO+CODIGO TO (vindCSal) 
GO TOP


SCAN
   SELE Salaux
   SEEK (Csalaux.Tipo+CSalaux.Codigo+Csalaux.Cuenta+STR(CSalaux.Ano,2))
   SCAN WHILE Salaux.Tipo+Salaux.Codigo+salaux.Cuenta+STR(Salaux.Ano,2)=Csalaux.Tipo+CSalaux.Codigo+Csalaux.Cuenta+STR(CSalaux.Ano,2) AND Salaux.Mes < CSalaux.Mes
        Sele CSalaux
        REPLACE CSalaux.ADSumana  WITH CSalaux.ADSumana+Salaux.DSumana
        REPLACE CSalaux.AHSumana  WITH CSalaux.AHSumana+Salaux.HSumana
   		SELE Salaux
   ENDSCAN
   SELE CSalaux
ENDSCAN
SELE CSalaux
SET RELATION TO TIPO+CODIGO INTO Auxi
SET RELATION TO CUENTA      INTO Cuen ADDITIVE
GO TOP

RETURN
