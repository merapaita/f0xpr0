*--------------------------------------------------------------------------
* VerCdr.Prg
* Verifica el Cuadro de necesidades por Dependencia
* Estado del Documento:
*   '00' Emitida     ** Este es el que se registra
*   '50' Aprobada
*   '70' Devuelta
*   '99' Anulada
* Autor: Luis Castillo D.   Piura,Setiembre 1994
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE CdrNec   IN 1   order tag CdrNec1  ALIAS Cuadro
USE IteCn    IN 2   order tag IteCn1   ALIAS IteCn
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE IteArt   IN 5   order tag IteArt1  ALIAS Iteart
USE Calen    IN 6   order tag calen1   ALIAS calen
USE Itepec   IN 7   order tag Itepec13 ALIAS Itepec
USE Pecosa   IN 8   order tag Pecosa1  ALIAS Pecosa

*- Mensajes de aviso al usuario
Vmens01 = 'Verificaci¢n del Cuadro de Necesidades'
Vmens02 = ' VERIFICACION : Cuadro de Necesidades '
Vmens04 = 'Dicho Cuadro de Necesidades no fue encontrado'
Vmens05 = 'No existe Cuadro de Necesidades anterior'
Vmens06 = 'No existe Cuadro de Necesidades siguiente'
Vmens07 = '¨ Desea ELIMINAR ‚ste Cuadro de Necesidades ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Cuadro de Necesidades ha sido anulado'
Vmens10 = 'El Cuadro de Necesidades ya fue atendido'
Vmens11 = 'El Cuadro de Necesidades ha sido devuelto'

SELECT Cuadro
GO BOTTOM
ON KEY LABEL F4 DO IMPRIME
ON KEY LABEL F11 DO ACTUALIZA
*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
hide popup all
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Verifica Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 10,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 11,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle : CN °  ®F9¯ Detalle Item   ®F4¯ Imprime  ®F11¯ Actualiza CN'  ;
 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Verifica'   AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
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
 ACTIVATE WINDOW Wind_1
 CLEAR

 @  1, 2 SAY "            Fecha :"
 @  3, 2 SAY "          Periodo :"
 @  4, 2 SAY "      Dependencia :"
 @  5, 2 SAY "    Recepcionista :"
 @  6, 2 SAY "N£mero Trabajador :"

 @  8, 2 SAY "    Observaciones :"

 RETURN

PROCEDURE Imprime
*----------------
select Cuadro
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   SET RELATION TO PERIODO+coddep INTO ITEcn
   SET SKIP TO ITECn
   do liscn with 2
   SET RELATION TO
endif
select Cuadro
go vtemp
DO VISTA
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Cuadro
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 ON KEY LABEL F9 DO VISTA_DET
 SCATTER MEMVAR
 @  0,60 SAY IIF( m.Estado= '00','Pendiente',IIF( m.Estado = '30','En Abastc.',IIF(m.Estado='99','Anulada ',IIF(m.Estado='50','Atendido','        '))))
 @  1,22 SAY m.FecCn
 @  3,22 SAY m.Periodo
 @  4,22 SAY m.Coddep
 @  4,29 SAY val_para(m.CodDep,'CODDEP','D',29,40)
 @  5,22 SAY m.Atte
 @  6,22 SAY m.Numtra PICTURE '9,999'
 @  8,22 SAY m.Observa
 DO VISTA_HIJO
 RETURN

PROCEDURE Vista_Hijo
*---------------------
 SELE ITECN                               && :V=VALART():F
   BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.CODDEP TIMEOUT 0.001 ;
   WINDOW Wind_2 FIELDS ;
     Descri      : H= 'Descripci¢n' :28 ,;
     Coduni      : H= 'Uni'      :4,;
     CanNec      : H= 'Pedido' :P='9,999'  ,;
     Nec_1       : H= '1§ T' :P='9,999'  ,;
     Nec_2       : H= '2§ T' :P='9,999'  ,;
     Nec_3       : H= '3§ T' :P='9,999'  ,;
     Nec_4       : H= '4§ T' :P='9,999'  ,;
     AjuCan      : H= 'Ajuste'   :P='9,999' ,;
     XX=AjuCan - CanNec : H='Dif'  :P='9,999'

SELECT CUADRO
RETU

PROCEDURE Vista_DET
*------------------
 SELE ITECN                               && :V=VALART():F
   BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.CODDEP ;
   WINDOW Wind_2 FIELDS ;
     Descri      : H= 'Descripci¢n' :28 ,;
     Coduni      : H= 'Uni'      :4,;
     CanNec      : H= 'Pedido' :P='9,999'  ,;
     Nec_1       : H= '1§ T' :P='99,999'  ,;
     Nec_2       : H= '2§ T' :P='99,999'  ,;
     Nec_3       : H= '3§ T' :P='99,999'  ,;
     Nec_4       : H= '4§ T' :P='99,999'  ,;
     AjuCan      : H= 'Ajuste'   :P='9,999' ,;
     XX=AjuCan - CanNec : H='Dif'  :P='9,999'

SELECT CUADRO
RETU

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT Cuadro
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SET RELATION TO PERIODO+CODDEP INTO ITECN
 SET SKIP TO ITECN
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 Feccn  :H='FecCdr' ,;
 Periodo:H='Pr' ,;
 Coddep :H='DEP',;
 Numtra :H='Trabj',;
 Itecn.Cannec :H='Cant.':P='99,999' ,;
 Itecn.Descri :H='Articulo' ,;
 Observa:H='Observaciones'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
 SELE CUADRO
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp    = RECNO()
 vPeriodo = space(2)
 vNum_sol = space(6)
 ACTIVATE WINDOW standby
 @ 1,1 SAY '            Periodo : ' GET vPeriodo  PICTURE 'XX'
 @ 2,1 SAY 'Ingrese Dependencia : ' GET vNum_sol  PICTURE 'XXXXXX' VALID Val_parA( vNum_sol,'CODDEP','C',22,40,7) 
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_sol) .or. LASTKEY()=27
    RETURN
 ELSE
   SET ORDE TO 1
   SEEK vPeriodo + vNum_sol
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
 IF Estado = '99'
   * Anulada
   DO STANDBY WITH Vmens09
   RETURN
 ENDIF
 IF Estado = '70'
   * El Cuadro ha sido devuelto
   DO STANDBY WITH Vmens11
   RETURN
 ENDIF
 *****
 SELECT Cuadro
 SCATTER MEMVAR
 ACTIVATE WINDOW Wind_1
 @  5,22 GET m.Atte
 @  6,22 GET m.Numtra    PICTURE '9,999'
 @  8,22 GET m.Observa
 READ VALID Val_read()
 SELE ITECN
 Ok = Trabaja_Hijo()
 IF Ok .AND. LASTKEY() # 27
       GATHER MEMVAR
 ENDIF
 SELE CUADRO
 DO VISTA
 UNLOCK
 RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
SELECT Cuadro
PUBLIC vEstado
SCATTER MEMVAR
 IF M.Estado = '50'
   * El cuadro esta ya verifcado
   DO STANDBY WITH 'El Cuadro ya esta Verificado'
*   RETURN
 ENDIF
 IF m.Estado = '99'
   * Anulada
   DO STANDBY WITH Vmens09
   RETURN
 ENDIF
 IF m.Estado = '70'
   * El Cuadro ha sido devuelto
   DO STANDBY WITH Vmens11
   RETURN
 ENDIF
 
ACTIVATE WINDOW Wind_1

 @  1,22 GET m.FecCn    DISABLE
 @  3,22 GET m.Periodo  DISABLE
 @  5,22 GET m.Atte
 @  6,22 GET m.Numtra    PICTURE '9,999'
 @  8,22 GET m.Observa
READ
SELECT Itecn
Ok = Trabaja_Hijo()

IF OK AND LASTKEY()#27
   SELECT CUADRO
 *  IF RLOCK() 
      REPLACE CUADRO.estado with '30'
 *  ENDIF
 *  UNLOCK ALL
ENDIF
SELECT Cuadro
DO Vista
RETURN

PROCEDURE Trabaja_Hijo
*---------------------
 SET RELATION TO 'B'+CODART INTO ITEART
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)

 BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2 KEY m.periodo+m.Coddep FIELD ;
     iteart.Marca : H= 'Marca':P='@S15' :v=marc(),;
     Descri       : H= 'Descripci¢n' :42 :W=.F.  ,;
     Coduni       : H= 'Uni'      :W=.F. :5,;
     CanNec       : H= 'Tot.Nec':P='9,999' :W=.F. ,;
     Nec_1        : H= '1§ T' :P='99,999' :W=!EMPTY(CodArt) ,;
     Nec_2        : H= '2§ T' :P='99,999' :W=!EMPTY(CodArt) ,;
     Nec_3        : H= '3§ T' :P='99,999' :W=!EMPTY(CodArt) ,;
     Nec_4        : H= '4§ T' :P='99,999' :W=!EMPTY(CodArt) :V=Sumar():F ,;
     AjuCan       : H= 'Ajuste' :P='9,999' :W=EMPTY(CodArt) ,;
     iteart.PreUni: H= 'Precio' :V=SUMCDR():F :P='99,999,999.99'

 SELECT IteCn
 vEstado = 1
 SEEK m.periodo+m.coddep
 SCAN WHILE m.Periodo = Periodo  .and.  m.Coddep = Coddep
    IF F_LOCK(1)
      IF EMPTY(CODART) OR NEC_1+NEC_2+NEC_3+NEC_4 = 0
        DELETE NEXT 1
      ENDIF
    ENDIF
 ENDSCAN

 UNLOCK ALL


 ON KEY LABEL F5
 ON KEY LABEL F8
 ON KEY LABEL F10

 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Cuadro
 RETURN .T.

PROCEDURE Sumar
*--------------
if rlock()
   Replace AjuCan with NEC_1+NEC_2+NEC_3+NEC_4
endif
unlock
return .t.

PROCEDURE Marc
*--------------
if rlock()
   Replace marca with iteart.marca
endif
unlock
return .t.


PROCEDURE SumCdr
*---------------
if rlock()
   Replace PreUni with IteArt.Preuni , estado with '00'
endif
unlock

return .t.

PROCEDURE Agreg_Item
*-----------------
IF F_appd()
  REPLACE Periodo WITH m.Periodo ,;
          Coddep WITH m.Coddep
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Elimi_Item
*-------------------
SELECT Itecn
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif
return

PROCEDURE Elimi
*--------------
 SELECT Cuadro
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ELIMINAR FISICAMENTE ‚str Cuadro ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
    SELECT IteCn
    SEEK m.periodo+m.coddep
    SCAN WHILE m.Periodo = Periodo  .and.   m.Coddep = Coddep
         IF F_LOCK(1)
            DELETE NEXT 1
         ENDIF
    ENDSCAN
    SELECT Cuadro
    DELETE NEXT 1
    GO BOTTOM
    DO Vista
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Lista
*--------------
select Cuadro
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   SET RELATION TO PERIODO+coddep INTO ITEcn
   SET SKIP TO ITECn
   do liscn with 1
*   do reporte with 2,"LisCdr",' Cuadro de Necesidades '
   SET RELATION TO
endif
select Cuadro
go vtemp
DO VISTA
RETURN

PROCEDURE LisCN
*---------------
parameter vtipo
vTemo = RECNO()
DEFINE WINDOW LIS FROM 5,15 TO 19,65 FLOAT DOUBLE TITLE 'Listado Ordenes de Compra' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro
vCli = CUADRO.CODDEP
@ 01,01 SAY "    Todas las C/N : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
@ 03,01 SAY "              C/N : "
@ 03,22 GET vCli    WHEN VTOCLI=2  PICTURE '!!!!!!' VALID ValCN()

@ 05,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Dependenc;Emision'  WHEN VTOCLI=1

@ 08,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos' WHEN VTOCLI=1

@ 12,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,CodDep,FecCN) TO (vInd) ;
      FOR IIF(vToCli=1,.T.,CodDep = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',Estado = '50' ))
   SET INDEX TO (VIND)
   GO TOP
   SET RELATION TO PERIODO+coddep INTO ITEcn
   SET SKIP TO ITEcn
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' Pendientes ',' Atendidos '))
   IF !EOF()
     if vtipo=1 
        DO REPORTE WITH 2,"LisCdrA",'  Cuadro de Necesidades ',1,.F.,.T.
     else
        DO REPORTE WITH 2,"LisCdrT",'  Cuadro de Necesidades ',1,.F.,.T.
     endif   
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   CLOSE INDEX
   ERASE (VIND)
ENDIF

RETURN

PROCEDURE ValCN
*--------------
SELECT Cuadro
 vTem=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO PERIODO+coddep INTO ITEcn
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 Feccn  :H='FecCdr' ,;
 Periodo:H='Pr' ,;
 Coddep :H='C¢digo',;
 XX=VAL_PARA(Coddep,'CODDEP','D') :H='Dependencia' ,;
 Numtra :H='Trabj',;
 Observa:H='Observaciones'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 VCLI=CodDep
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

FUNCTION VALARTD
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.
XX = Val_Artdet(_COD,.F.)
IF XX
  SELECT IteCn
  REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          Codart  WITH Iteart.codart,;
          Descri  WITH Iteart.descri
  vFun = .T.
ENDIF
RETURN vFun

PROCEDURE ACTUALIZA
*------------------
OK=VE_PASSW('PITIN')
If !ok
   return
endif   
PRIVATE ALIAS
ALIAS = ALIAS()
ACTIVATE WINDOW STANDBY

@ 1,10 SAY 'Espere un Momento...' COLOR W+/RB*

SELE ITEPEC
SET RELA TO PERIODO+NUMPEC INTO PECOSA

SELE ITECN
SET ORDE TO ITECN3
GO TOP
SCAN 
    vkey = itecn.periodo+itecn.coddep+itecn.codart
    SELECT ITEPEC
    P_1 = 0
    P_2 = 0
    P_3 = 0
    P_4 = 0
    SEEK vKey
    IF FOUND()
       SCAN while itepec.periodo+itepec.coddep+itepec.codart = vKey
            vfec = trimestre(pecosa.fecpec)
	        P_&vfec = P_&vfec + itepec.canreq
       ENDSCAN	 
       SELECT ITECN
       REPLACE PED_1 WITH P_1,PED_2 WITH P_2,PED_3 WITH P_3,PED_4 WITH P_4
    ELSE
     SELECT ITECN  
    ENDIF   
ENDSCAN
SELEC ITEPEC
SET RELA TO 
SELE ITECN
SET ORDE TO ITECN1    
SELECT (ALIAS)
DEACTIVATE WINDOW STANDBY
RETURN    

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
return vtrim



