**----------------------------------
** REPMAY .PRG
** Reporte del mayor
** AUTOR : UDEP - 1994. JECHS
**----------------------------------

* Abrir Archivos de trabajo
USE Parmae  IN 1 ORDER TAG Parmae1         ALIAS parma
*USE Auxil   IN 2 ORDER TAG Auxil1          ALIAS Auxi
*USE Cuentas IN 4 ORDER TAG Cuentas1        ALIAS Cuen

 SELECT parma
*SEEK 'MESANOACTUA'
*vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
*STORE 0  TO xano, xMes
*xMes     = MONTH(vmesact)
*xano     = VAL(SUBSTR(STR(YEAR(vmesact),4),3,2))
*STORE ' '    TO xmoneda
DO Inicia
ACTIVATE POPUP POLAP 
DO salida
RETURN


PROCEDURE Inicia
*---------------
  vcol ="w+/w,n/w,W/W,w/W,,w+/b"
  IF Escolor
     DEFINE POPUP POLAP FROM 06,52  to 10,75  SHADOW COLOR &vcol
  ELSE
     DEFINE POPUP POLAP FROM 06,52  to 10,75  COLOR SCHEME c_popup
  ENDIF

  DEFINE BAR  1 OF POLAP  PROMPT ' \<a AP-1   '
  DEFINE BAR  2 OF POLAP  PROMPT ' \<b AP-2   '

  ON SELECTION POPUP POLAP do Selpop
RETURN


PROCEDURE Selpop
*---------------
 IF LASTKEY() # 27
    STORE BAR() TO choice
    DO CASE
       CASE choice = 1
         DO StandBy WITH 'Opci¢n en desarrollo'
       CASE choice = 2
         DO StandBy WITH 'Opci¢n en desarrollo'
       CASE choice = 3
         DO StandBy WITH 'Opci¢n en desarrollo'
       CASE choice = 4
         DO StandBy WITH 'Opci¢n en desarrollo'
       CASE choice = 5
         DO StandBy WITH 'Opci¢n en desarrollo'
       CASE choice = 6
         DO StandBy WITH 'Opci¢n en desarrollo'
       CASE choice = 7
         DO StandBy WITH 'Opci¢n en desarrollo'
       CASE choice = 8
         DO StandBy WITH 'Opci¢n en desarrollo'
    ENDCASE
 ENDIF
 IF !escolor
    RESTORE SCREEN FROM principal
 ENDIF
 SHOW POPUP menu, pop_03,POLAP
RETURN


PROCEDURE Salida
*---------------
 RELEASE    POPUP  POLAP
 ACTIVATE   SCREEN
 CLOSE DATA
RETURN




    vcol ="w+/w,n/w,W/W,w/W,,w+/b"
    IF Escolor
       DEFINE WINDOW RepAP  FROM 10,52 TO 19,77  SHADOW COLOR &vcol
    ELSE
       DEFINE WINDOW RepAP  FROM 10,52 TO 19,77  COLOR SCHEME c_popup
    ENDIF
    ACTIVATE WINDOW RepAP 
*   xFecFi   = GOMONTH(vmesact,1) - 1
*  
*   SELECT Cuen
*   GO TOP
*   xCuenIn = Cuen.Cuenta
*   GO BOTTOM
*   xCuenFi = Cuen.Cuenta
*   STORE SPACE(06) TO xAuxIn, xAuxFi
*   DO CASE
*      CASE choice = 1
*           xTipo   = 'Todos     '
*           xTipAux = SPACE(2)
*           @ 0, 0 SAY '         Tipo:' GET xTipo    PICTURE '@M Todos     ,Cancelados,Pendientes,Por cuenta,Resumen   '
*           @ 1, 0 SAY 'Tipo Auxiliar:' GET xTipAux  PICTURE '!!'         VALID Val_Tip()
*           @ 2, 0 SAY '  Fecha final:' GET xFecFi   
*           @ 3, 0 SAY 'Auxiliar Inic:' GET xAuxIn   PICTURE '!!!!!!'     VALID Val_Aux(xAuxIn)
*           @ 4, 0 SAY 'Auxiliar Fin.:' GET xAuxFi   PICTURE '!!!!!!'     VALID Val_Aux(xAuxFi) .AND. xAuxIn <= xAuxFi
*           @ 5, 0 SAY ' Cuenta Inic.:' GET xCuenIn  PICTURE '9999999999' VALID Val_Fun('Cuen','Cuenta','Descri',xCuenIn,1);
*                                                    WHEN xTipo = 'Por cuenta'
*           @ 6, 0 SAY ' Cuenta Final:' GET xCuenFi  PICTURE '9999999999' VALID Val_Fun('Cuen','Cuenta','Descri',xCuenFi,1);
*                                                    .AND. xCuenIn <= xCuenFi WHEN xTipo = 'Por cuenta'
*           @ 7, 0 SAY '    Mes y A¤o:' GET xMes     PICTURE '99';
*                                                    VALID xMes > 0 .AND. xMes <= 12
*           @ 7,20 GET xAno                           PICTURE '99'
*           READ
*      
*           IF MONTH(vMesAct) # xMes .AND. VAL(SUBSTR(DTOC(vMesAct),7,2)) # xAno
*              xFecFi = GOMONTH(CTOD('01/'+STR(xMes,2)+'/'+STR(xAno,2)),1) -1
*           ENDIF
       CASE choice = 2
            xbalance = 'Mensual'
            xdigitos = 2
            xCuenta1 = '1000000000'
            xCuenta2 = '9900000000'
            @ 0, 0 SAY '  Genera Mayor:' GET xBalance PICTURE '@M Mensual,Anual  '
            @ 1, 0 SAY '     Mes y A¤o:' GET xMes     PICTURE '99' WHEN xBalance = 'Mensual';
                                                      VALID xMes > 0 .AND. xMes <= 12
            @ 1,20 GET xAno                           PICTURE '99'
            @ 2, 0 SAY 'Rango Cuen.:' GET xCuenta1    PICTURE '9999999999'
            @ 3,13 GET xCuenta2                       PICTURE '9999999999' VALID xCuenta1 <= xCuenta2
            READ
       CASE choice = 3
            xCuenta1 = '1000000000'
            xCuenta2 = '9900000000'
            @ 0, 0 SAY '     Mes y A¤o:' GET xMes     PICTURE '99' VALID xMes > 0 .AND. xMes <= 12
            @ 0,20 GET xAno                           PICTURE '99'
            @ 1, 0 SAY 'Rango Cuen.:' GET xCuenta1    PICTURE '9999999999'
            @ 2,13 GET xCuenta2                       PICTURE '9999999999' VALID xCuenta1 <= xCuenta2
            READ
       CASE choice = 4
            xbalance = 'Mensual'       
            xCuenta1 = '1000000000'
            xCuenta2 = '9900000000'
            @ 0, 0 SAY '  Genera Mayor:' GET xBalance PICTURE '@M Mensual,Anual  '
            @ 1, 0 SAY '     Mes y A¤o:' GET xMes     PICTURE '99' VALID xMes > 0 .AND. xMes <= 12
            @ 1,20 GET xAno                           PICTURE '99'
            @ 2, 0 SAY 'Rango Cuen.:' GET xCuenta1    PICTURE '9999999999'
            @ 3,13 GET xCuenta2                       PICTURE '9999999999' VALID xCuenta1 <= xCuenta2
            READ
    ENDCASE
 ENDIF
 IF LASTKEY() # 27


 DEACTIVATE WINDOW RepAP
 RELEASE WINDOW RepAP


FUNCTION Val_Tip
*---------------
SELECT parma
SEEK 'AUXIL ' + xTipAux
IF !FOUND() .OR. EMPTY(xTipaux)
   SET FILTER TO Tipo = 'AUXIL '
   GO TOP
   x = Val_Fun('parma','Codigo','Descri',xTipAux,1)
   SET FILTER TO
ELSE
   x = .T.
ENDIF
xTipAux = ALLTRIM(xTipAux)
IF x
   SELECT Auxi
   vidx = SYS(3) + '.idx'
   INDEX ON CODIGO TO (vidx) FOR xTipaux = Tipo
   SELECT Auxi
   GO TOP
   xAuxIn = Auxi.Codigo
   GO BOTTOM
   xAuxFi = Auxi.Codigo
   USE AUXIL ORDER TAG AUXIL1 ALIAS Auxi
   ERASE (vidx)
   GO TOP
   @  3, 15 SAY xAuxIn
   @  4, 15 SAY xAuxFi
ENDIF
RETURN x


FUNCTION Val_Aux
*---------------
 PARAMETERS xAux
 SELECT auxi
 SEEK xTipAux + xAux
 IF !FOUND() .OR. EMPTY(xAux)
    xaux = SPACE(6)
    vidx = SYS(3) + '.idx'
    INDEX ON CODIGO TO (vidx) FOR xTipaux = Tipo
    resx = val_fun('auxi','codigo','descri',xaux,1)
    SELECT auxi
    USE AUXIL ORDER TAG AUXIL1 ALIAS Auxi
    ERASE (vidx)
    IF !resx
       RETURN .F.
    ENDIF
 ENDIF
RETURN .T.

