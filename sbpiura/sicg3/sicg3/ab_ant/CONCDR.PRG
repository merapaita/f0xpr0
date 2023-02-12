*--------------------------------------------------------------------------
* VERHJC.Prg
* VERIFICA HOJAS DE AFECTACION
* Estado :
*   '00' Emitida   Este es el que se registra en la Orden de Compra
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: Luis Castillo Dur n
* FECHA: 12/10/94
*--------------------------------------------------------------------------
USE CdrNec   IN 1   order tag CdrNec1  ALIAS Cuadro
USE IteCn    IN 2   order tag IteCn2   ALIAS IteCn
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE Iteart   IN 5   order tag Iteart1  ALIAS iteart
*- Mensajes de aviso al usuario

IF EOF()
   DO standby WITH 'No Existe registros a Procesar'
   CLOSE DATA
   RETURN
ENDIF
 sele iteCN
 vtemp    = RECNO()
 as=order()
 vPeriodo = RIGHT(STR(YEAR(DATE()),4),2)

 DEFINE WINDOW Lis FROM 10,12 TO 14,68 DOUBLE ;
 TITLE ' °° Consolidaci¢n °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,12 say '        Periodo : '

 @  1,31  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)

 READ &&VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF EMPTY(vPeriodo) .or. LASTKEY()=27
    CLOSE DATA
    RETURN
 ELSE
    SET FILT TO PERIODO=VPERIODO
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
       vTitulo = "Per¡odo 19"+alltrim(vPeriodo)
      *DO REPORTE WITH 2,"ConCdr",' Consolidaci¢n Cuadro de Necesidades '
       DO REPORTE WITH 2,"ConCdr1",' Consolidaci¢n Cuadro de Necesidades '
    ENDIF
 ENDIF
SELE ITECN
SET FILT TO
CLOSE DATA
RETURN
