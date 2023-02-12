*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE POPUP archivos FROM 09, 05  ;
       TO 14, 25 PROMPT FIELDS  ;
       status + '³ ' + nombre IN  ;
       screen COLOR SCHEME 7
ON SELECTION POPUP archivos DEAC POPUP;
ARCHIVOS
DEFINE WINDOW unidad FROM 11, 02  ;
       TO 13, 28 IN screen COLOR  ;
       SCHEME 20
DEFINE WINDOW titpass FROM 08, 30  ;
       TO 10, 50 IN screen NONE  ;
       COLOR SCHEME 20
arch1 = f_archivo()
NUM1=ADIR(ARCHIVO1,'&MRUTA\*.DBF','A')
CREATE CURSOR &ARCH1 (NOMBRE C(12), STATUS;
C(1))
FOR x = 1 TO num1
     APPEND BLANK
     REPLACE nombre WITH  ;
             archivo1(x,1)
ENDFOR
INDEX ON nombre TAG nombre
DO p_footer WITH  ;
   '101000000000000000011', 1
ACTIVATE POPUP archivos NOWAIT
FOR count = 1 TO 3
     SIZE POPUP archivos BY 1, 0
ENDFOR
FOR count = 1 TO 5
     SIZE POPUP archivos BY 0, 1
ENDFOR
FOR count = 1 TO 5
     SIZE POPUP archivos BY 0, -1
ENDFOR
FOR count = 1 TO 25
     MOVE POPUP archivos BY 0, 1
ENDFOR
FOR count = 1 TO 4
     MOVE POPUP archivos BY 1, 0
ENDFOR
FOR count = 1 TO 2
     MOVE POPUP archivos BY -1, 0
ENDFOR
ACTIVATE WINDOW titpass
@ 0, 0 SAY  ;
  'ÚÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 1, 0 SAY  ;
  '³X³    ARCHIVOS     ³'
@ 2, 0 SAY  ;
  'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
ON KEY LABEL ENTER DO MARCA 
ON KEY LABEL F3 DO ASCII 
ACTIVATE POPUP archivos
DEACTIVATE POPUP archivos
DEACTIVATE WINDOW titpass
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001001100001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE marca
IF status = SPACE(1)
     REPLACE status WITH 'X'
ELSE
     REPLACE status WITH ' '
ENDIF
RETURN
*
PROCEDURE ascii
ON KEY
SELECT 1
GOTO TOP
flag = .F.
DO WHILE  .NOT. EOF()
     IF status = 'X'
          flag = .T.
     ENDIF
     SKIP
ENDDO
IF flag
     ACTIVATE WINDOW unidad
     SET CURSOR ON
     unidad = 'A'
     @ 0, 0 SAY  ;
       ' DESTINO (A: ¢ B:) : '  ;
       GET unidad PICTURE 'A'  ;
       VALID unidad $ 'ABC'
     READ
     IF LASTKEY() <> 27
          DEFINE WINDOW genera  ;
                 FROM 11, 20 TO  ;
                 14, 60 IN screen  ;
                 COLOR SCHEME 20
          ACTIVATE WINDOW genera
          SET COLOR TO W+/N*
          @ 01, 03 SAY  ;
            ' GENERANDO ARCHIVOS ASCII EN ' +  ;
            unidad + ' :'
          SELECT 1
          GOTO TOP
          DO WHILE  .NOT. EOF()
               IF status = 'X'
                    pos = AT('.',  ;
                          nombre)
                    aux_file = SUBSTR(nombre,  ;
                               1,  ;
                               (pos -  ;
                               1))
                    SELECT 2
                    USE &AUX_FILE
                    aux_file = aux_file +  ;
                               '.TXT'
                    COPY TO  ;
                         unidad +  ;
                         ':' +  ;
                         aux_file  ;
                         TYPE  ;
                         DELIMITED
                    USE
                    SELECT 1
               ENDIF
               SKIP
          ENDDO
          DEACTIVATE WINDOW  ;
                     genera
          DO p_mensaje WITH  ;
             'ARCHIVOS GENERADOS...PRESIONE <Enter> PARA CONTINUAR'
     ENDIF
     DEACTIVATE WINDOW unidad
     DEACTIVATE POPUP archivos
ELSE
     DO p_mensaje WITH  ;
        'VERIFIQUE QUE LOS ARCHIVOS ESTEN MARCADOS'
     ON KEY LABEL ENTER DO MARCA
     ON KEY LABEL F3 DO ASCII
ENDIF
DEACTIVATE WINDOW unidad
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
