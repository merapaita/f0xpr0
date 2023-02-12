*---------------------------------------
* Crea_Win.Prg
*
*---------------------------------------
DEFINE WINDOW oculta FROM 00, 00 TO 24, 79 NONE COLOR SCHEME 8
ACTIVATE WINDOW oculta
@ 00, 00, 24, 79 BOX '²²²²²²²²²'
DIMENSION a( 02)
a( 01) = empre1 + SPACE(75 - LEN(empre1) - LEN(chisof1)) + chisof1
a( 02) = empre2 + SPACE(75 - LEN(empre2) - LEN(wrk_progra)) + wrk_progra
DEFINE WINDOW indicar FROM 21, 0 TO 24, 1 SHADOW
DEFINE WINDOW trabajo FROM 23, 78 TO 24, 79 SHADOW
SET BORDER TO NONE
DEFINE POPUP ayu1 FROM 0, 0 TO 2, 2 IN trabajo
SET BORDER TO
FOR i = 1 TO 02
     DEFINE BAR i OF ayu1 PROMPT a(i)
ENDFOR
ACTIVATE WINDOW trabajo
ACTIVATE POPUP ayu1 NOWAIT
FOR j = 1 TO 11
     ZOOM WINDOW trabajo NORM FROM 23, 78 - (j * 7) TO 24, 79 - (j * 7)
ENDFOR
FOR i = 1 TO 11
     ZOOM WINDOW trabajo NORM FROM 23 - (i * 2), 0 TO 24 - (i * 2), 1
ENDFOR
MOVE POPUP ayu1 BY -1, 0
FOR i = 1 TO 17
     ZOOM WINDOW trabajo NORM FROM 1, 0 TO i, 1
     IF i < 3
          SIZE POPUP ayu1 BY 1, 0
     ENDIF
ENDFOR
FOR i = 1 TO 19
     ZOOM WINDOW trabajo NORM FROM 1, 0 TO 17, i * 4
     SIZE POPUP ayu1 BY 0, 4
ENDFOR
DEACTIVATE POPUP ayu1
SHOW POPUP ayu1
RELEASE POPUP ayu1
RELEASE BAR ALL OF ayu1
ACTIVATE WINDOW indicar
FOR i = 1 TO 38
     ZOOM WINDOW indicar NORM FROM 21, 0 TO 24, i * 2
ENDFOR
ACTIVATE WINDOW trabajo
RETURN
