*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER w_selpro, w_codusu,  ;
          w_varread
ON KEY
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 17
DEFINE WINDOW produ FROM 12, 18  ;
       TO 19, 73 IN screen COLOR  ;
       SCHEME 17
DEFINE POPUP prod FROM 16, 31  ;
       COLOR SCHEME 17
DEFINE BAR 1 OF prod PROMPT  ;
       '\<C¢digo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripci¢n '
ON SELECTION POPUP prod do bususu with;
bar(),w_selpro
DEFINE POPUP produ FROM 15, 18 TO  ;
       19, 73 PROMPT FIELDS  ;
       codigo + '³' + nombre IN  ;
       screen
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
RELEASE WINDOW pide, produ
ON KEY LABEL f6 do usuario with w_selpro,w_codusu,w_varread
IF LASTKEY() <> 27
     ON KEY
     w_codusu = password.codigo
ENDIF
RETURN
*
PROCEDURE bususu
PARAMETER bar, w_selpro
ON KEY
FOR w_cont = 1 TO 26
     MOVE POPUP prod BY 0, -1
ENDFOR
FOR w_cont = 1 TO 07
     MOVE POPUP prod BY -1, 0
ENDFOR
ACTIVATE WINDOW pide
SELECT (w_selpro)
IF bar = 1
     w_codusu = SPACE(10)
     SET ORDER TO usuario
     @ 00, 00 SAY 'C¢digo :'
     @ 00, 09 GET w_codusu  ;
       PICTURE '@!'
ENDIF
IF bar = 2
     w_codusu = SPACE(30)
     SET ORDER TO nombre
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET w_codusu  ;
       PICTURE '@!'
ENDIF
READ
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK w_codusu
     SET NEAR OFF
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do tomacod4
     BROWSE FIELDS codigo :R :H =  ;
            'C¢digo', nombre :R :  ;
            30 :H = 'Nombre'  ;
            FREEZE codigo IN  ;
            produ
ENDIF
ON KEY
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR w_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR w_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
RETURN
*
PROCEDURE tomacod4
ON KEY
w_codusu = password.codigo
KEYBOARD '{ENTER}'
IF w_varread = 'W_USUINI'
     w_usuini = w_codusu
ELSE
     w_usufin = w_codusu
ENDIF
DEACTIVATE WINDOW pide, produ
DEACTIVATE POPUP prod
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
