*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
set step on
CLOSE ALL
RESTORE FROM ruta.mem ADDITIVE
DO direccion
SET PROCEDURE TO rget0000
CLEAR
DEFINE WINDOW pie FROM 21, 0 TO 24, 79 IN screen
PUBLIC clave, xnnn, teclas, direc,tipo, teclos, wrk_tiper
PUBLIC rge_razsoc, rge_abrev, rge_calle, rge_distri,  ;
       rge_punemi, rge_provin,rge_igv, rge_intmen,  ;
       rge_monbas, rge_ctamax,rge_poracc, rge_porveh,  ;
       rge_porser, rge_codalm,rge_lptfac, rge_lptgui,  ;
       rge_lptped, rge_lptbol,rge_lptdes, wrk_busca,  ;
       wrkselec, wrk_retor,   wrk_var, wrk_campo,  ;
       uni_facto1, uni_facto2,rge_lispre, rge_pormax,  ;
       rge_vicodi, sys_simmon,sys_desmon, sys_codimp,  ;
       sys_simomb, rge_cosimp,rge_numruc
STORE SPACE(14) TO wrk_campo, wrk_tiper
STORE 1 TO gc_tipo
DIMENSION tecla1( 26, 2)
DIMENSION tecla2( 26, 2)
DO p_inicia
USE SHARED gc_par00
DO p_menpar
USE SHARED st_iparg
sys_desmon = sys_desmon
sys_codimp = sys_codimp
sys_simmon = sys_simmon
sys_simomb = sys_simomb
USE
clave = SPACE(10)
mono = 'W/N,N/W,N'
DO p_tecdedic
DO p_tecdedid
DO p_objetos
ret = .F.
DO ingreso
DEACTIVATE WINDOW ALL
= f_fondo1('    Repuestos ', 'W+/N')
DO salida
= f_cierra()
SET COLOR TO W/N
RETURN
*
PROCEDURE p_objetos
SET COLOR OF SCHEME 5 TO N/W, N/W, N/W,W+/W, N/W, W/N, W+/W, N+/N, N/W, W+/W
SET COLOR OF SCHEME 6 TO N/W, N/W, N/W,W+/W, N/W, W/N, W+/W, N+/N, N/W, W+/W
SET COLOR OF SCHEME 7 TO W+/N, W+/N, W/N,W/N, N/W, N+/W, W+/N, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 8 TO N/W, N/W, N/W,N/W, W/N, W/N, N/W, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 9 TO W+/N, W+/N, W/N,W+/N, W/N, N/W, W/N, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 10 TO W+/N, W/N, N/W,W+/N, W+/N, N/W, N/W, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 12 TO W+/N, W+/N,W+/N, W+/N, W/N, N+/W, N+/W, N+/N, W+/N,W+/N
SET COLOR OF SCHEME 15 TO N/W, N/W, N/W,N/W, W/N, W/N, N/W, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 16 TO N/W, N/W, N/W,N/W, W/N, W/N, N/W, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 17 TO N/W, N/W, N/W,N/W, W/N, W/N, N/W, N+/N, W+/N, N+/N
SET COLOR OF SCHEME 18 TO W/N, W/N, W/N,W/N, W+/N, N/W, W/N, W+/N, N/W
SET COLOR OF SCHEME 19 TO W+/N, W+/N,N+/W, W+/N, W+/N, N/W, W/N, W+/N, N/W
IF ISCOLOR()
     SET COLOR OF SCHEME 20 TO W+/N, W+/N,W+/N, W+/N, W+/N, N/W, W/N, N+/W, N+/W,W/N
     SET COLOR OF SCHEME 23 TO W+/N, W+/N,GR/N, R+/W, W+/N, W+/N, N+/W, W+/N, N+/W,W+/N
     SET COLOR OF SCHEME 24 TO W/N, N/W,B/N, W/N, R+/N, W+/N, R+/W, N+/N, GR+/B,W+/N
ELSE
     SET COLOR OF SCHEME 20 TO W+/N, W+/N,W+/N, W+/N, W+/N, N/W, W/N, N+/W, N+/W,W/N
     SET COLOR OF SCHEME 23 TO W+/N, N+/W,N+/W, N+/W, W+/N, W+/N, N+/W, W+/N, N+/W,W+/N
     SET COLOR OF SCHEME 24 TO W/N, N/W,W+/N, W+/N, W+/N, W+/N, N+/W, W+/N, N+/W,W+/N
ENDIF
DEFINE WINDOW titulo FROM 0, 0 TO 3, 79 IN screen NONE COLOR SCHEME 8
DEFINE WINDOW titulo FROM 0, 0 TO 3, 79 IN screen NONE COLOR SCHEME 8
DEFINE WINDOW tablas FROM 00, 00 TO 24, 79 IN screen DOUBLE
DEFINE WINDOW footer FROM 22, 01 TO 23, 78 IN screen NONE
DEFINE WINDOW titmenu FROM 06, 09 TO 09, 66 IN screen NONE COLOR SCHEME 20
DEFINE WINDOW marco FROM 07, 15 TO 19, 69 DOUBLE
DEFINE WINDOW busqueda FROM 08, 17 TO 18, 68 NONE
DEFINE WINDOW pantall FROM 0, 0 TO 23, 160 FLOAT ZOOM COLOR SCHEME 24
DEFINE MENU menup COLOR SCHEME 19
DEFINE PAD tabpad OF menup PROMPT '\<ORDENES     ' AT 09, 08
DEFINE PAD ordpad OF menup PROMPT '\<REPUESTOS' AT 09, 31
ON SELECTION PAD tabpad OF menup do ordenes
ON SELECTION PAD ordpad OF menup do repuesto
DEFINE WINDOW tablas FROM 00, 00  ;
       TO 24, 79 IN screen DOUBLE  ;
       COLOR SCHEME 7
DEFINE POPUP busca FROM 12, 27 TO  ;
       18, 53 PROMPT FIELDS ' ' +  ;
       tab_codtab + ' ?' +  ;
       tab_destab IN screen COLOR  ;
       SCHEME 7
ON SELECTION POPUP busca deac popup busca
DEFINE POPUP vende FROM 12, 27 TO  ;
       18, 53 PROMPT FIELDS ' ' +  ;
       vnd_code + ' ?' +  ;
       vnd_nombre IN screen COLOR  ;
       SCHEME 7
ON SELECTION POPUP vende deac popup vende
DEFINE POPUP clien FROM 12, 27 TO  ;
       18, 53 PROMPT FIELDS ' ' +  ;
       cli_codigo + ' ?' +  ;
       cli_razsoc IN screen COLOR  ;
       SCHEME 7
ON SELECTION POPUP clien deac popup clien
DEFINE POPUP hlp FROM 12, 27 TO  ;
       18, 53 PROMPT FIELDS ' ' +  ;
       hlp_codlis + ' ?' +  ;
       hlp_deslis IN screen COLOR  ;
       SCHEME 7
ON SELECTION POPUP hlp deac popup hlp
DEFINE POPUP uni FROM 12, 27 TO  ;
       18, 54 PROMPT FIELDS ' ' +  ;
       uni_unialt + ' ?' +  ;
       STR(uni_facto1, 5, 2) +  ;
       ' ?' + STR(uni_facto2, 5,  ;
       2) + ' ? ' + pro_unimed IN  ;
       screen COLOR SCHEME 7
ON SELECTION POPUP uni deac popup uni
DEFINE POPUP gas FROM 12, 26 TO  ;
       18, 54 PROMPT FIELDS ' ' +  ;
       descrip + ' ?' +  ;
       STR(importe, 11, 2) IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP gas deac popup gas
DEFINE POPUP man FROM 12, 16 TO  ;
       18, 54 PROMPT FIELDS ' ' +  ;
       descrip + ' ?' +  ;
       STR(importe, 11, 2) IN  ;
       screen COLOR SCHEME 7
ON SELECTION POPUP man deac popup man
RETURN
*
PROCEDURE p_inicia
CLEAR MACROS
SET ECHO OFF
SET TALK OFF
SET CLOCK OFF
SET DELETED ON
SET STATUS OFF
SET SAFETY OFF
SET ESCAPE OFF
SET NEAR OFF
SET SYSMENU ON
SET CURSOR OFF
SET SCOREBOARD OFF
SET NOTIFY OFF
SET EXCLUSIVE OFF
SET MESSAGE TO 24 CENTER
SET REPROCESS TO -1
SET REFRESH TO 2
SET CENTURY ON
SET DATE french
IF ISCOLOR()
     SET COLOR TO W/N, N/W, N
ELSE
     SET COLOR TO W/N, N/W, N
ENDIF
ON KEY LABEL f11 do p_calculadora
RETURN
*
PROCEDURE p_salir
PARAMETER bar
IF bar = 2
     RETURN TO MASTER
ENDIF
SET CLOCK OFF
DEACTIVATE POPUP menu06
RETURN
*
PROCEDURE salida
CLOSE DATABASES
susp
DEACTIVATE WINDOW ALL
DEACTIVATE MENU ALL
DEACTIVATE POPUP ALL
fin1 = 'Gesti?n Comercial'
DEFINE WINDOW cid FROM 10, 10 TO  ;
       14, 70 IN screen DOUBLE  ;
       COLOR SCHEME 12
ACTIVATE WINDOW cid
@ 0, (WCOLS() - LEN(fin1)) / 2  ;
  SAY fin1
@ 1, (WCOLS() - LEN(rge_razsoc)) /  ;
  2 SAY rge_razsoc
@ 2, (WCOLS() - LEN(rge_abrev)) /  ;
  2 SAY rge_abrev
ACTIVATE SCREEN
= INKEY(3)
RELEASE WINDOW cid
RETURN
*
PROCEDURE direccion
set path to &mruta
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
