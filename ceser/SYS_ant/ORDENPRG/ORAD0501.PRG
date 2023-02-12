*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<ORAD0501>'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
@ 02, 1 SAY DATE()
DO saycenter WITH 1,  ;
   ' ADMINISTRACION '
DO saycenter WITH 2,  ;
   ' ORDENADOR DE ARCHIVOS '
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'ESP', 'BBB', 'ESC'
CLOSE DATABASES
SELECT 1
USE EXCLUSIVE campofox
REPLACE codcla WITH '  ' ALL
cam = 'codcla + subst(campo1,1,40)'
men = 'Ayuda de Archivos'
tot_act = 0
tot_por = 0
act_reg = 0
tot_reg = 0
DO ayuda3 WITH cam, men, 'codcla'
IF LASTKEY() <> 27
     REPLACE codcla WITH SPACE(1) +  ;
             CHR(255) ALL FOR  ;
             codcla == 'û '
     SET FILTER TO SUBSTR(codcla, 2, 1);
== CHR(255)
     GOTO TOP
     DO WHILE  .NOT. EOF()
          arch = SUBSTR(campo1, 1,  ;
                 12)
          wk_campo2 = campo2
          SELECT 2
          use &arch
          tot_reg = tot_reg +  ;
                    (RECCOUNT() *  ;
                    wk_campo2) +  ;
                    (2 *  ;
                    wk_campo2)
          USE
          SELECT 1
          SKIP
     ENDDO
ENDIF
IF tot_reg <> 0 .AND. LASTKEY() <>  ;
   27
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     define popup ayu0 from 0,35 to 06,79;
promp field &cam shadow title 'Archivos Seleccionados';
in window trabajo COLOR SCHEME 8
     ON SELECTION POPUP ayu0 do porm0511
     ACTIVATE POPUP ayu0 NOWAIT
     FOR i = 1 TO 4
          MOVE POPUP ayu0 BY 1, 0
     ENDFOR
     FOR i = 1 TO 16
          MOVE POPUP ayu0 BY 0, - ;
               2
     ENDFOR
     MOVE POPUP ayu0 BY 0, -2
     DEFINE WINDOW ayu1 FROM 4,  ;
            49 TO 10, 72 SHADOW  ;
            IN trabajo COLOR  ;
            SCHEME 8
     ACTIVATE WINDOW ayu1
     @ 00, 2 SAY 'Inicio  : ' +  ;
       TIME()
     @ 02, 2 SAY 'T‚rmino : '
     @ 04, 2 SAY  ;
       '% Total : 000.00'
     SET HOURS TO 24
     SET CLOCK TO 9, 63
     ACTIVATE WINDOW trabajo
     @ 12, 10 CLEAR TO 14, 62
     @ 12, 10 TO 14, 62
     @ 13, 7 SAY '0%'
     @ 13, 64 SAY '100%'
     tot_act = 0
     tot_por = 0
     act_reg = 0
     KEYBOARD CHR(13)
     ACTIVATE POPUP ayu0
     ACTIVATE WINDOW TOP ayu1
     @ 04, 12 SAY 100 PICTURE  ;
       '999.99'
     ACTIVATE WINDOW trabajo
     @ 13, 11 SAY REPLICATE('°',  ;
       50)
     DEACTIVATE POPUP ayu0
     DEACTIVATE WINDOW ayu1
ENDIF
CLOSE DATABASES
wk_aux = INKEY()
DO WHILE wk_aux<>0
     wk_aux = INKEY()
ENDDO
SET CLOCK OFF
DO sacawin
RETURN
*
PROCEDURE porm0511
IF LASTKEY() <> 13
     RETURN
ENDIF
IF tot_act >= tot_reg .OR.  ;
   act_reg >= RECNO()
     DEACTIVATE POPUP ayu0
     RETURN
ENDIF
act_reg = RECNO()
DO CASE
     CASE RECNO() == 2
          SELECT 2
          USE EXCLUSIVE st_iclpr
          PACK
          DELETE TAG all
          INDEX ON indent +  ;
                codent TAG  ;
                codigo
          INDEX ON indent +  ;
                noment TAG  ;
                cli_noment
          INDEX ON noment TAG  ;
                noment
          INDEX ON codent TAG  ;
                codent
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 3
          SELECT 2
          USE EXCLUSIVE st_imode
          PACK
          DELETE TAG all
          INDEX ON codmar +  ;
                codmod TAG  ;
                codigo
          INDEX ON nommod TAG  ;
                mod_nommod
          INDEX ON linea + codmar +  ;
                codmod TAG linea
          INDEX ON codcla +  ;
                codmar + codmod  ;
                TAG clase
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 4
          SELECT 2
          USE EXCLUSIVE st_isrep
          PACK
          DELETE TAG all
          INDEX ON numdoc TAG  ;
                codigo
          INDEX ON codent +  ;
                dtoc2(fecemi) TAG  ;
                sol_codent
          INDEX ON dtoc2(fecemi)  ;
                TAG sol_fchemi
          INDEX ON codmar +  ;
                codmod +  ;
                dtoc2(fecemi) TAG  ;
                sol_marmod
          INDEX ON codstk +  ;
                numstk TAG  ;
                sol_codstk
          INDEX ON numser TAG  ;
                sol_serie
          INDEX ON codemi +  ;
                indori TAG  ;
                sol_emisor
          INDEX ON codent TAG  ;
                codent
          INDEX ON codemi +  ;
                codmod +  ;
                DTOC(fecemi) TAG  ;
                emimod
          INDEX ON codmar +  ;
                codmod + numser  ;
                TAG sol_mamose
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 5
     CASE RECNO() == 6
     CASE RECNO() == 7
          SELECT 2
          USE EXCLUSIVE st_iseri
          PACK
          DELETE TAG all
          INDEX ON codent +  ;
                codmar +  ;
                DTOC(fecing) TAG  ;
                codigo
          INDEX ON codmar +  ;
                modelo + numser  ;
                TAG ser_codmar
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 8
          SELECT 2
          USE EXCLUSIVE st_itecn
          PACK
          DELETE TAG all
          INDEX ON codent TAG  ;
                codigo
          INDEX ON noment TAG  ;
                tec_nomtec
          INDEX ON codtec TAG  ;
                tec_codtec
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 9
          SELECT 2
          USE EXCLUSIVE st_iorep
          PACK
          DELETE TAG all
          INDEX ON numdoc TAG  ;
                codigo
          INDEX ON codent +  ;
                dtoc2(fecemi) TAG  ;
                ord_codent
          INDEX ON dtoc2(fecemi)  ;
                TAG ord_fchemi
          INDEX ON codmar +  ;
                codmod +  ;
                dtoc2(fecemi) TAG  ;
                ord_codmar
          INDEX ON numsol TAG  ;
                ord_numsol
          INDEX ON numser TAG  ;
                ord_numser
          INDEX ON codent TAG  ;
                codent
          INDEX ON codtec +  ;
                DTOS(fecemi) +  ;
                numdoc TAG  ;
                ord_tecn
          INDEX ON DTOS(fecemi) +  ;
                numdoc TAG  ;
                ord_fecdoc
          INDEX ON auxest +  ;
                codemi +  ;
                DTOS(fecest) +  ;
                numdoc TAG  ;
                ord_esem
          INDEX ON codfabo +  ;
                numfabo + numdoc  ;
                TAG ord_numfab
          INDEX ON indori +  ;
                indest +  ;
                DTOS(fecemi) +  ;
                numdoc TAG  ;
                ord_inesta
          INDEX ON codtec +  ;
                auxest TAG  ;
                ord_tecest
          INDEX ON DTOS(fecfin) +  ;
                indori TAG  ;
                ord_fecind
          INDEX ON codmar +  ;
                codmod + numser  ;
                TAG ord_mamose
          INDEX ON DTOS(fecfabo)  ;
                TAG ord_fecfac
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 10
          SELECT 2
          USE EXCLUSIVE st_iredo
          PACK
          DELETE TAG all
          INDEX ON indodo +  ;
                numodo TAG  ;
                codigo
          INDEX ON indddo +  ;
                numddo TAG  ;
                rel_indica
          INDEX ON indodo +  ;
                numodo + indddo +  ;
                numddo TAG  ;
                ire_indica
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 11
          SELECT 2
          USE EXCLUSIVE st_ispre
          PACK
          DELETE TAG all
          INDEX ON numdoc TAG  ;
                codigo
          INDEX ON codent +  ;
                dtoc2(fecemi) TAG  ;
                pre_codent
          INDEX ON dtoc2(fecemi)  ;
                TAG pre_fecemi
          INDEX ON codmar +  ;
                codmod +  ;
                dtoc2(fecemi) TAG  ;
                pre_codmar
          INDEX ON numsol TAG  ;
                st_numsol
          INDEX ON numser TAG  ;
                st_numser
          INDEX ON indest +  ;
                numdoc TAG  ;
                pre_infenu
          INDEX ON numord TAG  ;
                pre_numord
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 12
          SELECT 2
          USE EXCLUSIVE st_idpre
          PACK
          DELETE TAG all
          INDEX ON numdoc +  ;
                numord + codpro  ;
                TAG codigo
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 13
     CASE RECNO() == 14
          SELECT 2
          USE EXCLUSIVE st_iprep
          PACK
          DELETE TAG all
          INDEX ON numdoc TAG  ;
                codigo
          INDEX ON numord TAG  ;
                rep_numord
          INDEX ON DTOS(fecemi) +  ;
                numdoc TAG  ;
                rep_fecnum
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 15
     CASE RECNO() == 16
          SELECT 2
          USE EXCLUSIVE st_estad
          PACK
          DELETE TAG all
          INDEX ON anorep TAG  ;
                codigo
          INDEX ON numord TAG  ;
                est_numord
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 17
     CASE RECNO() == 18
          SELECT 2
          USE EXCLUSIVE st_idped
          PACK
          DELETE TAG all
          INDEX ON numdoc +  ;
                numord + codpro  ;
                TAG codigo
          INDEX ON numord TAG  ;
                dre_numord
          INDEX ON DTOS(fecemi) +  ;
                numdoc TAG  ;
                rep_fecnum
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 19
     CASE RECNO() == 20
          SELECT 2
          USE EXCLUSIVE st_mvord
          PACK
          DELETE TAG all
          INDEX ON DTOC(dia) +  ;
                hora + orden TAG  ;
                codigo
          INDEX ON orden TAG  ;
                eor_nroord
          INDEX ON orden + estado  ;
                TAG estado
          INDEX ON orden + estado +  ;
                DTOS(dia) + dia  ;
                TAG ordia
          INDEX ON estado +  ;
                DTOS(dia) + orden  ;
                TAG mvo_tecnic
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 21
          SELECT 2
          USE EXCLUSIVE st_iscic
          PACK
          DELETE TAG all
          INDEX ON numsol TAG  ;
                numsol
          INDEX ON numord TAG  ;
                numord
          INDEX ON numsol +  ;
                DTOS(fecini) TAG  ;
                fecsol
          INDEX ON DTOS(feccom) +  ;
                user TAG fecuse
          INDEX ON DTOS(fecini) +  ;
                user TAG fecini
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 22
          SELECT 2
          USE EXCLUSIVE st_asiem
          PACK
          DELETE TAG all
          INDEX ON codemp + anio  ;
                TAG asi_cod
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 23
          SELECT 2
          USE EXCLUSIVE st_sint
          PACK
          DELETE TAG all
          INDEX ON codsin TAG  ;
                codigo
          INDEX ON dessin +  ;
                codsin TAG  ;
                dessin
          INDEX ON linea + codsin  ;
                TAG sin_lincod
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 24
          SELECT 2
          USE EXCLUSIVE st_mobra
          PACK
          DELETE TAG all
          INDEX ON mo_codmar +  ;
                mo_codart TAG  ;
                codigo
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 25
          SELECT 2
          USE EXCLUSIVE st_users
          PACK
          DELETE TAG all
          INDEX ON codemp +  ;
                estado TAG  ;
                codigo
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 26
          SELECT 2
          USE EXCLUSIVE st_movca
          PACK
          DELETE TAG all
          INDEX ON numord +  ;
                codcau TAG  ;
                codigo
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 27
          SELECT 2
          USE EXCLUSIVE st_movso
          PACK
          DELETE TAG all
          INDEX ON numord +  ;
                codsol TAG  ;
                codigo
          INDEX ON numsol TAG  ;
                numsol
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 28
          SELECT 2
          USE EXCLUSIVE st_sicli
          PACK
          DELETE TAG all
          INDEX ON numdoc +  ;
                codsin TAG  ;
                codigo
          INDEX ON codsin TAG  ;
                codsin
          INDEX ON DTOS(date) +  ;
                time TAG fecha
     CASE RECNO() == 29
          SELECT 2
          USE EXCLUSIVE st_idfac
          PACK
          DELETE TAG all
          INDEX ON tipdoc +  ;
                nrodoc TAG  ;
                codigo
          INDEX ON DTOS(fechad) +  ;
                codemi TAG  ;
                fecemi
          INDEX ON DTOS(date) +  ;
                time TAG fecha
ENDCASE
SELECT 2
USE
SELECT 1
REPLACE codcla WITH 'û' +  ;
        CHR(255)
KEYBOARD '{DNARROW}{ENTER}'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
