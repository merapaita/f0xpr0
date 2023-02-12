*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
STORE 0 TO t, tt
SET TALK OFF
rpt = f_yesno( ;
      'Desea procesar ? ')
IF  .NOT. rpt
     RETURN
ENDIF
DO p_mensaje WITH  ;
   'Procesando informaci¢n, espere un momento.... '
SET DATE BRIT
CLOSE DATABASES
SELECT 1
USE SHARED GC_PRO00
SET ORDER TO 1
SELECT 2
USE SHARED GC_KAR00
SET ORDER TO KAR_profec
SELECT 3
USE SHARED GC_NVE00
SET ORDER TO 1
GOTO TOP
SELECT 4
USE SHARED GE_TAB0
SET ORDER TO 1
SELECT 1
REPLACE pro_nivcit WITH SPACE(1)  ;
        ALL
PUBLIC i2a1, f2a1, i2a2, f2a2, f1,  ;
       wrk_prom2, wrk_prom3
f1 = GOMONTH(DATE(), 1)
f2 = f1 - DAY(f1)
i1 = (GOMONTH(DATE(), -5))
i2 = i1 - (DAY(i1) - 1)
f1a1 = GOMONTH(DATE(), 1)
f2a1 = f1a1 - DAY(f1a1)
i1a1 = (GOMONTH(DATE(), -11))
i2a1 = i1a1 - (DAY(i1a1) - 1)
f1a2 = GOMONTH(DATE(), 1)
f2a2 = f1a2 - DAY(f1a2)
i1a2 = (GOMONTH(DATE(), -23))
i2a2 = i1a2 - (DAY(i1a2) - 1)
GOTO TOP
DO WHILE  .NOT. EOF()
     SELECT 1
     IF  .NOT. EMPTY(pro_codpro)
          STORE pro_codpro TO  ;
                wrk_codigo
          SKIP
          DO promedio
     ENDIF
ENDDO
SELECT 2
SET ORDER TO kar_profec
SELECT 1
GOTO TOP
SCAN WHILE DATE() > {01/01/1996}  ;
     .AND.  .NOT. EOF()
     pro = pro_codpro
     SELECT 2
     SEEK pro
     IF FOUND()
          IF gc_kar00.kar_fecdoc <  ;
             i2a1
               SELECT 1
               REPLACE pro_nivcit  ;
                       WITH 'E'
               REPLACE pro_hora  ;
                       WITH  ;
                       TIME(),  ;
                       pro_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE pro_usuari  ;
                       WITH  ;
                       clave
          ENDIF
          IF gc_kar00.kar_fecdoc <  ;
             i2a2 .AND. date >  ;
             {01/01/1997}
               SELECT 1
               REPLACE pro_nivcit  ;
                       WITH 'F'
               REPLACE pro_hora  ;
                       WITH  ;
                       TIME(),  ;
                       pro_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE pro_usuari  ;
                       WITH  ;
                       clave
          ENDIF
     ENDIF
     SELECT 1
ENDSCAN
DO p_mensaje WITH  ;
   'Procesando Nivel de Criticidad..... '
SELECT 4
SEEK 'NICR' + 'D'
var1 = tab_factor
SEEK 'NICR' + 'C'
var2 = tab_factor
SEEK 'NICR' + 'B'
var3 = tab_factor
SEEK 'NICR' + 'A'
var4 = tab_factor
SEEK 'NICR' + 'S'
var5 = tab_factor
SEEK 'NICR' + 'E'
var6 = tab_factor
SELECT 3
GOTO TOP
DO WHILE  .NOT. EOF(3)
     STORE nve_codpro TO  ;
           wrk_codigo
     STORE ' ' TO nivel
     STORE 0 TO cantprome
     STORE (nve_cmes1 + nve_cmes2 +  ;
           nve_cmes3 + nve_cmes4 +  ;
           nve_cmes5 + nve_cmes6) /  ;
           6 TO cantprome
     DO CASE
          CASE cantprome = var1
               STORE 'D' TO nivel
          CASE cantprome >= var2  ;
               .AND. cantprome <  ;
               var3
               STORE 'C' TO nivel
          CASE cantprome >= var3  ;
               .AND. cantprome <  ;
               var4
               STORE 'B' TO nivel
          CASE cantprome >= var4  ;
               .AND. cantprome <  ;
               var5
               STORE 'A' TO nivel
          CASE cantprome >= var5
               STORE 'S' TO nivel
     ENDCASE
     SELECT 1
     SEEK (wrk_codigo)
     IF (pro_nivcit <> 'E' .AND.  ;
        pro_nivcit <> 'F')
          REPLACE pro_nivcit WITH  ;
                  nivel
     ENDIF
     IF (pro_rcom <> 'Z' .AND.  ;
        pro_rcom <> 'Y' .AND.  ;
        pro_rcom <> 'H')
          REPLACE pro_rcom WITH  ;
                  pro_nivcit
     ENDIF
     SELECT 3
     IF  .NOT. EOF()
          SKIP
          LOOP
     ELSE
          EXIT
     ENDIF
ENDDO
RELEASE WINDOW mensj
?? CHR(7)
?? CHR(7)
RETURN
*
PROCEDURE promedio
FOR x = 0 TO 5
     f11 = GOMONTH(f1, -x)
     f22 = f11 - DAY(f11)
     i11 = (GOMONTH(f1, -(x +  ;
           1)))
     i22 = i11 - (DAY(i11) - 1)
     SELECT 2
     SET FILTER TO kar_codpro = wrk_codigo;
.AND. (kar_fecdoc >= i22;
.AND. kar_fecdoc <= f22);
.AND. kar_almdes <> SPACE(4);
.AND. (kar_codmov <> 'TRBO';
.AND. kar_codmov <> 'COI ';
.AND. SUBSTR(kar_codmov, 1, 1) <> 'A')
     wrk_totpro = 0
     SUM kar_cantid TO wrk_totpro
     wrk_promed = wrk_totpro
     SELECT 3
     SEEK (wrk_codigo)
     IF  .NOT. FOUND()
          APPEND BLANK
          REPLACE nve_codpro WITH  ;
                  wrk_codigo
     ENDIF
     DO CASE
          CASE x = 5
               REPLACE nve_cmes6  ;
                       WITH  ;
                       wrk_promed
          CASE x = 4
               REPLACE nve_cmes5  ;
                       WITH  ;
                       wrk_promed
          CASE x = 3
               REPLACE nve_cmes4  ;
                       WITH  ;
                       wrk_promed
          CASE x = 2
               REPLACE nve_cmes3  ;
                       WITH  ;
                       wrk_promed
          CASE x = 1
               REPLACE nve_cmes2  ;
                       WITH  ;
                       wrk_promed
          CASE x = 0
               REPLACE nve_cmes1  ;
                       WITH  ;
                       wrk_promed
     ENDCASE
ENDFOR
SET FILTER TO
RETURN
*
FUNCTION f_yesno
PARAMETER mens, def
PRIVATE ALL
l = IIF(LEN(mens) < 18, 18,  ;
    LEN(mens))
sdef = IIF(def = .T.,  ;
       '\?\<Ok;\!\<Cancel',  ;
       '\?\<No;\!\<Si')
DEFINE WINDOW wyesno FROM 15, (80 -  ;
       l) / 2 - 4 TO 20, (80 + l) /  ;
       2 + 4 DOUBLE COLOR SCHEME  ;
       12
ACTIVATE WINDOW wyesno
@ 1, 4 SAY mens
@ 3, CEILING(l - 18) / 2 GET vok  ;
  DEFAULT 1 SIZE 1, 10, 4 PICTURE  ;
  '@*HT ' + sdef
READ CYCLE
RELEASE WINDOW wyesno
RETURN IIF(def = .T., IIF(vok = 1,  ;
       .T., .F.), IIF(vok = 1,  ;
       .F., .T.))
*
PROCEDURE p_mensaje
PARAMETER mens
SET CURSOR OFF
l = LEN(mens)
DEFINE WINDOW mensj FROM 11, (80 -  ;
       l) / 2 - 4 TO 15, (80 + l) /  ;
       2 + 4 COLOR SCHEME 12
ACTIVATE WINDOW mensj
SET COLOR TO GR+/N*
@ 1, (WCOLS('MENSJ') - l) / 2 SAY  ;
  mens
SET COLOR TO
= INKEY(1.5 )
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
