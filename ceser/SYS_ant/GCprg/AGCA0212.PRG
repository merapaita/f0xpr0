*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
STORE 0 TO t, tt
CLOSE DATABASES
SELECT 1
USE SHARED gc_kar00 ORDER  ;
    kar_profec
SELECT 2
USE SHARED gc_nve00 ORDER codigo
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED gc_pro00 ORDER codigo
w_fecha = CTOD(SPACE(8))
flag = .F.
DEFINE WINDOW ven1 FROM 10, 24 TO  ;
       15, 59 TITLE  ;
       'CONSUMO MENSUAL'
ACTIVATE WINDOW ven1
w_fecha = DATE()
w_nivcit = SPACE(1)
@ 02, 02 SAY 'INGRESE FECHA :'
@ 02, 18 GET w_fecha COLOR N/W 
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW ven1
     RELEASE WINDOW mensj
     CLOSE DATABASES
     ACTIVATE SCREEN
     RETURN
ENDIF
rpt = f_yesno( ;
      'Desea procesar ? ')
IF  .NOT. rpt
     DEACTIVATE WINDOW ven1
     RELEASE WINDOW mensj
     CLOSE DATABASES
     ACTIVATE SCREEN
     RETURN
ENDIF
DO p_mensaje WITH  ;
   'Calculando Consumo Mensual, Espere un momento.... '
f1 = GOMONTH(w_fecha, 1)
f2 = f1 - DAY(f1)
i1 = (GOMONTH(w_fecha, -5))
i2 = i1 - (DAY(i1) - 1)
f1a1 = GOMONTH(w_fecha, 1)
f2a1 = f1a1 - DAY(f1a1)
i1a1 = (GOMONTH(w_fecha, -11))
i2a1 = i1a1 - (DAY(i1a1) - 1)
f1a2 = GOMONTH(w_fecha, 1)
f2a2 = f1a2 - DAY(f1a2)
i1a2 = (GOMONTH(w_fecha, -23))
i2a2 = i1a2 - (DAY(i1a2) - 1)
SELECT gc_pro00
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF pro_nivcit = 'D' .OR.  ;
        pro_nivcit = 'E' .OR.  ;
        pro_nivcit = 'F' .OR.  ;
        pro_nivcit = 'G' .OR.  ;
        pro_nivcit = 'N'
          DO rbloquea
          REPLACE pro_nivcit WITH  ;
                  SPACE(1)
          UNLOCK
     ENDIF
     w_cons = 0
     IF  .NOT. EMPTY(pro_ultmov)
          DO promedio
     ENDIF
     SELECT gc_pro00
     IF ROUND(w_cons / 6, 2) <= 0
          IF (pro_crepro > i2a1  ;
             .AND.  ;
             EMPTY(pro_ultmov))  ;
             .OR. pro_ultmov >  ;
             i2a1
               IF  .NOT.  ;
                   EMPTY(pro_ultmov)
                    pro1 = pro_ultmov
               ELSE
                    pro1 = pro_crepro
               ENDIF
               IF pro1 <= i2
                    DO rbloquea
                    REPLACE pro_nivcit  ;
                            WITH  ;
                            'D'
                    IF (pro_rcom <>  ;
                       'Z' .AND.  ;
                       pro_rcom <>  ;
                       'Y' .AND.  ;
                       pro_rcom <>  ;
                       'H')
                         REPLACE pro_rcom  ;
                                 WITH  ;
                                 pro_nivcit
                    ENDIF
                    UNLOCK
               ELSE
                    IF  .NOT.  ;
                        EMPTY(pro_ultmov)
                         DO rbloquea
                         REPLACE pro_nivcit  ;
                                 WITH  ;
                                 'G'
                         IF (pro_rcom <>  ;
                            'Z'  ;
                            .AND.  ;
                            pro_rcom <>  ;
                            'Y'  ;
                            .AND.  ;
                            pro_rcom <>  ;
                            'H')
                              REPLACE  ;
                               pro_rcom  ;
                               WITH  ;
                               pro_nivcit
                         ENDIF
                         UNLOCK
                    ELSE
                         DO rbloquea
                         REPLACE pro_nivcit  ;
                                 WITH  ;
                                 'N'
                         IF (pro_rcom <>  ;
                            'Z'  ;
                            .AND.  ;
                            pro_rcom <>  ;
                            'Y'  ;
                            .AND.  ;
                            pro_rcom <>  ;
                            'H')
                              REPLACE  ;
                               pro_rcom  ;
                               WITH  ;
                               pro_nivcit
                         ENDIF
                         UNLOCK
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDSCAN
SELECT gc_pro00
SET RELATION TO pro_codpro INTO gc_nve00
GOTO TOP
RELEASE WINDOW mensj
DO p_mensaje WITH  ;
   'Chequeando 1 y m s de 2 a¤os,Espere un momento.... '
SCAN WHILE  .NOT. EOF()
     IF pro_nivcit <> 'D' .AND.  ;
        pro_nivcit <> 'G' .AND.  ;
        pro_nivcit <> 'N'
          IF (gc_nve00.nve_cmes1 +  ;
             gc_nve00.nve_cmes2 +  ;
             gc_nve00.nve_cmes3 +  ;
             gc_nve00.nve_cmes4 +  ;
             gc_nve00.nve_cmes5 +  ;
             gc_nve00.nve_cmes6) =  ;
             0
               w_nivcit = SPACE(1)
               pro = pro_codpro
               IF (pro_crepro <=  ;
                  i2a1 .AND.  ;
                  EMPTY(pro_ultmov))  ;
                  .OR. pro_ultmov <=  ;
                  i2a1
                    IF EMPTY(pro_ultmov)
                         IF pro_crepro <=  ;
                            i2a2
                              w_nivcit =  ;
                               'F'
                         ELSE
                              w_nivcit =  ;
                               'E'
                         ENDIF
                    ELSE
                         IF pro_ultmov <=  ;
                            i2a2
                              w_nivcit =  ;
                               'F'
                         ENDIF
                    ENDIF
                    IF w_nivcit =  ;
                       SPACE(1)
                         SELECT gc_kar00
                         SEEK pro
                         flag = .F.
                         IF FOUND()
                              SCAN  ;
                               WHILE  ;
                               kar_codpro =  ;
                               pro  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF()
                                   IF SUBSTR(kar_codmov, 1, 1) = 'E' .AND. kar_codmov <> 'EMER'
                                        flag = .T.
                                        IF kar_fecdoc <= i2a1
                                             IF kar_fecdoc <= i2a2
                                                  w_nivcit = 'F'
                                             ELSE
                                                  w_nivcit = 'E'
                                             ENDIF
                                        ENDIF
                                        EXIT
                                   ENDIF
                              ENDSCAN
                         ENDIF
                         IF  .NOT.  ;
                             flag
                              SELECT  ;
                               gc_pro00
                              IF pro_crepro <=  ;
                                 i2a2
                                   w_nivcit = 'F'
                              ELSE
                                   w_nivcit = 'E'
                              ENDIF
                         ENDIF
                    ENDIF
                    SELECT gc_pro00
                    IF w_nivcit <>  ;
                       SPACE(1)
                         DO rbloquea
                         REPLACE pro_nivcit  ;
                                 WITH  ;
                                 w_nivcit
                         IF (pro_rcom <>  ;
                            'Z'  ;
                            .AND.  ;
                            pro_rcom <>  ;
                            'Y'  ;
                            .AND.  ;
                            pro_rcom <>  ;
                            'H')
                              REPLACE  ;
                               pro_rcom  ;
                               WITH  ;
                               pro_nivcit
                         ENDIF
                         REPLACE pro_hora  ;
                                 WITH  ;
                                 TIME(),  ;
                                 pro_fecha  ;
                                 WITH  ;
                                 DATE()
                         REPLACE pro_usuari  ;
                                 WITH  ;
                                 clave
                         UNLOCK
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDSCAN
DEACTIVATE WINDOW ven1
RELEASE WINDOW mensj
SET RELATION TO
CLOSE DATABASES
ACTIVATE SCREEN
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
     SELECT gc_kar00
     SEEK gc_pro00.pro_codpro +  ;
          DTOS(i22)
     SET FILTER TO kar_codpro = gc_pro00.pro_codpro;
.AND. (kar_fecdoc >= i22;
.AND. kar_fecdoc <= f22);
.AND. (kar_codmov <> 'TRBO';
.AND. kar_codmov <> 'IFO ';
.AND. kar_codmov <> 'IFC ';
.AND. kar_codmov <> 'IFL ';
.AND. kar_codmov <> 'IFE ';
.AND. kar_codmov <> 'IFP ');
.AND. (kar_codmov <> 'COI ';
.AND. kar_codmov <> 'EMER';
.AND. SUBSTR(kar_codmov, 1, 1) <> 'A')
     STORE 0 TO w_totpro,  ;
           w_devolu
     SUM FOR kar_codmov <> 'IFD '  ;
         kar_cantid TO w_totpro
     SUM FOR kar_codmov = 'IFD '  ;
         kar_cantid TO w_devolu
     w_promed = w_totpro -  ;
                w_devolu
     w_cons = w_cons + w_promed
     SET FILTER TO
     SELECT gc_nve00
     SEEK gc_pro00.pro_codpro
     IF  .NOT. FOUND()
          APPEND BLANK
          DO rbloquea
          REPLACE nve_codpro WITH  ;
                  gc_pro00.pro_codpro
     ELSE
          DO rbloquea
     ENDIF
     DO CASE
          CASE x = 0
               REPLACE nve_cmes1  ;
                       WITH  ;
                       w_promed
          CASE x = 1
               REPLACE nve_cmes2  ;
                       WITH  ;
                       w_promed
          CASE x = 2
               REPLACE nve_cmes3  ;
                       WITH  ;
                       w_promed
          CASE x = 3
               REPLACE nve_cmes4  ;
                       WITH  ;
                       w_promed
          CASE x = 4
               REPLACE nve_cmes5  ;
                       WITH  ;
                       w_promed
          CASE x = 5
               REPLACE nve_cmes6  ;
                       WITH  ;
                       w_promed
     ENDCASE
     REPLACE nve_usuari WITH  ;
             clave, nve_fecha  ;
             WITH DATE()
     REPLACE nve_hora WITH TIME()
     UNLOCK
ENDFOR
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
@ 01, 4 SAY mens
@ 03, CEILING(l - 18) / 2 GET vok  ;
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
@ 1, (WCOLS('MENSJ') - l) / 2 SAY  ;
  mens
= INKEY(1.5 )
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
