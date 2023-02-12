*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
tit_prg = ' TRASFERENCIA '
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' EXPORTAR DATOS AIWA '
CLOSE DATABASES
SELECT 1
USE ST_IOREP ORDER CODIGO
SELECT 2
USE ST_ISERI ORDER SER_CODMAR
SELECT 3
USE ST_ICLPR ORDER CODENT
STORE SPACE(2) TO cc
SELECT 4
USE ST_IPREP ORDER REP_NUMORD
SELECT 5
USE ST_IDPED ORDER CODIGO
DO WHILE .T.
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE 'A' TO w_drive
     w_mes = MONTH(DATE())
     w_ano = YEAR(DATE())
     @ 05, 10 TO 11, 67
     @ 07, 25 SAY 'Mes :'
     @ 07, 40 SAY 'A¤o :'
     @ 09, 15 SAY  ;
       'Indique el Drive a Copiar :'
     SET CURSOR ON
     @ 07, 30 GET w_mes RANGE 1, ;
       12 PICTURE '99'
     @ 07, 45 GET w_ano PICTURE  ;
       '9999'
     @ 09, 45 GET w_drive PICTURE  ;
       '!' VALID (w_drive $  ;
       'ABC')
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'GRA', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO WHILE .T.
          = INKEY(0, 'H')
          IF LASTKEY() = 27
               EXIT
          ENDIF
          IF LASTKEY() = -1
               DO graba
               EXIT
          ENDIF
     ENDDO
ENDDO
CLOSE DATABASES
DO saca_win
*
PROCEDURE graba
DO mensa WITH  ;
   '*****  P R O C E S A N D O  *****',  ;
   'COLO'
bb = SUBSTR(SYS(3), 1, 8) +  ;
     '.DBF'
dd = SUBSTR(SYS(3), 1, 8) +  ;
     '.DBF'
SELECT 6
CREATE TABLE &BB (OFICINA  C(4), ORDEN;
   C(8),  FECHAING  D, FECHAREP D, FECHASAL;
D,   CLIENTE  C(40), DIRECCION C(50),;
DISTRITO C(20), TELEFONO N(8), MODELO;
  C(15), SERIE     C(20), INFORME1 C(200),;
INFORME2 C(200), INFORME3 C(200), TIPOGAR;
 C(4),   NRODOC C(15), FECHADOC;
 D,      ESTADO C(30), NROFACT;
 C(10),  FECHAFAC D, MANOBRA   N(9,2))
SELECT 7
CREATE TABLE &DD (ORDEN C(8), CODIGO C(14),;
CANTID N(5), PRECIO N(9,2))
SELECT st_iorep
SCAN FOR codmar = '37  ' .AND.  ;
     indest <> 'N' .AND. (auxest <  ;
     '011 ' .OR. auxest =  ;
     '021 ')
     w_orden = numdoc
     w_fecing = fecemi
     w_fecrep = fecfin
     w_fecsal = fecent
     w_tipgar = indori
     w_codest = auxest
     w_modelo = codmod
     w_serie = numser
     w_infte1 = SUBSTR(observ,  ;
                001, 200)
     w_infte2 = SUBSTR(observ,  ;
                201, 200)
     w_infte3 = SUBSTR(observ,  ;
                401, 200)
     w_mobra = cosmob
     w_codent = codent
     w_descri = SPACE(25)
     w_nrodoc = SPACE(10)
     w_fecvta = CTOD( ;
                '  /  /    ')
     w_descri = SPACE(40)
     w_direcc = SPACE(50)
     w_coddis = SPACE(4)
     w_numtel = 0
     w_nrofac = numfabo
     w_fecfac = fecfabo
     SELECT st_iclpr
     SEEK w_codent
     IF FOUND()
          w_descri = noment
          w_direcc = nomcal
          w_coddis = nomdis
          w_numtel = numte1
     ENDIF
     SELECT st_iseri
     SEEK '37  ' + w_modelo +  ;
          w_serie
     IF FOUND()
          w_nrodoc = docgar
          w_fecvta = fecvta
     ENDIF
     SELECT 6
     APPEND BLANK
     REPLACE orden WITH w_orden,  ;
             fechaing WITH  ;
             w_fecing, modelo  ;
             WITH w_modelo, serie  ;
             WITH w_serie,  ;
             informe1 WITH  ;
             w_infte1, informe2  ;
             WITH w_infte2,  ;
             informe3 WITH  ;
             w_infte3, cliente  ;
             WITH w_descri,  ;
             fechasal WITH  ;
             w_fecsal, oficina  ;
             WITH rge_codalm,  ;
             fecharep WITH  ;
             w_fecrep, tipogar  ;
             WITH w_tipgar,  ;
             manobra WITH w_mobra,  ;
             telefono WITH  ;
             w_numtel, direccion  ;
             WITH w_direcc,  ;
             distrito WITH  ;
             ootab('DIST', ;
             w_coddis), estado  ;
             WITH ootab('ESOR', ;
             w_codest), nrodoc  ;
             WITH w_nrodoc,  ;
             fechadoc WITH  ;
             w_fecvta, nrofact  ;
             WITH w_nrofac,  ;
             fechafac WITH  ;
             w_fecfac
     SELECT st_iprep
     SEEK w_orden
     IF FOUND()
          SCAN WHILE numord =  ;
               w_orden .AND.  ;
               indest <> 'N'
               w_numped = numdoc
               SELECT st_idped
               SET NEAR ON
               SEEK w_numped
               SET NEAR OFF
               SCAN WHILE numdoc =  ;
                    w_numped
                    w_codpro = codpro
                    w_cantid = canpro
                    w_precio = valpro
                    IF w_cantid >  ;
                       0
                         SELECT 7
                         APPEND BLANK
                         REPLACE codigo  ;
                                 WITH  ;
                                 w_codpro
                         REPLACE cantid  ;
                                 WITH  ;
                                 w_cantid
                         REPLACE orden  ;
                                 WITH  ;
                                 w_orden
                         REPLACE precio  ;
                                 WITH  ;
                                 w_precio
                    ENDIF
                    SELECT st_idped
               ENDSCAN
               SELECT st_iprep
          ENDSCAN
     ENDIF
     SELECT st_iorep
ENDSCAN
SELECT 6
USE
SELECT 7
USE
aa = SUBSTR(SYS(3), 1, 8) +  ;
     '.DBF'
cc = SUBSTR(SYS(3), 1, 8) +  ;
     '.DBF'
SELECT 6
CREATE TABLE &AA (OFICINA  C(4), ORDEN;
   C(8),  FECHAING  D, FECHAREP D, FECHASAL;
D,   CLIENTE  C(40), DIRECCION C(50),;
DISTRITO C(20), TELEFONO N(8), MODELO;
  C(15), SERIE     C(20), INFORME1 C(200),;
INFORME2 C(200), INFORME3 C(200), TIPOGAR;
 C(4),   NRODOC C(15), FECHADOC;
 D,      ESTADO C(30), NROFACT;
 C(10),  FECHAFAC D, MANOBRA   N(9,2))
SELECT 7
CREATE TABLE &CC (ORDEN C(8), CODIGO C(14),;
CANTID N(5), PRECIO N(9,2))
SELECT st_iorep
SCAN FOR codmar = '37  ' .AND.  ;
     indest <> 'N' .AND.  ;
     MONTH(fecemi) = w_mes .AND.  ;
     YEAR(fecemi) = w_ano
     w_orden = numdoc
     w_fecing = fecemi
     w_fecrep = fecfin
     w_fecsal = fecent
     w_tipgar = indori
     w_codest = auxest
     w_modelo = codmod
     w_serie = numser
     w_infte1 = SUBSTR(observ,  ;
                001, 200)
     w_infte2 = SUBSTR(observ,  ;
                201, 200)
     w_infte3 = SUBSTR(observ,  ;
                401, 200)
     w_mobra = cosmob
     w_codent = codent
     w_descri = SPACE(25)
     w_nrodoc = SPACE(10)
     w_fecvta = CTOD( ;
                '  /  /    ')
     w_descri = SPACE(40)
     w_direcc = SPACE(50)
     w_coddis = SPACE(4)
     w_numtel = 0
     w_nrofac = numfabo
     w_fecfac = fecfabo
     SELECT st_iclpr
     SEEK w_codent
     IF FOUND()
          w_descri = noment
          w_direcc = nomcal
          w_coddis = nomdis
          w_numtel = numte1
     ENDIF
     SELECT st_iseri
     SEEK '37  ' + w_modelo +  ;
          w_serie
     IF FOUND()
          w_nrodoc = docgar
          w_fecvta = fecvta
     ENDIF
     SELECT 6
     APPEND BLANK
     REPLACE orden WITH w_orden,  ;
             fechaing WITH  ;
             w_fecing, modelo  ;
             WITH w_modelo, serie  ;
             WITH w_serie,  ;
             informe1 WITH  ;
             w_infte1, informe2  ;
             WITH w_infte2,  ;
             informe3 WITH  ;
             w_infte3, cliente  ;
             WITH w_descri,  ;
             fechasal WITH  ;
             w_fecsal, oficina  ;
             WITH rge_codalm,  ;
             fecharep WITH  ;
             w_fecrep, tipogar  ;
             WITH w_tipgar,  ;
             manobra WITH w_mobra,  ;
             telefono WITH  ;
             w_numtel, direccion  ;
             WITH w_direcc,  ;
             distrito WITH  ;
             ootab('DIST', ;
             w_coddis), estado  ;
             WITH ootab('ESOR', ;
             w_codest), nrodoc  ;
             WITH w_nrodoc,  ;
             fechadoc WITH  ;
             w_fecvta, nrofact  ;
             WITH w_nrofac,  ;
             fechafac WITH  ;
             w_fecfac
     SELECT st_iprep
     SEEK w_orden
     IF FOUND()
          SCAN WHILE numord =  ;
               w_orden .AND.  ;
               indest <> 'N'
               w_numped = numdoc
               SELECT st_idped
               SET NEAR ON
               SEEK w_numped
               SET NEAR OFF
               SCAN WHILE numdoc =  ;
                    w_numped
                    w_codpro = codpro
                    w_cantid = canpro
                    w_precio = valpro
                    IF w_cantid >  ;
                       0
                         SELECT 7
                         APPEND BLANK
                         REPLACE codigo  ;
                                 WITH  ;
                                 w_codpro
                         REPLACE cantid  ;
                                 WITH  ;
                                 w_cantid
                         REPLACE orden  ;
                                 WITH  ;
                                 w_orden
                         REPLACE precio  ;
                                 WITH  ;
                                 w_precio
                    ENDIF
                    SELECT st_idped
               ENDSCAN
               SELECT st_iprep
          ENDSCAN
     ENDIF
     SELECT st_iorep
ENDSCAN
SELECT 6
USE
SELECT 7
USE
CLOSE DATABASES
DO mensa WITH  ;
   '*****  P R O C E S A N D O  *****',  ;
   'SACA'
DO mensa WITH  ;
   '*****  G R A B A N D O  *****',  ;
   'COLO'
w_file1 = w_drive + ':\ES' +  ;
          ALLTRIM(STR(w_mes)) +  ;
          rge_codalm + '.dbf'
w_file2 = w_drive + ':\PEND' +  ;
          rge_codalm + '.dbf'
w_file3 = w_drive + ':\RP' +  ;
          ALLTRIM(STR(w_mes)) +  ;
          rge_codalm + '.dbf'
w_file4 = w_drive + ':\REPP' +  ;
          rge_codalm + '.dbf'
COPY FILE &AA TO &w_file1
COPY FILE &BB TO &w_file2
COPY FILE &CC TO &w_file3
COPY FILE &DD TO &w_file4
DO mensa WITH  ;
   '*****  G R A B A N D O  *****',  ;
   'SACA'
DELE FILE &AA
DELE FILE &BB
DELE FILE &CC
DELE FILE &DD
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
