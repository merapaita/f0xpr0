PARAMETER vtipdoc
PUBLIC vperiodo, vnummes, vcodfte
USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 2 OrdSer ALIAS ordser  ;
    ORDER OrdSer1
USE IN 3 OrdCom ALIAS orden ORDER  ;
    OrdCom1
USE IN 4 IteOc ALIAS iteoc ORDER  ;
    IteOc1
USE IN 5 HojCon ALIAS hoja ORDER  ;
    Hojcon1
USE IN 6 IteHc ALIAS itehc ORDER  ;
    Itehc2
USE IN 7 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 8 Compag ALIAS compag  ;
    ORDER Compag4
USE IN 9 Pteanu ALIAS pteanu  ;
    ORDER Pteanu7
USE IN 10 SOLSER ALIAS solser  ;
    ORDER SOLSER1
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
vperiodo = RIGHT(DTOC(DATE()), 2)
vnummes = SPACE(2)
vcodfte = SPACE(2)
vtipdoc = '   '
DEFINE WINDOW lis FROM 9, 18 TO  ;
       16, 62 FLOAT TITLE  ;
       ' °° Listado Conciliaciones °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 1, 2 SAY '    Periodo : '
@ 2, 2 SAY '        Mes : '
@ 3, 2 SAY '     Fuente : '
@ 4, 2 SAY '  Documento : '
@ 1, 17 GET vperiodo PICTURE '!!'  ;
  VALID  .NOT. EMPTY(vperiodo)
@ 2, 17 GET vnummes PICTURE '!!'  ;
  VALID val_para(vnummes,'FECMES', ;
  ' ',17,10) .AND.  .NOT.  ;
  EMPTY(vnummes)
@ 3, 17 GET vcodfte PICTURE '!!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  ' ',17,16) .AND.  .NOT.  ;
  EMPTY(vcodfte)
@ 4, 17 GET vtipdoc PICTURE '!!!'  ;
  VALID val_para(vtipdoc,'TIPDOC', ;
  ' ',17,16) .AND.  .NOT.  ;
  EMPTY(vtipdoc)
READ VALID val_read()
DEACTIVATE WINDOW lis
IF LASTKEY() = 27
     RETURN
ELSE
     vind1 = SYS(3) + '.IDX'
     vind2 = SYS(3) + '.IDX'
     IF EOF()
          DO standby WITH  ;
             'No se tiene registros a Procesar'
     ELSE
          ACTIVATE WINDOW standby
          @ 1, 14 SAY  ;
            'Espere un Momento ....'  ;
            COLOR W/N* 
          SELECT hoja
          vind = SYS(3) + '.IDX'
          INDEX ON periodo +  ;
                nummes + numhc TO  ;
                (vind) FOR tipdoc =  ;
                ALLTRIM(vtipdoc)  ;
                .AND. periodo <=  ;
                ALLTRIM(vperiodo)  ;
                .AND. nummes <=  ;
                ALLTRIM(vnummes)  ;
                .AND. codfte =  ;
                ALLTRIM(vcodfte)  ;
                .AND. estado <>  ;
                '99' .AND. numref <>  ;
                'B' .AND. numref <>  ;
                'A'
          SET INDEX TO (vind)
          DO CASE
               CASE ALLTRIM(vtipdoc) =  ;
                    'O/C'
                    SELECT orden
                    SET RELATION TO periodo;
+ perhc + numhc INTO hoja
                    SET RELATION TO codprv;
INTO promae ADDITIVE
                    INDEX ON  ;
                          periodo +  ;
                          perhc +  ;
                          numhc  ;
                          TO  ;
                          (vind1)  ;
                          FOR  ;
                          estado >=  ;
                          '20'  ;
                          .AND.  ;
                          estado <>  ;
                          '99'  ;
                          .AND.  ;
                          periodo <=  ;
                          vperiodo  ;
                          .AND.  ;
                          perhc <=  ;
                          ALLTRIM(vnummes)  ;
                          .AND.  ;
                          (MONTH(fecdesp) >=  ;
                          VAL(vnummes)  ;
                          .OR.  ;
                          EMPTY(fecdesp))  ;
                          .AND.  ;
                          codfte =  ;
                          ALLTRIM(vcodfte)  ;
                          .AND.   ;
                          .NOT.  ;
                          EMPTY(hoja.numhc)  ;
                          .AND.  ;
                          orden.tipord =  ;
                          'B'
                    SET INDEX TO (vind1)
               CASE ALLTRIM(vtipdoc) =  ;
                    'O/S'
                    SELECT ordser
                    SET RELATION TO periodo;
+ perhc + numhc INTO hoja
                    SET RELATION TO codprv;
INTO promae ADDITIVE
                    INDEX ON  ;
                          periodo +  ;
                          perhc +  ;
                          numhc  ;
                          TO  ;
                          (vind2)  ;
                          FOR  ;
                          estado >=  ;
                          '20'  ;
                          .AND.  ;
                          estado <>  ;
                          '99'  ;
                          .AND.  ;
                          periodo <=  ;
                          vperiodo  ;
                          .AND.  ;
                          perhc <=  ;
                          ALLTRIM(vnummes)  ;
                          .AND.  ;
                          (MONTH(fecliq) >=  ;
                          VAL(vnummes)  ;
                          .OR.  ;
                          EMPTY(fecliq))  ;
                          .AND.  ;
                          codfte =  ;
                          ALLTRIM(vcodfte)  ;
                          .AND.   ;
                          .NOT.  ;
                          EMPTY(hoja.numhc)  ;
                          .AND.  ;
                          ordser.tipord =  ;
                          'B'
                    SET INDEX TO (vind2)
               OTHERWISE
                    RETURN
          ENDCASE
          GOTO TOP
          DEACTIVATE WINDOW  ;
                     standby
          DO CASE
               CASE ALLTRIM(vtipdoc) =  ;
                    'O/C'
                    DO reporte  ;
                       WITH 2,  ;
                       'conordC1',  ;
                       ' Listado Conciliaciones '
               CASE ALLTRIM(vtipdoc) =  ;
                    'O/S'
                    DO reporte  ;
                       WITH 2,  ;
                       'conordS1',  ;
                       ' Listado Conciliaciones '
          ENDCASE
          CLOSE INDEX
          SELECT hoja
          CLOSE DATABASES
          ERASE (vind)
          ERASE (vind1)
          ERASE (vind2)
     ENDIF
ENDIF
RETURN
*
FUNCTION busca_hc
PRIVATE ad
ad = ALIAS()
SELECT compag
SET FILTER TO estado <> '99'
SEEK hoja.nummes + hoja.numhc
IF FOUND()
     vtotalcp = compag.import
ELSE
     IF orden.estado = '50' .AND.   ;
        .NOT.  ;
        EMPTY(orden.numreb)
          vtotalcp = IIF(vtipdoc =  ;
                     'O/C',  ;
                     orden.valtot -  ;
                     orden.anultot,  ;
                     ordser.valtot -  ;
                     ordser.anultot)
     ELSE
          vtotalcp = IIF(vtipdoc =  ;
                     'O/C',  ;
                     orden.valtot,  ;
                     ordser.valtot)
     ENDIF
ENDIF
SET FILTER TO
SELECT (ad)
RETURN vtotalcp
*
PROCEDURE salida
RELEASE WINDOW lis
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
FUNCTION mfecha
PARAMETER vmes, vano
meses = 'ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE'
RETURN ALLTRIM(SUBSTR(meses, vmes *  ;
       9 - 8, 9)) + ' ' +  ;
       STR(vano, 2)
*
FUNCTION busca_cp
PRIVATE ad
ad = ALIAS()
vnumcp = '       '
SELECT compag
SET FILTER TO estado <> '99'
SEEK hoja.nummes + hoja.numhc
IF FOUND()
     vnumcp = compag.numcp + '.' +  ;
              compag.nummes
ELSE
     vnumcp = hoja.numcp + '.' +  ;
              hoja.nummescp
ENDIF
SET FILTER TO
SELECT (ad)
RETURN vnumcp
*
FUNCTION v_solser
PRIVATE ali, vkey
ali = ALIAS()
vkey = ordser.periodo +  ;
       ordser.numss
SELECT solser
SEEK vkey
IF FOUND()
     vret = val_para(solser.tipser, ;
            'TIPSER','D',22,60)
ELSE
     vret = 'SIN S/S'
ENDIF
SELECT (ali)
RETURN vret
*
FUNCTION valanrb
PRIVATE a1, a2
a2 = ALIAS()
a1 = 0
DO CASE
     CASE  .NOT.  ;
           EMPTY(orden.numreb)
          SELECT pteanu
          SEEK orden.numreb
          IF FOUND()
               IF pteanu.nummestr =  ;
                  ALLTRIM(vnummes)
                    a1 = orden.anultot
               ENDIF
          ENDIF
     CASE  .NOT.  ;
           EMPTY(orden.numanu)
          SELECT pteanu
          SEEK orden.perhc +  ;
               orden.numhc
          IF FOUND()
               IF pteanu.mespa =  ;
                  ALLTRIM(vnummes)
                    a1 = orden.anultot
               ENDIF
          ENDIF
     OTHERWISE
ENDCASE
SELECT (a2)
RETURN a1
*
FUNCTION valanrbos
PARAMETER tipo
PRIVATE a1, a2
a2 = ALIAS()
a1 = 0
DO CASE
     CASE tipo = 'R' .AND.  .NOT.  ;
          EMPTY(ordser.numreb)
          SELECT pteanu
          SEEK ordser.numreb
          IF FOUND()
               IF pteanu.nummestr =  ;
                  ALLTRIM(vnummes)
                    a1 = ordser.anultot
               ENDIF
          ENDIF
     CASE  .NOT.  ;
           EMPTY(ordser.numanu)
          SELECT pteanu
          SEEK ordser.numanu
          IF FOUND()
               BROWSE
               IF pteanu.nummestr =  ;
                  ALLTRIM(vnummes)
                    a1 = ordser.anultot
               ENDIF
          ENDIF
     OTHERWISE
ENDCASE
USE IN 9
SELECT (a2)
RETURN a1
*
FUNCTION busc_cd
PRIVATE as, vkey
as = ALIAS()
IF ALLTRIM(vcodfte) <> '  '
     vkey = LEFT(ordser.codpart,  ;
            2) +  ;
            RIGHT(ordser.codpart,  ;
            2)
ELSE
     CANCEL
ENDIF
SELECT clase
SEEK vkey
IF FOUND()
     vpar = clase.cuenta
ELSE
     vpar = '    *    '
ENDIF
SELECT (as)
RETURN vpar
*
FUNCTION buscatg
PARAMETER clavehc
valias = SELECT()
SELECT itehc
vord = ORDER()
SET ORDER TO 1
IF SEEK(clavehc)
     vcatgas = LEFT(itehc.codpart,  ;
               1)
ELSE
     vcatgas = ' '
ENDIF
SET ORDER TO (vord)
SELECT (valias)
RETURN vcatgas
*
