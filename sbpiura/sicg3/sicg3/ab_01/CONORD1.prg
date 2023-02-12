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
DO CASE
     CASE ALLTRIM(vtipdoc) =  ;
          'O/C'
          USE IN 9 Pteanu ALIAS  ;
              pteanu ORDER  ;
              Pteanu1
     CASE ALLTRIM(vtipdoc) =  ;
          'O/S'
          USE IN 9 Pteanu ALIAS  ;
              pteanu ORDER  ;
              Pteanu2
ENDCASE
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
vperiodo = '98'
vnummes = '  '
vcodfte = '  '
STORE DATE() TO vfecfin
STORE CTOD('01-01-98') TO vfecini
DEFINE WINDOW lis FROM 9, 18 TO  ;
       16, 62 FLOAT TITLE  ;
       ' °° Listado Conciliaciones °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 1, 2 SAY '    Periodo : '
@ 2, 2 SAY '     Fechas : '
@ 3, 2 SAY '     Fuente : '
@ 4, 2 SAY '  Documento : '
@ 1, 17 GET vperiodo PICTURE '!!'  ;
  VALID  .NOT. EMPTY(vperiodo)
@ 2, 17 GET vfecini PICTURE '@D'
@ 2, 27 GET vfecfin PICTURE '@D'  ;
  VALID vfecfin >= vfecini
@ 3, 17 GET vcodfte PICTURE '!!'  ;
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
          SELECT pteanu
          vind4 = SYS(3) + '.IDX'
          INDEX ON periodo +  ;
                perhc + numhc TO  ;
                (vind4) FOR  ;
                IIF(ALLTRIM(vtipdoc) =  ;
                'O/C', tipdoc =  ;
                'O/C', tipdoc =  ;
                'O/S') .AND.  ;
                MONTH(pteanu.fecpa) >=  ;
                VAL(vnummes)
          SET INDEX TO (vind4)
          DO CASE
               CASE ALLTRIM(vtipdoc) =  ;
                    'O/C'
                    SELECT orden
                    SET RELATION TO periodo;
+ perhc + numhc INTO hoja
                    SET RELATION TO periodo;
+ perhc + numhc INTO pteanu ADDITIVE
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
                          (fecdesp >=  ;
                          vfecini  ;
                          .OR.  ;
                          EMPTY(fecdesp))  ;
                          .AND.  ;
                          periodo =  ;
                          vperiodo  ;
                          .AND.  ;
                          perhc <=  ;
                          ALLTRIM(vnummes)  ;
                          .AND.  ;
                          codfte =  ;
                          ALLTRIM(vcodfte)  ;
                          .AND.   ;
                          .NOT.  ;
                          EMPTY(hoja.numhc)
                    SET FILTER TO IIF(;
.NOT. EMPTY(pteanu.numpa), orden.estado;
= '21';
.AND. vfecini >= pteanu.fecpa;
.AND. vfecfin <= pteanu.fecpa;
.AND. pteanu.periodo = vperiodo,;
.T.)
                    SET INDEX TO (vind1)
               CASE ALLTRIM(vtipdoc) =  ;
                    'O/S'
                    SELECT ordser
                    SET RELATION TO periodo;
+ perhc + numhc INTO hoja
                    SET RELATION TO periodo;
+ perhc + numhc INTO pteanu ADDITIVE
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
                          (fecliq >=  ;
                          vfecini  ;
                          .OR.  ;
                          EMPTY(fecliq))  ;
                          .AND.  ;
                          periodo =  ;
                          vperiodo  ;
                          .AND.  ;
                          perhc <=  ;
                          ALLTRIM(vnummes)  ;
                          .AND.  ;
                          codfte =  ;
                          ALLTRIM(vcodfte)  ;
                          .AND.   ;
                          .NOT.  ;
                          EMPTY(hoja.numhc)
                    SET FILTER TO IIF(;
.NOT. EMPTY(pteanu.numpa), ordser.estado;
= '21';
.AND. vfecini >= pteanu.fecpa;
.AND. vfecfin <= pteanu.fecpa;
.AND. pteanu.periodo = vperiodo,;
.T.)
                    SET INDEX TO (vind2)
               OTHERWISE
                    RETURN
          ENDCASE
          GOTO TOP
          DO reporte WITH 2,  ;
             'conordX',  ;
             ' Listado Conciliaciones '
          CLOSE INDEX
          SELECT hoja
          CLOSE DATABASES
          ERASE (vind)
          ERASE (vind1)
          ERASE (vind2)
          ERASE (vind4)
     ENDIF
ENDIF
RETURN
*
FUNCTION busca_hc
PRIVATE ad
ad = ALIAS()
IF hoja.nummes < '02'
     vtotalcp = valtot
ELSE
     SELECT compag
     SET FILTER TO estado <> '99'
     SEEK hoja.nummes +  ;
          hoja.numhc
     IF FOUND()
          vtotalcp = compag.import
     ELSE
          IF orden.estado = '50'  ;
             .AND.  .NOT.  ;
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
ENDIF
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
SELECT (ad)
RETURN vnumcp
*
