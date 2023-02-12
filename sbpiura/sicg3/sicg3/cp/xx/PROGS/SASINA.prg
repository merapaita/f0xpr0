USE IN 5 ItePar ALIAS itepar  ;
    ORDER ItePar1
USE IN 7 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 11 Maepre ALIAS maepre  ;
    ORDER Maepre1
USE IN 1 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 2 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 14 Auxil ALIAS auxil ORDER  ;
    Auxil2
USE IN 15 ComPag ALIAS compag  ;
    ORDER ComPag4
USE IN 16 Hojmod ALIAS hojmod  ;
    ORDER Hojmod2
USE IN 18 Reten ALIAS reten ORDER  ;
    Reten1
USE IN 17 ASTAUX ALIAS ast
DO inicia
CLOSE DATABASES
RETURN
*
PROCEDURE inicia
vtemp = RECNO()
as = ORDER()
vperiodo = '  '
vnummes1 = '  '
vtipfun = ' '
vcodprg = '  '
vcodfte = '   '
vcodsub = '   '
vcodpart = '     '
vcodpry = '   '
vcodspy = '  '
vcodact = '  '
DEFINE WINDOW lis FROM 5, 12 TO  ;
       18, 70 FLOAT TITLE  ;
       ' °° Saldos Asignaci¢n por Partidas °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 1, 2 SAY '        Periodo : '
@ 2, 2 SAY '            Mes : '
@ 3, 2 SAY '           Tipo : '
@ 4, 2 SAY '         Fuente : '
@ 5, 2 SAY '       Programa : '
@ 6, 2 SAY '    SubPrograma : '
@ 7, 2 SAY '       Proyecto : '
@ 8, 2 SAY '    SubProyecto : '
@ 9, 2 SAY '      Actividad : '
@ 10, 2 SAY ' Part. Anal¡tica: '
@ 1, 21 GET vperiodo PICTURE '!!'  ;
  VALID  .NOT. EMPTY(vperiodo)
@ 2, 21 GET vnummes1 PICTURE '!!'  ;
  VALID val_para(vnummes1, ;
  'FECMES','C',21,30,4) .AND.   ;
  .NOT. EMPTY(vnummes1)
@ 3, 21 GET vtipfun PICTURE '!'  ;
  VALID val_para(vtipfun,'TIPFUN', ;
  ' ',21,20)
@ 4, 21 GET vcodfte PICTURE '!!!'  ;
  VALID val_para(vcodfte,'CODFTE', ;
  ' ',21,30,4) .AND.  .NOT.  ;
  EMPTY(vcodfte)
@ 5, 21 GET vcodprg PICTURE '!!'  ;
  VALID val_para(vcodprg,'CODPRG', ;
  ' ',21,30,4) .AND.  .NOT.  ;
  EMPTY(vcodprg)
@ 6, 21 GET vcodsub PICTURE '!!!'  ;
  VALID IIF( .NOT. EMPTY(vcodsub),  ;
  val_subp(vcodsub,'CODSUB' +  ;
  vcodprg,' ',21,28), .T.)
@ 7, 21 GET vcodpry PICTURE '!!!'  ;
  WHEN ALLTRIM(vtipfun) = 'I'
@ 8, 21 GET vcodspy PICTURE '!!'  ;
  WHEN ALLTRIM(vtipfun) = 'I'
@ 9, 21 GET vcodact PICTURE '!!'  ;
  WHEN ALLTRIM(vtipfun) = 'F'
@ 10, 21 GET vcodpart PICTURE  ;
  '!!.!!'
READ VALID val_read()
DEACTIVATE WINDOW lis
IF EMPTY(vnummes1) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     ACTIVATE WINDOW standby
     @ 1, 14 SAY  ;
       'Espere un Momento ....'  ;
       COLOR W/N* 
     m.tipfun = ALLTRIM(vtipfun)
     SELECT ast
     vdbf = SYS(3) + '.dbf'
     COPY TO (vdbf) STRUCTURE
     USE IN 17 (vdbf) ALIAS ast
     vind = SYS(3) + '.IDX'
     IF ALLTRIM(vtipfun) = 'F'
          INDEX ON nummes +  ;
                codsubpr + codact +  ;
                codanal + tipo +  ;
                tipdoc + numref  ;
                TO (vind)
     ELSE
          INDEX ON nummes +  ;
                codsubpr +  ;
                codproy +  ;
                codsupry +  ;
                codpart + tipo +  ;
                tipdoc + numref  ;
                TO (vind)
     ENDIF
     SET INDEX TO (vind)
     vindi1 = SYS(3) + '.idx'
     SELECT hoja
     SET FILTER TO estado <> '99'
     GOTO TOP
     SELECT hojmod
     SET FILTER TO estado <> '99'
     GOTO TOP
     SELECT compag
     SET FILTER TO estado <> '99'
     GOTO TOP
     SELECT itehc
     SET RELATION TO nummes + numhc INTO;
hoja
     SET RELATION TO nummes + numhc INTO;
compag ADDITIVE
     SET RELATION TO nummes + numhc INTO;
hojmod ADDITIVE
     SELECT itehc
     INDEX ON nummes + numhc TO  ;
           (vindi1) FOR  ;
           EMPTY(hoja.numcp)  ;
           .AND. VAL(hoja.nummes) <=  ;
           VAL(vnummes1) .AND.  ;
           itehc.estado <> '9'  ;
           .AND. itehc.tipfun =  ;
           ALLTRIM(vtipfun) .AND.  ;
           EMPTY(hoja.numanu)
     GOTO TOP
     SET FILTER TO IIF(;
.NOT. EMPTY(hoja.numhm);
.AND. hojmod.operac $ 'RC',;
.F., IIF(EMPTY(hoja.numhm),;
.T.,;
.F.));
.AND. codprg = ALLTRIM(vcodprg);
.AND. codfte = ALLTRIM(vcodfte);
.AND. codsubpr = ALLTRIM(vcodsub);
.AND. IIF(;
.NOT. EMPTY(vcodpry), codproy = ALLTRIM(vcodpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(vcodspy), codsupry = ALLTRIM(vcodspy),;
.T.);
.AND. IIF(;
.NOT. EMPTY(vcodact), codact = ALLTRIM(vcodact),;
.T.);
.AND. IIF(vcodpart = '     ',;
.T., IIF(ALLTRIM(vtipfun) = 'F', IIF(RIGHT(vcodpart,;
2) = '00', LEFT(codanal, 2) = LEFT(vcodpart,;
2), codanal = vcodpart), IIF(RIGHT(vcodpart,;
2) = '00', LEFT(codpart, 2) = LEFT(vcodpart,;
2), codpart = vcodpart)))
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          SELECT ast
          IF f_appd()
               REPLACE fecref  ;
                       WITH  ;
                       hoja.fechc,  ;
                       codanal  ;
                       WITH  ;
                       m.codanal,  ;
                       codpart  ;
                       WITH  ;
                       m.codpart,  ;
                       nummes  ;
                       WITH  ;
                       m.nummes,  ;
                       tipdoc  ;
                       WITH 'H/C',  ;
                       numref  ;
                       WITH  ;
                       m.numhc,  ;
                       codprg  ;
                       WITH  ;
                       m.codprg,  ;
                       codsubpr  ;
                       WITH  ;
                       m.codsubpr,  ;
                       codproy  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I',  ;
                       m.codproy,  ;
                       '   '),  ;
                       codsupry  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I',  ;
                       m.codsupry,  ;
                       '   '),  ;
                       codact  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I', '   ',  ;
                       m.codact),  ;
                       valpart  ;
                       WITH  ;
                       m.valpart,  ;
                       tipo WITH  ;
                       '1',  ;
                       descri  ;
                       WITH fte(),  ;
                       tipfte  ;
                       WITH  ;
                       hoja.tipdoc,  ;
                       numfte  ;
                       WITH  ;
                       hoja.numref,  ;
                       mesfte  ;
                       WITH  ;
                       hoja.nummes,  ;
                       tipope  ;
                       WITH 1,  ;
                       destino  ;
                       WITH  ;
                       hoja.destino,  ;
                       tothc WITH  ;
                       hoja.imptot,  ;
                       desfte  ;
                       WITH  ;
                       hoja.desref
          ENDIF
          UNLOCK
          SELECT itehc
     ENDSCAN
     SELECT itehc
     SET FILTER TO
     SET INDEX TO
     ERASE (vindi1)
     SELECT hoja
     SET FILTER TO estado <> '99'
     GOTO TOP
     SELECT hojmod
     SET FILTER TO estado <> '99'
     GOTO TOP
     SELECT compag
     SET FILTER TO estado <> '99'
     GOTO TOP
     SELECT itehc
     SET RELATION TO nummes + numhc INTO;
hoja
     SET RELATION TO nummes + numhc INTO;
compag ADDITIVE
     SET RELATION TO nummes + numhc INTO;
hojmod ADDITIVE
     SELECT itehc
     vindi1 = SYS(3) + '.idx'
     INDEX ON nummes + numhc TO  ;
           (vindi1) FOR  .NOT.  ;
           EMPTY(hoja.numcp)  ;
           .AND. VAL(hoja.nummes) <=  ;
           VAL(vnummes1) .AND.   ;
           .NOT.  ;
           EMPTY(hoja.numhm)  ;
           .AND. hojmod.operac <>  ;
           'RC'
     GOTO TOP
     SET FILTER TO IIF(;
.NOT. EMPTY(hoja.numcp);
.AND. VAL(hoja.nummescp) > VAL(vnummes1),;
.T.,;
.F.);
.AND. IIF(hojmod.operac $ 'T';
.AND. EMPTY(hojmod.numcp),;
.T.,;
.F.);
.AND. codprg = ALLTRIM(vcodprg);
.AND. codfte = ALLTRIM(vcodfte);
.AND. codsubpr = ALLTRIM(vcodsub);
.AND. IIF(;
.NOT. EMPTY(vcodpry), codproy = ALLTRIM(vcodpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(vcodspy), codsupry = ALLTRIM(vcodspy),;
.T.);
.AND. IIF(;
.NOT. EMPTY(vcodact), codact = ALLTRIM(vcodact),;
.T.);
.AND. itehc.estado <> '9';
.AND. tipfun = ALLTRIM(vtipfun);
.AND. IIF(vcodpart = '     ',;
.T., IIF(ALLTRIM(vtipfun) = 'F', IIF(RIGHT(vcodpart,;
2) = '00', LEFT(codanal, 2) = LEFT(vcodpart,;
2), codanal = vcodpart), IIF(RIGHT(vcodpart,;
2) = '00', LEFT(codpart, 2) = LEFT(vcodpart,;
2), codpart = vcodpart)))
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          SELECT ast
          IF f_appd()
               REPLACE fecref  ;
                       WITH  ;
                       hoja.fechc,  ;
                       codanal  ;
                       WITH  ;
                       m.codanal,  ;
                       codpart  ;
                       WITH  ;
                       m.codpart,  ;
                       nummes  ;
                       WITH  ;
                       m.nummes,  ;
                       tipdoc  ;
                       WITH 'H/C',  ;
                       numref  ;
                       WITH  ;
                       m.numhc,  ;
                       codprg  ;
                       WITH  ;
                       m.codprg,  ;
                       codsubpr  ;
                       WITH  ;
                       m.codsubpr,  ;
                       codproy  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I',  ;
                       m.codproy,  ;
                       '   '),  ;
                       codsupry  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I',  ;
                       m.codsupry,  ;
                       '   '),  ;
                       codact  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I', '   ',  ;
                       m.codact),  ;
                       valpart  ;
                       WITH  ;
                       IIF(hojmod.operac $  ;
                       'T' .AND.  ;
                       EMPTY(hojmod.numcp),  ;
                       m.valpart * - ;
                       1,  ;
                       m.valpart),  ;
                       tipo WITH  ;
                       '1',  ;
                       descri  ;
                       WITH fte(),  ;
                       tipfte  ;
                       WITH  ;
                       hoja.tipdoc,  ;
                       numfte  ;
                       WITH  ;
                       hoja.numref,  ;
                       mesfte  ;
                       WITH  ;
                       hoja.nummes,  ;
                       tipope  ;
                       WITH 1,  ;
                       destino  ;
                       WITH  ;
                       hoja.destino,  ;
                       tothc WITH  ;
                       hoja.imptot,  ;
                       desfte  ;
                       WITH  ;
                       hoja.desref
          ENDIF
          UNLOCK
          SELECT itehc
     ENDSCAN
     SET RELATION TO
     SET INDEX TO
     ERASE (vindi1)
     SELECT itehc
     SET FILTER TO
     SET INDEX TO
     ERASE (vindi1)
     SELECT hoja
     SET FILTER TO estado <> '99'
     GOTO TOP
     SELECT hojmod
     SET FILTER TO estado <> '99'
     GOTO TOP
     SELECT compag
     SET FILTER TO estado <> '99'
     GOTO TOP
     SELECT itehc
     SET RELATION TO nummes + numhc INTO;
hoja
     SET RELATION TO nummes + numhc INTO;
compag ADDITIVE
     SET RELATION TO nummes + numhc INTO;
hojmod ADDITIVE
     SELECT itehc
     vindi1 = SYS(3) + '.idx'
     INDEX ON nummes + numhc TO  ;
           (vindi1) FOR  .NOT.  ;
           EMPTY(hoja.numcp)  ;
           .AND. VAL(hoja.nummes) <=  ;
           VAL(vnummes1) .AND.  ;
           EMPTY(hoja.numhm)  ;
           .AND.  ;
           VAL(hoja.nummescp) >  ;
           VAL(vnummes1)
     GOTO TOP
     SET FILTER TO codprg = ALLTRIM(vcodprg);
.AND. codfte = ALLTRIM(vcodfte);
.AND. codsubpr = ALLTRIM(vcodsub);
.AND. itehc.estado <> '9';
.AND. tipfun = ALLTRIM(vtipfun);
.AND. IIF(;
.NOT. EMPTY(vcodpry), codproy = ALLTRIM(vcodpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(vcodspy), codsupry = ALLTRIM(vcodspy),;
.T.);
.AND. IIF(;
.NOT. EMPTY(vcodact), codact = ALLTRIM(vcodact),;
.T.);
.AND. IIF(vcodpart = '     ',;
.T., IIF(ALLTRIM(vtipfun) = 'F', IIF(RIGHT(vcodpart,;
2) = '00', LEFT(codanal, 2) = LEFT(vcodpart,;
2), codanal = vcodpart), IIF(RIGHT(vcodpart,;
2) = '00', LEFT(codpart, 2) = LEFT(vcodpart,;
2), codpart = vcodpart)))
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          SELECT ast
          IF f_appd()
               REPLACE fecref  ;
                       WITH  ;
                       hoja.fechc,  ;
                       codanal  ;
                       WITH  ;
                       m.codanal,  ;
                       codpart  ;
                       WITH  ;
                       m.codpart,  ;
                       nummes  ;
                       WITH  ;
                       m.nummes,  ;
                       tipdoc  ;
                       WITH 'H/C',  ;
                       numref  ;
                       WITH  ;
                       m.numhc,  ;
                       codprg  ;
                       WITH  ;
                       m.codprg,  ;
                       codsubpr  ;
                       WITH  ;
                       m.codsubpr,  ;
                       codproy  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I',  ;
                       m.codproy,  ;
                       '   '),  ;
                       codsupry  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I',  ;
                       m.codsupry,  ;
                       '   '),  ;
                       codact  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I', '   ',  ;
                       m.codact),  ;
                       tipo WITH  ;
                       '1',  ;
                       descri  ;
                       WITH fte(),  ;
                       tipfte  ;
                       WITH  ;
                       hoja.tipdoc,  ;
                       numfte  ;
                       WITH  ;
                       hoja.numref,  ;
                       mesfte  ;
                       WITH  ;
                       hoja.nummes,  ;
                       tipope  ;
                       WITH 1,  ;
                       destino  ;
                       WITH  ;
                       hoja.destino,  ;
                       tothc WITH  ;
                       hoja.imptot,  ;
                       desfte  ;
                       WITH  ;
                       hoja.desref
               IF compag.partret =  ;
                  m.codanal .AND.  ;
                  compag.reten >  ;
                  0
                    REPLACE valpart  ;
                            WITH  ;
                            m.valpart -  ;
                            compag.reten
               ELSE
                    REPLACE valpart  ;
                            WITH  ;
                            m.valpart
               ENDIF
          ENDIF
          UNLOCK
          SELECT itehc
     ENDSCAN
     SET INDEX TO
     ERASE (vindi1)
     SELECT reten
     vindi2 = SYS(3) + '.idx'
     INDEX ON nummeshc + numhc TO  ;
           (vindi2) FOR  ;
           (EMPTY(reten.conpago)  ;
           .OR.  ;
           VAL(reten.mescppg) >  ;
           VAL(vnummes1)) .AND.   ;
           .NOT.  ;
           EMPTY(reten.numhc)
     GOTO TOP
     SELECT itehc
     SET RELATION TO nummes + numhc INTO;
reten ADDITIVE
     SELECT itehc
     vindi1 = SYS(3) + '.idx'
     INDEX ON nummes + numhc TO  ;
           (vindi1) FOR  .NOT.  ;
           EMPTY(hoja.numcp)  ;
           .AND. VAL(hoja.nummes) <=  ;
           VAL(vnummes1) .AND.   ;
           .NOT.  ;
           EMPTY(reten.numhc)
     GOTO TOP
     SET FILTER TO codprg = ALLTRIM(vcodprg);
.AND. codfte = ALLTRIM(vcodfte);
.AND. codsubpr = ALLTRIM(vcodsub);
.AND. itehc.estado <> '9';
.AND. tipfun = ALLTRIM(vtipfun);
.AND. IIF(;
.NOT. EMPTY(vcodpry), codproy = ALLTRIM(vcodpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(vcodspy), codsupry = ALLTRIM(vcodspy),;
.T.);
.AND. IIF(;
.NOT. EMPTY(vcodact), codact = ALLTRIM(vcodact),;
.T.);
.AND. IIF(vcodpart = '     ',;
.T., IIF(ALLTRIM(vtipfun) = 'F', IIF(RIGHT(vcodpart,;
2) = '00', LEFT(codanal, 2) = LEFT(vcodpart,;
2), codanal = vcodpart), IIF(RIGHT(vcodpart,;
2) = '00', LEFT(codpart, 2) = LEFT(vcodpart,;
2), codpart = vcodpart)))
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          SELECT ast
          IF f_appd()
               REPLACE fecref  ;
                       WITH  ;
                       hoja.fechc,  ;
                       codanal  ;
                       WITH  ;
                       m.codanal,  ;
                       codpart  ;
                       WITH  ;
                       m.codpart,  ;
                       nummes  ;
                       WITH  ;
                       m.nummes,  ;
                       tipdoc  ;
                       WITH 'H/C',  ;
                       numref  ;
                       WITH  ;
                       m.numhc,  ;
                       codprg  ;
                       WITH  ;
                       m.codprg,  ;
                       codsubpr  ;
                       WITH  ;
                       m.codsubpr,  ;
                       codproy  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I',  ;
                       m.codproy,  ;
                       '   '),  ;
                       codsupry  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I',  ;
                       m.codsupry,  ;
                       '   '),  ;
                       codact  ;
                       WITH  ;
                       IIF(ALLTRIM(vtipfun) =  ;
                       'I', '   ',  ;
                       m.codact),  ;
                       valpart  ;
                       WITH  ;
                       reten.valret,  ;
                       tipo WITH  ;
                       '1',  ;
                       descri  ;
                       WITH  ;
                       'RET - ' +  ;
                       fte(),  ;
                       tipfte  ;
                       WITH  ;
                       hoja.tipdoc,  ;
                       numfte  ;
                       WITH  ;
                       hoja.numref,  ;
                       mesfte  ;
                       WITH  ;
                       hoja.nummes,  ;
                       tipope  ;
                       WITH 1,  ;
                       destino  ;
                       WITH  ;
                       hoja.destino,  ;
                       tothc WITH  ;
                       hoja.imptot,  ;
                       desfte  ;
                       WITH  ;
                       hoja.desref
          ENDIF
          UNLOCK
          SELECT itehc
     ENDSCAN
     SET RELATION TO
     SET INDEX TO
     ERASE (vindi1)
     SELECT ast
     GOTO TOP
     DEACTIVATE WINDOW standby
     m.codfte = ALLTRIM(vcodfte)
     SET UNIQUE ON
     IF ALLTRIM(vtipfun) = 'F'
          INDEX ON LEFT(codanal,  ;
                2) TO (vind)
     ELSE
          INDEX ON LEFT(codpart,  ;
                2) TO (vind)
     ENDIF
     SET INDEX TO (vind)
     xdbf = SYS(3) + '.dbf'
     COPY TO (xdbf)
     USE IN 18 (xdbf) ALIAS ast1
     SET UNIQUE OFF
     vind = SYS(3) + '.IDX'
     IF ALLTRIM(vtipfun) = 'F'
          INDEX ON codanal TO  ;
                (vind)
     ELSE
          INDEX ON codpart TO  ;
                (vind)
     ENDIF
     SET INDEX TO (vind)
     SELECT ast1
     GOTO TOP
     SCAN
          IF ALLTRIM(vtipfun) =  ;
             'F'
               vf = LEFT(codanal,  ;
                    2)
               SELECT ast
               SUM FOR  ;
                   LEFT(codanal,  ;
                   2) = vf  ;
                   valpart *  ;
                   tipope TO sw
               IF f_appd()
                    REPLACE codanal  ;
                            WITH  ;
                            vf +  ;
                            '.00',  ;
                            valpart  ;
                            WITH  ;
                            sw,  ;
                            tipope  ;
                            WITH  ;
                            1
               ENDIF
          ENDIF
          SELECT ast1
     ENDSCAN
     SELECT ast
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No se tiene registros a Procesar'
     ELSE
          IF ALLTRIM(vtipfun) =  ;
             'F'
               DO reporte WITH 2,  ;
                  'ASIGF',  ;
                  ' Auxiliar de Standar'
          ELSE
               INDEX ON nummes +  ;
                     codsubpr +  ;
                     codproy +  ;
                     codsupry +  ;
                     codpart +  ;
                     tipo +  ;
                     tipdoc +  ;
                     numref TO  ;
                     (vind)
               DO reporte WITH 2,  ;
                  'ASIGI',  ;
                  ' Auxiliar de Standar'
          ENDIF
     ENDIF
ENDIF
SET FILTER TO
CLOSE INDEX
ERASE (vind)
CLOSE DATABASES
RETURN
*
FUNCTION fte
devul = '**'
DO CASE
     CASE tipdoc = 'H/C'
          devul = IIF(hoja.tipprv =  ;
                  'P',  ;
                  val_pro(hoja.codprv, ;
                  '20'),  ;
                  IIF(hoja.tipprv =  ;
                  'E',  ;
                  val_pro(hoja.codemp, ;
                  '30'),  ;
                  hoja.nombre))
     CASE tipdoc = 'C/P'
          devul = IIF(compag.tipprv =  ;
                  'P',  ;
                  val_pro(compag.codprv, ;
                  '20'),  ;
                  IIF(compag.tipprv =  ;
                  'E',  ;
                  val_pro(compag.codemp, ;
                  '30'),  ;
                  compag.nompre))
     CASE tipdoc = 'P/A'
          devul = 'PARTE DE ANULACION DE LA ' +  ;
                  tipfte + ' ' +  ;
                  numfte
     CASE tipdoc = 'PCH'
          devul = 'PARTE DE ANULACION DE CHEQUE DE ' +  ;
                  tipfte + ' ' +  ;
                  numfte
ENDCASE
RETURN devul
*
FUNCTION ftehm
vdescr = ' ** '
DO CASE
     CASE hojmod.operac = 'T'
          vdescr = 'Transf.Int.de la H/C: ' +  ;
                   hojmod.nummeshc +  ;
                   '.' +  ;
                   hojmod.numhc +  ;
                   ' ' +  ;
                   val_para(hojmod.tiphm, ;
                   'HOJMOD','D', ;
                   22,18)
     CASE hojmod.operac = 'R'
          vdescr = 'Rebaja de la H/C : ' +  ;
                   hojmod.nummeshc +  ;
                   '.' +  ;
                   hojmod.numhc +  ;
                   ' ' +  ;
                   val_para(hojmod.tiphm, ;
                   'HOJMOD','D', ;
                   22,18)
     CASE hojmod.operac = 'C'
          vdescr = 'Cambio Fte. de la H/C: ' +  ;
                   hojmod.nummeshc +  ;
                   '.' +  ;
                   hojmod.numhc +  ;
                   ' ' +  ;
                   val_para(hojmod.tiphm, ;
                   'HOJMOD','D', ;
                   22,18)
ENDCASE
RETURN vdescr
*
FUNCTION xfte
devul = '**'
DO CASE
     CASE tipdoc = 'H/C'
          devul = IIF(hoja.tipprv =  ;
                  'P',  ;
                  val_pro(hoja.codprv, ;
                  '20'),  ;
                  IIF(hoja.tipprv =  ;
                  'E',  ;
                  val_pro(hoja.codemp, ;
                  '30'),  ;
                  hoja.nombre))
     CASE tipdoc = 'C/P'
          devul = compag.ccnombre
          devul = IIF(compag.tipprv =  ;
                  'P',  ;
                  val_prv(compag.codprv),  ;
                  IIF(compag.tipprv =  ;
                  'E',  ;
                  val_pro(compag.codemp),  ;
                  compag.nompre))
     CASE tipdoc = 'P/A'
          devul = 'PARTE DE ANULACION DE LA ' +  ;
                  tipfte + ' ' +  ;
                  numfte
     CASE tipdoc = 'PCH'
          devul = 'PARTE DE ANULACION DE CHEQUE DE ' +  ;
                  tipfte + ' ' +  ;
                  numfte
ENDCASE
RETURN devul
*
FUNCTION val_pro
PARAMETER xcod, _tipo
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
_campo = VARREAD()
malias = ALIAS()
SELECT auxil
_oldwnd = WOUTPUT()
SET ORDER TO AUXIL1
SEEK _tipo + xcod
v_fun = IIF(FOUND(), descri,  ;
        'No encontrado')
SELECT (malias)
RETURN v_fun
*
FUNCTION tiphmcd
PRIVATE vfun
IF (operac = 'C' .AND. tipfte =  ;
   'CYP' .AND. tipope <> -1) .OR.  ;
   (operac = 'C' .AND. tipfte =  ;
   'C  ' .AND. tipope <> -1) .OR.  ;
   (operac = 'T' .AND. tipfte =  ;
   'CYP' .AND. tipope <> -1) .OR.  ;
   (operac = 'T' .AND. tipfte =  ;
   'C  ' .AND. tipope <> -1) .OR.  ;
   (operac = 'R' .AND. tipfte =  ;
   'C  ' .AND. tipope = -1)
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION tiphmch
PRIVATE vfun
IF (operac = 'C' .AND. tipfte =  ;
   'CYP' .AND. tipope = -1) .OR.  ;
   (operac = 'C' .AND. tipfte =  ;
   'C  ' .AND. tipope = -1) .OR.  ;
   (operac = 'T' .AND. tipfte =  ;
   'CYP' .AND. tipope = -1) .OR.  ;
   (operac = 'T' .AND. tipfte =  ;
   'C  ' .AND. tipope = -1) .OR.  ;
   (operac = 'R' .AND. tipfte =  ;
   'CYP' .AND. tipope = -1) .OR.  ;
   (operac = 'R' .AND. tipfte =  ;
   'C  ' .AND. tipope = -1)
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION tiphmad
PRIVATE vfun
IF (operac = 'C' .AND. tipfte =  ;
   'CYP') .OR. (operac = 'C'  ;
   .AND. tipfte = 'C  ') .OR.  ;
   (operac = 'T' .AND. tipfte =  ;
   'CYP') .OR. (operac = 'T'  ;
   .AND. tipfte = 'C  ' .AND.  ;
   tipope = -1) .OR. (operac =  ;
   'T' .AND. tipfte = 'P  ' .AND.  ;
   tipope <> -1) .OR. (operac =  ;
   'R' .AND. tipfte = 'CYP' .AND.  ;
   tipope = -1)
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION tiphmah
PRIVATE vfun
IF (operac = 'C' .AND. tipfte =  ;
   'CYP') .OR. (operac = 'C'  ;
   .AND. tipfte = 'P  ') .OR.  ;
   (operac = 'T' .AND. tipfte =  ;
   'CYP') .OR. (operac = 'T'  ;
   .AND. tipfte = 'C  ' .AND.  ;
   tipope <> -1) .OR. (operac =  ;
   'T' .AND. tipfte = 'P  ' .AND.  ;
   tipope = -1) .OR. (operac =  ;
   'R' .AND. tipfte = 'CYP' .AND.  ;
   tipope = -1) .OR. (operac =  ;
   'R' .AND. tipfte = 'P  ' .AND.  ;
   tipope = -1)
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION tiphmed
PRIVATE vfun
IF (operac = 'C' .AND. tipfte =  ;
   'CYP' .AND. tipope = -1) .OR.  ;
   (operac = 'C' .AND. tipfte =  ;
   'P  ' .AND. tipope = -1) .OR.  ;
   (operac = 'T' .AND. tipfte =  ;
   'CYP' .AND. tipope = -1) .OR.  ;
   (operac = 'T' .AND. tipfte =  ;
   'P  ' .AND. tipope = -1) .OR.  ;
   (operac = 'R' .AND. tipfte =  ;
   'CYP' .AND. tipope = -1) .OR.  ;
   (operac = 'R' .AND. tipfte =  ;
   'P  ' .AND. tipope = -1)
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION tiphmeh
PRIVATE vfun
IF (operac = 'C' .AND. tipfte =  ;
   'CYP' .AND. tipope <> -1) .OR.  ;
   (operac = 'C' .AND. tipfte =  ;
   'P  ' .AND. tipope <> -1) .OR.  ;
   (operac = 'T' .AND. tipfte =  ;
   'CYP' .AND. tipope <> -1) .OR.  ;
   (operac = 'T' .AND. tipfte =  ;
   'P  ' .AND. tipope <> -1)
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION buscprg
PRIVATE vkey
zx = ALIAS()
qw = ORDER()
vkey = ALLTRIM(vperiodo) +  ;
       ALLTRIM(vcodprg) +  ;
       ALLTRIM(vcodsub) +  ;
       ALLTRIM(vcodpry)
SELECT maepre
SET ORDER TO maepre2
SEEK vkey
vproyecto = maepre.descri
SELECT (zx)
SET ORDER TO (qw)
RETURN (vproyecto)
*
FUNCTION buscact
PARAMETER vact
PRIVATE as
as = ALIAS()
PRIVATE vkey
vkey = ALLTRIM(vperiodo) +  ;
       ALLTRIM(codprg) +  ;
       ALLTRIM(codsubpr) +  ;
       ALLTRIM(vact)
SELECT maepre
SET ORDER TO maepre3
SEEK vkey
vactividad = IIF(FOUND(),  ;
             maepre.descri, '?')
SELECT (as)
RETURN (vactividad)
*
