USE IN 1 HojCon ALIAS hoja ORDER  ;
    HojCon1
USE IN 2 Itehc ALIAS itehc ORDER  ;
    Itehc1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 5 ItePar ALIAS itepar  ;
    ORDER ItePar1
USE IN 7 Promae ALIAS promae  ;
    ORDER Promae1
USE IN 8 Calen ALIAS calen ORDER  ;
    calen1
USE IN 10 Clase ALIAS clase ORDER  ;
    Clase1
USE IN 11 Maepre ALIAS maepre  ;
    ORDER Maepre1
USE IN 13 astpre ALIAS astpre  ;
    ORDER Astpre5
USE IN 14 Auxil ALIAS auxil ORDER  ;
    Auxil2
USE IN 15 Astaux_P ALIAS ast_p
USE IN 16 Astaux ALIAS ast
SELECT astpre
SET RELATION TO nummes + numref INTO itehc
DO inicia
CLOSE DATABASES
RETURN
*
PROCEDURE inicia
IF EOF()
     DO standby WITH  ;
        'No Existe registros a Procesar'
     RETURN
ENDIF
vtemp = RECNO()
as = ORDER()
vperiodo = '  '
vnummes2 = '  '
vtipfun = ' '
vcodprg = '  '
vcodfte = '   '
vcodsub = '   '
vsubpry = '  '
vproyec = '   '
vcodact = '  '
vcodpart = '00.00'
DEFINE WINDOW lis FROM 5, 12 TO  ;
       18, 70 FLOAT TITLE  ;
       ' °° Auxiliar Standard °° '  ;
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
@ 2, 21 GET vnummes2 PICTURE '!!'  ;
  VALID val_para(vnummes2, ;
  'FECMES','C',31,20,4) .AND.   ;
  .NOT. EMPTY(vnummes2)
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
  VALID val_subp(vcodsub,'CODSUB' +  ;
  vcodprg,' ',21,28) .OR.  ;
  EMPTY(vcodprg)
@ 7, 21 GET vproyec PICTURE '!!!'  ;
  WHEN ALLTRIM(vtipfun) = 'I'
@ 8, 21 GET vsubpry PICTURE '!!'  ;
  WHEN ALLTRIM(vtipfun) = 'I'
@ 9, 21 GET vcodact PICTURE '!!'  ;
  WHEN ALLTRIM(vtipfun) = 'F'
@ 10, 21 GET vcodpart PICTURE  ;
  '!!.!!'
READ VALID val_read()
DEACTIVATE WINDOW lis
IF EMPTY(vnummes2) .OR. LASTKEY() =  ;
   27
     RETURN
ELSE
     ACTIVATE WINDOW standby
     @ 1, 14 SAY  ;
       'Espere un Momento ....'  ;
       COLOR W/N* 
     m.tipfun = ALLTRIM(vtipfun)
     m.periodo = ALLTRIM(vperiodo)
     m.codfte = ALLTRIM(vcodfte)
     SELECT ast
     vdbf = SYS(3) + '.dbf'
     COPY TO (vdbf) STRUCTURE
     USE IN 16 EXCLUSIVE (vdbf)  ;
         ALIAS ast
     SELECT ast_p
     vdbf = SYS(3) + '.dbf'
     xidx = SYS(3) + '.idx'
     COPY TO (vdbf) STRUCTURE
     USE IN 15 EXCLUSIVE (vdbf)  ;
         ALIAS ast_p
     SELECT ast_p
     IF ALLTRIM(vtipfun) = 'I'
          INDEX ON codsubpr +  ;
                codproy +  ;
                codsupry +  ;
                codpart TO  ;
                (xidx)
     ELSE
          INDEX ON codsubpr +  ;
                codact + codanal  ;
                TO (xidx)
     ENDIF
     vind1 = SYS(3) + '.IDX'
     vind2 = SYS(3) + '.IDX'
     vind3 = SYS(3) + '.IDX'
     vind4 = SYS(3) + '.IDX'
     vind5 = SYS(3) + '.IDX'
     IF ALLTRIM(vtipfun) = 'F'
          xvalor = vperiodo +  ;
                   ALLTRIM(vcodprg) +  ;
                   ALLTRIM(vcodsub)
          proact = ALLTRIM(vcodact)
     ELSE
          xvalor = vperiodo +  ;
                   ALLTRIM(vcodprg) +  ;
                   ALLTRIM(vcodsub) +  ;
                   ALLTRIM(vproyec)
          proact = ALLTRIM(vsubpry)
     ENDIF
     SELECT itehc
     IF ALLTRIM(vtipfun) = 'I'
          SELECT itehc.numhc,  ;
                 itehc.nummes,  ;
                 itehc.codpart,  ;
                 itehc.estado,  ;
                 itehc.tipope,  ;
                 itehc.valpart,  ;
                 itehc.codproy,  ;
                 itehc.tipdoc,  ;
                 itehc.codsupry,  ;
                 itehc.codprg,  ;
                 itehc.codsubpr,  ;
                 itehc.codfte,  ;
                 itehc.tipfun,  ;
                 itehc.totcal,  ;
                 itehc.nummeshm,  ;
                 itehc.numhm,  ;
                 itehc.mespa,  ;
                 itehc.numpa,  ;
                 itehc.mespr,  ;
                 itehc.numpr,  ;
                 itehc.operac,  ;
                 itehc.tipcom,  ;
                 itehc.mesocos  ;
                 FROM ITEHC WHERE  ;
                 tipfun + codfte +  ;
                 periodo + codprg +  ;
                 IIF( NOT  ;
                 EMPTY(vcodsub),  ;
                 codsubpr, '') +  ;
                 IIF( NOT  ;
                 EMPTY(vproyec),  ;
                 codproy, '') +  ;
                 IIF( NOT  ;
                 EMPTY(proact),  ;
                 codsupry, '') =  ;
                 ALLTRIM(m.tipfun) +  ;
                 ALLTRIM(m.codfte) +  ;
                 xvalor + IIF(  ;
                 NOT  ;
                 EMPTY(proact),  ;
                 proact, '') AND  ;
                 itehc.estado <>  ;
                 '99' AND  ;
                 itehc.tipfun =  ;
                 'I' AND  ;
                 IIF(estado =  ;
                 '92',  ;
                 VAL(nummeshm) =  ;
                 VAL(vnummes2),  ;
                 VAL(nummes) =  ;
                 VAL(vnummes2))  ;
                 AND IIF( NOT  ;
                 EMPTY(mespa),  ;
                 VAL(mespa) =  ;
                 VAL(vnummes2),  ;
                 .T.) AND IIF(  ;
                 NOT EMPTY(mespr),  ;
                 VAL(mespr) =  ;
                 VAL(vnummes2),  ;
                 .T.) AND IIF(  ;
                 NOT  ;
                 EMPTY(nummeshm),  ;
                 VAL(nummeshm) <=  ;
                 VAL(vnummes2),  ;
                 .T.) AND  ;
                 IIF(RIGHT(vcodpart,  ;
                 2) = '00',  ;
                 LEFT(codpart, 2) =  ;
                 LEFT(vcodpart,  ;
                 2), codpart =  ;
                 vcodpart) INTO  ;
                 CURSOR Hojax
     ELSE
          SELECT itehc.numhc,  ;
                 itehc.nummes,  ;
                 itehc.codanal,  ;
                 itehc.estado,  ;
                 itehc.tipope,  ;
                 itehc.valpart,  ;
                 itehc.tipdoc,  ;
                 itehc.codsubpr,  ;
                 itehc.codprg,  ;
                 itehc.codact,  ;
                 itehc.codfte,  ;
                 itehc.tipfun,  ;
                 itehc.totcal,  ;
                 itehc.nummeshm,  ;
                 itehc.numhm,  ;
                 itehc.mespa,  ;
                 itehc.numpa,  ;
                 itehc.mespr,  ;
                 itehc.numpr,  ;
                 itehc.operac,  ;
                 itehc.tipcom,  ;
                 itehc.mesocos  ;
                 FROM ITEHC WHERE  ;
                 tipfun + codfte +  ;
                 periodo + codprg +  ;
                 IIF( NOT  ;
                 EMPTY(vcodsub),  ;
                 codsubpr, '') +  ;
                 IIF( NOT  ;
                 EMPTY(proact),  ;
                 codact, ' ') =  ;
                 ALLTRIM(m.tipfun) +  ;
                 ALLTRIM(m.codfte) +  ;
                 xvalor + IIF(  ;
                 NOT  ;
                 EMPTY(proact),  ;
                 proact, ' ') AND  ;
                 itehc.estado <>  ;
                 '99' AND  ;
                 itehc.tipfun =  ;
                 'F' AND  ;
                 IIF(estado =  ;
                 '92',  ;
                 VAL(nummeshm) =  ;
                 VAL(vnummes2),  ;
                 VAL(nummes) =  ;
                 VAL(vnummes2))  ;
                 AND IIF( NOT  ;
                 EMPTY(mespa),  ;
                 VAL(mespa) =  ;
                 VAL(vnummes2),  ;
                 .T.) AND IIF(  ;
                 NOT EMPTY(mespr),  ;
                 VAL(mespr) =  ;
                 VAL(vnummes2),  ;
                 .T.) AND IIF(  ;
                 NOT  ;
                 EMPTY(nummeshm),  ;
                 VAL(nummeshm) <=  ;
                 VAL(vnummes2),  ;
                 .T.) AND  ;
                 IIF(RIGHT(vcodpart,  ;
                 2) = '00',  ;
                 LEFT(codanal, 2) =  ;
                 LEFT(vcodpart,  ;
                 2), codanal =  ;
                 vcodpart) INTO  ;
                 CURSOR HOJAX
     ENDIF
     vind = SYS(3) + '.DBF'
     jind = SYS(3) + '.IDX'
     COPY TO (vind)
     USE IN 24 EXCLUSIVE (vind)  ;
         ALIAS hoja1
     SELECT hoja1
     INDEX ON nummes + numhc TO  ;
           (jind)
     SET RELATION TO nummes + numhc INTO;
hoja
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          SELECT ast
          IF f_appd()
               REPLACE nummes  ;
                       WITH  ;
                       IIF(m.estado =  ;
                       '94',  ;
                       m.nummes,  ;
                       IIF(m.estado =  ;
                       '92',  ;
                       m.nummeshm,  ;
                       m.nummes)),  ;
                       numref  ;
                       WITH  ;
                       IIF(m.tipdoc =  ;
                       'P/R',  ;
                       m.numpr,  ;
                       IIF(m.tipdoc =  ;
                       'H/M',  ;
                       m.numhm,  ;
                       m.numhc)),  ;
                       mesfte  ;
                       WITH  ;
                       m.nummes,  ;
                       numfte  ;
                       WITH  ;
                       IIF(m.tipdoc $  ;
                       'H/MP/R',  ;
                       hoja.numhc,  ;
                       hoja.numref),  ;
                       tipfte  ;
                       WITH  ;
                       IIF(m.tipdoc $  ;
                       'H/MP/R',  ;
                       'H/C',  ;
                       hoja.tipdoc),  ;
                       tipdoc  ;
                       WITH  ;
                       IIF(m.tipdoc =  ;
                       'P/R',  ;
                       'P/R',  ;
                       IIF(m.tipdoc =  ;
                       'H/M',  ;
                       'H/M',  ;
                       'H/C')),  ;
                       codprg  ;
                       WITH  ;
                       m.codprg,  ;
                       codsubpr  ;
                       WITH  ;
                       m.codsubpr,  ;
                       codproy  ;
                       WITH  ;
                       IIF(vtipfun =  ;
                       'I',  ;
                       m.codproy,  ;
                       '   '),  ;
                       codsupry  ;
                       WITH  ;
                       IIF(vtipfun =  ;
                       'I',  ;
                       m.codsupry,  ;
                       '   '),  ;
                       codact  ;
                       WITH  ;
                       IIF(vtipfun =  ;
                       'F',  ;
                       m.codact,  ;
                       '  '),  ;
                       valpart  ;
                       WITH  ;
                       m.valpart,  ;
                       mesocos  ;
                       WITH  ;
                       m.mesocos,  ;
                       mespr WITH  ;
                       m.mespr,  ;
                       mespa WITH  ;
                       m.mespa,  ;
                       tipo WITH  ;
                       IIF(m.tipdoc =  ;
                       'P/A', '4',  ;
                       IIF(m.tipdoc =  ;
                       'P/R', '5',  ;
                       IIF(m.tipdoc =  ;
                       'H/M', '3',  ;
                       '1'))),  ;
                       fecref  ;
                       WITH  ;
                       hoja.fechc,  ;
                       codcal  ;
                       WITH  ;
                       hoja.codcal,  ;
                       operac  ;
                       WITH  ;
                       m.operac,  ;
                       tipcom  ;
                       WITH  ;
                       m.tipcom,  ;
                       tipope  ;
                       WITH  ;
                       IIF(m.tipdoc =  ;
                       'P/A', -1,  ;
                       IIF(m.tipdoc =  ;
                       'P/R', -1,  ;
                       IIF(m.tipdoc =  ;
                       'H/M',  ;
                       IIF(m.tipope =  ;
                       '-', -1,  ;
                       1), 1))),  ;
                       descri  ;
                       WITH fte(),  ;
                       desfte  ;
                       WITH  ;
                       hoja.desref,  ;
                       destino  ;
                       WITH  ;
                       hoja.destino
               IF ALLTRIM(vtipfun) =  ;
                  'F'
                    REPLACE codanal  ;
                            WITH  ;
                            m.codanal
               ELSE
                    REPLACE codpart  ;
                            WITH  ;
                            m.codpart
               ENDIF
          ENDIF
          UNLOCK
          IF  .NOT.  ;
              EMPTY(m.numpa)  ;
              .AND. mespa <=  ;
              ALLTRIM(vnummes2)
               IF f_appd()
                    REPLACE nummes  ;
                            WITH  ;
                            m.nummes,  ;
                            tipdoc  ;
                            WITH  ;
                            'P/A',  ;
                            numref  ;
                            WITH  ;
                            m.numpa,  ;
                            codprg  ;
                            WITH  ;
                            m.codprg,  ;
                            codsubpr  ;
                            WITH  ;
                            m.codsubpr,  ;
                            codproy  ;
                            WITH  ;
                            IIF(vtipfun =  ;
                            'I',  ;
                            m.codproy,  ;
                            '   ' ;
                            ),  ;
                            codsupry  ;
                            WITH  ;
                            IIF(vtipfun =  ;
                            'I',  ;
                            m.codsupry,  ;
                            '   ' ;
                            ),  ;
                            codact  ;
                            WITH  ;
                            IIF(vtipfun =  ;
                            'F',  ;
                            m.codact,  ;
                            '  '),  ;
                            valpart  ;
                            WITH  ;
                            m.valpart,  ;
                            mesocos  ;
                            WITH  ;
                            m.mesocos,  ;
                            mespa  ;
                            WITH  ;
                            m.mespa,  ;
                            tipo  ;
                            WITH  ;
                            '4',  ;
                            tipfte  ;
                            WITH  ;
                            'H/C',  ;
                            fecref  ;
                            WITH  ;
                            hoja.fechc,  ;
                            codcal  ;
                            WITH  ;
                            hoja.codcal,  ;
                            numfte  ;
                            WITH  ;
                            hoja.numhc,  ;
                            mesfte  ;
                            WITH  ;
                            hoja.nummes,  ;
                            operac  ;
                            WITH  ;
                            m.operac,  ;
                            tipcom  ;
                            WITH  ;
                            m.tipcom,  ;
                            tipope  ;
                            WITH - ;
                            1,  ;
                            descri  ;
                            WITH  ;
                            fte(),  ;
                            desfte  ;
                            WITH  ;
                            hoja.desref,  ;
                            destino  ;
                            WITH  ;
                            hoja.destino
                    IF ALLTRIM(vtipfun) =  ;
                       'F'
                         REPLACE codanal  ;
                                 WITH  ;
                                 m.codanal
                    ELSE
                         REPLACE codpart  ;
                                 WITH  ;
                                 m.codpart
                    ENDIF
               ENDIF
               UNLOCK
          ENDIF
          SELECT hoja1
     ENDSCAN
     SET RELATION TO
     USE IN 1
     USE IN 2
     SELECT ast
     GOTO TOP
     DEACTIVATE WINDOW standby
     m.codfte = ALLTRIM(vcodfte)
     aind = SYS(3) + '.idx'
     IF RIGHT(vcodpart, 2) = '00'
          INDEX ON nummes + tipo +  ;
                tipdoc + numref +  ;
                IIF(ALLTRIM(vtipfun) =  ;
                'I', LEFT(codpart,  ;
                2), LEFT(codanal,  ;
                2)) TO (aind)
     ELSE
          INDEX ON nummes + tipo +  ;
                tipdoc + numref +  ;
                IIF(ALLTRIM(vtipfun) =  ;
                'I', codpart,  ;
                codanal) TO  ;
                (aind)
     ENDIF
     IF EOF()
          DO standby WITH  ;
             'No se tiene registros a Procesar'
     ELSE
          DO reporte WITH 2,  ;
             'SALAXS',  ;
             ' Auxiliar de Standar'
     ENDIF
     SET UNIQUE OFF
ENDIF
SET FILTER TO
CLOSE INDEX
ERASE (aind)
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
     CASE tipdoc = 'P/R'
          devul = 'PARTE DE REBAJA DE LA ' +  ;
                  tipfte + ' ' +  ;
                  numfte
     CASE tipdoc = 'H/M'
          DO CASE
               CASE operac = 'T'
                    devul = 'Transf.Int.de la H/C: ' +  ;
                            nummes +  ;
                            '.' +  ;
                            numhc +  ;
                            ' ' +  ;
                            val_para(tipcom, ;
                            'HOJMOD', ;
                            'D', ;
                            22, ;
                            18)
               CASE operac = 'R'
                    devul = 'Rebaja de la H/C : ' +  ;
                            nummes +  ;
                            '.' +  ;
                            numhc +  ;
                            ' ' +  ;
                            val_para(tipcom, ;
                            'HOJMOD', ;
                            'D', ;
                            22, ;
                            18)
               CASE operac = 'C'
                    devul = 'Cambio Fte. de la H/C: ' +  ;
                            nummes +  ;
                            '.' +  ;
                            numhc +  ;
                            ' ' +  ;
                            val_para(tipcom, ;
                            'HOJMOD', ;
                            'D', ;
                            22, ;
                            18)
               CASE operac = 'A'
                    devul = 'Anulaci¢n del Compromiso de la H/C: ' +  ;
                            nummes +  ;
                            '.' +  ;
                            numhc +  ;
                            ' ' +  ;
                            val_para(tipcom, ;
                            'HOJMOD', ;
                            'D', ;
                            22, ;
                            18)
          ENDCASE
ENDCASE
RETURN devul
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
vfun = .F.
IF tipo = '3'
     IF (operac = 'C' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope <> -1) .OR.  ;
        (operac = 'C' .AND.  ;
        tipcom = 'C  ' .AND.  ;
        tipope <> -1) .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope <> -1) .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'C  ' .AND.  ;
        tipope <> -1)
          vfun = .T.
     ENDIF
ENDIF
RETURN vfun
*
FUNCTION tiphmch
PRIVATE vfun
vfun = .F.
IF tipo = '3'
     IF (operac = 'C' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope = -1) .OR. (operac =  ;
        'C' .AND. tipcom = 'C  '  ;
        .AND. tipope = -1) .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope = -1) .OR. (operac =  ;
        'T' .AND. tipcom = 'C  '  ;
        .AND. tipope = -1) .OR.  ;
        (operac = 'R' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope = -1) .OR. (operac =  ;
        'R' .AND. tipcom = 'C  '  ;
        .AND. tipope = -1) .OR.  ;
        (operac = 'A' .AND.  ;
        tipcom = 'C  ')
          vfun = .T.
     ENDIF
ENDIF
RETURN vfun
*
FUNCTION tiphmad
PRIVATE vfun
vfun = .F.
IF tipo = '3'
     IF (operac = 'C' .AND.  ;
        tipcom = 'CYP') .OR.  ;
        (operac = 'C' .AND.  ;
        tipcom = 'C  ') .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'CYP') .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'C  ' .AND.  ;
        tipope = -1) .OR. (operac =  ;
        'T' .AND. tipcom = 'P  '  ;
        .AND. tipope <> -1) .OR.  ;
        (operac = 'R' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope = -1) .OR. (operac =  ;
        'R' .AND. tipcom = 'C  '  ;
        .AND. tipope = -1) .OR.  ;
        (operac = 'A' .AND.  ;
        tipcom = 'C  ')
          vfun = .T.
     ENDIF
ENDIF
RETURN vfun
*
FUNCTION tiphmah
PRIVATE vfun
vfun = .F.
IF tipo = '3'
     IF (operac = 'C' .AND.  ;
        tipcom = 'CYP') .OR.  ;
        (operac = 'C' .AND.  ;
        tipcom = 'P  ') .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'CYP') .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'C  ' .AND.  ;
        tipope <> -1) .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'P  ' .AND.  ;
        tipope = -1) .OR. (operac =  ;
        'R' .AND. tipcom = 'CYP'  ;
        .AND. tipope = -1) .OR.  ;
        (operac = 'R' .AND.  ;
        tipcom = 'P  ' .AND.  ;
        tipope = -1)
          vfun = .T.
     ENDIF
ENDIF
RETURN vfun
*
FUNCTION tiphmed
PRIVATE vfun
vfun = .F.
IF tipo = '3'
     IF (operac = 'C' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope = -1) .OR. (operac =  ;
        'C' .AND. tipcom = 'P  '  ;
        .AND. tipope = -1) .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope = -1) .OR. (operac =  ;
        'T' .AND. tipcom = 'P  '  ;
        .AND. tipope = -1) .OR.  ;
        (operac = 'R' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope = -1) .OR. (operac =  ;
        'R' .AND. tipcom = 'P  '  ;
        .AND. tipope = -1)
          vfun = .T.
     ENDIF
ENDIF
RETURN vfun
*
FUNCTION tiphmeh
PRIVATE vfun
vfun = .F.
IF tipo = '3'
     IF (operac = 'C' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope <> -1) .OR.  ;
        (operac = 'C' .AND.  ;
        tipcom = 'P  ' .AND.  ;
        tipope <> -1) .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'CYP' .AND.  ;
        tipope <> -1) .OR.  ;
        (operac = 'T' .AND.  ;
        tipcom = 'P  ' .AND.  ;
        tipope <> -1)
          vfun = .T.
     ENDIF
ENDIF
RETURN vfun
*
FUNCTION buscprg
PRIVATE vkey
zx = ALIAS()
qw = ORDER()
vkey = ALLTRIM(vperiodo) +  ;
       ALLTRIM(vcodprg) +  ;
       ALLTRIM(vcodsub) +  ;
       ALLTRIM(vproyec)
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
FUNCTION buscal
PRIVATE vkey, ali
ali = ALIAS()
vkeyi = vperiodo +  ;
        ALLTRIM(vnummes2) +  ;
        ALLTRIM(vcodfte) +  ;
        ALLTRIM(vcodprg) +  ;
        ALLTRIM(vcodsub) +  ;
        vproyec + vsubpry
vkeyf = vperiodo +  ;
        ALLTRIM(vnummes2) +  ;
        ALLTRIM(vcodfte) +  ;
        ALLTRIM(vcodprg) +  ;
        ALLTRIM(vcodsub) +  ;
        vcodact
SELECT calen
IF ALLTRIM(vtipfun) = 'I'
     SET ORDER TO calen1
     SEEK vkeyi
ELSE
     SET ORDER TO calen2
     SEEK vkeyf
ENDIF
vtotal = 0
IF FOUND()
     IF ALLTRIM(vtipfun) = 'I'
          SCAN WHILE periodo +  ;
               nummes + codfte +  ;
               codprg + codsubpr +  ;
               codproy + codsupry =  ;
               vkeyi
               vtotal = vtotal +  ;
                        IIF(codpart =  ;
                        vcodpart,  ;
                        valpart,  ;
                        0)
          ENDSCAN
     ELSE
          SCAN WHILE periodo +  ;
               nummes + codfte +  ;
               codprg + codsubpr +  ;
               codact = vkeyf
               vtotal = vtotal +  ;
                        IIF(vcodpart =  ;
                        codpart,  ;
                        valpart,  ;
                        0)
          ENDSCAN
     ENDIF
ENDIF
SELECT (ali)
RETURN vtotal
*
