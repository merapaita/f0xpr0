USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 2 ArtMae ALIAS produ ORDER  ;
    ArtMae1
USE IN 3 IteArt ALIAS iteart  ;
    ORDER IteArt1
USE IN 6 IteOc ALIAS iteoc ORDER  ;
    IteOC1
USE IN 5 Itepec ALIAS itepec  ;
    ORDER Itepec1
USE IN 15 OrdCom ALIAS orden  ;
    ORDER OrdCom1
USE IN 16 Pecosa ALIAS pecosa  ;
    ORDER Pecosa1
ON KEY LABEL F2 DO FUNBUSDET 
DEFINE WINDOW lis FROM 06, 10 TO  ;
       18, 70 FLOAT TITLE  ;
       ' 같 Emisi줻 del Kardex 같 '  ;
       DOUBLE COLOR SCHEME 5
DEFINE WINDOW wind_3 FROM 04, 01  ;
       TO 21, 77 TITLE  ;
       'Relaci줻 de Art죅ulos'  ;
       DOUBLE COLOR SCHEME 10
PUBLIC vmesk, vbanpec, vbanoc,  ;
       vcodfte
ACTIVATE WINDOW lis
STORE SPACE(10) TO vcodart
STORE DATE() TO vfecini, vfecfin
vbanoc = .F.
vbanpec = .F.
vacuoc = 0
ventoc = 0
vacupec = 0
ventpec = 0
vte = 1
SELECT parma
SEEK 'CIERRE'
vfecini = IIF(FOUND(),  ;
          parma.fecha + 1, {})
vcodfte = SPACE(2)
SELECT produ
@ 01, 02 SAY  ;
  '     Entre los d죂s: '
@ 01, 24 GET vfecini PICTURE '@D'  ;
  COLOR SCHEME 7
@ 01, 34 GET vfecfin PICTURE '@D'  ;
  VALID (vfecfin >= vfecini)  ;
  .AND. as_mes() COLOR SCHEME 7
@ 03, 02 SAY  ;
  '           Fte.Fto.: ' GET  ;
  vcodfte PICTURE '!!' VALID  ;
  val_para(vcodfte,'CODFTE',' ', ;
  24,20)
@ 05, 02 SAY  ;
  'Todos los productos: ' GET vte  ;
  SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No' VALID  ;
  lmplin(vte,4,24)
@ 06, 02 SAY  ;
  'C줰igo del producto: '
@ 06, 24 GET vcodart PICTURE  ;
  '!!!!!!!!!!' VALID fbut() WHEN  ;
  vte = 2 COLOR SCHEME 7
@ 08, 15 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
     ACTIVATE WINDOW standby
     @ 01, 04 SAY  ;
       'Espere un momento ...'
     USE IN 4 Kardex ALIAS  ;
         kardexa ORDER kardex1
     SELECT kardexa
     vind = SYS(3) + '.IDX'
     xind = SYS(3) + '.DBF'
     COPY TO (xind) STRUCTURE
     USE IN 10 EXCLUSIVE (xind)  ;
         ALIAS kardex
     SELECT itepec
     SET RELATION TO periodo + numpec;
INTO pecosa
     SET FILTER TO pecosa.codfte = ALLTRIM(vcodfte);
.AND. itepec.estado = '50';
.AND. IIF(vte = 1,;
.T., codart = ALLTRIM(vcodart));
.AND. BETWEEN(itepec.fecdesp, vfecini,;
vfecfin)
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          m.tipdoc = 'PEC'
          m.tipord = '2'
          m.numdoc = m.numpec
          m.coddep = pecosa.coddep
          m.fecdoc = pecosa.fecpec
          m.fecsal = m.fecdesp
          m.fecha = m.fecdesp
          m.salcan = m.candesp
          m.numref = m.numoc
          m.preunis = IIF(m.tippec =  ;
                      'S',  ;
                      m.cosmed,  ;
                      m.preuni)
          m.salimp = m.candesp *  ;
                     IIF(m.tippec =  ;
                     'S',  ;
                     m.cosmed,  ;
                     m.preuni)
          SELECT kardex
          APPEND BLANK
          GATHER MEMVAR
          m.salcan = 0
          m.salimp = 0
          m.fecsal = CTOD( ;
                     '  -  -  ')
          SELECT itepec
     ENDSCAN
     SET FILTER TO
     SET RELATION TO
     SELECT iteoc
     SET RELATION TO periodo + numoc INTO;
orden
     SET FILTER TO orden.codfte = ALLTRIM(vcodfte);
.AND. iteoc.estado = '50';
.AND. IIF(vte = 1,;
.T., codart = ALLTRIM(vcodart));
.AND. BETWEEN(iteoc.fecdesp, vfecini,;
vfecfin)
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          m.tipdoc = 'O/C'
          m.tipord = '1'
          m.numdoc = m.numoc
          m.coddep = m.coddep
          m.fecdoc = orden.fecoc
          m.entcan = m.candesp
          m.preunie = m.preuni
          m.fecent = m.fecdesp
          m.fecha = m.fecdesp
          m.numref = m.numpec
          m.entimp = m.candesp *  ;
                     m.preuni
          SELECT kardex
          APPEND BLANK
          GATHER MEMVAR
          m.entcan = 0
          m.entimp = 0
          SELECT iteoc
     ENDSCAN
     SET FILTER TO
     SET RELATION TO
     SELECT kardex
     INDEX ON codart +  ;
           DTOS(fecha) + tipord  ;
           TO (vind)
     GOTO TOP
     IF  .NOT. EOF()
          vstkini = 0
          vstkfin = 0
          vctoini = 0
          DEACTIVATE WINDOW  ;
                     standby
          DO reporte WITH 2,  ;
             'EmiKar4',  ;
             ' Emisi줻 del Kardex de Existencias '
     ELSE
          DEACTIVATE WINDOW  ;
                     standby
          DO standby WITH  ;
             'No se tienen movimientos'
     ENDIF
     CLOSE DATABASES
     ERASE (vind)
ENDIF
CLOSE DATABASES
ON KEY LABEL F2
RELEASE WINDOW wind_3
RETURN
*
FUNCTION fbut
SELECT iteart
SET ORDER TO 3
SEEK vcodart
IF  .NOT. FOUND() .OR.  ;
    EMPTY(vcodart)
     SET ORDER TO 2
     DO funbusdet
     BROWSE FIELDS iteart.codart  ;
            :H = 'Articulo' : 10,  ;
            iteart.descri :H =  ;
            'Descripci줻' : 35,  ;
            produ.codart :H =  ;
            'Det' : 7,  ;
            produ.descri :H =  ;
            'Detalle' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            wind_3
     SET ORDER TO 1
     SET RELATION TO
ENDIF
vcodart = iteart.codart
RETURN .T.
*
FUNCTION as_mes
IF MONTH(vfecini) >= 1 .AND.  ;
   MONTH(vfecini) <= 5
     vmesk = '12' +  ;
             RIGHT(ALLTRIM(STR(YEAR(vfecini) -  ;
             1)), 2)
ELSE
     vmesk = '06' +  ;
             RIGHT(ALLTRIM(STR(YEAR(vfecini))),  ;
             2)
ENDIF
RETURN .T.
*
FUNCTION lmplin
PARAMETER _todo, _x, _y
PRIVATE _todo, _x, _y
IF _todo = 1
     @ _x, _y CLEAR TO _x, 79
ENDIF
RETURN .T.
*
FUNCTION sal_ini
PRIVATE vkey, acp
vkey = kardex.codart
SELECT iteart
SET ORDER TO IteArt3
SEEK vkey
IF FOUND()
     vcampo = 'INV' + vmesk +  ;
              vcodfte
     xSaldo = iteart.&vcampo
ELSE
     xsaldo = 0
ENDIF
SELECT kardex
RETURN xsaldo
*
FUNCTION saldoini
PRIVATE vkey
vkey = kardex.codart
SELECT iteart
SET ORDER TO IteArt3
SEEK vkey
IF FOUND()
     vcampo = 'SDO' + vmesk +  ;
              vcodfte
     xSaldo = iteart.&vcampo
ELSE
     xsaldo = 0
ENDIF
SET ORDER TO Iteart1
SELECT kardex
RETURN xsaldo
*
FUNCTION stkini
PARAMETER vca, vfi
PRIVATE vca, vfi, valias
valias = ALIAS()
vstkini = 0
vctoini = 0
SELECT saldo
SEEK vca
IF FOUND() .AND. (saldo.fecha <  ;
   vfi)
     SCAN WHILE (vca =  ;
          saldo.codart) .AND.  ;
          (saldo.fecha < vfi)
     ENDSCAN
     SKIP -1
     IF saldo.codart == vca
          vstkini = saldo.stkact
          vctoini = saldo.ctoact
          vfeccie = saldo.fecha
     ELSE
          STORE 0 TO vstkini,  ;
                vctoini
          vfeccie = {}
     ENDIF
ELSE
     vstkini = produ.stocki
     vctoini = produ.costototal
     vfeccie = {}
ENDIF
SELECT itmov
IF EMPTY(vfeccie)
     SEEK vca
ELSE
     SEEK vca + DTOS(vfeccie + 1)
ENDIF
SCAN WHILE (itmov.codart = vca  ;
     .AND. movim.fecha <= vfi)
     vstkini = vstkini +  ;
               IIF(itmov.tiptra =  ;
               'I',  ;
               itmov.cantidad, -1 *  ;
               itmov.cantidad)
     vctoini = vctoini +  ;
               IIF(itmov.tiptra =  ;
               'I', 1, -1) *  ;
               ROUND(itmov.costo *  ;
               itmov.cantidad,  ;
               2)
ENDSCAN
SELECT (valias)
RETURN ' '
*
PROCEDURE saldo
IF BOF()
     vsalant = 0
ELSE
     SKIP -1
     vsaldant = IIF(kardex.tipdoc =  ;
                'PEC',  ;
                kardex.salcan,  ;
                kardex.entcan)
ENDIF
*
FUNCTION ctoprm
IF kardex.tipdoc = 'O/C'
     SELECT iteoc
     SET ORDER TO IteOC1
     SEEK kardex.periodo +  ;
          kardex.numdoc +  ;
          kardex.codfte
     IF EMPTY(numpec)
          vbanoc = .T.
     ELSE
          SELECT kardex
          vbanoc = .F.
     ENDIF
     IF vbanoc
          vacuoc = vacuoc +  ;
                   kardex.entcan *  ;
                   kardex.preunie
          ventoc = ventoc +  ;
                   kardex.entcan
          SELECT kardex
          RETURN 0
     ELSE
          IF ventoc <> 0
               vretoc = vacuoc /  ;
                        ventoc
          ELSE
               vretoc = 0
          ENDIF
          vacuoc = 0
          ventoc = 0
          SELECT kardex
          RETURN vretoc
     ENDIF
ELSE
     SELECT itepec
     SET ORDER TO ItePEC1
     SEEK kardex.periodo +  ;
          kardex.numdoc +  ;
          kardex.codfte
     IF EMPTY(numoc)
          vbanpec = .T.
     ELSE
          vbanpec = .F.
     ENDIF
     IF vbanpec
          vacupec = vacupec +  ;
                    kardex.entcan *  ;
                    kardex.preunie
          ventpec = ventpec +  ;
                    kardex.entcan
          SELECT kardex
          RETURN 0
     ELSE
          IF ventpec <> 0
               vretpec = vacupec /  ;
                         ventpec
          ELSE
               vretpec = 0
          ENDIF
          vacupec = 0
          ventpec = 0
          SELECT kardex
          RETURN vretpec
     ENDIF
ENDIF
RETURN
*
PROCEDURE cost_pro
zind = SYS(3) + '.IDX'
xind = SYS(3) + '.IDX'
IF vte = 2
     SELECT iteoc
     INDEX ON codart +  ;
           DTOS(fecdesp) + codfte +  ;
           tipfun TO (zind) FOR   ;
           .NOT. EMPTY(fecdesp)  ;
           .AND. EMPTY(numpec)  ;
           .AND. codfte =  ;
           ALLTRIM(vcodfte) .AND.  ;
           codart =  ;
           ALLTRIM(vcodart)
     SELECT itepec
     INDEX ON codart +  ;
           DTOS(fecdesp) + codfte +  ;
           tipfun TO (xind) FOR   ;
           .NOT. EMPTY(fecdesp)  ;
           .AND. tippec <> 'O'  ;
           .AND. preuni = 0 .AND.  ;
           codfte =  ;
           ALLTRIM(vcodfte) .AND.  ;
           codart =  ;
           ALLTRIM(vcodart)
ELSE
     SELECT iteoc
     INDEX ON codart +  ;
           DTOS(fecdesp) + codfte +  ;
           tipfun TO (zind) FOR   ;
           .NOT. EMPTY(fecdesp)  ;
           .AND. EMPTY(numpec)  ;
           .AND. codfte =  ;
           ALLTRIM(vcodfte)
     SELECT itepec
     INDEX ON codart +  ;
           DTOS(fecdesp) + codfte +  ;
           tipfun TO (xind) FOR   ;
           .NOT. EMPTY(fecdesp)  ;
           .AND. tippec <> 'O'  ;
           .AND. preuni = 0 .AND.  ;
           codfte =  ;
           ALLTRIM(vcodfte)
ENDIF
SELECT iteart
GOTO TOP
SCAN
     vcodigo = codart
     SELECT itepec
     SEEK vcodigo
     IF FOUND()
          SCAN FOR vcodigo =  ;
               itepec.codart
               vkey = itepec.codart +  ;
                      itepec.codfte +  ;
                      itepec.tipfun
               SELECT iteoc
               SEEK itepec.codart
               IF FOUND()
                    DO WHILE .T.
                         IF codart +  ;
                            codfte +  ;
                            tipfun <>  ;
                            vkey
                              SKIP
                              IF codart <>  ;
                                 itepec.codart  ;
                                 .OR.  ;
                                 EOF()
                                   vprecio = .F.
                                   EXIT
                              ENDIF
                              LOOP
                         ELSE
                              vprecio =  ;
                               .T.
                              vcantot =  ;
                               candesp
                              vpreuni =  ;
                               preuni
                              EXIT
                         ENDIF
                    ENDDO
               ELSE
                    vprecio = .F.
               ENDIF
               IF vprecio
                    SELECT itepec
                    SCAN FOR  ;
                         codart +  ;
                         codfte +  ;
                         tipfun =  ;
                         vkey
                         IF vcantot >=  ;
                            candesp
                              vcantot =  ;
                               vcantot -  ;
                               candesp
                         ELSE
                              vdiferenci =  ;
                               candesp -  ;
                               vcantot
                              SELECT  ;
                               iteoc
                              IF vcantot <>  ;
                                 0
                                   vdividendo = (preuni * candesp)
                                   vdivisor = candesp
                              ELSE
                                   vdividendo = 0
                                   vdivisor = 0
                              ENDIF
                              ok1 =  ;
                               .T.
                              ok =  ;
                               .T.
                              SKIP
                              DO WHILE  ;
                                 .T.  ;
                                 .AND.   ;
                                 .NOT.  ;
                                 EOF()
                                   IF codart + codfte + tipfun = vkey
                                        vcantot = vcantot + candesp
                                        IF vcantot < itepec.candesp
                                             vdividendo = vdividendo + (preuni * candesp)
                                             vdivisor = vdivisor + candesp
                                             vdiferenci = candesp - vcantot
                                             SKIP
                                             IF EOF()
                                                  ok = .F.
                                                  EXIT
                                             ENDIF
                                             LOOP
                                        ELSE
                                             vdividendo = vdividendo + (preuni * vdiferenci)
                                             vdivisor = vdivisor + vdiferenci
                                             EXIT
                                        ENDIF
                                   ELSE
                                        ok1 = .F.
                                        EXIT
                                   ENDIF
                              ENDDO
                              SELECT  ;
                               itepec
                              IF   ;
                               .NOT.  ;
                               ok
                              ENDIF
                              IF   ;
                               .NOT.  ;
                               ok1
                              ENDIF
                              IF ok  ;
                                 .AND.  ;
                                 ok1
                                   vcantot = vcantot - candesp
                              ENDIF
                         ENDIF
                    ENDSCAN
               ENDIF
               SELECT itepec
          ENDSCAN
     ENDIF
     SELECT iteart
ENDSCAN
SELECT iteoc
SET INDEX TO
ERASE (zind)
SET ORDER TO IteOC1
SELECT itepec
SET INDEX TO
ERASE (xind)
SET ORDER TO Itepec1
RETURN
*
FUNCTION val_artk
PARAMETER xcod, _tipo, _x, _y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND.  ;
       _tipo
_campo = VARREAD()
malias = ALIAS()
SELECT iteart
_oldwnd = WOUTPUT()
IF  .NOT. medita
     SEEK xcod
     v_fun = IIF(FOUND(), descri,  ;
             '')
ELSE
     IF EMPTY(xcod)
          SET ORDER TO 1
          SET FILTER TO SUBSTR(codart,;
2, 2) = SUBSTR(ALLTRIM(parma.codigo),;
2, 2)
          ACTIVATE WINDOW standby
          @ 1, 14 SAY  ;
            'Espere un Momento ...'  ;
            COLOR W/N* 
          GOTO TOP
          IF EOF()
               DEACTIVATE WINDOW  ;
                          standby
               ACTIVATE SCREEN
               SET FILTER TO
               v_fun = .F.
          ELSE
               DEACTIVATE WINDOW  ;
                          standby
               ACTIVATE SCREEN
               ON KEY LABEL f10 KEYBOARD;
CHR(23)
               ON KEY LABEL f2 DO funbus
               DEFINE WINDOW  ;
                      _busart  ;
                      FROM 2, 02  ;
                      TO 22, 77
               BROWSE FIELDS  ;
                      codart :H =  ;
                      'C줰igo' :W =  ;
                      EMPTY(SUBSTR(codart,  ;
                      5, 3)),  ;
                      descri :H =  ;
                      'Nombre' :  ;
                      60 :W =  ;
                      EMPTY(descri)  ;
                      NOMENU  ;
                      NOAPPEND  ;
                      NODELETE  ;
                      WINDOW  ;
                      _busart  ;
                      TITLE  ;
                      '께께 [F10] Selecciona   [F2] Buscar 께께'  ;
                      NOLGRID
               ON KEY LABEL f10
               ON KEY LABEL f2
               RELEASE WINDOW  ;
                       _busart
               SET ORDER TO 1
               SET FILTER TO
               IF LASTKEY() = 27
                    v_fun = .F.
               ELSE
                    xcod = codart
                    IF mmsg
                         @ _x, _y  ;
                           SAY  ;
                           descri
                    ENDIF
                    SELECT (malias)
                    IF  .NOT.  ;
                        _tipo
                         REPLACE &_campo;
WITH  xcod
                    ENDIF
                    v_fun = .T.
               ENDIF
          ENDIF
     ELSE
          SEEK xcod
          IF mmsg .AND. FOUND()
               @ _x, _y SAY  ;
                 descri
          ENDIF
          v_fun = FOUND()
     ENDIF
ENDIF
SELECT (malias)
RETURN v_fun
*
