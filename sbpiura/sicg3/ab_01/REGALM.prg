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
       ' 같 Control Visible de Almac굈 같 '  ;
       DOUBLE COLOR SCHEME 5
DEFINE WINDOW wind_3 FROM 01, 04  ;
       TO 21, 77 TITLE  ;
       'Relaci줻 de Art죅ulos'  ;
       DOUBLE COLOR SCHEME 10
PUBLIC vmesk, vbanpec, vbanoc
ACTIVATE WINDOW lis
STORE SPACE(10) TO vcodart
STORE DATE() TO vfecini, vfecfin
vte = 1
SELECT parma
SEEK 'CIERRE'
vfecini = IIF(FOUND(),  ;
          parma.fecha + 1, {})
vcodfte = '  '
SELECT produ
@ 01, 02 SAY  ;
  '     Entre los d죂s: '
@ 01, 24 GET vfecini PICTURE '@D'  ;
  COLOR SCHEME 7
@ 01, 34 GET vfecfin PICTURE '@D'  ;
  VALID (vfecfin >= vfecini)  ;
  .AND. as_mes() COLOR SCHEME 7
@ 03, 02 SAY  ;
  'Fuente de Financiam: ' GET  ;
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
       'Espere un momento........'
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
     DEACTIVATE WINDOW standby
     IF  .NOT. EOF()
          vstkini = 0
          vctoini = 0
          DO reporte WITH 2,  ;
             'Covial2',  ;
             ' Emisi줻 del Kardex de Existencias '
     ELSE
          DO standby WITH  ;
             'No se tienen movimientos'
     ENDIF
     CLOSE DATABASES
     ERASE (vind)
ENDIF
ON KEY LABEL F2
CLOSE DATABASES
RETURN
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
FUNCTION lmplin
PARAMETER _todo, _x, _y
PRIVATE _todo, _x, _y
IF _todo = 1
     @ _x, _y CLEAR TO _x, 79
ENDIF
RETURN .T.
*
FUNCTION sal_ini
PRIVATE vkey
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
SET ORDER TO IteArt1
SELECT kardex
RETURN xsaldo
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
            'Espere un Momento ....'  ;
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
