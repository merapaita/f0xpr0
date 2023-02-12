CLOSE DATABASES
USE IN 1 KardexV ALIAS kardex  ;
    ORDER KardexV1
USE IN 2 StkAlmV ALIAS stkalmv  ;
    ORDER StkAlmV1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 4 ArtMae ALIAS artmae  ;
    ORDER ArtMae1
USE IN 5 IteArt ALIAS iteart  ;
    ORDER IteArt1
USE IN 6 fiteop ORDER FITEOP1
USE IN 8 fitedon ORDER FITEDON1
USE IN 9 fordped ORDER FORDPED1
USE IN 10 fDonac ORDER FDonac1
SELECT fiteop
SET RELATION TO periodo + numped INTO;
fordped ADDITIVE
SELECT fitedon
SET RELATION TO periodo + numdon INTO;
fdonac ADDITIVE
SELECT kardex
SET RELATION TO periodo + numdoc + item;
INTO fiteop ADDITIVE
SET RELATION TO periodo + numdoc + item;
INTO fitedon ADDITIVE
DO inicia
DO salida
*
PROCEDURE inicia
vtogg = 2
vtoge = 1
vtodet = 1
m.periodo = RIGHT(STR(YEAR(m.fecsis),  ;
            4), 2)
m.codgen = '62'
m.codcla = SPACE(3)
m.coddet = SPACE(4)
mfecha1 = CTOD('01/01/' +  ;
          STR(YEAR(m.fecsis),  ;
          4))
mfecha2 = DATE()
DEFINE WINDOW wlista FROM 3, 15  ;
       TO 20, 70 FLOAT TITLE  ;
       'Reporte de Kardex' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 01 SAY  ;
  '    Todas los Grupos : ' GET  ;
  vtogg SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No' WHEN .F.
@ 03, 01 SAY '  Grupo: ' GET  ;
  m.codgen VALID val_ggx() WHEN  ;
  .F.
@ 03, 01 SAY val_ggx()
@ 05, 01 SAY  ;
  '  Todas Laboratorios : ' GET  ;
  vtoge SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No' WHEN vtogg =  ;
  2
@ 07, 01 SAY '  Grupo: ' GET  ;
  m.codcla VALID val_espx() WHEN  ;
  vtogg = 2 .AND. vtoge = 2
@ 09, 01 SAY  ;
  '  Todas los Detalles : ' GET  ;
  vtodet SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No' WHEN vtogg = 2  ;
  .AND. vtoge = 2
@ 11, 01 SAY  ;
  '              Detalle: ' GET  ;
  m.coddet VALID det_karx() WHEN  ;
  vtogg = 2 .AND. vtoge = 2 .AND.  ;
  vtodet = 2
@ 13, 01 SAY  ;
  '      Rango de Fechas: ' GET  ;
  mfecha1 WHEN .F.
@ 13, 36 GET mfecha2
@ 15, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1,  ;
   'Procesando Kardex'
IF LASTKEY() <> 27 .AND. okcancel <>  ;
   2
     SET FILTER TO codart = IIF(vtogg;
= 1, '', m.codgen) + IIF(vtoge = 1, '',;
'.' + m.codcla) + IIF(vtodet = 1, '', '.' + m.coddet) .AND. BETWEEN(fecha, mfecha1, mfecha2)
     GOTO TOP
     DO espera WITH 2
     IF  .NOT. EOF()
          DO reporte WITH 2,  ;
             'Kardex',  ;
             'Reporte de Kardex',  ;
             2, .F., .T.
     ELSE
          DO standby WITH  ;
             'No Existe Informaci¢n para Procesar.'
     ENDIF
     SET FILTER TO
ELSE
     DO espera WITH 2
     DO standby WITH  ;
        'Proceso Cancelado.'
ENDIF
RETURN
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
FUNCTION val_ggx
PRIVATE mret
= val_gg()
IF SEEK(m.periodo + m.codgen)
     mret = .T.
ELSE
     DO standby WITH  ;
        'El grupo Especificado no tiene Art¡culos en al Kardex'
     m.codgen = SPACE(2)
     mret = .F.
ENDIF
RETURN mret
*
FUNCTION val_espx
PRIVATE mret
= val_esp('B' + m.codgen + '.' +  ;
  m.codcla,' ',23)
IF SEEK(m.periodo + m.codgen +  ;
   '.' + m.codcla)
     mret = .T.
ELSE
     DO standby WITH  ;
        'El grupo Especificado no tiene Art¡culos en al Kardex'
     m.codcla = SPACE(3)
     mret = .F.
ENDIF
RETURN mret
*
FUNCTION det_karx
PRIVATE mret
= det_kar(m.periodo + m.codgen +  ;
  '.' + m.codcla,m.coddet,' ', ;
  23)
IF SEEK(m.periodo + m.codgen +  ;
   '.' + m.codcla + '.' +  ;
   m.coddet)
     mret = .T.
ELSE
     DO standby WITH  ;
        'El grupo Especificado no tiene Art¡culos en al Kardex'
     m.codcla = SPACE(4)
     mret = .F.
ENDIF
RETURN mret
*
