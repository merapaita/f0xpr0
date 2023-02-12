CLOSE DATABASES
USE IN 1 StkAlmV ALIAS stkalmv  ;
    ORDER StkAlmV3
USE IN 2 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 3 ArtMae ALIAS artmae  ;
    ORDER ArtMae1
USE IN 4 IteArt ALIAS iteart  ;
    ORDER IteArt1
SELECT stkalmv
SET RELATION TO 'B' + LEFT(codart, 6);
INTO artmae
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
vtogg = 2
vtoge = 1
m.periodo = RIGHT(STR(YEAR(m.fecsis),  ;
            4), 2)
m.codgen = '62'
m.codcla = SPACE(3)
DEFINE WINDOW wlista FROM 3, 15  ;
       TO 20, 70 FLOAT TITLE  ;
       'Reporte de Existencias a la Fecha'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 01 SAY  ;
  '    Todas los Grupos : ' GET  ;
  vtogg SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No' WHEN .F.
@ 03, 01 SAY '  Grupo: ' GET  ;
  m.codgen WHEN .F.
@ 03, 01 SAY val_ggx()
@ 05, 01 SAY  ;
  'Todas los Laboratorios: ' GET  ;
  vtoge SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No' WHEN vtogg =  ;
  2
@ 07, 01 SAY '  Grupo: ' GET  ;
  m.codcla VALID val_espx() WHEN  ;
  vtogg = 2 .AND. vtoge = 2
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
'.' + m.codcla)
     GOTO TOP
     DO espera WITH 2
     IF  .NOT. EOF()
          DO reporte WITH 2,  ;
             'Stock',  ;
             'Reporte de Existencias a la Fecha',  ;
             2, .F., .T.
     ELSE
          DO standby WITH  ;
             'No Existe Informaci¢n para Procesar.'
     ENDIF
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
