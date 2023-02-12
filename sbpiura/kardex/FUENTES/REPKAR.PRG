CLOSE DATABASES
USE IN 1 KardexV ALIAS kardex  ORDER KardexV1
USE IN 2 StkAlmV ALIAS stkalmv ORDER StkAlmV1
USE IN 3 Parmae ALIAS parma    ORDER Parmae1
*USE IN 4 ArtMae ALIAS artmae   ORDER ArtMae1
USE IN 4 ArtMAeG ALIAS ArtMaeG ORDER ArtMaeG1
USE IN 5 ArtMaeC ALIAS ArtMaeC ORDER ArtMaeC1
USE IN 6 ArtMaeF ALIAS ArtMaeF ORDER ArtMaeF1

DO inicia
DO salida
*
PROCEDURE inicia
*---------------
vtogg = 1
vtocl = 1
vtofm = 1
vtodet = 1
m.periodo = RIGHT(STR(YEAR(m.fecsis), 4), 2)
m.tipkar = SPACE(1)
m.grubie = SPACE(2)
m.clabie = SPACE(2)
m.fambie = SPACE(4)
m.item   = SPACE(4)
mfecha1 = CTOD('01/01/' + STR(YEAR(m.fecsis), 4))
mfecha2 = DATE()
DEFINE WINDOW wlista FROM 1,15 TO 23,70 FLOAT TITLE 'Reporte de Kardex' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 00,01  SAY '  Tipo de Existencias: ' GET m.tipkar VALID val_para(m.tipkar,'TIPKAR',' ',22,50)
@ 02, 01 SAY '  Todas los Grupos: ' GET vtogg SIZE 1, 10, 2 FUNCTION '*RNH \<Si;\<No'
@ 04, 01 SAY '  Grupo: ' GET m.grubie VALID val_grupo(m.grubie,'B',.T.,5,15) WHEN vtogg = 2
@ 06, 01 SAY '  Todas las Clases: '    GET vtocl SIZE 1, 10, 2 FUNCTION '*RNH \<Si;\<No'
@ 08, 01 SAY '  Clase: ' GET m.clabie VALID val_clase(m.clabie,m.grubie,.T.,9,15) WHEN vtocl = 2
@ 10, 01 SAY '  Todas las Familia: '    GET vtofm SIZE 1, 10, 2 FUNCTION '*RNH \<Si;\<No'
@ 12, 01 SAY 'Familia: ' GET m.fambie VALID val_familia(m.fambie,m.grubie+m.clabie,.T.,13,15) WHEN vtofm = 2
@ 14, 01 SAY '  Todas los Detalles : ' GET vtodet SIZE 1, 10, 2 FUNCTION '*RNH \<Si;\<No' WHEN vtogg = 2 .AND. vtocl = 2 .AND. vtofm = 2
@ 16, 01 SAY '              Detalle: ' GET m.item VALID det_karx() WHEN vtogg = 2 .AND. vtocl = 2 .AND. vtofm = 2 and vtodet=2
@ 18, 01 SAY '      Rango de Fechas: ' GET mfecha1 WHEN .F.
@ 18, 36 GET mfecha2
@ 20, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE
ctipkar = val_para(m.tipkar,'TIPKAR','D',22,50)
RELEASE WINDOW wlista
DO espera WITH 1,'Procesando Kardex'
IF LASTKEY() <> 27 .AND. okcancel <> 2
   SET FILTER TO tipkar = m.tipkar AND codart = IIF(vtogg = 1, '', m.grubie)+IIF(vtocl = 1, '', '.' + m.clabie)+ IIF(vtofm = 1, '', '.' + m.fambie)+IIF(vtodet = 1, '', '.' + m.ITEM) .AND. BETWEEN(fecha, mfecha1, mfecha2)
   GOTO TOP
   DO espera WITH 2
   IF !EOF()
      DO reporte WITH 2,'Kardex', 'Reporte de Kardex', 2, .F., .T.
   ELSE
      DO standby WITH 'No Existe Informaci¢n para Procesar.'
   ENDIF
ELSE
   DO espera WITH 2
   DO standby WITH 'Proceso Cancelado.'
ENDIF
RETURN
*
PROCEDURE salida
*---------------
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
FUNCTION val_ggx
*---------------
PRIVATE mret
= val_gg()
IF SEEK(m.periodo + m.tipkar +  ;
   m.codgen)
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
IF SEEK(m.periodo + m.tipkar +  ;
   m.codgen + '.' + m.codcla)
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
*----------------
PRIVATE mret
= det_kar(m.periodo + m.tipkar + m.grubie + '.' + m.clabie + '.' + m.fambie + '.' + m.item,' ',23)
IF SEEK(m.periodo + m.tipkar + m.grubie + '.' + m.clabie  + '.' + m.fambie  + '.' + m.item)
     mret = .T.
ELSE
     DO standby WITH 'El grupo Especificado no tiene Art¡culos en al Kardex'
     m.codcla = SPACE(4)
     mret = .F.
ENDIF
RETURN mret


*FUNCTION det_karx
*----------------
PRIVATE mret
= det_kar(m.periodo + m.tipkar + m.codgen + '.' + m.codcla + '.' + m.coddet,' ',23)
IF SEEK(m.periodo + m.tipkar + m.codgen + '.' + m.codcla + '.' + m.coddet)
     mret = .T.
ELSE
     DO standby WITH 'El grupo Especificado no tiene Art¡culos en al Kardex'
     m.codcla = SPACE(4)
     mret = .F.
ENDIF
RETURN mret
*
