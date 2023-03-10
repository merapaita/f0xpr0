CLOSE DATABASES
USE IN 1 StkAlmV ALIAS stkalmv ORDER StkAlmV3
USE IN 2 KardexV ALIAS kardexv ORDER Kardexv1
USE IN 3 Parmae ALIAS parma    ORDER Parmae1
*USE IN 4 ArtMae ALIAS artmae   ORDER ArtMae1
USE IN 5 IteArt ALIAS iteart   ORDER IteArt1
USE IN 6 ArtMAeG ALIAS ArtMaeG ORDER ArtMaeG1
USE IN 7 ArtMaeC ALIAS ArtMaeC ORDER ArtMaeC1
USE IN 8 ArtMaeF ALIAS ArtMaeF ORDER ArtMaeF1

SELECT stkalmv
*SET RELATION TO 'B' + LEFT(codart, 6) INTO artmae
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
*---------------
vtogg = 1
vtocl = 1
vtofm = 1
vtoge = 1
m.periodo = RIGHT(STR(YEAR(m.fecsis), 4), 2)
m.tipkar = SPACE(1)
m.grubie = SPACE(2)
m.clabie = SPACE(2)
m.fambie = SPACE(4)
mfecha1 = DATE()
DEFINE WINDOW wlista FROM 1, 15 TO 23, 70 FLOAT TITLE 'Reporte de Existencias a la Fecha' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 01 SAY '  Tipo de Existencias: ' GET m.tipkar VALID val_para(m.tipkar,'TIPKAR',' ',22,50)
@ 03, 01 SAY '  Todas los Grupos: '    GET vtogg SIZE 1, 10, 2 FUNCTION '*RNH \<Si;\<No'
@ 05, 01 SAY '  Grupo: ' GET m.grubie VALID val_grupo(m.grubie,'B',.T.,5,15) WHEN vtogg = 2
@ 07, 01 SAY '  Todas las Clases: '    GET vtocl SIZE 1, 10, 2 FUNCTION '*RNH \<Si;\<No'
@ 09, 01 SAY '  Clase: ' GET m.clabie VALID val_clase(m.clabie,m.grubie,.T.,9,15) WHEN vtocl = 2
@ 11, 01 SAY '  Todas las Familia: '    GET vtofm SIZE 1, 10, 2 FUNCTION '*RNH \<Si;\<No'
@ 13, 01 SAY 'Familia: ' GET m.fambie VALID val_familia(m.fambie,m.grubie+m.clabie,.T.,13,15) WHEN vtofm = 2
@ 15, 01 SAY '      Fecha de Corte: ' GET mfecha1
@ 17, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE

ctipkar = val_para(m.tipkar,'TIPKAR','D',22,50)
RELEASE WINDOW wlista
DO espera WITH 1, 'Procesando Kardex'
IF LASTKEY() <> 27 .AND. okcancel <> 2
   SELECT kardexv
   SET FILTER TO tipkar=m.tipkar AND codart=IIF(vtogg=1,'', m.grubie)+IIF(vtocl=1,'','.'+m.clabie)+IIF(vtofm=1,'','.'+m.fambie) AND fecha<=mfecha1
   GOTO TOP
   SELECT stkalmv
   vdbf = SYS(3) + '.dbf'
   COPY TO (vdbf) STRUCTURE
   USE (vdbf) ALIAS stkalmv
*   SET RELATION TO 'B' + LEFT(codart,6) INTO artmae
   SELECT kardexv
   SCAN
      mcodart = codart
      mcorrel = correl
      nsaldo = 0
      SCAN WHILE codart+correl=mcodart+mcorrel
         soper = IIF(tipmov = '+', 1, - 1)
         nsaldo = nsaldo+soper*totcan
      ENDSCAN
      SKIP -1
      IF nsaldo = 0
         LOOP
      ENDIF
      SCATTER MEMVAR
      m.salfrac = nsaldo
      SELECT iteart
      IF SEEK('B' + m.codart)
         m.fraccion = fraccion
      ENDIF
      m.salcant = m.salfrac / m.fraccion
      SELECT stkalmv
      APPEND BLANK
      GATHER MEMVAR
      SELECT kardexv
   ENDSCAN
   SELECT stkalmv
   SET FILTER TO salfrac <> 0
   GOTO TOP
   DO espera WITH 2
*   SET STEP ON
   IF !EOF()
      DO reporte WITH 2,'Stock','Reporte de Existencias a la Fecha',2,.F.,.T.
     ELSE
      DO standby WITH 'No Existe Informaci?n para Procesar.'
   ENDIF
   USE IN stkalmv
   ERASE (vdbf)
ELSE
   DO espera WITH 2
   DO standby WITH 'Proceso Cancelado.'
ENDIF
RETURN
*
FUNCTION grupo
*-------------
PARAMETER xgg
PRIVATE calias
calias = ALIAS()
SELECT ArtMaeG
IF SEEK("B" + xgg)
     mret = ALLTRIM(ArtMaeG.descri)
ELSE
     mret = 'Descripcion Desconocida'
ENDIF
RETURN mret
*
FUNCTION Clase
*-------------
PARAMETER xgg
PRIVATE calias
calias = ALIAS()
SELECT ArtMaeC
IF SEEK("B" + xgg)
     mret = ALLTRIM(ArtMaeC.descri)
ELSE
     mret = 'Descripcion Desconocida'
ENDIF
RETURN mret
*
FUNCTION familia
*---------------
PARAMETER xgg
PRIVATE calias
calias = ALIAS()
SELECT ArtMaeF
IF SEEK("B" + xgg)
     mret = ALLTRIM(ArtMaeF.descri)
ELSE
     mret = 'Descripcion Desconocida'
ENDIF
RETURN mret
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
FUNCTION val_ggx
*---------------
PRIVATE mret
= val_gg()
IF SEEK(m.periodo+m.tipkar+m.codgen)
   mret = .T.
ELSE
   DO standby WITH 'El grupo Especificado no tiene Art?culos en al Kardex'
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
        'El grupo Especificado no tiene Art?culos en al Kardex'
     m.codcla = SPACE(3)
     mret = .F.
ENDIF
RETURN mret
*
