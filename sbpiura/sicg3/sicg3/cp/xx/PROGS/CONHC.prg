CLOSE DATABASES
USE IN 1 PARMAE ALIAS parma ORDER  ;
    parmae1
USE IN 2 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 3 IteHc ALIAS itehc ORDER  ;
    Itehc1
USE IN 4 HOJCON ALIAS hoja ORDER  ;
    HOJCON1
PUBLIC vcodsub, vcodact, vproyec,  ;
       vsubpry, vcalend
vmens01 = 'Registro de Presupuesto'
vmens02 = ' Presupuesto : REVISION '
vmens04 = 'Dicho Presupuesto no fue encontrado'
vmens05 = 'No existe Presupuesto anterior'
vmens06 = 'No existe Presupuesto siguiente'
vmens07 = '¨ Desea Anular ‚ste Presupuesto ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Presupuesto ha sido anulado'
vmens10 = 'El Presupuesto ya est  Atendido'
vmens11 = 'El Presupuesto ha sido devuelto'
tot = 0
tot1 = 0
DO inicia
DO salida
CLOSE DATABASES
RETURN
*
PROCEDURE inicia
PRIVATE vtexp
ord = ORDER()
vtexp = RECNO()
STORE 0 TO vtotal, vtipo
vuniges = '01'
vunieje = '001'
STORE SPACE(2) TO vperiodo,  ;
      vcodfte, vcodfun, vcalend
STORE SPACE(3) TO vcodprg
STORE SPACE(5) TO vcodcom
DEFINE WINDOW lis_1 FROM 4, 10 TO  ;
       20, 70 FLOAT TITLE  ;
       ' °°  Saldo Presupuestal °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
@ 0, 2 SAY '     Periodo : ' GET  ;
  vperiodo PICTURE '!!' VALID   ;
  .NOT. EMPTY(vperiodo)
@ 1, 2 SAY '         Mes : ' GET  ;
  vcalend PICTURE '!!' VALID  ;
  val_para(vcalend,'FECMES',' ', ;
  18,25)
@ 3, 2 SAY '  U. Gestora : ' GET  ;
  vuniges PICTURE '!!' VALID  ;
  val_para(vuniges,'UNIGES',' ', ;
  18,30)
@ 4, 2 SAY 'U. Ejecutora : ' GET  ;
  vunieje PICTURE '!!!' VALID  ;
  val_para1(vunieje,'UNIEJE' +  ;
  vuniges,' ',18,30)
@ 6, 2 SAY '     Funci¢n : ' GET  ;
  vcodfun PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vcodfun),  ;
  val_para(vcodfun,'CODFUN',' ', ;
  18,30), .T.)
@ 7, 2 SAY '    Programa : ' GET  ;
  vcodprg PICTURE '!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodprg),  ;
  val_para1(vcodprg,'CODPRG' +  ;
  vcodfun,' ',18,30), .T.)
@ 9, 2 SAY ' X Componete : ' GET  ;
  vtipo SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 10, 2 SAY '  Componente : ' GET  ;
  vcodcom PICTURE '!!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodcom),  ;
  val_para(vcodcom,'CODCOM',' ', ;
  18,30), .T.) WHEN vtipo = 1
@ 12, 2 SAY '   Fte. Fto. : ' GET  ;
  vcodfte PICTURE '!!' VALID  ;
  val_para(vcodfte,'CODFTE',' ', ;
  18,30)
READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY() = 27
     RETURN
ENDIF
SELECT itehc
SET RELATION TO nummes + numhc INTO hoja
SET SKIP TO hoja
IF EOF()
     DO standby WITH vmens08
ELSE
     DEFINE WINDOW xwait FROM 20,  ;
            06 TO 22, 78 COLOR  ;
            SCHEME 05
     ACTIVATE WINDOW xwait
     @ 0, 10 SAY  ;
       ' Espere un Momento...Procesando Documentos !'  ;
       COLOR W+/RB* 
     vkey = ALLTRIM(vuniges) +  ;
            ALLTRIM(vunieje) +  ;
            IIF( .NOT.  ;
            EMPTY(vcodfun),  ;
            ALLTRIM(vcodfun), '') +  ;
            IIF( .NOT.  ;
            EMPTY(vcodprg),  ;
            ALLTRIM(vcodprg), '') +  ;
            ALLTRIM(vcodfte)
     vind = SYS(3) + '.DBF'
     COPY TO (vind) STRUCTURE
     USE IN 6 EXCLUSIVE (vind)  ;
         ALIAS itehc1
     vind = SYS(3) + '.IDX'
     SELECT itehc1
     INDEX ON LEFT(codart, 5) +  ;
           LEFT(codpart, 2) +  ;
           codfte + numhc TO  ;
           (vind)
     SELECT itehc
     SET FILTER TO nummes = ALLTRIM(vcalend);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcom)), codcom;
= ALLTRIM(vcodcom),;
.T.);
.AND. codfte = ALLTRIM(vcodfte);
.AND. uniges = ALLTRIM(vuniges);
.AND. unieje = ALLTRIM(vunieje);
.AND. estado <> '99';
.AND. IIF(;
.NOT. EMPTY(numpa), mespr <> nummes,;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpr), mespr = nummes,;
.T.)
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          SELECT maepre
          SEEK vperiodo +  ;
               itehc.uniges +  ;
               itehc.unieje +  ;
               itehc.codcad
          vkey1 = uniges + unieje +  ;
                  IIF( .NOT.  ;
                  EMPTY(vcodfun),  ;
                  codfun, '') +  ;
                  IIF( .NOT.  ;
                  EMPTY(vcodprg),  ;
                  codprg, '') +  ;
                  itehc.codfte
          SELECT itehc
          IF vkey = vkey1
               SELECT itehc1
               vkey2 = maepre.codfun +  ;
                       maepre.codprg +  ;
                       LEFT(itehc.codpart,  ;
                       2) +  ;
                       itehc.codfte +  ;
                       itehc.numhc
               SEEK vkey2
               IF FOUND()
                    REPLACE valpart  ;
                            WITH  ;
                            valpart +  ;
                            IIF(itehc.tipope =  ;
                            '-',  ;
                            itehc.valpart * - ;
                            1,  ;
                            itehc.valpart)
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
                    REPLACE codart  ;
                            WITH  ;
                            maepre.codfun +  ;
                            maepre.codprg
                    REPLACE descri  ;
                            WITH  ;
                            hoja.nombre
                    REPLACE modif  ;
                            WITH  ;
                            hoja.numref
                    REPLACE valpart  ;
                            WITH  ;
                            IIF(itehc.tipope =  ;
                            '-',  ;
                            itehc.valpart * - ;
                            1,  ;
                            itehc.valpart)
               ENDIF
          ENDIF
          SELECT itehc
     ENDSCAN
     SELECT itehc1
     GOTO TOP
     RELEASE WINDOW xwait
     IF EOF()
          DO standby WITH  ;
             'No existe Registros para procesar'
     ELSE
          USE IN 7 COMPAG ORDER  ;
              COMPAG4
          SET RELATION TO itehc1.nummes;
+ itehc1.numhc INTO compag ADDITIVE
          SET RELATION TO vperiodo + uniges;
+ unieje + codcad INTO maepre ADDITIVE
          IF vtipo = 2
               DO reporte WITH 2,  ;
                  'ConHc1',  ;
                  ' Consolidado de Afectaci¢n ',  ;
                  1, .F., .T.
          ELSE
               vind = SYS(3) +  ;
                      '.IDX'
               INDEX ON  ;
                     LEFT(codart,  ;
                     5) + codcom +  ;
                     LEFT(codpart,  ;
                     2) + codcad +  ;
                     codfte +  ;
                     numhc TO  ;
                     (vind)
               GOTO TOP
               DO reporte WITH 2,  ;
                  'ConHc3',  ;
                  ' Consolidado de Afectaci¢n x Componente',  ;
                  1, .F., .T.
               SET RELATION OFF INTO maepre
          ENDIF
          vind = SYS(3) + '.IDX'
          INDEX ON LEFT(codpart,  ;
                2) + LEFT(codart,  ;
                5) TO (vind)
          GOTO TOP
          DO reporte WITH 2,  ;
             'ConHc2',  ;
             ' Consolidado de GENERAL x Componente ',  ;
             1, .F., .T.
     ENDIF
ENDIF
RETURN
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
FUNCTION totfun
PARAMETER vkey
vrecno = RECNO()
SUM FOR LEFT(codart, 2) = vkey  ;
    valpart TO vsuma
GOTO vrecno
RETURN vsuma
*
FUNCTION totprg
PARAMETER vkey
vrecno = RECNO()
SUM FOR LEFT(codart, 5) = vkey  ;
    valpart TO vsuma
GOTO vrecno
RETURN vsuma
*
FUNCTION totcom
PARAMETER vkey
vrecno = RECNO()
SUM FOR LEFT(codart, 5) + codcom =  ;
    vkey valpart TO vsuma
GOTO vrecno
RETURN vsuma
*
FUNCTION destino
PARAMETER vkey
vrecno = RECNO()
valias = ALIAS()
SELECT hoja
SEEK vkey
IF FOUND()
     SELECT (valias)
     GOTO vrecno
     RETURN hoja.destino
ELSE
     SELECT (valias)
     GOTO vrecno
     RETURN 'No Tiene'
ENDIF
*
