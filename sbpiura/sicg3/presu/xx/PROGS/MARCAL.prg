USE IN 1 parmae ALIAS parma ORDER  ;
    parmae1
USE IN 2 calen ALIAS calen ORDER  ;
    calen2
USE IN 3 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 4 maepar ALIAS presu ORDER  ;
    maepar1
USE IN 5 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 10 Itetra ALIAS itetra  ;
    ORDER Itetra1
USE IN 11 Itecre ALIAS itecre  ;
    ORDER Itecre1
USE IN 12 HOJMOD ALIAS hojmod  ;
    ORDER hojmod1
USE IN 13 cresup ALIAS cresup  ;
    ORDER cresup1
USE IN 14 Trapar ALIAS trapar  ;
    ORDER trapar1
vmens01 = 'Registro de Calendario'
vmens02 = ' Calendario : REVISION '
vmens04 = 'Dicho Calendario no fue encontrado'
vmens05 = 'No existe Calendario anterior'
vmens06 = 'No existe Calendario siguiente'
vmens07 = '¨ Desea Anular ‚ste Calendario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Calendario ha sido anulado'
vmens10 = 'El Calendario ya est  Atendido'
vmens11 = 'El Calendario ha sido devuelto'
USE IN 7
USE IN 8
USE IN 7 repocal
USE IN 8 repcal1
SELECT 7
vind = SYS(3) + '.DBF'
COPY TO (vind) STRUCTURE
USE IN 18 EXCLUSIVE (vind) ALIAS  ;
    repo
SELECT 8
vind = SYS(3) + '.DBF'
COPY TO (vind) STRUCTURE
USE IN 19 EXCLUSIVE (vind) ALIAS  ;
    rep1
DO inicia
DO salida
CLOSE DATABASES
RETURN
*
PROCEDURE inicia
USE IN 7
USE IN 8
USE IN 7 repocal
USE IN 8 repcal1
SELECT 7
vind = SYS(3) + '.DBF'
COPY TO (vind) STRUCTURE
USE IN 18 EXCLUSIVE (vind) ALIAS  ;
    repo
SELECT 8
vind = SYS(3) + '.DBF'
COPY TO (vind) STRUCTURE
USE IN 19 EXCLUSIVE (vind) ALIAS  ;
    rep1
SELECT calen
vrecno = RECNO()
vorder = ORDER()
vperiodo = RIGHT(STR(YEAR(DATE()),  ;
           4), 2)
STORE 0 TO vtotal, vtipo
STORE SPACE(2) TO vcodfte,  ;
      vcalend, vcodfun, vuniges
STORE SPACE(3) TO vcodprg,  ;
      vunieje
STORE SPACE(4) TO vcodcad,  ;
      vcodspr
STORE SPACE(5) TO vcodcom,  ;
      vcodmet
STORE SPACE(6) TO vcodpart,  ;
      vactpry
DEFINE WINDOW lis_1 FROM 4, 10 TO  ;
       20, 70 FLOAT TITLE  ;
       ' °° Calendario Marco Presupuestal °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
@ 0, 2 SAY '     Periodo : ' GET  ;
  vperiodo PICTURE '!!' VALID   ;
  .NOT. EMPTY(vperiodo)
@ 1, 2 SAY '  Por Cadena : ' GET  ;
  vtotal SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 2, 2 SAY '  Espec¡fico : ' GET  ;
  vtipo SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 3, 2 SAY '  U. Gestora : ' GET  ;
  vuniges PICTURE '!!' VALID  ;
  val_para(vuniges,'UNIGES',' ', ;
  18,30)
@ 4, 2 SAY 'U. Ejecutora : ' GET  ;
  vunieje PICTURE '!!!' VALID  ;
  val_para1(vunieje,'UNIEJE' +  ;
  vuniges,' ',18,30)
@ 6, 2 SAY 'Cad. Funcion.: ' GET  ;
  vcodcad PICTURE '!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodcad),  ;
  val_codcad(vcodcad,vperiodo +  ;
  ALLTRIM(vuniges) +  ;
  ALLTRIM(vunieje),' ',18,30),  ;
  .T.) WHEN vtotal = 1
@ 8, 2 SAY '     Funci¢n : ' GET  ;
  vcodfun PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vcodfun),  ;
  val_para(vcodfun,'CODFUN',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 9, 2 SAY '    Programa : ' GET  ;
  vcodprg PICTURE '!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodprg),  ;
  val_para1(vcodprg,'CODPRG' +  ;
  vcodfun,' ',18,30), .T.) WHEN  ;
  vtotal = 2
@ 10, 2 SAY ' SubPrograma : ' GET  ;
  vcodspr PICTURE '!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodspr),  ;
  val_para1(vcodspr,'CODSPR' +  ;
  vcodprg,' ',18,30), .T.) WHEN  ;
  vtotal = 2
@ 11, 2 SAY 'Activ/Proyec : ' GET  ;
  vactpry PICTURE '!!!!!!' VALID  ;
  IIF( .NOT. EMPTY(vactpry),  ;
  val_para(vactpry,'ACTPRY',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 12, 2 SAY '  Componente : ' GET  ;
  vcodcom PICTURE '!!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodcom),  ;
  val_para(vcodcom,'CODCOM',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 13, 2 SAY '   Al mes de : ' GET  ;
  vcalend PICTURE '!!' VALID  ;
  val_para(vcalend,'FECMES',' ', ;
  18,30)
@ 14, 2 SAY '   Fte. Fto. : ' GET  ;
  vcodfte PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vcodfte),  ;
  val_para(vcodfte,'CODFTE',' ', ;
  18,30), .T.)
READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY() = 27
     RETURN
ENDIF
ACTIVATE WINDOW standby
@ 1, 14 SAY  ;
  'Espere un momento ...' COLOR W+/ ;
  RB* 
SELECT rep1
ZAP
vind = SYS(3) + '.IDX'
zind = SYS(3) + '.IDX'
INDEX ON LEFT(estfun, 5) + codcad +  ;
      codfte + codpart TO (vind)
IF vtotal = 1
     SELECT itepar.codpart,  ;
            itepar.valpart,  ;
            itepar.periodo,  ;
            itepar.codcad,  ;
            itepar.cresup,  ;
            itepar.tra001,  ;
            itepar.tra003,  ;
            itepar.tra004,  ;
            itepar.tra005,  ;
            itepar.codfte,  ;
            itepar.totafe,  ;
            itepar.totcal,  ;
            itepar.ubicac,  ;
            itepar.estfun FROM  ;
            ITEPAR WHERE periodo =  ;
            ALLTRIM(vperiodo) AND  ;
            IIF( NOT  ;
            EMPTY(ALLTRIM(vcodcad)),  ;
            codcad =  ;
            ALLTRIM(vcodcad),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodfte)),  ;
            codfte =  ;
            ALLTRIM(vcodfte),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vuniges)),  ;
            uniges =  ;
            ALLTRIM(vuniges),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vunieje)),  ;
            unieje =  ;
            ALLTRIM(vunieje),  ;
            .T.) INTO CURSOR  ;
            PRESUX
ELSE
     SELECT itepar.codpart,  ;
            itepar.valpart,  ;
            itepar.periodo,  ;
            itepar.codcad,  ;
            itepar.cresup,  ;
            itepar.tra001,  ;
            itepar.tra003,  ;
            itepar.tra004,  ;
            itepar.tra005,  ;
            itepar.codfte,  ;
            itepar.totafe,  ;
            itepar.totcal,  ;
            itepar.ubicac,  ;
            itepar.estfun FROM  ;
            ITEPAR WHERE periodo =  ;
            ALLTRIM(vperiodo) AND  ;
            IIF( NOT  ;
            EMPTY(ALLTRIM(vuniges)),  ;
            SUBSTR(estfun, 1, 2) =  ;
            ALLTRIM(vuniges),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vunieje)),  ;
            SUBSTR(estfun, 3, 3) =  ;
            ALLTRIM(vunieje),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodfun)),  ;
            SUBSTR(estfun, 6, 2) =  ;
            ALLTRIM(vcodfun),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodprg)),  ;
            SUBSTR(estfun, 8, 3) =  ;
            ALLTRIM(vcodprg),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodspr)),  ;
            SUBSTR(estfun, 11, 4) =  ;
            ALLTRIM(vcodspr),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vactpry)),  ;
            SUBSTR(estfun, 15, 6) =  ;
            ALLTRIM(vactpry),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodcom)),  ;
            SUBSTR(estfun, 21, 5) =  ;
            ALLTRIM(vcodcom),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodfte)),  ;
            codfte =  ;
            ALLTRIM(vcodfte),  ;
            .T.) INTO CURSOR  ;
            PRESUX
ENDIF
GOTO TOP
vind = SYS(3) + '.DBF'
COPY TO (vind)
USE IN 17 EXCLUSIVE (vind) ALIAS  ;
    presu1
SELECT presu1
INDEX ON LEFT(estfun, 5) + codcad +  ;
      codfte + codpart TO (zind)
IF vtotal = 1
     SELECT calen.nummes,  ;
            calen.codpart,  ;
            calen.valpart,  ;
            calen.periodo,  ;
            calen.codcad,  ;
            calen.codfte,  ;
            calen.totafe,  ;
            calen.ampliar,  ;
            calen.totoc,  ;
            calen.totos,  ;
            calen.estfun FROM  ;
            CALEN WHERE periodo =  ;
            ALLTRIM(vperiodo) AND  ;
            IIF( NOT  ;
            EMPTY(ALLTRIM(vcodcad)),  ;
            codcad =  ;
            ALLTRIM(vcodcad),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodfte)),  ;
            codfte =  ;
            ALLTRIM(vcodfte),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vuniges)),  ;
            uniges =  ;
            ALLTRIM(vuniges),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vunieje)),  ;
            unieje =  ;
            ALLTRIM(vunieje),  ;
            .T.) AND nummes <=  ;
            ALLTRIM(vcalend) INTO  ;
            CURSOR CalenX
ELSE
     SELECT calen.nummes,  ;
            calen.codpart,  ;
            calen.valpart,  ;
            calen.periodo,  ;
            calen.codcad,  ;
            calen.codfte,  ;
            calen.totafe,  ;
            calen.ampliar,  ;
            calen.totoc,  ;
            calen.totos,  ;
            calen.estfun FROM  ;
            CALEN WHERE periodo =  ;
            ALLTRIM(vperiodo) AND  ;
            IIF( NOT  ;
            EMPTY(ALLTRIM(vuniges)),  ;
            SUBSTR(estfun, 1, 2) =  ;
            ALLTRIM(vuniges),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vunieje)),  ;
            SUBSTR(estfun, 3, 3) =  ;
            ALLTRIM(vunieje),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodfun)),  ;
            SUBSTR(estfun, 6, 2) =  ;
            ALLTRIM(vcodfun),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodprg)),  ;
            SUBSTR(estfun, 8, 3) =  ;
            ALLTRIM(vcodprg),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodspr)),  ;
            SUBSTR(estfun, 11, 4) =  ;
            ALLTRIM(vcodspr),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vactpry)),  ;
            SUBSTR(estfun, 15, 6) =  ;
            ALLTRIM(vactpry),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodcom)),  ;
            SUBSTR(estfun, 21, 5) =  ;
            ALLTRIM(vcodcom),  ;
            .T.) AND IIF( NOT  ;
            EMPTY(ALLTRIM(vcodfte)),  ;
            codfte =  ;
            ALLTRIM(vcodfte),  ;
            .T.) AND nummes <=  ;
            ALLTRIM(vcalend) INTO  ;
            CURSOR CalenX
ENDIF
SELECT presu1
GOTO TOP
SCAN
     SCATTER MEMVAR
     m.valpres = m.valpart
     m.cresup = getcre()
     m.transf = gettra()
     SELECT rep1
     IF f_appd()
          GATHER MEMVAR
     ENDIF
     SELECT presu1
ENDSCAN
SELECT calenx
GOTO TOP
SCAN
     SCATTER MEMVAR
     vrep2 = LEFT(m.estfun, 5) +  ;
             m.codcad + m.codfte +  ;
             m.codpart
     STORE 0 TO m.c_01, m.c_02,  ;
           m.c_03, m.c_04, m.c_05,  ;
           m.c_06, m.c_07, m.c_08,  ;
           m.c_09, m.c_10, m.c_11,  ;
           m.c_12
     SELECT rep1
     SEEK ALLTRIM(vrep2)
     IF  .NOT. FOUND()
          vcod = 'C_' +  ;
                 ALLTRIM(m.nummes)
          m.&vcod=m.valpart + m.Ampliar
          APPEND BLANK
          GATHER MEMVAR
          vkey = LEFT(m.estfun,  ;
                 5) + m.codcad +  ;
                 m.codfte +  ;
                 m.codpart
          SELECT presu1
          GOTO TOP
          SEEK vkey
          IF FOUND()
               vtransf = presu1.tra001 +  ;
                         presu1.tra003 +  ;
                         presu1.tra004 +  ;
                         presu1.tra005
               SELECT rep1
               REPLACE valpres  ;
                       WITH  ;
                       presu1.valpart,  ;
                       cresup  ;
                       WITH  ;
                       presu1.cresup,  ;
                       transf  ;
                       WITH  ;
                       vtransf
          ENDIF
          m.valpart = 0
     ELSE
          vcod = 'C_' +  ;
                 ALLTRIM(m.nummes)
          REPLACE &vcod WITH &vcod + CALENX.VALPART;
+ CALENx.ampliar
          m.valpart = 0
     ENDIF
     SELECT calenx
ENDSCAN
USE IN 17
SELECT rep1
DEACTIVATE WINDOW standby
GOTO TOP
IF vtotal = 1
     IF vtipo = 1
          DO reporte WITH 2,  ;
             'MarCal1',  ;
             ' A nivel de Marco Presupuestal'
     ELSE
          DO reporte WITH 2,  ;
             'MarCal11',  ;
             ' A nivel de Marco Presupuestal'
     ENDIF
ELSE
     IF vtipo = 1
          DO reporte WITH 2,  ;
             'MarCaG1',  ;
             ' A nivel de Marco Presupuestal'
     ELSE
          DO reporte WITH 2,  ;
             'MarCaG11',  ;
             ' A nivel de Marco Presupuestal'
     ENDIF
ENDIF
CLOSE INDEX
SET FILTER TO
SELECT 13
SET RELATION TO
SELECT 14
SET RELATION TO
SELECT calen
SET ORDER TO CALEN2
RETURN
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
FUNCTION sumpre
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
SUM valpRES+cresup+TRANSF TO suma FOR;
&vFiltro = vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumar
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+left(codpart,2)'
ENDCASE
SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12;
 TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION summes
PARAMETER vcalen, part, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM &part TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION salprg
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+left(codpart,2)'
ENDCASE
SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12);
TO sumA FOR &vFiltro = vCalen
GOTO vrec
RETURN suma
*
FUNCTION summes1
PARAMETER vfiltro, part
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
IF vtipo <= 25
     SUM &part TO suma FOR LEFT(ESTFUN,vtipo);
= vFiltro
ELSE
     SUM &part TO suma FOR LEFT(ESTFUN,25)+codfte;
= vFiltro
ENDIF
GOTO vrec
RETURN suma
*
FUNCTION sumar1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
DO CASE
     CASE vtipo <= 25
          SUM FOR LEFT(estfun,  ;
              vtipo) = vfiltro  ;
              c_01 + c_02 + c_03 +  ;
              c_04 + c_05 + c_06 +  ;
              c_07 + c_08 + c_09 +  ;
              c_10 + c_11 + c_12  ;
              TO suma
     CASE vtipo > 25 .AND. vtipo <  ;
          28
          SUM FOR LEFT(estfun,  ;
              25) + codfte =  ;
              vfiltro c_01 + c_02 +  ;
              c_03 + c_04 + c_05 +  ;
              c_06 + c_07 + c_08 +  ;
              c_09 + c_10 + c_11 +  ;
              c_12 TO suma
     CASE vtipo > 28 .AND. vtipo <  ;
          30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              LEFT(codpart, 2) =  ;
              vfiltro c_01 + c_02 +  ;
              c_03 + c_04 + c_05 +  ;
              c_06 + c_07 + c_08 +  ;
              c_09 + c_10 + c_11 +  ;
              c_12 TO suma
     CASE vtipo > 30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              codpart = vfiltro  ;
              c_01 + c_02 + c_03 +  ;
              c_04 + c_05 + c_06 +  ;
              c_07 + c_08 + c_09 +  ;
              c_10 + c_11 + c_12  ;
              TO suma
ENDCASE
GOTO vrec
RETURN suma
*
FUNCTION salprg1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
DO CASE
     CASE vtipo <= 25
          SUM FOR LEFT(estfun,  ;
              vtipo) = vfiltro  ;
              valpres + cresup +  ;
              transf - (c_01 +  ;
              c_02 + c_03 + c_04 +  ;
              c_05 + c_06 + c_07 +  ;
              c_08 + c_09 + c_10 +  ;
              c_11 + c_12) TO  ;
              suma
     CASE vtipo > 25 .AND. vtipo <  ;
          28
          SUM FOR LEFT(estfun,  ;
              25) + codfte =  ;
              vfiltro valpres +  ;
              cresup + transf -  ;
              (c_01 + c_02 + c_03 +  ;
              c_04 + c_05 + c_06 +  ;
              c_07 + c_08 + c_09 +  ;
              c_10 + c_11 + c_12)  ;
              TO suma
     CASE vtipo > 28 .AND. vtipo <  ;
          30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              LEFT(codpart, 2) =  ;
              vfiltro valpres +  ;
              cresup + transf -  ;
              (c_01 + c_02 + c_03 +  ;
              c_04 + c_05 + c_06 +  ;
              c_07 + c_08 + c_09 +  ;
              c_10 + c_11 + c_12)  ;
              TO suma
     CASE vtipo > 30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              codpart = vfiltro  ;
              valpres + cresup +  ;
              transf - (c_01 +  ;
              c_02 + c_03 + c_04 +  ;
              c_05 + c_06 + c_07 +  ;
              c_08 + c_09 + c_10 +  ;
              c_11 + c_12) TO  ;
              suma
ENDCASE
GOTO vrec
RETURN suma
*
FUNCTION sumpre1
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM valpres+cresup+transf TO suma FOR;
&vFiltro= vCalen
GOTO vrec
RETURN suma
*
