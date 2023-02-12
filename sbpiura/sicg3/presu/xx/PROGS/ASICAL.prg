USE IN 1 Itepar ALIAS itepar  ;
    ORDER Itepar1
USE IN 2 Calen ALIAS calen ORDER  ;
    Calen2
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
PUBLIC wmes, vuniges, vunieje
STORE SPACE(4) TO wmes
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
vcol = 'w+/w,n/w,W/W,w/W,,w+/b'
IF escolor
     DEFINE WINDOW AsiCenTe FROM 12,52;
TO 17,78  SHADOW COLOR &vcol
ELSE
     DEFINE WINDOW asicente FROM  ;
            12, 52 TO 17, 78  ;
            COLOR SCHEME c_popup
ENDIF
ACTIVATE WINDOW asicente
xbalance = 'Mensual'
xdigitos = 4
xmes = MONTH(DATE())
xano = VAL(SUBSTR(STR(YEAR(DATE()),  ;
       4), 3, 2))
vuniges = '  '
vunieje = '   '
@ 0, 2 SAY '       UNI.GES. :'  ;
  GET vuniges PICTURE '!!' VALID  ;
  val_para(vuniges,'UNIGES','C')
@ 1, 2 SAY '       UNI.EJE. :'  ;
  GET vunieje PICTURE '!!!' VALID  ;
  val_para1(vunieje,'UNIEJE' +  ;
  vuniges,'C')
@ 2, 0 SAY 'Gen. Calendario :' +  ;
  xbalance
@ 3, 0 SAY '      Mes y A¤o :'  ;
  GET xmes PICTURE '99' VALID  ;
  xmes > 0 .AND. xmes <= 12 WHEN  ;
  xbalance = 'Mensual'
@ 3, 20 GET xano PICTURE '9999'
READ
wmes = PADL(xmes, 2, '0')
IF LASTKEY() <> 27
     SELECT parma
     SEEK 'ASICAL' + wmes
     ok = .T.
     IF FOUND()
          IF yesno( ;
             'Ya se asigno anteriormente ...¨Deseas Reasignar?' ;
             )
               SELECT calen
               GOTO TOP
               DELETE FOR nummes =  ;
                      wmes .AND.  ;
                      IIF( .NOT.  ;
                      EMPTY(ALLTRIM(vuniges)),  ;
                      uniges =  ;
                      ALLTRIM(vuniges),  ;
                      .T.) .AND.  ;
                      IIF( .NOT.  ;
                      EMPTY(ALLTRIM(vunieje)),  ;
                      unieje =  ;
                      ALLTRIM(vunieje),  ;
                      .T.)
          ELSE
               ok = .F.
          ENDIF
     ELSE
          IF f_appd()
               REPLACE tipo WITH  ;
                       'ASICAL'
               REPLACE codigo  ;
                       WITH wmes
          ENDIF
     ENDIF
     IF ok
          DEFINE WINDOW xwait  ;
                 FROM 20, 06 TO  ;
                 22, 78 COLOR  ;
                 SCHEME 05
          ACTIVATE WINDOW xwait
          vind1 = SYS(3) + '.idx'
          @ 0, 10 SAY  ;
            ' Espere un Momento...Asignaci¢n de Calendario en Proceso!'  ;
            COLOR W+/RB* 
          DO CASE
               CASE xbalance =  ;
                    'Mensual'
                    DO asigna
          ENDCASE
          RELEASE WINDOW xwait
     ENDIF
ENDIF
DEACTIVATE WINDOW asicente
IF  .NOT. escolor
     RESTORE SCREEN FROM  ;
             principal
ENDIF
SHOW POPUP menu, pop_09
RETURN
*
PROCEDURE asigna
SELECT itepar
SET FILTER TO uniges + unieje = ALLTRIM(vuniges);
+ ALLTRIM(vunieje)
GOTO TOP
vmonto = 'M_' + wmes
wmes = ALLTRIM(wmes)
SCAN
     SCATTER MEMVAR
     vkey = m.periodo + m.uniges +  ;
            m.unieje + m.codcad +  ;
            m.codfte + m.codpart +  ;
            wmes
     vkey1 = m.periodo + m.uniges +  ;
             m.unieje + m.codcad +  ;
             m.codfte +  ;
             m.codpart
     SELECT calen
     SEEK vkey
     IF FOUND()
          REPLACE valpart WITH valpart+&vmonto
     ELSE
          APPEND BLANK
          GATHER MEMVAR
          REPLACE valpart WITH &vmonto
          REPLACE nummes WITH  ;
                  wmes
     ENDIF
     GOTO TOP
     SEEK vkey1
     vacu = 0
     SCAN WHILE periodo + uniges +  ;
          unieje + codcad +  ;
          codfte + codpart =  ;
          m.periodo + m.uniges +  ;
          m.unieje + m.codcad +  ;
          ALLTRIM(m.codfte) +  ;
          m.codpart
          REPLACE itepar.totcal  ;
                  WITH vacu +  ;
                  calen.valpart
          IF nummes < '04'
               REPLACE calen.valpre  ;
                       WITH  ;
                       itepar.totcal
          ELSE
               IF nummes < '07'
                    REPLACE calen.valpre  ;
                            WITH  ;
                            itepar.totcal -  ;
                            itepar.tri_01
               ELSE
                    IF nummes <  ;
                       '10'
                         REPLACE calen.valpre  ;
                                 WITH  ;
                                 itepar.totcal -  ;
                                 (itepar.tri_01 +  ;
                                 itepar.tri_02)
                    ELSE
                         REPLACE calen.valpre  ;
                                 WITH  ;
                                 itepar.totcal -  ;
                                 (itepar.tri_01 +  ;
                                 itepar.tri_02 +  ;
                                 itepar.tri_03)
                    ENDIF
               ENDIF
          ENDIF
          vacu = vacu +  ;
                 calen.valpart
     ENDSCAN
     SELECT itepar
ENDSCAN
SET FILTER TO
RETURN
*
PROCEDURE salida
RELEASE WINDOW asicente
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
