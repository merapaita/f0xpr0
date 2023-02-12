vmens08 = 'No hay registros para procesar'
CLOSE DATABASES
USE IN 1 parmae ALIAS parma ORDER  ;
    parmae1
USE IN 2 maepar ALIAS presu ORDER  ;
    maepar1
USE IN 3 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 4 maepre ALIAS maepre  ;
    ORDER maepre1
DO pantalla
CLOSE DATABASES
RETURN
*
PROCEDURE pantalla
DEFINE WINDOW lis_1 FROM 10, 10  ;
       TO 15, 70 FLOAT TITLE  ;
       ' °°  Actualizaci¢n Presupuestal °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
STORE 0 TO vtotal, vtipo
STORE SPACE(2) TO vperiodo,  ;
      vcalend
@ 1, 2 SAY '     Periodo : ' GET  ;
  vperiodo PICTURE '!!' VALID   ;
  .NOT. EMPTY(vperiodo)
@ 3, 2 SAY '  Calendario : ' GET  ;
  vcalend PICTURE '!!' VALID  ;
  val_para(vcalend,'FECMES',' ', ;
  18,25)
READ VALID val_read()
DEACTIVATE WINDOW lis_1
RELEASE WINDOW lis_1
IF LASTKEY() = 27
     RETURN
ENDIF
SELECT itepar
IF EOF()
     DO standby WITH vmens08
ELSE
     DEFINE WINDOW xwait FROM 20,  ;
            06 TO 22, 78 COLOR  ;
            SCHEME 05
     ACTIVATE WINDOW xwait
     @ 0, 10 SAY  ;
       ' Espere un Momento...Actualizando Presupuesto!'  ;
       COLOR W+/RB* 
     DO actualiza
     DEACTIVATE WINDOW xwait
     RELEASE xwait
ENDIF
CLOSE DATABASES
RETURN
*
PROCEDURE actualiza
SELECT itepar
GOTO TOP
SCAN
     vmes = 'M_' + vcalend
     REPLACE &vmes with 1
     DO chequea
ENDSCAN
RETURN
*
FUNCTION chequea
vtotmes = m_01 + m_02 + m_03 +  ;
          m_04 + m_05 + m_06 +  ;
          m_07 + m_08 + m_09 +  ;
          m_10 + m_11 + m_12
vvalpart = valpart + cresup +  ;
           tra001 + tra003 +  ;
           tra004 + tra005
IF vvalpart < vtotmes
     vsaldo = vtotmes - vvalpart
ENDIF
REPLACE tri_01 WITH m_01 + m_02 +  ;
        m_03
REPLACE tri_02 WITH m_04 + m_05 +  ;
        m_06
REPLACE tri_03 WITH m_07 + m_08 +  ;
        m_09
REPLACE tri_04 WITH m_10 + m_11 +  ;
        m_12
RETURN .T.
*
