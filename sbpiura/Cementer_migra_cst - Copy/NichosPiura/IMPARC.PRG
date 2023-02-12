*----------------------------------------------------------------------
* ImpArc. prg                                         Sistema Contable
* Imprime Archivos
* Autor: Carola Arrese Orellana
* Fecha: 08-06-94
*----------------------------------------------------------------------
 DECLARE arreglo(1,5)
 N = ADIR( arreglo,'*.LST')
 IF N > 0
    DEFINE POPUP _arch FROM 08,52 TO 23,79 TITLE " Impresi¢n de Archivos " MARK '' FOOTER ' F10 para Confirmar ' MULTISELECT  SCROLL MARGIN   COLOR SCHEME 2

    FOR i = 1 TO N
        DEFINE BAR i OF _arch PROMPT  Arreglo(i,1)
    ENDFOR
    ON KEY LABEL F10  DEACTIVATE POPUP
    ACTIVATE POPUP _arch
    ON KEY LABEL F10
    IF LASTKEY() # 27
       IF !EMPTY(LEFT(SYS(0),15))
          IF !YESNO("¨Imprime en impresora local?")
         **-- Impresora de red.
             SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
             SET PRINTER TO \\SPOOLER\NB
          ENDIF
       ENDIF
       IF !READY2PR()
          RETURN
       ENDIF
     
       N = CNTBAR('_Arch')

       FOR i= 1 TO N
           IF MRKBAR('_arch',i)
              fil  = ALLTRIM ( PRMBAR('_Arch',i))
              !TYPE &fil > PRN
              IF YesNo('Se borra el archivo ' + fil)
                 ERASE (FIL)
              ENDIF
           ENDIF
       ENDFOR
    ENDIF
    DEACTIVATE POPUP _Depu
    RELEASE POPUP _Depu
 ELSE
    DO STANDBY WITH 'No existen archivos para impresi¢n'
 ENDIF
 RETURN