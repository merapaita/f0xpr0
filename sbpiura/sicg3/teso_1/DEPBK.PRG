*----------------------------------------------------------------------
* DepBk. prg                                         
* Depura backups
* Autor: C?sar Ch?varry Arias
* Fecha: 19-05-93
*----------------------------------------------------------------------
 DECLARE arreglo(1,5)
 N = ADIR( arreglo,'..\DBFBAK\*.ZIP')
 IF N > 0
    DEFINE POPUP _depu FROM 08,50 TO 23,77 TITLE " Depuraci?n de Backups " MARK '' FOOTER ' F10 para Confirmar ' MULTISELECT  SCROLL MARGIN   COLOR SCHEME 2

    FOR i = 1 TO N
        DEFINE BAR i OF _depu PROMPT  Arreglo(i,1)
    ENDFOR
    ON KEY LABEL F10  DEACTIVATE POPUP
    ACTIVATE POPUP _depu
    ON KEY LABEL F10
    IF LASTKEY() # 27
       N = CNTBAR('_depu')

       FOR i= 1 TO N
           IF MRKBAR('_Depu',i)
              fil = '..\DBFBAK\'+ALLTRIM ( PRMBAR('_Depu',i))
              ERASE &FIL
           ENDIF
       ENDFOR
    ENDIF
    DEACTIVATE POPUP _Depu
    RELEASE POPUP _Depu
 ELSE
    DO STANDBY WITH 'Los archivos de backups est?n depurados'
 ENDIF
 RETURN


