DIMENSION arreglo( 1, 5)
n = ADIR(arreglo,  ;
    '..\BACKUP\*.ZIP')
IF n > 0
     DEFINE POPUP _depu FROM 08,  ;
            50 TO 22, 70 TITLE  ;
            'Depuraci¢n de Backups'  ;
            MARK '' MARGIN MULTI  ;
            FOOTER  ;
            'F10 para Confirmar'  ;
            SCROLL COLOR SCHEME  ;
            2
     FOR i = 1 TO n
          DEFINE BAR i OF _depu  ;
                 PROMPT arreglo(i, ;
                 1)
     ENDFOR
     ON KEY LABEL F10 DEACTIVATE POPUP
     ACTIVATE POPUP _depu
     ON KEY LABEL F10
     IF LASTKEY() <> 27
          n = CNTBAR('_depu')
          FOR i = 1 TO n
               IF MRKBAR('_Depu',  ;
                  i)
                    fil = '..\BACKUP\' +  ;
                          ALLTRIM(PRMBAR('_Depu',  ;
                          i))
                    ERASE &FIL
               ENDIF
          ENDFOR
     ENDIF
     DEACTIVATE POPUP _depu
     RELEASE POPUP _depu
ELSE
     DO standby WITH  ;
        'Los archivos de backups est n depurados'
ENDIF
RETURN
*
