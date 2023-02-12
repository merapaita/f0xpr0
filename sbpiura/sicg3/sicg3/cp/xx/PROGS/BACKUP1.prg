PARAMETER vtipo
set path to &vpath;..\CPRESU\
DO CASE
     CASE vtipo = 1
          = epk_fil('..\DATA\', ;
            '*.DBF *.FPT')
     CASE vtipo = 2
          = poperror( ;
            '   Al restaurar los archivos de datos de una copia de ' +  ;
            'seguridad se destruyen los anteriores y no se podr n recuperar.' +  ;
            CHR(13) +  ;
            '   Se recomienda sacar un "BackUp" antes de hacer este procedimiento.' ;
            )
          IF yesno( ;
             '¨Est  seguro que desea restaurar?' ;
             )
               = dpk_fil('..\DATA\')
          ENDIF
ENDCASE
set path to &vpath
RETURN
*
