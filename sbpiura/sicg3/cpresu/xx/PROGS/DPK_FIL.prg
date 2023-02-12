PARAMETER vtodir, vfromdir
PRIVATE vnamezip, volddir, vret,  ;
        vdd
IF PARAMETERS() = 0
     WAIT WINDOW NOWAIT  ;
          'Tiene que definirse par metros.'
     RETURN .F.
ENDIF
IF PARAMETERS() = 1
     vfromdir = '..\ZIP\'
ENDIF
vret = .T.
volddir = SYS(5) + CURDIR()
vtodir = FULLPATH(vtodir)
SET DEFAULT TO (vfromdir)
IF SYS(5) = 'A:'
     vnamezip = get_file('ZIP', ;
                'Restaurar archivos desde:', ;
                .F.,.F.,.T.,.T., ;
                2)
ELSE
     vnamezip = get_file('ZIP', ;
                'Restaurar archivos desde:', ;
                .F.,.F.,.F.,.F.)
ENDIF
IF EMPTY(vnamezip)
     WAIT WINDOW NOWAIT  ;
          'No seleccion¢ nombre del archivo.'
     vret = .F.
ELSE
     SET DEFAULT TO (vtodir)
     = ADIR(vdd, vnamezip)
     IF TYPE('vDD') <> 'C'
          wait window 'No se hall¢ fecha del archivo &vNameZip ';
nowait
          vret = .F.
     ELSE
          if yesno('La copia &vNameZip se hizo el d¡a '+dtoc(vdd[1,3])+;
', a las &vDD[1,4] hrs. ¨Restaurar?')
               !pkunzip -o &vnamezip >nul
          ELSE
               vret = .F.
          ENDIF
     ENDIF
ENDIF
SET DEFAULT TO (volddir)
RETURN vret
*
