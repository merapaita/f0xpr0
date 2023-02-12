PARAMETER vdir, vext, vdirdest
PRIVATE vnamezip, vdefazip,  ;
        volddir, vret
IF PARAMETERS() = 2
     vdirdest = '..\ZIP\'
ENDIF
vret = .T.
volddir = SYS(5) + CURDIR()
vdir = FULLPATH(vdir)
SET DEFAULT TO (vdirdest)
vnamezip = get_file('ZIP', ;
           'Grabar el "BackUp" como:', ;
           .T.,.F.,.F.,.F.)
IF vnamezip = 'NEW FILE'
     IF pidfile(vnamezip,vdirdest, ;
        'ZIP')
          vnamezip = vdirdest +  ;
                     vnamezip
     ELSE
          vnamezip = ''
     ENDIF
ENDIF
IF EMPTY(vnamezip)
     WAIT WINDOW NOWAIT  ;
          'No seleccion¢ nombre del archivo.'
     vret = .F.
ELSE
     SET DEFAULT TO (vdir)
     = msgpro(.T.,23, ;
       ' ®   Espere un Momento Guardando Archivos  ¯ ' ;
       )
     run foxswap pkzip -u &vnamezip;
 &vext >nul
     = msgpro(.F.)
ENDIF
SET DEFAULT TO (volddir)
RETURN vnamezip
*
