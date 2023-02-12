PARAMETER vfilename, vpath, vext
_msg = 'Nombre Inv lido.'
vpath = IIF(PARAMETERS() = 1, '',  ;
        vpath)
DEFINE WINDOW _pidfil FROM 19, 27  ;
       TO 23, 75 IN screen COLOR  ;
       SCHEME 5
ACTIVATE WINDOW _pidfil
vtit = 'Ingrese el nombre del archivo a grabar :'
@ 0, (WCOLS() - LEN(vtit)) / 2  ;
  SAY vtit
@ 2, (WCOLS() - 6) / 2 GET  ;
  vfilename PICTURE  ;
  'NNNNNNNNNNNN' VALID val_file()  ;
  ERROR _msg
READ CYCLE
RELEASE WINDOW _pidfil
IF LASTKEY() = 27
     RETURN .F.
ENDIF
RETURN
*
FUNCTION val_file
PRIVATE _pasa
_pasa = .T.
_msg = 'Nombre Inv lido.'
IF LASTKEY() = 27
     vfilename = SPACE(12)
     RETURN .T.
ENDIF
vfilename = STRTRAN(vfilename,  ;
            ' ', '')
vpos = AT('.', vfilename)
IF vpos <> 0
     vname = LEFT(vfilename,  ;
             IIF(vpos < 9, vpos -  ;
             1, 8))
ELSE
     vfilename = ALLTRIM(vfilename)
     vname = IIF(LEN(vfilename) >  ;
             8, LEFT(vfilename,  ;
             8), vfilename)
ENDIF
vfilename = PADR(vname + '.' +  ;
            vext, 12, ' ')
DO CASE
     CASE EMPTY(vname)
          _pasa = .F.
     CASE FILE(vpath + vfilename)
          _pasa = .F.
          _msg = 'Archivo ya existe. Ingrese otro nombre.'
ENDCASE
IF _pasa
     wait window 'Ud. ha elegido el archivo &vFileName.';
nowait
     CLEAR READ
ENDIF
RETURN _pasa
*
