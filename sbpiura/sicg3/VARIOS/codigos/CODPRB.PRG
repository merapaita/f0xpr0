*
*Aqui va la funcion: 
*Nota: param.dbf es mi tabla que yo he protegido.
*
Function Protec
******************************************
set step on
handle = FOPEN("PARAM.DBF",2)		&& abre en modo lectura escritura
primerbyte = FREAD(handle,1) 		&& lee el 1er byte
Do case
	Case primerbyte == CHR(255)		&& si protegido
		escrib = chr(3)				&& desprotege
	Case primerbyte == CHR(3)		&& si desprotegido
		escrib = chr(255)			&& protege
Endcase    
=FSEEK(handle,0,0)
=FWRITE(handle,escrib,1)
=FCLOSE(handle)
Return .t.  

*
** FUNCTION DesactivarTeclasWindows(tlEstado)
** Funcion para Desactivar las Teclas CRTL + ALT + SUPR de Windows
*
** Parametros
**    tlEstado   =  Estado de las Teclas  (VALOR LOGICO)
**          .T.  = Desactiva las Teclas
**          .F.  = Activa las Teclas
*
*** Rutina suministrada por: ROD       -  eendor@hotmail.com
*** Ultima Modificacion:     Foxperto  -  foxperto@yahoo.com
***                          Marzo 01 2002
*   Esta funcion la pueden copiar en su archivo de Rutinas e invocarla 
*   desde cualquier parte de su programa. (Visual FoxPro)
*
**************************************************************************
*
FUNCTION DesactivarTeclasWindows(tlEstado)

DECLARE INTEGER SystemParametersInfo IN USER32.DLL ;
LONG uAction, ;
LONG uParam, ;
STRING lpvParam, ;
LONG fuWinIni

uAction = 97
lpvParam = '0'
fuWinIni = 0

If tlEstado = .T.
	*--- deshabilita las teclas
	uParam = 1
	tnResultado = SystemParametersInfo(@uAction,@uParam,lpvParam,@fuWinIni)
 Else
	*--- habilita las teclas
	uParam = 0
	tnResultado = SystemParametersInfo(@uAction,@uParam,lpvParam,@fuWinIni)
EndIf

RETURN tnResultado 

ENDFUNC
*
** Fin DesactivarTeclasWindows()
