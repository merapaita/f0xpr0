PUBLIC var_p_dir

var_p_dir = "D:\Marco\001\sicg3_20110907\varvfp"

WAIT WINDOW NOWAIT TIMEOUT 3 "Reconociendo la ruta... " + var_p_dir

Set Default To &var_p_dir
SET PATH TO progs, repos, forms


SET EXCLUSIVE OFF

SET EXACT OFF
SET TALK OFF
SET ECHO OFF
SET CENTURY ON
SET SAFETY OFF
SET CONFIRM ON
SET DATE TO dmy
SET STATUS BAR OFF
SET DELETED ON
SET SYSMENU OFF
SET ESCAPE OFF
SET NOTIFY OFF
CLEAR
*DO var_p_dir+'menu_principal.mpr'
*DO var_p_dir+'prg_addtoolbar.prg'
*_SCREEN.windowstate = 2
*_SCREEN.caption = "Software Inventario Mobiliario Institucional"
*_SCREEN.backcolor = RGB(128, 128, 128)
*_SCREEN.icon = var_p_dir + '\imagenes\simi.ico'
*_SCREEN.closable = .F.
*_SCREEN.picture = var_p_dir + '\imagenes\simi.jpg'
*POP MENU _MSYSMENU
*READ EVENTS
*ENDPROC
