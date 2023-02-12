** ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
** Programa	: GenPrv98.prg
** 			  Reporta el Record de las O/C y O/S por C¢digo de Clase,
**			  Proveedor seg£n la Clasificaci¢n.
** Fecha	: 18 de Setiembre de 1998.
** ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
** Autor	: Norberto Recarte Romero.
** ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

Set Talk off
Set Echo off

*-Abre Archivos.

USE Parmae  IN 1 ORDER TAG Parmae1  ALIAS parma
USE Prvaux  IN 2 ORDER TAG prvaux3  ALIAS aux
USE OrdCom  IN 3 ORDER TAG OrdCom4  ALIAS Orden
USE OrdSer  IN 4 ORDER TAG OrdSer6  ALIAS OrdSer
USE Promae  IN 6 order tag Promae1  ALIAS Promae

*- Mensajes de aviso al usuario

Vmens01 = ' Gu¡as de Internamiento : REVISION '
Vmens02 = ' Registro de Gu¡as de Internamiento '
Vmens04 = 'Dicho Gu¡as de Internamiento no fue encontrado'
Vmens05 = 'No existe Gu¡as de Internamiento anterior'
Vmens06 = 'No existe Gu¡as de Internamiento siguiente'
Vmens07 = '¨ Desea ANULAR ‚ste Gu¡as de Internamiento ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Gu¡as de Internamiento ha sido anulado'
Vmens10 = 'Este Gu¡as de Internamiento ya fue Contabilizado'
Vmens11 = 'Este Gu¡as de Internamiento ha sido devuelto'
