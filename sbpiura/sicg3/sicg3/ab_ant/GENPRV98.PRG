** 컴컴컴컴컴컴컴컴컴?
** Programa	: GenPrv98.prg
** 			  Reporta el Record de las O/C y O/S por C줰igo de Clase,
**			  Proveedor seg즢 la Clasificaci줻.
** Fecha	: 18 de Setiembre de 1998.
** 컴컴컴컴컴컴컴컴컴?
** Autor	: Norberto Recarte Romero.
** 컴컴컴컴컴컴컴컴컴?

Set Talk off
Set Echo off

*-Abre Archivos.

USE Parmae  IN 1 ORDER TAG Parmae1  ALIAS parma
USE Prvaux  IN 2 ORDER TAG prvaux3  ALIAS aux
USE OrdCom  IN 3 ORDER TAG OrdCom4  ALIAS Orden
USE OrdSer  IN 4 ORDER TAG OrdSer6  ALIAS OrdSer
USE Promae  IN 6 order tag Promae1  ALIAS Promae

*- Mensajes de aviso al usuario

Vmens01 = ' Gu죂s de Internamiento : REVISION '
Vmens02 = ' Registro de Gu죂s de Internamiento '
Vmens04 = 'Dicho Gu죂s de Internamiento no fue encontrado'
Vmens05 = 'No existe Gu죂s de Internamiento anterior'
Vmens06 = 'No existe Gu죂s de Internamiento siguiente'
Vmens07 = '? Desea ANULAR 굎te Gu죂s de Internamiento ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Gu죂s de Internamiento ha sido anulado'
Vmens10 = 'Este Gu죂s de Internamiento ya fue Contabilizado'
Vmens11 = 'Este Gu죂s de Internamiento ha sido devuelto'
