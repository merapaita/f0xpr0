** 靈컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캠
** ?  Name Program : Duplica.prg												  ?
** ?  Descripci줻	: Marca aquellos registros que se encuentran duplicados.	  ?
** ?  Situaci줻		:															  ?
** ?		  Actualmente el registro de proveedores cuenta con varios registros  ?
** ?		  que estan duplicados por que se debe realizar una depuraci줻 total  ?
** ?		  para el proximo a쨚.												  ?
** ?  Autor			: Norberto Recarte Romero.									  ?
** 聃컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캭

close all
clea all

Set defa to C:\bkmon98
Use c:\bkmon98\promae.dbf in 1 order tag promae1 alias Promae

codant=" "
SCAN
    wait window 'Procesando ...'+allt(promae.nompro) nowait
    if codant=promae.numruc
       ? promae.numruc+' '+promae.nompro
    endif
    codant=promae.numruc
ENDSCAN
CLOS DATA
RETURN
