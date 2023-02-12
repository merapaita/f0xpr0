** ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
** º  Name Program : Duplica.prg												  º
** º  Descripci¢n	: Marca aquellos registros que se encuentran duplicados.	  º
** º  Situaci¢n		:															  º
** º		  Actualmente el registro de proveedores cuenta con varios registros  º
** º		  que estan duplicados por que se debe realizar una depuraci¢n total  º
** º		  para el proximo a¤o.												  º
** º  Autor			: Norberto Recarte Romero.									  º
** ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

close all
clea all

Set defa to C:\bkmon98
Use c:\bkmon98\promae.dbf in 1 order tag promae1 alias Promae

codant=" "
SCAN
    wait window 'Procesando ...'+allt(promae.nompro) nowait
    if codant=promae.codprv
       ? promae.codprv+' '+promae.nompro
    endif
    codant=promae.Codprv
ENDSCAN
CLOS DATA
RETURN