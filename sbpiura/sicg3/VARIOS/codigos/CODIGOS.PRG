*---------------------------------------------
* Funcion que permite Generar Claves de Acceso
*---------------------------------------------

Function Retorna
****************
PRIVATE cClave
CLOS DATA
USE Claves
SCAN

	cClave = chrtran(Claves.Clave,'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#',;
                                  'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789')

	REPLACE Claves.Clave2 WITH cClave
	
ENDSCAN
USE
RETURN


