USE PROMAE IN 1 ORDER TAG PROMAE1 ALIAS PROMAE
USE AUXIL  IN 2 ORDER TAG AUXIL1  ALIAS AUXIL

SELECT PROMAE
SCAN
	m.tipo="20"
	m.codigo=promae.codprv
	m.descri=promae.nompro
	select auxil
	seek m.tipo+m.codigo
	if !found()
		append blank
		gather memvar
	endif
	select promae
ENDSCAN
RETURN
