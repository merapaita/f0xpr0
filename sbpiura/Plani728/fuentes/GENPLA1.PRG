SET PROCEDURE TO Con_Lib
SET PATH TO ..\DATA
cMes_Sue ='08'
*set step on

CLOS DATA
USE Personal IN 1 ORDER TAG Persona3
USE maeniv	 IN 2 ORDER TAG MaeNiv1
USE IteNiv	 IN 3 ORDER TAG IteNiv1
USE ItePer	 IN 4 ORDER TAG ItePer1

SELE Personal
SET RELATION TO Niv_Per INTO MaeNiv ADDITIVE
SELE MaeNiv
SET RELATION TO Niv_Per INTO IteNiv ADDITIVE

SELE Personal

SCAN FOR !EMPTY(Ape_Nom)
	SELE ItePer
	SEEK Personal.Lib_Ele+cMes_Sue
	IF !FOUND()
		SELE IteNiv
		m.Lib_Ele = Personal.Lib_Ele
		m.Mes_Sue = cMes_Sue
		m.Estado  = IIF(Personal.Mes_Vac#cMes_Sue,Personal.Estado,'03')
		m.Niv_Per = Personal.Niv_Per
		SCAN WHILE m.Niv_Per = IteNiv.Niv_Per								&& esto preguntar en personal 
			m.Tip_Adn = IteNiv.Tip_Adn
			m.Cod_Adn = IteNiv.Cod_Adn
			m.Tot_Adn = IteNiv.Tot_Adn
			m.Porcent = IteNiv.Porcent
			m.Suj_Des = IteNiv.Suj_Des
			m.Periodo = ALLTRIM(STR(YEAR(DATE())))
			m.Num_Pla = Personal.Num_Pla
			SELE ItePer
			IF f_Appd()
				GATHER MEMVAR
				UNLOCK
			ENDIF
			SELE IteNiv
		ENDSCAN
	ENDIF
	SELE Personal
ENDSCAN