vap = ''
vNom = ''
 	SELECT Difunto.CodCem,Difunto.CodCuar,Difunto.Estado,CodDif,ApeDif,NomDif;
 		FROM Difunto,Cuartel;
 		WHERE Difunto.CodCem+Difunto.CodCuar=Cuartel.CodCem+Cuartel.CodCuar and Difunto.ApeDif = ALLTRIM(vAp) AND Difunto.NomDif = ALLTRIM(vNom);
		ORDER BY ApeDif,NomDif;
		INTO CURSOR xTbl


*m.codacc = 0
*go top
*scan
*	m.CodAcc = m.CodAcc + 1
*	REPLACE CodAcc WITH padl(alltrim(str(m.CodAcc)),4,'0')
*endscan