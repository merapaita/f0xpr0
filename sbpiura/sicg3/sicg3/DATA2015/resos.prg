nAnyo = 2010
nMesIni = 01
nMesFin = 12

cPeriodo = RIGH(STR(nAnyo),2)
cRuta = "h:\sicgDATA\data" + STR(nAnyo,4)+'\'

CLOSE data
SET EXCLU OFF
SET DELETE ON
set path to (cRuta)

sele OS.Periodo,OS.NumMes,OS.NumOS,OS.CodFte, OS.FecOS, OS.CodPrv, Prv.NomPro , PerHC, NumHC, ValTot, Detalle  ;
	FROM OrdSer OS;
		LEFT JOIN ProMae Prv ON (OS.CodPrv=Prv.CodPrv);
	WHERE OS.Periodo=cPeriodo and betw(val(OS.nummes),nMesIni,nMesFin);
	ORDER BY NomPro,OS.Periodo,OS.NumOS;
	INTO TABLE ResOS
RETURN

