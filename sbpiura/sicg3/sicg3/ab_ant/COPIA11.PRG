DO RELA
Canti1=0
selec stock
set filter to  Stock.codart=IteArt.CodArt and tipdoc="DESP"
do while !eof() 
		Canti1=Canti1+Cantidad
		SKIP
enddo
set filter to
@12,12 SAY Canti1