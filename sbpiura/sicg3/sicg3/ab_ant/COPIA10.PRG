DO RELA
Canti=0
toma=alias()
selec stock
do while !eof() 
	if Stock.CodArt="17.550.097" and tipdoc="O/C"
		Canti=canti+Cantidad
	endif
enddo
@12,12 SAY Canti
*return Canti
