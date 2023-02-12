*set step on

handle  = FOPEN("itecp1.DBF",0)		&& abre en modo lectura
handle1 = FCREATE("itecp2.DBF",0)		&& abre en modo lectura escritura

=FSEEK(handle,0,0)			&& muevo el puntero al inicio

FOR i = 1 to 500000
	byte = FGETS(handle,1)
	
	=FSEEK(handle1,0,2)
	=FWRITE(handle1,Byte,1)

	=FSEEK(handle,1,1)			&& muevo el puntero una posicion
ENDFOR

=FCLOSE(handle)
=FCLOSE(handle1)

Return .t.  

