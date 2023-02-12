*IF FILE('catasi.txt')
*	arch = FOPEN('catasi.txt',12)
*ELSE
	arch = FCREATE('catasi.txt')	&& No. Crearlo
*ENDIF
IF arch < 0
	WAIT 'No puedo abrir o crear el archivo de salida' WINDOW NOWAIT
ELSE
	USE catasi ORDER TAG CatAsi5
	SCAN
		m.TipPre = TIPPRE
		m.Generic = PADL(GENERIC,1," ")
		m.sgn1 =  IIF(VAL(SGN1)#0, PADL(STR(VAL( SGN1),1),1,' ')," ")	&&	STR(VAL(SGN1),1)
		m.sgn2 =  IIF(VAL(SGN2)#0, PADL(STR(VAL( SGN2),2),2,' '),"  ")	&&	STR(VAL(SGN2),2)
		m.EspN1 = IIF(VAL(ESPN1)#0,PADL(STR(VAL(ESPN1),2),2,' '),"  ")	&&	STR(VAL(ESPN1),2)
		m.EspN2 = IIF(VAL(ESPN2)#0,PADL(STR(VAL(ESPN2),2),2,' '),"  ")	&&	STR(VAL(ESPN2),2)
		m.EspN3 = IIF(VAL(ESPN3)#0,PADL(STR(VAL(ESPN3),2),2,' '),"  ")
		m.EspN4 = IIF(VAL(ESPN4)#0,PADL(STR(VAL(ESPN4),2),2,' '),"  ") 		&&  ESPN4
		m.Detalle = DETALLE
		m.CodPart = m.TipPre+'.'+m.Generic+'.'+m.sgn1+'.'+m.SGN2+'.'+m.EspN1+'.'+m.EspN2+'.'+m.EspN3+'.'+m.EspN4
		DO WHILE RIGHT(ALLTRIM(m.CodPart),1)="."
			m.CodPart = SUBSTR(m.CodPart,1,LEN(ALLTRIM(m.CodPart))-1)
		ENDDO
*		set step on
		m.Descri = ALLTRIM(DESCRI)
		
		Cad_Ins = "insert into catasi (tippre, Generic, SGN1, SGN2, EspN1, EspN2, EspN3, EspN4, Detalle, CodPart, Descri) VALUES ('" + m.tippre+"','"+ m.Generic+"','"+ m.SGN1+"','"+ m.SGN2+"','"+ m.EspN1+"','"+ m.EspN2+"','"+ m.EspN3+"','"+ m.EspN4+"','"+m.detalle+"','"+m.codpart+"','"+m.descri+"');"+chr(13)
		=FWRITE(arch, Cad_Ins)
	ENDSCAN
ENDIF

=FCLOSE(arch)




