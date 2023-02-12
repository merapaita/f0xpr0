*   PARA AGREGAR NOMBRE DEL CUARTEL EN LA CONSULTA
*

AP = "RODRIGUEZ"
AM = ''
NO = ''

	SELECT Fallecid.Cod_Cement,Fallecid.Cod_Cuarte,Fallecid.Cod_Fallec,Columna,Fila,Ape_Patern,Ape_Matern,Nombres,Cod_Tipo,Nom_Cuarte;
		FROM Fallecid,Cuartel ;
		WHERE Fallecid.Cod_Cement=Cuartel.Cod_cement and Fallecid.Cod_Cuarte = Cuartel.Cod_Cuarte and Ape_patern = alltrim(ap) AND Ape_Matern = alltrim(am) AND Nombres = ALLTRIM(no) ;
		ORDER BY Ape_Patern,Ape_Matern,Nombres;
		INTO Cursor tmpCre
