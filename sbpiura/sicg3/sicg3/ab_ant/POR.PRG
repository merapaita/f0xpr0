ACTIVATE WINDOW Wind_C2
CLEAR
@ 05,25 SAY "No tiene detalle, Revise ..."
    

 @  0,2  SAY "Tipo Articulo   : " GET m.TipArt
 @  0,40 SAY "Cod. Articulo : " GET  m.codArt color "G+/r"
 @  1,2  SAY "Fecha Registro  : " GET m.Fecreg 
 @  1,40 SAY "Cantidad Inicial: " GET m.CantIni 
 @  2, 2 SAY "Cod. Generico   : " GET m.Codgen 
 @  3, 2 SAY "Descripci¢n     : " GET m.Descri 
 @  4, 2 SAY "Cod. Clave      : " GET m.Codcla 
 @  5, 2 SAY "Cod. Detenc.    : " GET m.Coddet 
 @  6, 2 SAY "Cod. Unico      : " GET m.CodUni
 @  7, 2 SAY "Precio Unitario : " GET m.PreUni
 @  8, 2 SAY "Marca           : " GET m.Marca 
 @  9, 2 SAY "Cod. Contable   : " GET m.cod_Cont 
 @  10,2 SAY "Cod. Partida    : " GET m.CodPart 

@  0,2  SAY "Tipo Articulo   : " + m.TipArt
 @  0,40 SAY "Cod. Articulo : "  + m.codArt color "G+/r"
 @  1,2  SAY "Fecha Registro  : " + dtoc(m.Fecreg) 
 @  1,40 SAY "Cantidad Inicial: " + str(m.CantIni) 
 @  2, 2 SAY "Cod. Generico   : " + m.Codgen 
 @  3, 2 SAY "Descripci¢n     : " + m.Descri 
 @  4, 2 SAY "Cod. Clave      : " + m.Codcla 
 @  5, 2 SAY "Cod. Detenc.    : " + m.Coddet 
 @  6, 2 SAY "Cod. Unico      : " + m.CodUni
 @  7, 2 SAY "Precio Unitario : " + str(m.PreUni)
 @  8, 2 SAY "Marca           : " + m.Marca 
 @  9, 2 SAY "Cod. Contable   : " + m.cod_Cont 
 @  10,2 SAY "Cod. Partida    : " + m.CodPart 