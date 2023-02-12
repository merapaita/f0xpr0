USE STOCK
BROWSE   NOEDIT NOAPPEND NOMENU NOWAIT;
	TITLE " REGISTRO DE STOCK ";
	FIELDs;
	CodArt      : H= 'C¢digo',;
	Cantidad    : H= 'Cantidad' :P='99,999.99',;
   	Cosmed      : H= 'Uni'      :W=.F. :3,;
   	NumDoc      : H= 'N§ Dcto.',;   	
    TipDoc =iif(TipDoc="O/C","E","")  : H= 'Entr¢',;
   	TipoMov=iif(TipoMov="S","S","") : H= 'Sali¢',;
   	FechaMov    : H='Fecha',;
   	OBservA=substr(stock.OBservA,1,20) : H= 'Observaci¢n'
RETURN
