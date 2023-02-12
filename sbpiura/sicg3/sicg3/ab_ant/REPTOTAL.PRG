*----------------------------------------------------------------------*
* Programa: Presenta.prg											   *	
* 
* Este programa permite visualizar e imprimir el Stock de uno o todos  *
* los Art¡culos Registrados hasta la fecha.							   *
*----------------------------------------------------------------------*

DEACTIVA WINDOW ALL
*-Ambiente
SET DATE DMY
SET TALK OFF
SET ECHO OFF
SET SAFETY OFF
SET DELETED ON
SET ESCAPE OFF
SET CLOCK ON
CLOSE DATA
SET CURSOR ON
*- Abre los archivos correspondientes
*- Abre los archivos correspondientes
close index
USE Iteart   IN 1   order tag CODARTINI  ALIAS IteArt
USE stock    IN 2   order tag Stockx   ALIAS stock

SELECT iteart
go top
set relation to codart into stock
Selec IteArt
go top
If !eof() 	
DO REPORTE WITH 2, "Total", " Lista de todos los Producto " ,1,.F.,.T. 
EndIf
SET FILTER TO
return

PROCEDURE Val_entradas
*----------------------
SET FILTER TO
toma=alias()
Canti=0
selec stock
set filter to  Stock.codart=IteArt.CodArt and Stock.tipoMov="E"
GO TOP
do while !eof() 
		Canti=canti+Stock.Cantidad
		SKIP
enddo
set filter to
selec IteArt
return Canti

PROCEDURE Val_Salidas
*----------------------
Canti1=0
selec stock
go top
set filter to  Stock.codart=IteArt.CodArt and Stock.tipoMov="S"
Seek IteArt.CodArt
do while !eof() 
		Canti1=Canti1+Stock.Cantidad
		SKIP
enddo
set filter to
selec IteArt
return Canti1
