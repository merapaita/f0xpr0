PUBLIC c_panta, c_borde, c_popup, c_texto, c_fondo

*DO Fox_Ambi
SET TALK OFF
SET CENT ON
SET EXCLU OFF
SET DELETE ON
SET CONFIRM ON
SET DATE FRENCH

SET FUNCTION  1 TO
SET FUNCTION  2 TO
SET FUNCTION  3 TO
SET FUNCTION  4 TO
SET FUNCTION  5 TO
SET FUNCTION  6 TO
SET FUNCTION  7 TO
SET FUNCTION  8 TO
SET FUNCTION  9 TO
SET FUNCTION 10 TO
SET CLOCK ON
SET CLOCK TO         00,69

worker = .T.
rotulo1 = "SBP" + space(30) + "Difuntos" + spac(30) + space(11)

**-- Colores
escolor = iscolor()
if escolor
   set color to ,n/w,b
   c_panta = 8
   c_borde = 10
   c_popup = 4  &&15
   c_texto = "N/W"
   c_fondo = 1
else
   c_panta = 8
   c_borde = 7
   c_popup = 3
   c_texto = "N+/W"
   c_fondo = 1
endif
