*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\REPVEL01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 14/12/2006 at 18:24:02
*:
*:         Set by: VELACI01.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : REPVEL11.PRG
*:               : REPVEL12.PRG
*:               : REPVEL13.PRG
*:
*:      Documented 20:34:26                                FoxDoc version 3.00a
*:*****************************************************************************
* repvel01.PRG Programa Menú Reportes de Velaciones Cementerio 1

clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE VELACIONES' fill file fondo03.bmp
   activate window his1
   clea
   @1,14 get new function '*' picture '\<Listado Contratos por Cuarteles;\<Resumen por Cuarteles;Reporte Boletas \<Anuladas;Retornar \<Menú Anterior' size 1.5,35,1 default 'Salir'
   read
   if new='Retornar Menú Anterior'
      close data
      clear
      return
   endif
   if new='Listado Contratos por Cuarteles'
      do repvel11
      new='Listado Contratos por Cuarteles'
   endif
   if new='Resumen por Cuarteles'
      do repvel12
      new='Resumen por Cuarteles'
   endif
   if new='Reporte Boletas Anuladas'
      do repvel13
      new='Reporte Boletas Anuladas'
   endif
   new=''
enddo

* ichf 2006
*: EOF: REPVEL01.PRG
