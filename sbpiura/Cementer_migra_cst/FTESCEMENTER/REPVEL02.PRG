*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\REPVEL02.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 14/12/2006 at 20:29:52
*:
*:         Set by: VELACI02.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : REPVEL21.PRG
*:               : REPVEL22.PRG
*:               : REPVEL23.PRG
*:
*:      Documented 20:34:26                                FoxDoc version 3.00a
*:*****************************************************************************
* repvel02.PRG Programa Menú Reportes de Velaciones Cementerio 2

clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE VELACIONES' fill file fondo05.bmp
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
      do repvel21
      new='Listado Contratos por Cuarteles'
   endif
   if new='Resumen por Cuarteles'
      do repvel22
      new='Resumen por Cuarteles'
   endif
   if new='Reporte Boletas Anuladas'
      do repvel23
      new='Reporte Boletas Anuladas'
   endif
   new=''
enddo

* ichf 2006
*: EOF: REPVEL02.PRG
