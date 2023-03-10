*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\CUARTEL2.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 06/12/2006 at 20:29:24
*:
*:         Set by: SIS_ING2.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : NEWCUA02.PRG
*:               : CONCUA02.PRG
*:               : REPCUA02.PRG
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* cuartel1.PRG Programa Men? para datos generales de cuarteles
clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'MANTENIMIENTO CUARTELES SAN MIGUEL ARCANGEL' fill file fondo05.bmp
   activate window his1
   clea
   @1,14 get new function '*' picture '\<Nuevo Cuartel;Consulta / Modificaci?n de Cuartel;\<Reportes Cuarteles;Retornar \<Men? Anterior' size 1.5,35,1 default 'Salir'
   read
   if new='Retornar Men? Anterior'
      close data
      clear
      return
   endif
   if new='Nuevo Cuartel'
      do newcua02
      new='Nuevo Cuartel'
   endif
   if new='Consulta / Modificaci?n de Cuartel'
      do concua02
      new='Consulta / Modificaci?n de Cuartel'
   endif
   if new='Reportes Cuarteles'
      do repcua02
      new='Reportes Cuarteles'
   endif
   new=''
enddo

* ichf 2006
*: EOF: CUARTEL2.PRG
