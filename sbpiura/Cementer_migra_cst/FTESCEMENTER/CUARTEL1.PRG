*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\CUARTEL1.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 01/12/2006 at 13:34:04
*:
*:         Set by: SIS_ING1.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : NEWCUA01.PRG
*:               : CONCUA01.PRG
*:               : REPCUA01.PRG
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* cuartel1.PRG Programa Menú para datos generales de cuarteles
set escape off
clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'MANTENIMIENTO CUARTELES SAN TEODORO' fill file fondo03.bmp
   activate window his1
   clea
   @1,14 get new function '*' picture '\<Nuevo Cuartel;Consulta / Modificación de Cuartel;\<Reportes Cuarteles;Retornar \<Menú Anterior' size 1.5,35,1 default 'Salir'
   read
   if new='Retornar Menú Anterior'
      close data
      clear
      return
   endif
   if new='Nuevo Cuartel'
      do newcua01
      new='Nuevo Cuartel'
   endif
   if new='Consulta / Modificación de Cuartel'
      do concua01
      new='Consulta / Modificación de Cuartel'
   endif
   if new='Reportes Cuarteles'
      do repcua01
      new='Reportes Cuarteles'
   endif
   new=''
enddo

* ichf 2006
*: EOF: CUARTEL1.PRG
