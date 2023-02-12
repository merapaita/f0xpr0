*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\REPCUA02.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 06/12/2006 at 20:39:14
*:
*:         Set by: CUARTEL2.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : REPCUA21.PRG
*:               : REPCUA22.PRG
*:               : REPCUA23.PRG
*:               : REPCUA24.PRG
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* repcua02.PRG Programa Menú Reportes de Cuarteles Cementerio 2

clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE CUARTELES SAN MIGUEL ARCANGEL' fill file fondo05.bmp
   activate window his1
   clea
   @1,14 get new function '*' picture 'Por \<Código Cuartel;Por \<Nombre Cuartel;Nichos \<Desocupados por Cuartel;Nichos comprados Ocupación \<Futura;Retornar \<Menú Anterior' size 1.5,35,1 default 'Salir'
   read
   if new='Retornar Menú Anterior'
      close data
      clear
      return
   endif
   if new='Por Código Cuartel'
      do repcua21
      new='Por Código Cuartel'
   endif
   if new='Por Nombre Cuartel'
      do repcua22
      new='Por Código Cuartel'
   endif
   if new='Nichos comprados Ocupación Futura'
      do repcua23
      new='Nichos comprados Ocupación Futura'
   endif
   if new='Nichos Desocupados por Cuartel'
      do repcua24
      new='Nichos Desocupados por Cuartel'
   endif
   new=''
enddo

* ichf 2006

*: EOF: REPCUA02.PRG
