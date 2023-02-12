*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\REPCUA01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 06/12/2006 at 21:08:28
*:
*:         Set by: CUARTEL1.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : REPCUA11.PRG
*:               : REPCUA12.PRG
*:               : REPCUA13.PRG
*:               : REPCUA14.PRG
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* repcua01.PRG Programa Menú Reportes de Cuarteles Cementerio 1

clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE CUARTELES SAN TEODORO' fill file fondo03.bmp
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
      do repcua11
      new='Por Código Cuartel'
   endif
   if new='Por Nombre Cuartel'
      do repcua12
      new='Por Código Cuartel'
   endif
   if new='Nichos comprados Ocupación Futura'
      do repcua13
      new='Nichos comprados Ocupación Futura'
   endif
   if new='Nichos Desocupados por Cuartel'
      do repcua14
      new='Nichos Desocupados por Cuartel'
   endif
   new=''
enddo

* ichf 2006
*: EOF: REPCUA01.PRG
