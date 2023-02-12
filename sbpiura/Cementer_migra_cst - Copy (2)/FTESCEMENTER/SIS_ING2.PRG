*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\SIS_ING2.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 07/12/2006 at 16:12:30
*:
*:         Set by: SIS_CEM.PRG                       
*:
*:          Calls: RGB()              (function in ?)
*:               : INGGEN02.PRG
*:               : CUARTEL2.PRG
*:               : FALLEC02.PRG
*:               : VELACI02.PRG
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* SIS_ING2.PRG Programa Menú cementerio 2
clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'MANTENIMIENTO CEMENTERIO SAN MIGUEL ARCANGEL' fill file fondo05.bmp
   activate window his1
   clea
   @1,14 get new function '*' picture '\<Datos Generales Cementerio;Datos Generales Cuarteles;Datos Fallecidos;Control \<Velaciones;Retornar \<Menú Anterior' size 1.5,35,1 default 'Salir'
   read
   if new='Retornar Menú Anterior'
      close data
      clear
      return
   endif
   if new='Datos Generales Cementerio'
      do inggen02
      new='Datos Generales Cementerio'
   endif
   if new='Datos Generales Cuarteles'
      do cuartel2
      new='Datos Generales Cuarteles'
   endif
   if new='Datos Fallecidos'
      do fallec02
      new='Datos Fallecidos'
   endif
   if new='Control Velaciones'
      do velaci02
      new='Control Velaciones'
   endif
   new=''
enddo

* ichf 2006
*: EOF: SIS_ING2.PRG
