*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\FALLEC02.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 08/12/2006 at 18:49:52
*:
*:         Set by: SIS_ING2.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : NEWFAL02.PRG
*:               : CONFAL02.PRG
*:               : REPFAL02.PRG
*:               : CONOCF02.PRG
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* fallec02.PRG Programa Menú para datos fallecidos cementerio 2
clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'MANTENIMIENTO FALLECIDOS SAN MIGUEL ARCANGEL' fill file fondo05.bmp
   activate window his1
   clea
   @1,14 get new function '*' picture '\<Ingreso Nuevo Fallecido;Consulta / Modificación Fallecido;Consulta / Modificación Ocupación Futura;\<Reportes Fallecidos;Retornar \<Menú Anterior' size 1.5,35,1 default 'Salir'
   read
   if new='Retornar Menú Anterior'
      close data
      clear
      return
   endif
   if new='Ingreso Nuevo Fallecido'
      do newfal02
      new='Ingreso Nuevo Fallecido'
   endif
   if new='Consulta / Modificación Fallecido'
      do confal02
      new='Consulta / Modificación Fallecido'
   endif
   if new='Reportes Fallecidos'
      do repfal02
      new='Reportes Fallecidos'
   endif
   if new='Consulta / Modificación Ocupación Futura'
      do conocf02
      new='Consulta / Modificación Ocupación Futura'
   endif
   new = ''
enddo

* ichf 2006
*: EOF: FALLEC02.PRG
