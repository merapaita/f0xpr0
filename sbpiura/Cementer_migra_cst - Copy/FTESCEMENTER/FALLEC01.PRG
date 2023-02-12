*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\FALLEC01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 08/12/2006 at 18:44:10
*:
*:         Set by: SIS_ING1.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : NEWFAL01.PRG
*:               : CONFAL01.PRG
*:               : REPFAL01.PRG
*:               : CONOCF01.PRG
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* FALLEC01.PRG Programa Men� para FALLECIDOS cementerio 1
clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'MANTENIMIENTO FALLECIDOS SAN TEODORO' fill file fondo03.bmp
   activate window his1
   clea
   @1,14 get new function '*' picture '\<Ingreso Nuevo Fallecido;Consulta / Modificaci�n Fallecido;Consulta / Modificaci�n Ocupaci�n Futura;\<Reportes Fallecidos;Retornar \<Men� Anterior' size 1.5,35,1 default 'Salir'
   read
   if new='Retornar Men� Anterior'
      close data
      clear
      return
   endif
   if new='Ingreso Nuevo Fallecido'
      do newfal01
      new='Ingreso Nuevo Fallecido'
   endif
   if new='Consulta / Modificaci�n Fallecido'
      do confal01
      new='Consulta / Modificaci�n Fallecido'
   endif
   if new='Reportes Fallecidos'
      do repfal01
      new='Reportes Fallecidos'
   endif
   if new='Consulta / Modificaci�n Ocupaci�n Futura'
      do conocf01
      new='Consulta / Modificaci�n Ocupaci�n Futura'
   endif
   new = ''
   
   
enddo

* ichf 2006
*: EOF: FALLEC01.PRG