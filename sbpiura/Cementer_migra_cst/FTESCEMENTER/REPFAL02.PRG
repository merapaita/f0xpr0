*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\REPFAL02.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 08/12/2006 at 18:51:36
*:
*:         Set by: FALLEC02.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : REPFAL21.PRG
*:               : REPFAL22.PRG
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* repfal02.PRG Programa Menú Reportes de Fallecidos Cementerio 2

clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE FALLECIDOS METROPOLITANO' fill file fondo05.bmp
   activate window his1
   clea
   *	@1,14 GET new FUNCTION '*' PICTURE '\<Relación de Fallecidos;Fallecidos por Tipo \<Pabellón;\<Estadísticas;Retornar \<Menú Anterior' SIZE 1.5,35,1 DEFAULT 'Salir'
   @1,14 get new function '*' picture '\<Relación de Fallecidos;Fallecidos por Tipo \<Pabellón;Retornar \<Menú Anterior' size 1.5,35,1 default 'Salir'
   read
   if new='Retornar Menú Anterior'
      close data
      clear
      return
   endif
   if new='Relación de Fallecidos'
      do repfal21
      new='Relación de Fallecidos'
   endif
   if new='Fallecidos por Tipo Pabellón'
      do repfal22
      new='Fallecidos por Tipo Pabellón'
   endif
   if new='Estadísticas'
      wait window 'PROGRAMA EN PROCESO DE PROGRAMACIÓN...'
      *		do repfal23
      new='Estadísticas'
   endif
   new=''
enddo

* ichf 2006
*: EOF: REPFAL02.PRG
