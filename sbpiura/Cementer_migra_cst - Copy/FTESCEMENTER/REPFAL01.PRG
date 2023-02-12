*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\REPFAL01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 08/12/2006 at 18:51:20
*:
*:         Set by: FALLEC01.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : REPFAL11.PRG
*:               : REPFAL12.PRG
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* repfal01.PRG Programa Menú Reportes de Fallecidos Cementerio 1

clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE FALLECIDOS SAN TEODORO' fill file fondo03.bmp
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
      do repfal11
      new='Relación de Fallecidos'
   endif
   if new='Fallecidos por Tipo Pabellón'
      do repfal12
      new='Fallecidos por Tipo Pabellón'
   endif
   if new='Estadísticas'
      wait window 'PROGRAMA EN PROCESO DE PROGRAMACIÓN...'
      *		do repfal13
      new='Estadísticas'
   endif
   new=''
enddo

* ichf 2006
*: EOF: REPFAL01.PRG
