*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\REPFAL21.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 01/12/2006 at 13:45:08
*:
*:         Set by: REPFAL02.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : REPFA_20.PRG
*:               : REPFA_21.PRG
*:               : REPFA_22.PRG
*:               : REPFA_23.PRG
*:               : REPFA_24.PRG
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* repfal21.PRG Programa Menú Reportes de Fallecidos Cementerio 2

clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE CUARTELES METROPOLITANO' fill file fondo05.bmp
   activate window his1
   clea
   @1,14 get new function '*' picture 'Orden Alfabético;Fecha Fallecimiento;Fecha Nacimiento;Por Sexo y Apellido Paterno;Personas Ilustres;Retornar \<Menú Anterior' size 1.5,35,1 default 'Salir'
   read
   if new='Retornar Menú Anterior'
      close data
      clear
      return
   endif
   if new='Orden Alfabético'
      do repfa_20
      new='Orden Alfabético'
   endif
   if new='Fecha Fallecimiento'
      do repfa_21
      new='Fecha Fallecimiento'
   endif
   if new='Fecha Nacimiento'
      do repfa_22
      new='Fecha Nacimiento'
   endif
   if new='Por Sexo y Apellido Paterno'
      do repfa_23
      new=''
   endif
   if new='Personas Ilustres'
      do repfa_24
      new='Personas Ilustres'
   endif
   new=''
enddo

* ichf 2006
*: EOF: REPFAL21.PRG
