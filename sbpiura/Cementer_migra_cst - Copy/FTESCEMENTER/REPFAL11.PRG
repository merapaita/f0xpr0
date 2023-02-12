*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\REPFAL11.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 01/12/2006 at 15:13:10
*:
*:         Set by: REPFAL01.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : REPFA_00.PRG
*:               : REPFA_01.PRG
*:               : REPFA_02.PRG
*:               : REPFA_03.PRG
*:               : REPFA_04.PRG
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* repfal01.PRG Programa Menú Reportes de Fallecidos Cementerio 1

clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE CUARTELES SAN TEODORO' fill file fondo03.bmp
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
      do repfa_00
      new='Orden Alfabético'
   endif
   if new='Fecha Fallecimiento'
      do repfa_01
      new='Fecha Fallecimiento'
   endif
   if new='Fecha Nacimiento'
      do repfa_02
      new='Fecha Nacimiento'
   endif
   if new='Por Sexo y Apellido Paterno'
      do repfa_03
      new='Por Sexo y Apellido Paterno'
   endif
   if new='Personas Ilustres'
      do repfa_04
      new='Personas Ilustres'
   endif
   new=''
enddo

* ichf 2006
*: EOF: REPFAL11.PRG
