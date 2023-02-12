*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\VELACI01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 12/12/2006 at 10:49:20
*:
*:         Set by: SIS_ING1.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : GENVEL01.PRG
*:               : NEWVEL01.PRG
*:               : ANUVEL01.PRG
*:               : REPVEL01.PRG
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* VELACI01.PRG Programa Menú para VELACIONES cementerio 1
clear

do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'MANTENIMIENTO VELACIONES SAN MIGUEL ARCANGEL' fill file fondo03.bmp
   activate window his1
   clea
   @1,14 get new function '*' picture '\<Datos Generales Velaciones;\<Boleta de Pago Velaciones;\<Consultas Velaciones;\<Anulación Boleta de Pago Velaciones;\<Reportes de Velaciones;Retornar \<Menú Anterior' size 1.5,35,1 default 'Salir'
   read
   if new='Retornar Menú Anterior'
      close data
      clear
      return
   endif
   if new='Datos Generales Velaciones'
      do genvel01
      new='Datos Generales Velaciones'
   endif
   if new='Boleta de Pago Velaciones'
      nlin =0
      do newvel01
      new='Boleta de Pago Velaciones'
   endif
   if new='Consultas Velaciones'
      nlin =0
      do Boletas
      new='Consultas Velaciones'
   endif
   if new='Anulación Boleta de Pago Velaciones'
      do anuvel01
      new='Anulación Boleta de Pago Velaciones'
   endif
   if new='Reportes de Velaciones'
      do repvel01
      new='Reportes de Velaciones'
   endif
   new = ''
   
enddo

* ichf 2006
*: EOF: VELACI01.PRG
