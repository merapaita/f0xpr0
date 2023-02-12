*:*****************************************************************************
*:
*: Procedure file: C:\CEMENTER\VELACI02.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 10/12/2006 at 13:01:00
*:
*:         Set by: SIS_ING2.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : GENVEL02.PRG
*:               : NEWVEL02.PRG
*:               : ANUVEL02.PRG
*:               : REPVEL02.PRG
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* VELACI02.PRG Programa Menú para VELACIONES cementerio 2
clear
do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'MANTENIMIENTO VELACIONES SAN TEODORO' fill file fondo05.bmp
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
      do genvel02
      new='Datos Generales Velaciones'
   endif
   if new='Boleta de Pago Velaciones'
      do newvel02
      new='Boleta de Pago Velaciones'
   endif
   if new='Consultas Velaciones'
      nlin =0
      do Boletas
      new='Consultas Velaciones'
   endif
   if new='Anulación Boleta de Pago Velaciones'
      do anuvel02
      new='Anulación Boleta de Pago Velaciones'
   endif
   if new='Reportes de Velaciones'
      do repvel02
      new='Reportes de Velaciones'
   endif
   new = ''
enddo

* ichf 2006
*: EOF: VELACI02.PRG
