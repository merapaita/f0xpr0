*:*****************************************************************************
*:
*:        Program: C:\CEMENTER\SIS_CEM.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 14/12/2006 at 15:24:50
*:
*:          Calls: RGB()              (function in ?)
*:               : SPACE()            (function in ?)
*:               : SIS_ING1.PRG
*:               : SIS_ING2.PRG
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* SIS_CEM.PRG Programa Menú para Aplicativo de uso para los Cementerios de la Beneficiencia Pública de Piura

set safe off
set talk off
set echo off
set exclu off
close all
clear all
set century on
set dele on
set date to briti
*set PATH to csma\cementer
set PATH to y:\cementer
set device to screen
set sysmenu off
set confirm off
SET COLOR TO "W/N,N+/W,W+/N,W+/N, W/N, U+/N , W+/W, - , W+/N, W/N "
clear

*set proc to lisproc2


do while .t.
   define window his1 from 05, 5 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'SISTEMA INFORMACIÓN CEMENTERIOS BENEFICIENCIA PÚBLICA PIURA' fill	file fondo04.bmp
   activate window his1
   clea
   nue=space(10)
   @5,25 get nue function '*' picture 'Cementerio \<San Teodoro;Cementerio San Miguel \<Arcángel;\<Ingresos de Difuntos;\<Report Dif x cuarteles;\<Boletas Velaciones;Sa\<lir' size 1.25,35,1 default 'Salir'
   read
   release window his1
   do case
   case nue='Cementerio San Teodoro'
      do sis_ing1
   case nue='Cementerio San Miguel Arcángel'
      do sis_ing2
   case nue='Ingresos de Difuntos'
      do Difuntos
   case nue='Report Dif x cuarteles'
      do LisDif
   case nue='Boletas Velaciones'
      do Boletas
   case nue='Salir'
      close all
      clear all
      clear
      return
   otherw
      loop
   endcase
   close all
enddo

*ichf 2006
*: EOF: SIS_CEM.PRG
