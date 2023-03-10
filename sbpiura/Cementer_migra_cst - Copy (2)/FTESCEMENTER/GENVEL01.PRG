*:*****************************************************************************
*:
*: Procedure file: y:\CEMENTER\GENVEL01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 12/12/2006 at 11:35:50
*:
*:  Procs & Fncts: ELECCION()
*:
*:         Set by: VELACI01.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : TRIM()             (function in ?)
*:               : RECNO()            (function in ?)
*:               : SPACE()            (function in ?)
*:               : ELECCION()         (function in GENVEL01.PRG)
*:               : LASTKEY()          (function in ?)
*:               : EOF()              (function in ?)
*:
*:           Uses: MASTERVE.DBF           Alias: MV
*:               : MAESTRO.DBF            Alias: MA
*:
*:      CDX files: MAESTRO.CDX
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* GENVEL01.PRG  Ingresar la informaci?n general de Velaciones para el Cementerio 1

set exclu off
clear
close all
clear all

use masterve in 0 share alia mv
use maestro  in 0 share orde cementerio alia ma
sele ma
loca for cod_cement = 1
cod = cod_cement
nom = nom_cement
define window ingr01 from 02,3 to 25,70 float double color rgb(192,192,192,192,192,192) title 'DATOS GENERALES PARA VELACIONES CEMENTERIO '+trim(nom) fill file blanco.bmp

activ window ingr01

@2,5 get mielecc function '*V' picture '\<Nuevo A?o;\<Consultar A?o Existente;\<Salir' size 2,17,1 default 'Salir'
read cycle

if mielecc = 'Salir'
   release window ingr01
   close data
   clear
   return
endif

sele mv
if mielecc='Consultar A?o Existente'
   set filter to cod_cement=cod
   define window encontrado from 8,1 to 23,85 title 'REGISTRO DE VELACIONES... ELEGIR EL QUE DESEA CONSULTAR' close float grow shadow zoom
   browse fields cod_cement :2:h='C',anyo :15:h='A?O VELACIONES',num_inicia :20:h='N? INICIO BOLETAS',valor_foco :10:h='COSTO FOCO';
      nomodify noappend nodelete window encontrado
   registro = recno()
   release window encontrado
   any = anyo
   nib = num_inicia
   vaf = valor_foco
   abi = abierto
   if abi = .t.
      apecie = 1
   else
      apecie = 2
   endif
else
   sele mv
   any = space(4)
   nib = 0
   vaf = 0
   apecie = 0
endif

do while .t.
   clear
   do while .t.
      clear
      @2,3 say 'C?digo              : ' get cod color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '##'
      @3,3 say 'Nombre Cementerio   : ' get nom color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      clear gets
      @4,3 say 'A?o de la Velaci?n  : ' get any color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @6,3 say 'N?mero Inical de las Boletas de Pago: ' get nib color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @7,3 say 'Valor de Foco       : ' get vaf color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      *		@9,3 say 'Apertura / Cierre Velaciones: ' Color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      
      @9,10 get apecie function '*RNH Apertura;Cierre' size 1, 5, 1 valid eleccion()
      if mielecc = 'Nuevo A?o'
         @11,5 get mielecc2 function '*H' picture '\<Salir;\<Actualizar' size 2,17,1 default 'Actualizar'
      else
         clear gets
         @11,5 get mielecc2 function '*H' picture '\<Salir' size 2,17,1 default 'Salir'
      endif
      read cycle
      if mielecc2='Salir' or lastkey() = 27
         clear
         release window ingr01
         close data
         clear
         return
      else
         if mielecc2='Actualizar'
            sele mv
            loca for anyo = any and cod_cement = cod
            if !eof()
               wait window 'ATENCI?N EL A?O INGRESADO YA EXISTE... NO SE PUEDE ACEPTAR ESTE A?O...'
               loop
            endif
            appen blank
            repla cod_cement with cod,anyo with any,num_inicia with nib,valor_foco with vaf
            wait window 'LA INFORMACI?N FUE GRABADA...'
            clear
            release window ingr01
            close data
            clear
            return
         endif
      endif
   enddo
enddo
clear
close data
return

*!*****************************************************************************
*!
*!       Function: ELECCION
*!
*!      Called by: GENVEL01.PRG                      
*!               : GENVEL02.PRG                      
*!
*!*****************************************************************************
function eleccion
if apecie <1 or apecie>2
   wait window 'Debe elegir si la velaci?n est? abierta o cerrada...'
   return .f.
endif
return .t.

* ichf 2006
*: EOF: GENVEL01.PRG
