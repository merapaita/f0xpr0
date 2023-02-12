*:*****************************************************************************
*:
*: Procedure file: y:\CEMENTER\ANUVEL01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 13/12/2006 at 15:28:00
*:
*:         Set by: VELACI01.PRG                      
*:
*:          Calls: ALLT()             (function in ?)
*:               : STR()              (function in ?)
*:               : RGB()              (function in ?)
*:               : TRIM()             (function in ?)
*:               : LASTKEY()          (function in ?)
*:               : RIGHT()            (function in ?)
*:               : YEAR()             (function in ?)
*:               : DATE()             (function in ?)
*:               : EOF()              (function in ?)
*:               : DTOC()             (function in ?)
*:               : RECNO()            (function in ?)
*:               : UPPER()            (function in ?)
*:
*:           Uses: VELACION.DBF           Alias: VE
*:               : MAESTRO.DBF            Alias: MA
*:               : FALLECID.DBF           Alias: FA
*:
*:      CDX files: VELACION.CDX
*:               : MAESTRO.CDX
*:               : FALLECID.CDX
*:
*:      Documented 20:34:26                                FoxDoc version 3.00a
*:*****************************************************************************
* anuvel01.PRG  Anula Boletas de Velaciones que ya han sido cargadas del Cementerio 1

set exclu off
clear
close all
clear all


use velacion in 0 share orde x_num_reci alia ve
use maestro  in 0 share orde cementerio alia ma
use fallecid in 0 share orde fallecido alia fa

sele ve
set relation to allt(str(ve.cod_cement))+allt(str(ve.cod_fallec)) into fa additive

sele ma
loca for cod_cement = 1
cod = cod_cement
nom = nom_cement

define window ingr01 from 02,3 to 25,70 float double color rgb(192,192,192,192,192,192) title 'ANULACIÓN RECIBO DE VELACIONES CEMENTERIO '+trim(nom) fill file blanco.bmp
activ window ingr01
do while .t.
   nr1=0
   @2,3 say 'N° Recibo que va ANULAR : ' get nr1 color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '#######'
   read
   if lastkey() = 27
      release window ingr01
      close data
      clear
      return
   endif
   nr = '0000000'+allt(str(nr1))
   nr = right(nr,7)
   anyo = allt(str(year(date())))
   sele ve
   br = allt(str(cod))+anyo+nr
   seek br
   if eof()
      wait window 'EL NUMERO DE BOLETA BUSCADA NO EXISTE...'
      loop
   else
      if anulado = .t.
         wait window 'RECIBO YA HA SIDO ANULADO EL '+dtoc(fec_anul)
         loop
      endif
      exit
   endif
enddo

regis = recno()
cf = cod_fallec
fr = fec_recibo
nre= num_recibo
nad= nom_adquir
dad= dir_adquir
any= anyo_velac
nfa= trim(fa.nombres)+' '+trim(fa.ape_patern)+' '+trim(fa.ape_matern)



clear
do while .t.
   clear
   @2,3 say 'Código              : ' get cod color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '##'
   @3,3 say 'Nombre Cementerio   : ' get nom color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
   clear gets
   @4,3 say 'Año de la Velación  : ' get any color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @6,3 say 'Número Boleta Pago  : ' get nre color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
   @7,3 say 'Fallecido          : ' get nfa color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
   @9,3 say 'Contratante        : ' get nad color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
   @11,3 say 'Dirección Contrat. : ' get dad color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
   clear gets
   @13,10 get mielecc2 function '*H Anular;Salir' size 2, 10, 1 default 'Anular'
   read cycle
   if mielecc2='Salir' or lastkey() = 27
      release window ingr01
      close data
      return
   else
      if mielecc2='Anular'
         sele ve
         goto regis
         wait window '¿Seguro que desea ANULAR EL RECIBO?  S / N  : ' to sn
         if upper(sn) = 'S'
            repla anulado with .t., fec_anul with date()
            wait window 'EL RECIBO FUE ANULADO... RECUERDE QUE DEBE RETENER LAS BOLETAS DE PAGO ANULADAS.'
            release window ingr01
            close data
            return
         endif
      endif
   endif
enddo

* ichf 2006
*: EOF: ANUVEL01.PRG
