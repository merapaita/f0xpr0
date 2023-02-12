*:*****************************************************************************
*:
*: Procedure file: y:\CEMENTER\INGGEN01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 10/12/2006 at 11:30:20
*:
*:         Set by: SIS_ING1.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : TRIM()             (function in ?)
*:               : LASTKEY()          (function in ?)
*:
*:           Uses: MAESTRO.DBF            Alias: MA
*:
*:      CDX files: MAESTRO.CDX
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* INGGEN01.PRG  INgresar la información general Cementerio 1

set exclu off
clear
close all
clear all

use maestro  in 0 share orde cementerio alia ma
sele ma
loca for cod_cement=1
cod = cod_cement
nom = nom_cement
define window ingr01 from 02, 3 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'DATOS GENERALES CEMENTERIO '+trim(nom) fill file blanco.bmp
fea = fecha_aper
dir = direccion
res = responsabl
do while .t.
   clear
   activ window ingr01
   do while .t.
      clear
      @2,3 say 'Código              : ' get cod color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '##'
      clear gets
      @3,3 say 'Nombre Cementerio   : ' get nom color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @4,3 say 'Fecha de Apertura   : ' get fea color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @6,3 say 'Dirección           : ' get dir color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @7,3 say 'Responsable Actual  : ' get res color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @16,5 get mielecc function '*H' picture '\<Salir;\<Actualizar' size 2,17,1 default 'Actualizar'
      read
      if mielecc='Salir' or lastkey() = 27
         clear
         release window ingr01
         close data
         clear
         return
      else
         if mielecc='Actualizar'
            sele ma
            repla nom_cement with nom,fecha_aper with fea,direccion with dir,responsabl with res
            wait window 'LA INFORMACIÓN FUE ACTUALIZADA...'
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
*: EOF: INGGEN01.PRG
