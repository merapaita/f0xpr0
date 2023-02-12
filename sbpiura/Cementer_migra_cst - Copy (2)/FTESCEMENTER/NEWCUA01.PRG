*:*****************************************************************************
*:
*: Procedure file: \CEMENTER\NEWCUA01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 01/12/2006 at 15:20:38
*:
*:  Procs & Fncts: ESCAPAR()
*:
*:         Set by: CUARTEL1.PRG                      
*:
*:          Calls: CHR()              (function in ?)
*:               : RGB()              (function in ?)
*:               : TRIM()             (function in ?)
*:               : SPACE()            (function in ?)
*:               : DATE()             (function in ?)
*:               : LASTKEY()          (function in ?)
*:               : ESCAPAR()          (function in NEWCUA01.PRG)
*:
*:           Uses: CUARTEL.DBF            Alias: CU
*:               : TIP_CUAR.DBF           Alias: TI
*:               : MAESTRO.DBF            Alias: MA
*:               : CU.DBF             
*:
*:      CDX files: CUARTEL.CDX
*:               : TIP_CUAR.CDX
*:               : MAESTRO.CDX
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* newcua01.PRG  Ingresa información general cuarteles del Cementerio San Teodoro

set exclu off
clear
close all
clear all
define popup cdcampo prompt field nom_tipo scroll margin mark chr(16)
define window entrada from 6,14 to 14,65


SET REPROCESS TO -1	

use cuartel  in 0 share orde cementerio alia cu
use tip_cuar in 0 share orde tipo alia ti
use maestro  in 0 share orde cementerio alia ma
USE ultcua1  in 0 share alia ul
sele ma
loca for cod_cement = 1
cod = cod_cement
nom = nom_cement
cementerio = nom
res = responsabl
define window ingr01 from 02, 3 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'DATOS GENERALES CEMENTERIO '+trim(cementerio) fill file blanco.bmp
store space(30) to nomcua
fec = date()
store 0 to cn,cna,cnn, ult, paso, error,cf,cc
do while .t.
   clear
   activ window ingr01
   do while .t.
      clear
      @2,3 say 'Cementerio:  ' get cod color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '#'
      @2,21 get nom color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      clear gets
      select ti
      @4,2 get listacampo popup cdcampo size 8, 20 default nom_tipo
      @4,25 say 'Nombre : ' get nomcua color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @6,25 say 'Fecha Construcción : ' get fec color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @8,25 say 'Cantidad nichos    : ' get cn color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '####'  &&pict '@(!'
      @10,25 say 'Cantidad nichos Adultos: ' get cna color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '####'
      @12,25 say 'Cantidad nichos Niños  : ' get cnn color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '####'
      if paso = 0
         read
      endif
      if lastkey() = 27
         release window ingr01
         close data
         clear
         return
      endif
      
      select ti
      tc = cod_tipo
      if tc = 1
         @14,2 say 'Cantidad Filas (Letras): ' get cf color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
         @14,35 say 'Cantidad Columnas : ' get cc color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
      endif
      if paso=0
         @16,5 get mielecc function '*H' picture '\<Cancelar;\<Grabar' size 2,17,1 default 'Cancelar'
      else
         @16,5 get mielecc function '*H' picture '\<Salir' size 2,17,1 default 'Salir'
      endif
      read
      tc= cod_tipo
      error = 0
      
      if mielecc='Cancelar' or mielecc = 'Salir' or lastkey()=27
         clear
         release window ingr01
         close data
         clear
         return
      else
         if mielecc='Grabar'
            **** VALIDACIONES DE LA INFORMACIÓN QUE SE ESTÁ INGRESANDO
            if cn = 0
               wait window 'No ha ingresado Total de Nichos. Revisar...'
               error = error + 1
            endif
            if cna + cnn <> cn
               wait window 'No cuadra nichos adultos + niños con total de nichos. Revisar...'
               error = error + 1
            endif
            
                  if tc=1
                     ********************* aqui validar total nichos con la multiplicación de fila x columnas
                     ********************* cuando se trata de cuartel.
                     if cn <> (cf * cc)
                        wait window 'No cuadra total de nichos con Total Filas x Total Columnas...'
                        wait window '¿Desea grabar a pesar de esta diferencia ???  S = Si  /  N = No ' to snd
                        if upper(snd) <> 'S'
                           error = error + 1
                        endif
                     endif
                  endif
                  
            sele cu
			SET EXACT ON
            sele count(nom_cuarte) as n from cu where nom_cuarte = allt(nomcua) and cod_cement= cod into cursor qq
            SET EXACT OFF
            sele qq
            if n>0
               wait window 'El nombre ingresado ya existe. Revise sus datos...'
               error = error + 1
            endif
            if error > 0
               loop
            endif
            
            sele max(cod_cuarte) as u from cu where cod_cement = cod into cursor pp
            sele pp
            ult = u + 1
            
			SELE ul
			? RLOCK('1','ul')
		    GO TOP	
			ult = ultimo+1
		    repla ultimo with ult
			flush
			unlock
			
            sele cu
            append blank
            repla cod_cement with cod, cod_cuarte with ult,cod_tipo with tc,;
               nom_cuarte with alltrim(nomcua),fec_constr with fec,tot_nichos with cn,tot_adulto with cna,;
               tot_ninos with cnn,filas with cf, columnas with cc
            paso = 1
         endif
      endif
   enddo
   loop
enddo
do escapar
clear
close all
return

*!*****************************************************************************
*!
*!       Function: ESCAPAR
*!
*!      Called by: NEWCUA01.PRG                      
*!               : NEWFAL01.PRG                      
*!               : NEWCUA02.PRG                      
*!               : NEWFAL02.PRG                      
*!
*!          Calls: LASTKEY()          (function in ?)
*!
*!*****************************************************************************
function escapar
if lastkey() = 27
   release window ingr01
   close data
   clear
   return
endif
*: EOF: NEWCUA01.PRG
