*:*****************************************************************************
*:
*: Procedure file: y:\CEMENTER\CONCUA01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 07/12/2006 at 18:27:38
*:
*:         Set by: CUARTEL1.PRG                      
*:
*:          Calls: CHR()              (function in ?)
*:               : RGB()              (function in ?)
*:               : TRIM()             (function in ?)
*:               : SPACE()            (function in ?)
*:               : DATE()             (function in ?)
*:               : LASTKEY()          (function in ?)
*:               : LEN()              (function in ?)
*:               : ALLTRIM()          (function in ?)
*:               : EOF()              (function in ?)
*:               : UPPER()            (function in ?)
*:               : STR()              (function in ?)
*:
*:           Uses: MAESTRO.DBF            Alias: MA
*:               : CUARTEL.DBF            Alias: CU
*:               : TIP_CUAR.DBF           Alias: TI
*:               : CU.DBF             
*:
*:      CDX files: MAESTRO.CDX
*:               : CUARTEL.CDX
*:               : TIP_CUAR.CDX
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* concua01.PRG  Para Consultar/Modificar información de cuarteles del Cementerio San Teodoro

set exclu off
clear
close all
clear all

define popup cdcampo prompt field nom_tipo scroll margin mark chr(16)

use maestro  in 0 share orde cementerio alia ma
use cuartel  in 0 share orde cementerio alia cu
use tip_cuar in 0 share orde tipo alia ti

sele ma
loca for cod_cement = 1   && código cementerio
cod = cod_cement
nomce = nom_cement
nomce = nom_cement
res = responsabl
define window ingr01 from 02, 3 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'CONSULTA / MODIFICACIÓN DATOS GENERALES CEMENTERIO '+trim(nomce) fill file fondo03.bmp
res = responsabl
store space(30) to nomcua,nombus
fec = date()
store 0 to cn,cna,cnn, ult, paso, error
do while .t.
   clear
   activ window ingr01
   @2,2 say "Ingrese Nombre a Buscar : " get nombus color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
   read
   if lastkey() = 27 or len(alltrim(nombus))=0
      release window ingr01
      close data
      clear
      return
   endif
   sele cu
   select cu.* from cu where alltrim(cu.nom_cuarte) == alltrim(nombus) and cu.cod_cement = cod into cursor kk
   sele kk
   if eof()
      wait window "NO EXISTE EL NOMBRE BUSCADO..."
      loop
   endif
   codcu = cod_cuarte
   tipcu = cod_tipo
   sele ti
   loca for cod_tipo = tipcu
   nomti = nom_tipo
   sele kk
   nomcu = nom_cuarte
   nomaguardar = nom_cuarte
   feccu = fec_constr
   cn  = tot_nichos
   cna = tot_adulto
   cnn = tot_ninos
   cf  = filas
   cc  = columnas
   do while .t.
      clear
      @1,3 say 'Cementerio:  ' get cod color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '#'
      @1,21 say '' get nomce color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @3,25 say 'Tipo   : ' get nomti color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @4,25 say 'Nombre : ' get nomcu color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @6,25 say 'Fecha Construcción : ' get feccu color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @8,25 say 'Cantidad nichos    : ' get cn color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
      @10,25 say 'Cantidad nichos Adultos: ' get cna color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
      @12,25 say 'Cantidad nichos Niños  : ' get cnn color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
      if tipcu=1
         @14,2 say 'Cantidad Filas : ' get cf  color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
         @14,30 say 'Cantidad Columnas : ' get cc  color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
      endif
      clear gets
      if lastkey() = 27
         release window ingr01
         close data
         clear
         return
      endif
      @16,5 get mielecc function '*H' picture '\<Salir;\<Modificar;\<Eliminar' size 2,17,1 default 'Salir'
      read
      if mielecc='Eliminar'
         wait window 'Seguro que desea eliminarlo S/N' to sn
         if upper(sn) = 'S'
            * procedemos a eliminar lògicamente el registro
            sele cu
            seek alltrim(str(cod))+alltrim(str(codcu))+alltrim(str(tipcu))
            if !eof()
               delete
               wait window 'El Registro ha sido ELIMINADO'
               release window ingr01
               close data
               clear
               return
            endif
         endif
      endif
      error = 0
      select ti
      tc = cod_tipo
      if mielecc = 'Salir' or lastkey() = 27
         clear
         release window ingr01
         close data
         clear
         return
      endif
      if mielecc = 'Modificar'
         do while .t.
            clear
            @1,3 say 'Cementerio:  ' get cod color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '#'
            @1,21 say '' get nomce color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
            clear gets
            @4,2 get listacampo popup cdcampo size 8, 20 default nom_tipo
            @4,25 say 'Nombre : ' get nomcu color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
            @6,25 say 'Fecha Construcción : ' get feccu color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
            @8,25 say 'Cantidad nichos    : ' get cn color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '####'  &&pict '@(!'
            @10,25 say 'Cantidad nichos Adultos: ' get cna color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '####'
            @12,25 say 'Cantidad nichos Niños  : ' get cnn color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '####'
            select ti
            tc = cod_tipo
            if tc=1
               @14,2 say 'Cantidad Filas : ' get cf  color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
               @14,30 say 'Cantidad Columnas : ' get cc  color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
            endif
            if lastkey() = 27
               release window ingr01
               close data
               clear
               return
            endif
            @16,5 get mielecc function '*H' picture '\<Salir;\<Actualizar' size 2,17,1 default 'Salir'
            read
            tc = cod_tipo
            error = 0
            if mielecc = 'Salir' or lastkey() = 27
               clear
               release window ingr01
               close data
               clear
               return
            else
               if mielecc='Actualizar'
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
                  
                  
                  if nomcu<>nomaguardar
                     sele cu
                     sele count(nom_cuarte) as n from cu where nom_cuarte = nomcua into cursor qq
                     sele qq
                     if n>0
                        wait window 'El nombre ingresado ya existe. Revise sus datos...'
                        error = error + 1
                     endif
                  endif
                  if error > 0
                     wait window 'No se están actualizando los datos... Revisar y volver a grabar o Salir sin grabar...'
                     loop
                  endif
                  sele cu
                  seek alltrim(str(cod))+alltrim(str(codcu))+alltrim(str(tipcu))
                  if tc<>1
                     store 0 to cf,cc
                  endif
                  repla cod_cement with cod, cod_cuarte with codcu,cod_tipo with tc,nom_cuarte with nomcu,;
                     fec_constr with feccu,tot_nichos with cn,tot_adulto with cna,tot_ninos with cnn,;
                     filas with cf, columnas with cc
                  wait window 'INFORMACIÓN FUE ACTUALIZADA...'
                  clear
                  release window ingr01
                  close data
                  clear
                  return
               endif
            endif
         enddo
      endif
   enddo
   loop
enddo

clear
close all
return
*: EOF: CONCUA01.PRG
