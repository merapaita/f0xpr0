*:*****************************************************************************
*:
*: Procedure file: y:\CEMENTER\REPCUA24.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 08/12/2006 at 12:56:48
*:
*:         Set by: REPCUA02.PRG                      
*:
*:          Calls: CHR()              (function in ?)
*:               : RGB()              (function in ?)
*:               : TRIM()             (function in ?)
*:               : LASTKEY()          (function in ?)
*:               : EOF()              (function in ?)
*:               : SPACE()            (function in ?)
*:               : SUBSTR()           (function in ?)
*:               : LEN()              (function in ?)
*:               : ALLT()             (function in ?)
*:               : STR()              (function in ?)
*:               : ALLTRIM()          (function in ?)
*:               : AT()               (function in ?)
*:
*:           Uses: FALLECID.DBF           Alias: FA
*:               : CUARTEL.DBF            Alias: CU
*:               : TIP_CUAR.DBF           Alias: TI
*:               : TEMPO2.DBF         
*:               : MAESTRO.DBF            Alias: MA
*:
*:      CDX files: FALLECID.CDX
*:               : CUARTEL.CDX
*:               : TIP_CUAR.CDX
*:               : MAESTRO.CDX
*:
*:   Report Forms: REP_FAL4.FRX
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* repcua24.PRG  Reporte de Nichos Desocupados Cementerio 2

set exclu off
clear
close all
clear all
set safe off

define popup cdcampo prompt field nom_cuarte scroll margin mark chr(16)
define popup cdtipo prompt field nom_tipo scroll margin mark chr(16)


use fallecid in 0 share
use cuartel  in 0 share
use tip_cuar in 0 share
use tempo2 in 0 exclu
sele tempo2
zap
use maestro  in 0 share orde cementerio alia ma
sele ma
loca for cod_cement = 2
cod = cod_cement
nom = nom_cement
cementerio = nom
res = responsabl
define window nicho01 from 02, 3 to 40, 80 float double color rgb(192,192,192,192,192,192) title 'DATOS GENERALES CEMENTERIO '+trim(nom_cement) fill file blanco.bmp

reporte = 'TODOS LOS PABELLONES'

activ window nicho01

melige = 1
@ 2,2 get melige function '*RNV Todos;Uno en Particular'	size 1, 10, 1 default 'Todos'
@ 4,25 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 1, 10, 1
read cycle
if aceptarcancelar = 2 or lastkey() = 27
   close data
   release window nicho01
   return
endif

if melige = 2
   clear
   * AQUI VAMOS A PONER PARA QUE PUEDAN ELEGIR UN ÚNICO PABELLÓN
   sele tip_cuar
   @2,2 get listac2 popup cdtipo size 12,20 default nom_tipo
   @ 15,10 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 1, 10, 1
   read cycle
   if aceptarcancelar = 2 or lastkey() = 27
      close data
      release window nicho01
      return
   endif
   cdt = cod_tipo
   sele cuartel
   selec * from cuartel where cod_tipo = cdt and cod_cementer = cod into cursor uc
   sele uc
   @2,25 get listacampo popup cdcampo size 12,20 default nom_cuarte
   @15,10 clear to 17,40
   @ 15,10 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 1, 10, 1
   read
   if aceptarcancelar = 2 or lastkey() = 27
      close data
      release window ingr01
      return
   endif
   nomcuartel = nom_cuarte
   reporte = 'PABELLON '+nomcuartel
   codcuartel = cod_cuarte
endif

if melige = 1
   selec cod_cement,cod_cuarte,fila,columna,0 as cod_tipo,space(20) as nom_tipo from fallecid where cod_cement = cod into table tempo1 order by cod_cuarte
else
   selec cod_cement,cod_cuarte,fila,columna,0 as cod_tipo,space(20) as nom_tipo from fallecid where cod_cement = cod ;
      and cod_cuarte = codcuartel into table tempo1 order by cod_cuarte
endif

sele tempo1
do while !eof()
   i = cod_cuarte
   sele cuartel
   loca for cod_cuarte = i
   q = 0
   n = space(20)
   if !eof()
      q = cod_tipo
      select tip_cuar
      loca for cod_tipo = q
      if !eof()
         n = nom_tipo
      endif
   endif
   select tempo1
   repla cod_tipo with q,nom_tipo with n
   skip
   if eof()
      exit
   endif
enddo
if melige = 1
   sele tip_cuar
   goto top
else
   sele tip_cuar
   set filter to cod_tipo = cdt
   goto top
endif
do while !eof()
   ctip = cod_tipo
   ntip = nom_tipo
   sele cuartel
   set orde to tag x_ctip_cu
   if melige=1
      set filter to cod_tipo = ctip and cod_cement = cod
      goto top
   else
      set filter to cod_cuarte = codcuartel and cod_cement = cod
   endif
   do while !eof()
      ccu = cod_cuarte
      ncu = nom_cuarte
      tni = tot_nichos	&& nos sirve para el caso de mausoleos
      ********************* en el caso de cuartel
      armo_1 = ''
      if ctip = 1
         longitud=0
         letras='ABCDEFGHIJKLMNÑOPQRSTUVWXYZ'
         letras_cua = substr(letras,1,filas)
         totletras = len(trim(letras_cua))
         armo_1 = ''
         for ifil = 1 to totletras
            letra_a = substr(letras_cua,ifil,1)
            for icol = 1 to columnas
               armo_1 = armo_1+letra_a+allt(str(icol))+','
            next
         next ij
         sele tempo1
         set filter to cod_cuarte = ccu
         goto top
         nichocup =0
         do while !eof()
            nichocup = nichocup + 1
            sacar = fila+alltrim(str(columna))+','
            q = at(sacar,armo_1)
            if q>0
               armo_1 = substr(armo_1,1,q-1)+substr(armo_1,q+len(sacar))
            endif
            skip
            if eof()
               sobran = tni - nichocup
               sele tempo2
               append blank
               repla cod_tipo with ctip, nom_tipo with ntip, nom_cuarte with ncu, tot_nicho with tni, tot_ocup with nichocup,tot_desocu with sobran,lista_nich with armo_1
               exit
            endif
         enddo
      endif
      ********************** en el caso de mausoleo
      if at('MAUS',ntip)>0
         sele tempo1
         set filter to cod_cuarte = ccu
         count to pp
         sobran = tni - pp
         sele tempo2
         append blank
         repla cod_tipo with ctip, nom_tipo with ntip, nom_cuarte with ncu, tot_nicho with tni, tot_ocup with pp,tot_desocu with sobran,lista_nich with armo_1
      endif
      
      ********************** en los demás casos no hay control de nichos no ocupados
      ********************** CASO CONTRARIO AGREGARLO AQUÍ
      
      sele cuartel
      skip
      if eof()
         exit
      endif
   enddo
   sele tip_cuar
   skip
   if eof()
      exit
   endif
enddo

clear
@3,5 get pantimp function '*H' picture 'Impresora;Pantalla' size 2, 20,10 default 'Pantalla'
read
if pantimp = 'Pantalla'
   sele tempo2
   report form rep_fal4 previ
else
   report form rep_fal4 to print prompt
endif
release window nicho01
close data
clear
return

close data
return
*: EOF: REPCUA24.PRG
