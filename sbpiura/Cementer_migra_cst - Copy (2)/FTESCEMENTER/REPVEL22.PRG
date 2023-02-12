*:*****************************************************************************
*:
*: Procedure file: y:\CEMENTER\REPVEL22.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 14/12/2006 at 15:45:22
*:
*:         Set by: REPVEL02.PRG                      
*:
*:          Calls: CHR()              (function in ?)
*:               : ALLT()             (function in ?)
*:               : STR()              (function in ?)
*:               : YEAR()             (function in ?)
*:               : DATE()             (function in ?)
*:               : EOF()              (function in ?)
*:               : RGB()              (function in ?)
*:               : TRIM()             (function in ?)
*:               : LASTKEY()          (function in ?)
*:
*:           Uses: MASTERVE.DBF           Alias: MV
*:               : MAESTRO.DBF            Alias: MA
*:               : FALLECID.DBF           Alias: FA
*:               : CUARTEL.DBF            Alias: CU
*:               : TIP_CUAR.DBF           Alias: TI
*:               : CU.DBF             
*:               : VELACION.DBF           Alias: VE
*:
*:      CDX files: MAESTRO.CDX
*:               : FALLECID.CDX
*:               : CUARTEL.CDX
*:               : TIP_CUAR.CDX
*:               : VELACION.CDX
*:
*:   Report Forms: REP_VEL2.FRX
*:
*:      Documented 20:34:26                                FoxDoc version 3.00a
*:*****************************************************************************
* repvel22.PRG  Reporte de RESUMEN de Boletas Velaciones por Pabellón cementerio 2

set exclu off
clear
close all
clear all
define popup cdcampo prompt field nom_cuarte scroll margin mark chr(16)
define popup cdtipo prompt field nom_tipo scroll margin mark chr(16)

use masterve in 0 share alia mv
use maestro  in 0 share orde cementerio alia ma
sele ma
loca for cod_cement = 2
cod = cod_cement
cementerio = nom_cement
responsable = responsabl
reporte = 'TODOS LOS PABELLONES'
sele mv
loca for cod_cement = cod and anyo = allt(str(year(date())))
if !eof()
   anyov = anyo
   costo = valor_foco
   abce = 'CERRADO'
   if abierto
      abce = 'ABIERTO'
   endif
endif

define window ingr01 from 02, 3 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE POR PABELLÓN CEMENTERIO '+trim(cementerio) fill file blanco.bmp
activ window ingr01
use fallecid in 0 share orde 1 alia fa
use cuartel in 0 share orde 1 alia cu
use tip_cuar in 0 share alia ti

melige = 1
@ 2,2 get melige function '*RNV Todos;Uno en Particular'	size 1, 10, 1 default 'Todos'
@ 4,25 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 2, 10, 1
read cycle
if aceptarcancelar = 2 or lastkey() = 27
   close data
   release window ingr01
   return
endif

if melige = 2
   clear
   * AQUI VAMOS A PONER PARA QUE PUEDAN ELEGIR UN ÚNICO PABELLÓN
   sele ti
   @2,2 get listac2 popup cdtipo size 12,20 default nom_tipo
   @ 15,10 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 2, 10, 1
   read cycle
   if aceptarcancelar = 2 or lastkey() = 27
      close data
      release window ingr01
      return
   endif
   cdt = cod_tipo
   sele cu
   selec * from cu where cod_tipo = cdt and cod_cementer = cod into cursor uc
   sele uc
   @2,25 get listacampo popup cdcampo size 12,20 default nom_cuarte
   @15,10 clear to 19,40
   @ 15,10 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 2, 10, 1
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

sele fa
set relation to allt(str(fa.cod_cement))+allt(str(fa.cod_cuarte)) into cu
sele cu
set relation to cu.cod_tipo into ti
if melige = 1
   select a.*, b.ape_patern,b.ape_matern,b.nombres,b.fila,b.columna,c.nom_tipo,d.nom_cuarte from velacion a, fallecid b, tip_cuar c, cuartel d ;
      where a.cod_cement = cod and a.cod_cement = b.cod_cement and a.cod_cement = d.cod_cement ;
      and a.cod_fallec = b.cod_fallec and b.cod_cuarte = d.cod_cuarte and c.cod_tipo = d.cod_tipo into cursor kk order by nom_tipo,nom_cuarte,fila,columna
   *todos los pabellones
else
   select a.*, b.ape_patern,b.ape_matern,b.nombres,b.fila,b.columna,c.nom_tipo,d.nom_cuarte from velacion a, fallecid b, tip_cuar c, cuartel d where a.cod_cement = cod and a.cod_cement = b.cod_cement and a.cod_cement = d.cod_cement ;
      and a.cod_fallec = b.cod_fallec and b.cod_cuarte = d.cod_cuarte and c.cod_tipo = d.cod_tipo and b.cod_cuarte = codcuartel into cursor kk order by nom_tipo,nom_cuarte,fila,columna
endif
sele kk
clear
@3,5 get pantimp function '*H' picture 'Impresora;Pantalla' size 2, 20,10 default 'Pantalla'
read
if pantimp = 'Pantalla'
   sele kk
   report form rep_vel2 previ
else
   report form rep_vel2 to print prompt
endif
release window ingr01
close data
clear
return

* ichf 2006
*: EOF: REPVEL22.PRG
