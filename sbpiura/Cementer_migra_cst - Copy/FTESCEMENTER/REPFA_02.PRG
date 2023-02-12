*:*****************************************************************************
*:
*: Procedure file: y:\CEMENTER\REPFA_02.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 29/11/2006 at 20:23:38
*:
*:         Set by: REPFAL11.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : TRIM()             (function in ?)
*:               : DATE()             (function in ?)
*:               : ALLT()             (function in ?)
*:               : STR()              (function in ?)
*:
*:           Uses: MAESTRO.DBF            Alias: MA
*:               : FALLECID.DBF           Alias: FA
*:               : CUARTEL.DBF            Alias: CU
*:               : TIP_CUAR.DBF           Alias: TI
*:               : FA.DBF             
*:               : TI.DBF             
*:               : CU.DBF             
*:
*:      CDX files: MAESTRO.CDX
*:               : FALLECID.CDX
*:               : CUARTEL.CDX
*:               : TIP_CUAR.CDX
*:
*:   Report Forms: REP_FAL1.FRX
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* repfa_02.PRG  Reporte de fallecidos por Fecha de Nacimiento Cementerio 1

set exclu off
clear
close all
clear all

use maestro  in 0 share orde cementerio alia ma
sele ma
loca for cod_cement = 1
cod = cod_cement
cementerio = nom_cement
responsable = responsabl
reporte = 'POR FECHA DE NACIMIENTO'
define window ingr01 from 02, 3 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE POR FECHA NACIMIENTO CEMENTERIO '+trim(cementerio) fill file blanco.bmp
activ window ingr01

melige = 1
@ 2,2 get melige function '*RNV Todos;Entre Fechas'	size 1, 10, 1 default 'Todos'
*** Define una serie de botones de comando para terminar el READ. ***
@ 4,25 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1;
   size 1, 10, 1
read cycle
if aceptarcancelar = 2
   close data
   release window ingr01
   return
endif
store date() to fecini,fecfin
if melige = 2
   do while .t.
      clear
      @2,2 say 'Fecha Inicial : ' get fecini color rgb(255,255,0,0,0,128)
      @4,2 say 'Fecha Final   : ' get fecfin color rgb(255,255,0,0,0,128)
      read
      if fecfin < fecini
         wait window 'Fecha Final no puede ser menor que la fecha INICIAL...'
         loop
      else
         exit
      endif
   enddo
endif
use fallecid in 0 share orde 1 alia fa
use cuartel in 0 share orde 1 alia cu
use tip_cuar in 0 share alia ti
sele fa
set relation to allt(str(fa.cod_cement))+allt(str(fa.cod_cuarte)) into cu
sele cu
set relation to cu.cod_tipo into ti
if melige = 1
   select a.*, b.nom_tipo,c.nom_cuarte from fa a, ti b, cu c where a.cod_cement = cod and a.cod_cement = c.cod_cement ;
      and a.cod_cuarte = c.cod_cuarte and b.cod_tipo = c.cod_tipo into cursor kk orde by fec_nacim
else
   select a.*, b.nom_tipo,c.nom_cuarte from fa a, ti b, cu c where a.cod_cement = cod and a.cod_cement = c.cod_cement and a.cod_cuarte = c.cod_cuarte and b.cod_tipo = c.cod_tipo ;
      and fec_nacim >= fecini and fec_nacim <= fecfin into cursor kk orde by fec_nacim
endif
sele kk
clear
@3,5 get pantimp function '*H' picture 'Impresora;Pantalla' size 2, 20,10 default 'Pantalla'
read
if pantimp = 'Pantalla'
   sele kk
   report form rep_fal1 previ
else
   report form rep_fal1 to print prompt
endif
release window ingr01
close data
clear
return

* ichf 2006
*: EOF: REPFA_02.PRG
