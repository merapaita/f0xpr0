*:*****************************************************************************
*:
*: Procedure file: y:\CEMENTER\REPCUA12.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 01/12/2006 at 17:19:58
*:
*:         Set by: REPCUA01.PRG                      
*:
*:          Calls: RGB()              (function in ?)
*:               : TRIM()             (function in ?)
*:
*:           Uses: MAESTRO.DBF            Alias: MA
*:               : CUARTEL.DBF            Alias: CU
*:               : TIP_CUAR.DBF           Alias: TI
*:
*:      CDX files: MAESTRO.CDX
*:               : CUARTEL.CDX
*:               : TIP_CUAR.CDX
*:
*:   Report Forms: COD_CUA1.FRX
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* repcua12.PRG  Reporte de Cuarteles por orden alfabético Cementerio 1

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
define window ingr01 from 02, 3 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE DE PABELLONES CEMENTERIO '+trim(cementerio) fill file blanco.bmp
use cuartel in 0
use tip_cuar in 0
sele cuartel
set relation to cod_tipo into tip_cuar
select a.*,b.nom_tipo from cuartel a, tip_cuar b where cod_cement = 1 and a.cod_tipo = b.cod_tipo into cursor kk order by nom_cuarte
activ window ingr01
@3,5 get pantimp function '*H' picture 'Impresora;Pantalla' size 2, 20,10 default 'Pantalla'
read
if pantimp = 'Pantalla'
   sele kk
   report form cod_cua1 previ
else
   report form cod_cua1 to print prompt
endif
release window ingr01
close data
clear
return

* ichf 2006
*: EOF: REPCUA12.PRG
