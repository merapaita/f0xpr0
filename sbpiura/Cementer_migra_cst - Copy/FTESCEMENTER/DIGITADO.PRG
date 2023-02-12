* DIGITADO.PRG  Reporte de PRODUCCIÒN POR MAQUINA DE DIGITACIÒN

set exclu off
clear
close all
clear all
SET SAFETY OFF
SET DELE ON
SET CONFIRM ON

define window ingr01 from 02, 3 to 25, 70 float double color rgb(192,192,192,192,192,192) title 'REPORTE POR DIGITADOR' fill file blanco.bmp
reporte = 'ORDENADO POR MAQUINA Y HORA'

activ window ingr01
fecrepo=date()
@3,5 say 'Fecha del Reporte de Producción:  ' get fecrepo color rgb(255,255,0,0,0,128) pict '  /  /    ' 
read
fecrepo1 = fecrepo-1
clear
melige = 1
@ 2,2 get melige function '*RNV Todos;Por Turnos; Por Máquina'	size 1, 10, 1 default 'Todos'
@ 4,25 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 1, 10, 1
read cycle
if aceptarcancelar = 2
   close data
   release window ingr01
   return
endif
store 0 to turno1

if melige = 2
   do while .t.
      clear
      @2,2 say 'Turno <1> Día <2> Noche: ' get turno1 color rgb(255,255,0,0,0,128) pict '#'
      read
      if turno1=1 or turno1=2
		exit
      else
         loop
      endif
   enddo
endif
maq = space(10)
if melige = 3
   clear
   @2,2 say 'Nombre de la Máquina:' get maq color rgb(255,255,0,0,0,128) pict '!!!!!!!!!!'
   read
endif

rele wind

use fallecid in 0 share orde 1 alia fa
sele fa
copy to c:\fallec02 for fec_digit=fecrepo or fec_digit=fecrepo1

use cuartel in 0 share orde 1 alia cu
sele cu
copy to c:\cuart02

use tip_cuar in 0 share alia ti
sele ti
copy to c:\tip_cu02

CLOSE ALL

set defa to c:\
use fallec02 exclu

SELE A.*,1 AS TURNO FROM FALLEC02 A INTO CURSOR FA

SELE FA
COPY TO C:\KK
CLOSE DATA
USE C:\KK IN 0 ALIA FA EXCLU
SELE FA
inde on ALLTRIM(STR(cod_cement))+ALLTRIM(STR(cod_cuarte))+DIGITADOR tag cod_fall
INDE ON DIGITADOR+HOR_DIGIT TAG DIGITADOR

use cuart02 exclu ALIA CU IN 0
SELE CU
inde on ALLTRIM(STR(cod_cement))+ALLTRIM(STR(cod_cuarte))+ALLTRIM(STR(cod_tipo)) tag cementerio


use tip_cu02 exclu ALIA TI IN 0
SELE TI
inde on cod_tipo tag tipo

sele fa
set relation to allt(str(fa.cod_cement))+allt(str(fa.cod_cuarte)) into cu
sele cu
set relation to cu.cod_tipo into ti
SELE FA

repla turno with 2 FOR hor_digit>='23' or hor_digit<'08'
dele all for fec_digit=fecrepo1 and turno=1

IF MELIGE=2
	DELE ALL FOR TURNO<>TURNO1
ENDIF
IF MELIGE=3
	DELE ALL FOR DIGITADOR<>MAQ
ENDIF

clear
@3,5 get pantimp function '*H' picture 'Impresora;Pantalla' size 2, 20,10 default 'Pantalla'
read
SELE FA
SET ORDE TO DIGITADOR
if pantimp = 'Pantalla'
   report form rep_maqu previ
else
   report form rep_maqu to print prompt
endif

release window ingr01
close data
clear
return

* ichf 2007
*: EOF: DIGITADO.PRG
