clear all
clear macros
set safety off
set echo off
set talk off
set notify off
set bell off
set date briti
set score off
set dele on
SET STAT OFF
set escape off
SET EXCLUSIVE OFF
set sysmenu ON
set century on
set procedure to funcione

CLEAR
CLOSE DATA
SELE 1
USE ST_IOREP 
SELE 2
USE ST_ISPRE
SELE 3
USE ST_IDPRE ORDER CODIGO
SELE 4
USE GC_DLP00


@ 05,05 TO  20,70 double
@ 12,15 SAY "Calculando Ordenes Cerradas :" 
@ 15,15 SAY "Calculando Presupuestos     :"

a=0
b=0
c=0
d=0

*Calcula nuevo IGV en ordenes cerradas
*-------------------------------------
SELE ST_IOREP
REPL totigv WITH  totnet * 0.19    FOR indest = "C"
REPL totbru WITH (totnet + totigv) FOR indest = "C"
		


*Calcula nuevo IGV en presupuestos pendientes
*--------------------------------------------
*SELE ST_ISPRE
*SCAN WHILE !EOF()
*	IF indest = "C" OR indest = "V" OR indest = "P"
*		w_numero = numdoc
*		SELE ST_IDPRE
*		SEEK w_numero
*		IF FOUND() 
*			w_codigo = codpro
*			SELE GC_dlp00
*			SEEK "PUBL"+w_codigo
*			IF FOUND()
*				w_precio = dlp_prcigv
*		REPL totigv WITH (monrep+monman) - ((monrep+monman)/1.18) 
	
*ENDSCAN

*Recalcula nueva lista de precios
*--------------------------------
@ 18,25 SAY "CALCULANDO LISTA DE PRECIOS " COLOR I*

SELE GC_DLP00
REPL ALL dlp_prcigv WITH dlp_prsigv * 1.19

close data
