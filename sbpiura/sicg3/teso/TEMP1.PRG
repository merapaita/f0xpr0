use h:\sis97\data\PARMAE
SET FILT TO TIPO='TIPRI '
GO TOP
REPLACE ALL DESCRIAUX WITH LEFT(DESCRIAUX,3)+'01010100'

USE


RETURN
CANCEL
WWW

use h:\sisrg\data96\movbco   in 1 alias mb
use h:\sis97\data\movbco     in 2 alias mov

sele mb
go top
scan for ALLTRIM(STR(YEAR(FECHA)))='1997' AND !eof()
	sele mov
	append blank
	replace periodo with mb.periodo
	replace nummes  with mb.nummes  
	replace numero  with mb.numero  
	replace transa  with mb.transa
	replace fecha   with mb.fecha
	replace motivo  with mb.motivo
	replace tipdoc  with mb.tipdoc  
	replace numref  with mb.numref  
	replace codfte  with mb.codfte  
	replace codctc  with mb.codctc 
	replace nombre  with mb.nombre
	replace concept with mb.concept
	replace forma   with mb.forma 
	replace monto   with mb.monto  
	replace estado  with mb.estado 
	replace codban  with mb.codban 
	replace usuario with mb.usuario
	replace codpart with mb.codpart
	replace conban  with mb.conban 
	replace perri   with mb.perri  
	replace nummesri with mb.nummesri
	replace numri   with mb.numri  
	replace nummesc with mb.nummesc
	replace fechac  with mb.fechac 
	replace nummeshc with mb.nummeshc
	replace numhc    with mb.numhc
	replace tipcla   with mb.tipcla  
	replace parting  with mb.parting
	replace hojcon   with mb.hojcon
	replace asiento  with mb.asiento 	
	sele mb
endscan
	