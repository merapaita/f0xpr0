nMes = 3
CLOSE data
USE H:\SICGDATA\DATA2011\SALDONA IN 0 ALIAS sa_ant ORDER tag saldona4
USE H:\SICGDATA\DATA2012\SALDONA IN 0 ALIAS sa_act ORDER tag saldona4
USE H:\SICGDATA\DATA2011\cuentas IN 0 ALIAS cu_ant ORDER tag cuentas1
USE H:\SICGDATA\DATA2012\cuentas IN 0 ALIAS cu_act ORDER tag cuentas1

SELECT cu_act.cuenta ,cu_act.Descri des_act,ano,mes, sa_act.dsaldona dsal_act, sa_act.hsaldona hsal_act ;
 FROM cu_act INNER JOIN sa_act ON (cu_act.cuenta=sa_act.Cuenta AND sa_act.mes=nmes);
 INTO TABLE sal0312

SELECT cu_ant.cuenta ,cu_ant.Descri des_ant,ano,mes, sa_ant.dsaldona dsal_ant, sa_ant.hsaldona hsal_ant ;
 FROM cu_ant INNER JOIN sa_ant ON (cu_ant.cuenta=sa_ant.Cuenta AND sa_ant.mes=nmes);
 INTO TABLE sal0311

CLOSE DATABASES
USE sal0311
GO top

SCAN 
	cCuenta = PADR(Cuenta(),14,' ')
	REPLACE Cuenta WITH cCuenta
ENDSCAN

USE sal0312
GO top

SCAN 
	cCuenta = PADR(Cuenta(),14,' ')
	REPLACE Cuenta WITH cCuenta
ENDSCAN




*SELECT cuenta,mes,ano,dsaldona,hsaldona from sal_ant WHERE for mes=nmes
*union
*SELECT cuenta,mes,ano,dsaldona,hsaldona from sal_act WHERE for mes=nmes


*CONSULTA PARA SACAR UN RESUMEN DE COMPROBANTES DE PAGO

*SELECT compag.periodo,compag.nummes, compag.numcp, feccp,nummeshc,numhc,codfte,import,compag.codctc,tipprv,compag.codprv,promae.nompro,codemp,emp.descri,compag.codotr,otr.descri,nompre,destino, cheque.numchq, cheque.nomgir,cheque.valchq;
	FROM compag;
	left JOIN Promae ON compag.codprv=promae.codprv AND compag.tipprv='P';
	left JOIN auxil otr ON compag.codotr=otr.codigo and otr.tipo='09' AND compag.tipprv='O';
	left JOIN auxil emp ON compag.codemp=emp.codigo and emp.tipo='03' AND compag.tipprv='E';
	left join cheque on compag.nummes+compag.numcp= cheque.nummes+cheque.numcp



FUNCTION Cuenta
*--------------
*PARAMETER xC1
*xAli=ALIAS()
*SELE &xC1
DO CASE
	CASE SUBSTR(Cuenta,5,11)='00000000000'
	   RETURN LEFT(Cuenta,4)
	CASE SUBSTR(Cuenta,7,9)='000000000'
	   RETURN LEFT(Cuenta,6)
	CASE SUBSTR(Cuenta,9,7)='0000000'
	   RETURN LEFT(Cuenta,8)
	CASE SUBSTR(Cuenta,11,5)='00000'
	   RETURN LEFT(Cuenta,10)
	CASE SUBSTR(Cuenta,13,3)='000'
	   RETURN LEFT(Cuenta,12)
 	OTHERWISE
	   RETURN Cuenta
ENDCASE
*SELE &xAli
RETURN mRet
