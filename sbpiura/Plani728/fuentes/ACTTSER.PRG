USE ..\DATA\PERSONAL
SET DELE ON
SET FILTER TO ESTADO#'03' AND !EMPTY(APE_nOM)
SCAN
	=tieser(Fec_ing,dia_sus)
ENDSCAN

Function tieser
*--------------
PARAMETER vfecing,vdiasus
vfecact=date()
*calcula los d¡as
vtotdia=(vfecact-vfecing)-vdiasus
*total a¤os
vtotano=INT(vtotdia/365)
vresto =MOD(vtotdia,365)
*total meses
vtotmes=INT(vresto/30)
vtotdia1=MOD(vresto,30)
*m.a¤o_ser = STR(vtotano,2)+'A, '+STR(vtotmes,2)+'M y '+STR(vtotdia,2)+'D'
m.a¤o_ser = STR(vtotano,2)+'A, '+STR(vtotmes,2)+'M, '+STR(vtotDia1,2)+'D.'
REPLACE A¤o_ser WITH m.A¤o_ser
RETURN .T.
