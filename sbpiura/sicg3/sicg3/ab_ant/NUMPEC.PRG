SELE ITEPEC
SET ORDE TO ITEPEC1
GO TOP
SCAN
	V1=NUMPEC
	V2=NUMPEC
	V=1
    DO WHILE V1=V2
       REPLA NUMORD WITH PADL(ALLTRIM(STR(V,2)),2,'0')
       SKIP
       V1 = NUMPEC
       V=V+1
    ENDDO
    SKIP -1    
ENDSCAN    