ACTIVATE SCREE
SELE 1
USE ITEPEC 
INDEX ON CODART+NUMPEC TO WW
GO TOP
SCAN
	V1=NUMPEC
	V2=NUMPEC
	V=1
    DO WHILE V1=V2
       
       *EPLA NUMORD WITH PADL(ALLTRIM(STR(V,2)),2,'0')
       SKIP
       V1 = NUMPEC
       IF V1=V2
          BROW FIEL NUMPEC,CODART,AA=LEFT(DESCRI,40)
       ENDIF   
       V=V+1
    ENDDO
    SKIP -1    

ENDSCAN    