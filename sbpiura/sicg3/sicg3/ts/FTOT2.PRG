* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ FTOT2.PRG    08/06/96                                L: 0480 ³	
* ³ Reporte de Informe Mensual del Gasto T-2                     ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Abre archivos
IF SET('PATH')='ON'
   SET EXACT OFF
ENDIF   
USE MovBco   IN  1 	ORDER 	TAG  Movbco1   ALIAS Movb
USE ComPag   IN  2 	ORDER 	TAG  ComPag1   ALIAS ComPag
USE IteCp    IN  3 	ORDER 	TAG  IteCp1    ALIAS IteCp
USE Cheque   IN  4 	ORDER 	TAG  Cheque1   ALIAS Cheque
USE Clase    IN  6  ORDER   TAG  Clase1    ALIAS Clase
USE Parmae   IN  7  ORDER   TAG  Parmae1   ALIAS Parma
USE IteHc    IN  5 	ORDER 	TAG  Itehc1    ALIAS IteHc
USE Hojcon   IN 08 	ORDER 	TAG  Hojcon1   ALIAS hoja
USE Cajas    IN 10  ORDER   TAG  Cajas2    ALIAS Caja

*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,m.fecha,vMes,vMontoF,vMontoA,vMontoE,vMontoH,vMontoS,;
       vMontoP,vMontoO,hmonto,terfavor,tesfavor,vMontoR,ichq
STORE .T. TO Agrega,vFun
STORE 0 TO vMontoF,vMontoA,vMontoE,vMontoH,vMontoS,vMontoP,vMontoO,hmonto,terfavor,tesfavor,vMontoR,ichq
Vmens08= 'Registros no existen'
m.fecha=DATE()
DO inicia
DO salida
RETURN

PROCEDURE inicia
*------------------
  vcol ="w+/w,n/w,W/W,w/W,,w+/b"
  IF Escolor
     DEFINE WINDOW T2 FROM 11,46 TO 17,78  SHADOW COLOR &vcol
  ELSE
     DEFINE WINDOW T2 FROM 11,46 TO 17,78  COLOR SCHEME c_popup
  ENDIF
 ACTIVATE WINDOW t2
 CLEAR
 vCodCtc  = SPACE(14)
 vMes     = MONTH(DATE())
 vano     = VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
 @ 0, 0 SAY '     Formato T-2       '
 @ 2,4  SAY "  Mes y A¤o: " GET vMes    PICTURE '99'
 @ 2,22 GET vAno                        PICTURE '99'
 @ 4,4  SAY "Cta.Corriente" GET vcodctc PICTURE '@!' VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",vcodctc,1,4,22)
 READ       
 DEACTIVATE WINDOW T2
 IF LASTKEY()#27       
	 DEFINE WINDOW Xwait FROM 20,35 TO 22,75 COLOR SCHEME 5 
	 ACTIVATE WINDOW Xwait
	 @0,1 SAY "   Procesando T2 ......" COLOR W+/BR*
	 SELE caja
	 SEEK ALLT(vcodctc)
	 vfte=caja.CodFte
	 vofigir=ALLT(Caja.Codofg)
     vind1= SYS(3) +'.idx'
     vind2= SYS(3) +'.idx'
     vind3= SYS(3) +'.idx'
     vind4= SYS(3) +'.idx'
     vind5= SYS(3) +'.idx'
     vind6= SYS(3) +'.idx'
     vind7= SYS(3) +'.idx'
     vind8= SYS(3) +'.idx'
	 filaux = SYS(3) + '.DBF'
	 filrep = SYS(3) + '.DBF'
	 vtempo = SYS(3) + '.DBF'
	 SELE 15
	 CREATE TABLE (filaux);
	  (CodPart   C(005),;
	   descri    C(024),;
	   ImportH   N(18,2),;
	   ImportA   N(18,2),;
	   ImportF   N(18,2),;
	   ImportE   N(18,2),;
	   ImportS   N(18,2),;
	   ImportP   N(18,2),;
	   codctc    C(014),;
	   tipdoc    C(002),;
	   numchq    C(014),;
	   liquido   N(18,2),;
	   flag      C(001))
	 USE 
	 USE (filaux) IN 15 EXCLUSIVE ALIAS Temp
	 SELE Itecp
	 INDEX ON Nummes+Numcp+Codctc TO (vind1) FOR CodCtC = vCodctc .AND. ;
	 		Nummes=PADL(vmes,2,'0') AND periodo=STR(vano,2) AND ;
	 		estado<>'99' 
	 SET RELA TO Nummes+NumCp+CodCtc INTO Compag ADDI
	 SET RELA TO Nummes+NumCp+CodCtc INTO Cheque ADDI
     GO TOP
     IF !EOF()
        DO Tempcp
	 ELSE
        DO Standby WITH vmens08
        CLOSE DATA
        RETURN
     ENDIF
	 SELE Movb
	 INDEX ON Numero TO (Vind2) FOR CodCtC = vCodctc .AND. ;
	 		Nummes=PADL(vmes,2,'0') and periodo=str(vano,2) AND estado<>'99' AND ;
	 		tipdoc='N/C' AND (motivo='15' OR motivo = '14')
	 GO TOP
	 IF !EOF()
	    DO proCnc	
	    DO mues_rep
	 ELSE
	    DO mues_rep
	 ENDIF      
	ERASE (vind1)
	ERASE (vind3)
	ERASE (vind4)
	ERASE (vind5)
	ERASE (vind6)
	ERASE (vind7)
	ERASE (vind8)
ENDIF
RETURN


FUNCTION Tempcp
*---------------
    IF Itecp.Tipfun='F'
       IF LEFT(Itecp.Codanal,2)='01' OR ;
       	  LEFT(Itecp.Codanal,2)='02' OR ;
       	  LEFT(Itecp.Codanal,2)='03' OR ;
       	  LEFT(Itecp.Codanal,2)='04' OR ;
       	  LEFT(Itecp.Codanal,2)='05' OR ;
       	  LEFT(Itecp.Codanal,2)='07' OR ;
       	  LEFT(Itecp.Codanal,2)='08' OR ;
       	  LEFT(Itecp.Codanal,2)='11' 
       	  vcodpar = LEFT(Itecp.Codanal,2)+'.00'
       	  vdescri = despart(vcodpar)
       ELSE	  
       	  vcodpar = 'OTROS'
       	  vdescri = 'OTRAS PARTIDAS'
       ENDIF
    ELSE
       IF LEFT(Itecp.CodPart,2)='01' OR ;
       	  LEFT(Itecp.Codpart,2)='02' OR ;
       	  LEFT(Itecp.codpart,2)='03' OR ;
       	  LEFT(Itecp.codpart,2)='04' OR ;
       	  LEFT(Itecp.codpart,2)='05' OR ;
       	  LEFT(Itecp.codpart,2)='07' OR ;
       	  LEFT(Itecp.codpart,2)='08' OR ;
       	  LEFT(Itecp.codpart,2)='11' 
       	  vcodpar = LEFT(Itecp.codpart,2)+'.00'
       	  vdescri = despart(vcodpar)
       ELSE	  
       	  vcodpar = 'OTROS'
       	  vdescri = 'OTRAS PARTIDAS'
       ENDIF
    ENDIF
Vflag   = IIF(!EMPTY(Compag.Numhc),'H',IIF(Compag.Tipdoc='SU','S',IIF(Compag.Prestamo='S','P',compag.flagcon)))
VImportH= IIF(vflag='H',Itecp.Impparc,0)
VImportA= IIF(vflag='A',Itecp.Impparc,0)
VImportF= IIF(vflag='F',Itecp.Impparc,0)
VImportE= IIF(vflag='E',Itecp.Impparc,0)
VImportS= IIF(vflag='S',Itecp.Impparc,0)
VImportP= IIF(vflag='P',Itecp.Impparc,0)
vnumchq = Cheque.Numchq
vliquido = Cheque.ValChq 
vtipdoc  = Compag.Tipdoc 
SCAN
    IF Itecp.Tipfun='F'
       IF LEFT(Itecp.Codanal,2)='01' OR ;
       	  LEFT(Itecp.Codanal,2)='02' OR ;
       	  LEFT(Itecp.Codanal,2)='03' OR ;
       	  LEFT(Itecp.Codanal,2)='04' OR ;
       	  LEFT(Itecp.Codanal,2)='05' OR ;
       	  LEFT(Itecp.Codanal,2)='07' OR ;
       	  LEFT(Itecp.Codanal,2)='08' OR ;
       	  LEFT(Itecp.Codanal,2)='11' 
       	  vcodpar = LEFT(Itecp.Codanal,2)+'.00'
       	  vdescri = despart(vcodpar)
       ELSE	  
       	  vcodpar = 'OTROS'
       	  vdescri = 'OTRAS PARTIDAS'
       ENDIF
    ELSE
       IF LEFT(Itecp.CodPart,2)='01' OR ;
       	  LEFT(Itecp.Codpart,2)='02' OR ;
       	  LEFT(Itecp.codpart,2)='03' OR ;
       	  LEFT(Itecp.codpart,2)='04' OR ;
       	  LEFT(Itecp.codpart,2)='05' OR ;
       	  LEFT(Itecp.codpart,2)='07' OR ;
       	  LEFT(Itecp.codpart,2)='08' OR ;
       	  LEFT(Itecp.codpart,2)='11' 
       	  vcodpar = LEFT(Itecp.codpart,2)+'.00'
       	  vdescri = despart(vcodpar)
       ELSE	  
       	  vcodpar = 'OTROS'
       	  vdescri = 'OTRAS PARTIDAS'
       ENDIF
    ENDIF
	Vflag   = IIF(!EMPTY(Compag.Numhc),'H',IIF(Compag.Tipdoc='SU','S',IIF(Compag.Prestamo='S','P',compag.flagcon)))
	VImportH= IIF(vflag='H',Itecp.Impparc,0)
	VImportA= IIF(vflag='A',Itecp.Impparc,0)
	VImportF= IIF(vflag='F',Itecp.Impparc,0)
	VImportE= IIF(vflag='E',Itecp.Impparc,0)
	VImportS= IIF(vflag='S',Itecp.Impparc,0)
	VImportP= IIF(vflag='P',Itecp.Impparc,0)
	vnumchq = Cheque.Numchq
	vliquido = Cheque.ValChq 
    vtipdoc  = Compag.Tipdoc 
    SELECT Temp
    IF f_appd()
            REPLACE codpart WITH vcodpar        
            REPLACE descri  WITH vdescri        
            REPLACE importH WITH vimportH       
            REPLACE importA WITH vimportA       
            REPLACE importF WITH vimportF       
            REPLACE importE WITH vimportE       
            REPLACE importS WITH vimportS       
            REPLACE importP WITH vimportP       
            REPLACE codctc  WITH vcodctc
            REPLACE flag    WITH vflag
            REPLACE numchq  WITH vnumchq
            REPLACE liquido WITH vliquido
            REPLACE Tipdoc  WITH vtipdoc 
    ENDIF
    SELE Itecp
ENDSCAN
SET INDEX TO
RETURN


FUNCTION PROCnc
*---------------
Vcodpar = LEFT(MovB.CodPart,2)+'.00'
vdescri = despart(vcodpar)
VImport = MovB.Monto
Vflag   = 'H'
vtipdoc  = 'MB'
ichq=1
SCAN
	Vcodpar = LEFT(MovB.CodPart,2)+'.00'
	vdescri = despart(vcodpar)
	VImport = MovB.Monto
	Vflag   = 'H'
	vtipdoc  = 'MB'
    SELECT Temp
    IF f_appd()
            REPLACE codpart WITH vcodpar        
            REPLACE descri  WITH vdescri        
            REPLACE importH WITH vimport        
            REPLACE codctc  WITH vcodctc
            REPLACE flag    WITH vflag
            REPLACE Tipdoc  WITH vtipdoc 
            REPLACE liquido WITH vimport        
            REPLACE numchq  WITH str(ichq)
     ENDIF
     ichq=ichq+1
     SELE MovB
ENDSCAN
SET INDEX TO
RETURN

PROCEDURE mues_rep
*------------------
SELE Temp
=vgiro()
=vhoja()
=vreten()
=vitehc()
SELE temp
INDEX ON Codpart TO (vind3)  FOR tipdoc<>'RE'
SELE 11
CREATE TABLE (filrep);
	  (CodPart   C(005),;
	   descri    C(024),;
	   totfp     N(18,2),;
	   totan     N(18,2),;
	   toten     N(18,2),;
	   tothc     N(18,2),;
	   totsu     N(18,2),;
	   totpr     N(18,2),;
	   totafec   N(18,2),;
	   codctc    C(014))
USE 
USE (filRep) IN 11 EXCLUSIVE ALIAS Temp2
SELE temp2
index on codpart TO (vind8)
GO TOP
SELE temp
SCAN
	SELE temp2
	SEEK temp.codpart
	IF FOUND()
	   REPLACE temp2.totfp WITH temp2.totfp+temp.importF
	   REPLACE temp2.totan WITH temp2.totan+temp.importA
	   REPLACE temp2.toten WITH temp2.toten+temp.importE
	   REPLACE temp2.tothc WITH temp2.tothc+temp.importH
	   REPLACE temp2.totsu WITH temp2.totsu+temp.importS
	   REPLACE temp2.totpr WITH temp2.totpr+temp.importP
	ELSE
	   IF f_appd()
		  REPLACE temp2.codpart WITH temp.codpart
		  REPLACE temp2.descri  WITH temp.descri 
		  REPLACE temp2.codctc  WITH temp.codctc 
	      REPLACE temp2.totfp 	WITH temp2.totfp+temp.importF
	      REPLACE temp2.totan 	WITH temp2.totan+temp.importA
	      REPLACE temp2.toten 	WITH temp2.toten+temp.importE
	      REPLACE temp2.tothc 	WITH temp2.tothc+temp.importH
	      REPLACE temp2.totsu 	WITH temp2.totsu+temp.importS
	      REPLACE temp2.totpr 	WITH temp2.totpr+temp.importP
	   ENDIF   
	ENDIF
	SELE temp
ENDSCAN	
SET INDEX TO
USE IN 12 (vtempo) ALIAS vtemp
SELE vtemp
SCAN
	SELE temp2
	SEEK vtemp.codpart
	IF FOUND()
	   REPLACE temp2.totafec WITH temp2.totafec+vtemp.valpart
	ELSE
	   IF f_appd()
	   REPLACE temp2.codpart WITH vtemp.codpart
	   REPLACE temp2.descri  WITH vtemp.descri 
	   REPLACE temp2.codctc  WITH vtemp.codctc 
	   REPLACE temp2.totafec WITH temp2.totafec+vtemp.valpart
	   ENDIF   
	ENDIF
	SELE vtemp
ENDSCAN	
SET INDEX TO
SELE temp2
DEACTIVATE WINDOW xwait
DO REPORTE WITH 2, "T2_ofg", ' Reporte T-2 ',1,.F.,.T.
SET INDEX TO
RETURN

FUNCTION vGiro
*-------------
vAli=ALIAS()
SELECT temp
INDEX ON numchq TO (vind4) UNIQUE
GO TOP
SCAN 
    DO CASE
       CASE temp.ImportF <> 0
		    vMontoF= vMontoF + temp.liquido
       CASE temp.ImportA <> 0
		    vMontoA= vMontoA + temp.Liquido
       CASE temp.ImportE <> 0
		    vMontoE= vMontoE + temp.Liquido
       CASE temp.ImportH <> 0 OR temp.tipdoc='RE'
            IF temp.tipdoc<>'RE'
               VmontoR= vMontoR + temp.Liquido
            ENDIF   
		    vMontoH= vMontoH + temp.Liquido
       CASE temp.ImportS <> 0
		    vMontoS= vMontoS + temp.Liquido
       CASE temp.ImportP <> 0
		    vMontoP= vMontoP + temp.Liquido
     ENDCASE		    
ENDSCAN
SET INDEX TO
SELECT (vAli)
RETURN 

FUNCTION vhoja
*--------------
zAli=ALIAS()
SELECT Hoja
INDEX ON numhc TO (vind5) FOR nummes=PADL(vmes,2,'0') AND VAL(periodo)=Vano AND codctc=vcodctc AND (estado='50'  OR tipdoc='N/C')
GO TOP
SCAN 
     hmonto = hmonto+hoja.imptot-hoja.valreb-hoja.valanu
ENDSCAN
SET INDEX TO
SELECT (zAli)
RETURN 


FUNCTION despart
*---------------
PARAMETERS part
nali=ALIAS()
SELE CLASE
SEEK LEFT(part,2)+RIGHT(part,2) 
IF FOUND()
   vdescri=LEFT(despar,24)
ENDIF
SELE (nali)
RETURN vdescri   


FUNCTION vreten
*---------------
tAli=ALIAS()
SELE Parma
SET FILT TO Tipo='CODRET'
GO TOP
USE Reten    IN  0 	ORDER 	TAG  Reten5    ALIAS Reten
SELECT Reten
INDEX ON numcppg TO (vind6) FOR mescppg=PADL(vmes,2,'0') AND VAL(periodo)=Vano AND codctc=vcodctc AND conpago='û' 
GO TOP
vret=ALLT(reten.codret)
valr=reten.valret
SCAN
   vret=ALLT(reten.codret)
   valr=reten.valret
   SELE Parma
   LOCATE FOR ALLT(Parma.Codigo)=vret
   IF ALLT(parma.descriau2)='T'
      tesfavor=tesfavor+valr
   ELSE
      terfavor=terfavor+valr
   ENDIF
   SELE reten       
ENDSCAN
SELE Parma
SET FILT TO
SELE reten
USE
SELE (Tali)
RETURN   

FUNCTION vitehc
*--------------
iali=ALIAS()
SELECT DISTINCT I.*,H.CODCTC ;
  	   FROM IteHC I, Hoja H;
   	   WHERE (I.NumMes+I.Numhc=H.NumMes+H.NumHc) ;
 	   AND (H.codctc=vcodctc AND H.nummes=PADL(vmes,2,'0') AND (H.estado='50' OR h.tipdoc='N/C')) AND (I.estado#"99");
   	   ORDER BY I.numHc ;
INTO TABLE iteh
SELECT ITeh
INDEX ON NumMes+NumHc TO (Vind7)
GO TOP
SCAN
   DO CASE
	  CASE Iteh.estado='94' 
	       REPLACE Iteh.valpart WITH Iteh.Valpart*-1	   	   
	  CASE Iteh.estado='92'  
	       IF !EMPTY(iteh.numhm) AND iteh.Nummeshm=PADL(Vmes,2,'0')
	       	  IF iteh.tipope='-'
		          REPLACE Iteh.valpart WITH Iteh.Valpart*-1	   	   
		      ENDIF
		   ELSE
		      REPLACE Iteh.valpart WITH 0
		   ENDIF
	  CASE Iteh.estado='00'  
	       IF !EMPTY(iteh.numPa) AND iteh.mesPa=PADL(Vmes,2,'0')
	          REPLACE Iteh.valpart WITH Iteh.Valpart*-1	   	   
		   ENDIF
   ENDCASE  
   IF Iteh.tipfun='I'
      REPLACE iteh.codpart WITH left(iteh.codpart,2)+'.00'
      REPLACE Iteh.codanal WITH SPACE(5)
   ELSE   
      REPLACE iteh.codpart WITH left(iteh.codanal,2)+'.00'
      REPLACE Iteh.codanal WITH SPACE(5)
   ENDIF
   REPLACE descri WITH  despart(iteh.codpart)
ENDSCAN   
COPY TO (vtempo) 
SET INDEX TO	       
SELE (iali)
RETURN
	       
	       
      
PROCEDURE Salida
*---------------
  ACTIVATE   SCREEN
  CLOSE DATA
RETURN

FUNCTION MFecha
*--------------
PARAMETERS xmes 
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) &&+ ' ' + STR(xAno,2)