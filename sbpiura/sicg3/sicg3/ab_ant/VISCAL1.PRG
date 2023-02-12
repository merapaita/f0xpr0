*--------------------------------------------------------------------------
* Clasif.Prg
* Calcula Calidad de Vida
* Estado :
*   '00' Emitida     ** 	
*   '99' Anulada
* Autor: LCD 17 feb'95
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE MaeFic   IN 2   order tag MaeFic1  ALIAS MaeFic
USE IteFic   IN 3   order tag IteFic1  ALIAS IteFic
USE MaeOcu   IN 4   order tag MaeOcu2  ALIAS MaeOcu
USE Maeenf   IN 5   order tag MaeEnf1  ALIAS Maeenf
USE Iteenf   IN 6   order tag IteEnf1  ALIAS iteenf
USE MaeFam   IN 7   order tag MaeFam1  ALIAS MaeFam


PUBLIC VSEC,VZON,OP,VFIC,VBAJO,VMEDIANO,VALTO,TOTALA,TOTALB,TOTALM
* Mensajes de aviso al usuario
Vmens01 = ' Ficha Familiares : REVISION '
Vmens02 = 'Registro de Ficha Familiares'
Vmens04 = 'Dicha Ficha Familiar no fue encontrada'
Vmens05 = 'No existe Ficha Familiar anterior'
Vmens06 = 'No existe Ficha Familiar siguiente'
Vmens07 = '¨ Desea Anular ‚sta Ficha Familiar ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Ficha Familiar ha sido anulada'
Vmens10 = 'El Ficha Familiar ya est  Atendida'
Vmens11 = 'El Ficha Familiar ha sido devuelta'
Vmens12 = 'El Ficha Familiar ya tiene O/C'
Vmens13 = 'Dicho Miembro Familiar no fue encontrada'


DO LISFIC
IF LASTKEY()=27 
   RETURN
ENDIF   
go top
DO CASE
	CASE OP=1
   		 do reporte with 2,"RIESGO",' RIESGO ',1,.F.,.T.
	CASE OP=2
    	 do reporte with 2,"RIESGO1",' RIESGO ',1,.F.,.T.
	CASE OP=3
         do reporte with 2,"RIESGO2",' RIESGO '
	CASE OP=4
ENDCASE
set filter to
set index to 
IF OP#4
*erase (vind)
ENDIF	
CLOSE DATA 
RETURN


PROCEDURE LisFic
*--------------
vTemo = RECNO()
DEFINE WINDOW LIS FROM 3,15 TO 21,65 FLOAT DOUBLE TITLE 'Listado Calidad de Vida' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vTofic,vToSec,vToZon,vOrden,vListado
vFic = Space(5)
vSec = Space(1)
vZon = Space(2)
vFic = Space(6)

@ 01,01 SAY "          Listado : " GET vlistado  FUNCTION '^ Todos;Ficha'

@ 05,01 SAY "         Ficha N§ : "
@ 05,22 GET vfic    WHEN vlistado = 2  PICTURE '!!!!!!' VALID valefic()

@ 07,01 SAY "Todo los Sectores : " GET vToSec  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToSec,7,22) AND vToSec =2 WHEN vTofic = 1 and vlistado = 1
@ 09,01 SAY "        Sector N§ : "
@ 09,22 GET vSec    WHEN (vToSec=2 AND vlistado=1)  PICTURE '!' valid val_para(vSec,'SECTOR',' ',22,20) 

@ 11,01 SAY "  Todas las Zonas : " GET vToZon  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToZon,11,22) and vToSec=2 WHEN vToFic = 1 and vlistado = 1
@ 13,01 SAY "          Zona N§ : "
@ 13,22 GET vZon    WHEN (vToZon=2 AND vToSec=2 AND vlistado = 1)  PICTURE '!!' valid val_zona(vZon,'ZONAS ',' ',vSec,22,20) 


@ 16,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS
IF LASTKEY()=27
   RETURN
ENDIF   
IF OKCANCEL = 1
   IF !EOF()
     SELE MAEFIC
     vind = sys(3)+'.idx'
     IF vlistado = 1
        DO CASE
           CASE vTosec=2 and vtozon=1
              OP=2         
              INDEX ON NUMFIC TO (vind) FOR sector=ALLTRIM(VSEC) &&and iif(vlistado=1,.t.,vfic=numfic)
 	       CASE vTosec=2 and vtozon=2
 	          OP=3
          	  INDEX ON NUMFIC TO (vind) FOR zona=ALLTRIM(VZON) and sector=ALLTRIM(vsec) &&and iif(vlistado=1,.t.,vfic=numfic)
           OTHERWISE
              OP=1
        ENDCASE
      ELSE
        OP = 4
        DO CLADET 
      ENDIF  
   ELSE
      DO STANDBY WITH VMENS08
   ENDIF
ELSE 
  OP=4   
ENDIF
GO TOP
RETURN 

PROCEDURE CLADET
*-----------------
 select parma
 seek 'RIESGO'+'A'
 vBAJO = PARMA.NUMDEC
 seek 'RIESGO'+'B'
 vMEDIANO = PARMA.NUMDEC 
 seek 'RIESGO'+'C'
 vALTO = PARMA.NUMDEC

SELECT MaeFic
GO TOP

* Relaciones con los otros archivos
SET RELATION TO NUMFIC INTO ITEFIC
SET RELATION TO NUMFIC INTO MAEFAM ADDIT

vCodigo = vfic
SEEK VCodigo
IF !FOUND()
     DO standby WITH Vmens04
	 return
ENDIF

STORE 0 to sumaa,sumab,sumam
ZONA1=ALLTRIM(ZONA)

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
P_FIL = SYS(3)+".LST"
SET DEVICE TO FILE &P_FIL
* CARATULA
*------------
@PROW()+1,0 SAY CHR(18) 
@PROW()+1,0 SAY ALLTRIM(CIA)
@PROW(),23 SAY '   RIESGO FAMILIAR POR DETALLE'
@PROW(),70 SAY '  Pag 01'
@PROW()+1,0 SAY 'Cladet'
@PROW(),31 SAY 'FICHA :'
@PROW(),39 SAY VCODIGO
@PROW(),70 SAY DATE()
@PROW()+1,0 SAY 'Ú'+REPLICA('Ä',79)+'¿'
@PROW()+1,0 SAY '³'
@PROW(),1 SAY 'SECTOR:'
@PROW(),9 SAY VAL_PARA( LEFT(VCODIGO,1),'SECTOR','D',2,60)
@PROW(),80 SAY '³'
@PROW()+1,0 SAY '³'
@PROW(),1 SAY 'ZONA  :'
@PROW(),9 SAY VAL_PARA( LEFT(VCODIGO,2),'ZONAS ','D',2,60)
@PROW(),80 SAY '³'
@PROW()+1,0 SAY '³'
@PROW(),1 SAY 'JEFE DE FAMILIA: '
@PROW(),19 SAY GRUFAM
@PROW(),80 SAY '³'
@PROW()+1,0 SAY '³'
@PROW(),1 SAY 'DIRECCION      : '
@PROW(),19 SAY DIRECCION
@PROW(),80 SAY '³'
@PROW()+1,0 SAY 'À'+REPLICA('Ä',79)+'Ù'
*@PROW()+2,0 SAY REPLI('-',80)


* A .- CONDICION SOCIO ECONOMICA
 
 select itefic
 seek maefic.Numfic+'01'
 select parma
 seek 'FRACLA'+SUBSTR(ITEFIC.ocupacion,2,1)
 *------
 valorA = PARMA.NUMDEC
*----------------
@PROW()+1,1 SAY  '(A) CONDICION SOCIO-ECONOMICO'
@PROW()+1,4 SAY IIF(EMPTY(DESCRI),'No hay dato de ocupaci¢n',left(DESCRI,50))
@PROW(),60 SAY VALORA
@PROW()+1,40 SAY 'SUBTOTAL'
@PROW(),65 SAY VALORA
@PROW()+2,0 SAY REPLI('-',80)
 *------
 
*------------------------------------------------------
* B .- CONDICION BIOLOGICA
  **EDAD
  SELECT ITEFIC
  seek maefic.Numfic
  vFun = 0
  vFan = 0
  SCAN WHILE itefic.NumFic = maefic.Numfic
       DO CASE
          CASE (edad<5 OR EDAD>60) AND AAMMDD='A'
              vFun = vFun + 1
          CASE (edad>4 OR EDAD<16) AND AAMMDD='A'    
              vFan = vFan + 1
       endCASE
  ENDSCAN
  
  
 * PARTE PREMATURO = 2 PTOS ,FUE EUTOCICO
 valorB = 2+iif(vFun>vFan,1,2)
**-----------
@PROW()+1,1 SAY '(B) CONDICION BIOLOGICA'
@PROW()+1,4 SAY  IIF(VFUN>VFAN,'Hay mas menores de 5 y mayores de 60','Hay mas entre 5 y 15 a¤os')
RES=iif(vFun>vFan,1,2)
@PROW(),60 SAY RES
@PROW()+1,4 SAY 'Otros'
RES=2
@PROW(),60 SAY RES
@PROW()+1,40 SAY 'SUBTOTAL'
@PROW(),65 SAY VALORB
@PROW()+2,0 SAY REPLI('-',80)
*------------------------------------------------------ 
 * C.- ACCESO A LA EDUCACION 
 
 select itefic
 seek maefic.Numfic+'01'
 select parma
 seek 'GRDINS'+ITEFIC.GRAINS
 *------
 valorC = IIF(FOUND(),PARMA.NUMDEC,6)
 *------
@PROW()+1,1 SAY  '(C) ACCESO A LA EDUCACION'
@PROW()+1,4 SAY IIF(EMPTY(DESCRI),'No hay dato de la educaci¢n',left(DESCRI,50))
@PROW(),60 SAY VALORC
@PROW()+1,40 SAY 'SUBTOTAL'
@PROW(),65 SAY VALORC
@PROW()+2,0 SAY REPLI('-',80)

*------------------------------------------------------ 
 * D.- ACCESO A LA VIVIENDA
 
 select MAEFAM
 seek maefic.Numfic
 select parma
 seek 'TIPVIV'+MAEFAM.VI_TIP_VIV
 *------
 vD1 = IIF(FOUND(),PARMA.NUMDEC,8)
 *------
@PROW()+1,1 SAY  '(D) ACCESO A LA VIVIENDA'
@PROW()+1,4 SAY IIF(EMPTY(DESCRI),'No hay dato tipo de vivienda',left(DESCRI,50))
@PROW(),60 SAY VD1
 
*------- 
 select MAEFAM
 seek maefic.Numfic
 select parma
 seek 'TIPVIV'+MAEFAM.VI_TIP_VIV
 XX=PARMA.CODIGOAUX
 SEEK 'UBICAC'+XX
 *------
 vD2 = IIF(FOUND(),PARMA.NUMDEC,4)
 *------
@PROW()+1,4 SAY IIF(EMPTY(DESCRI),'No hay dato de la ubicaci¢n',left(DESCRI,50))
@PROW(),60 SAY VD2

*-------
 select MAEFAM
 seek maefic.Numfic
 select parma
 seek 'SITPRO'+MAEFAM.VI_SIT_PRO
 *------
 vD3 = IIF(FOUND(),PARMA.NUMDEC,2)
 *------
@PROW()+1,4 SAY IIF(EMPTY(DESCRI),'No hay dato de al propiedad',left(DESCRI,50))
@PROW(),60 SAY VD3

*-------
 select maefic
 seek maefic.numfic
 dd = nrointeg

 select maefam
 seek Maefic.Numfic
 do case 
    case INT(DD/vi_nro_hab)>=5 
	     vPtot=0        
    case INT(DD/vi_nro_hab)=3 or INT(DD/vi_nro_hab)=4
	     vPtot=1        
    case INT(DD/vi_nro_hab)=2 
         vPtot=2        
    case INT(DD/vi_nro_hab)=1 or INT(DD/vi_nro_hab)=0
	     vPtot=3        
 endcase	     
*-------------
 vD4 = vPtot 
*------------- 
@PROW()+1,4 SAY 'Tiene '+STR(VI_NRO_HAB,2)+' Habitaciones'
@PROW(),60 SAY VPTOT

*-------------
 select maefam
 seek Maefic.Numfic
 vPtot=0
 do case 
    case vi_nro_dor>=2 
	     vPtot=0        
    case vi_nro_dor=3 or vi_nro_dor=4
	     vPtot=1        
    case vi_nro_dor>4 
         vPtot=2        
 endcase	     
*------------
 vD5 = vPtot 
 *-------------
@PROW()+1,4 SAY 'Tiene '+STR(VI_NRO_DOR,2)+' Dormitorios'
@PROW(),60 SAY VPTOT

 *-------------
 select maefam
 seek Maefic.Numfic
 select parma
 seek 'FTEAGU'+MAEFAM.VI_1_AGUA
 *------
 vD6 = IIF(FOUND(),PARMA.NUMDEC,2)
 *------

@PROW()+1,4 SAY IIF(EMPTY(DESCRI),'No hay dato de la conexi¢n de agua',left(DESCRI,50))
@PROW(),60 SAY VD6

*---------
 select maefam
 seek Maefic.Numfic
 select parma
 seek 'DESAGU'+MAEFAM.VI_1_DESAG
 *------
 vD7 = IIF(FOUND(),PARMA.NUMDEC,2)
 *------
@PROW()+1,4 SAY IIF(EMPTY(DESCRI),'No hay dato de la conexi¢n de desague',left(DESCRI,50))
@PROW(),60 SAY VD7

*--------
 select maefam
 seek Maefic.Numfic
 select parma
 seek 'BASURA'+MAEFAM.VI_1_BASUR
 *------
 vD8 = IIF(FOUND(),PARMA.NUMDEC,2)
 *------
@PROW()+1,4 SAY IIF(EMPTY(DESCRI),'No hay dato del almacenamiento de basura',left(DESCRI,50))
@PROW(),60 SAY VD8
 
*-------- 
 valorD = vD1+vD2+vD3+vD4+vD5+vD6+vD7+vD8
*----------
@PROW()+1,40 SAY 'SUBTOTAL'
@PROW(),65 SAY VALORD
@PROW()+2,0 SAY REPLI('-',80)
 
*--------------------
 select ITEFIc   && SI TIENE SEGURO
 seek maefic.Numfic+'01'
 *------
 vE1 = IIf(ITEFIC.SEGURO=' ',4,0)
 *------
@prow()+1,1 SAY '(E) ACCESO A LOS SERVICIOS DE SALUD'
@PROW()+1,4 SAY IIF(ITEFIC.SEGURO=' ','Ninguno posee seguro','Todos poseen seguro')
@PROW(),60 SAY ve1

 *-------
 select Itefic   && A QUIEN ACUDIO
 seek maefic.Numfic+'01'
 do case
    case itefic.III_acudio='H' or  itefic.III_acudio='HS' or itefic.III_acudio='PS' or itefic.III_acudio='PR' or itefic.III_acudio='M' 
         VTOT=0
   OTHER      
         VTOT=2 
 ENDCASE

 *------
 vE2 = VTOT
 *------
@PROW()+1,4 SAY IIF(VTOT=0,'Generalmente acuden a los servicios formales de salud','Generalmente acuden a los servicios informales de salud')
@PROW(),60 SAY ve2

*--------
VALORE = VE1+VE2+14
*---------
OTROS=14
@PROW()+1,4 SAY 'Otros'
@PROW(),60 SAY OTROS
@PROW()+1,40 SAY 'SUBTOTAL'
@PROW(),65 SAY VALORE
@PROW()+2,0 SAY REPLI('-',80)

*--------- 
 select ITEFIC	
 seek maefic.Numfic
 VTOT=0
 SCAN WHILE maefic.Numfic=Numfic
      if !emptY(III_CodEnf)
         vTot = vTot+1
      endif
 endSCAN        
 *------
 vF1 = IIF (VTOT=0,0,2)
 *------
@prow()+1,1 SAY '(F) EXPRESION BIO-PSICO-SOCIAL'
@PROW()+1,4 SAY IIF(VTOT=0,'No se evidencian problemas de conducta en el grupo','Si se evidencian problemas de conducta en el grupo')
@PROW(),60 SAY VF1

*--------
 select maefam
 seek maefic.Numfic
 do case 
    case v1_MORTALI='S' 
	     vPtot=1        
    OTHER
	     vPtot=0
 ENDCASE
*---------- 
 VF2=VPTOT	  
*----------
@PROW()+1,4 SAY IIF(VpTOT=0,'No hubo ningun fallecido en el ultimo a¤o','Si hubo ningun fallecido en el ultimo a¤o')
@PROW(),60 SAY VF2

*----------    
   

 select maefIC
 seek maefic.Numfic
 do case 
    case NROINTEG - 1 <=2
         TOT =0      
    case NROINTEG - 1 =3 OR  NROINTEG - 1 =4  
         TOT=1
    case NROINTEG - 1 >4 
         TOT=2
    OTHER
    
 ENDCASE
*-----------
 VF3=TOT	     
*------------
@PROW()+1,4 SAY str(nrointeg-1,2)+' Personas dependen del jefe'
@PROW(),60 SAY VF3
@PROW()+1,4 SAY  'Otros'
vdato=11
@PROW(),60 SAY Vdato

*------------
VALORF=VF1+VF2+VF3+11
*--------------
@PROW()+1,40 SAY 'SUBTOTAL'
@PROW(),65 SAY VALORF
@PROW()+2,0 SAY REPLI('-',80)

*--------------
VALORT = VALORA+VALORB+VALORC+VALORD+VALORE+VALORF
*GO VREC
*--------------
@PROW()+1,40 SAY 'TOTAL'
@PROW(),65 SAY VALORT
@PROW()+1,1 SAY 'PERTENECE A :'
*--------------

DO CASE
   CASE VALORT<VBAJO
	   @PROW()+1,15 SAY 'BAJO RIESGO'
       sumab=sumab+1   
   CASE VALORT>=VBAJO and valort<=VMEDIANO
		@PROW()+1,15 SAY 'MEDIANO RIESGO'
       sumam=sumam+1   
   CASE VALORT>VALTO
	   @PROW()+1,15 SAY 'ALTO RIESGO'
       sumaa=sumaa+1   

endcase    

@PROW()+2,0 SAY REPLI('-',80)
sele maefic
SET DEVICE TO SCREEN
  do Xreporte with 4,"datos",' CLASIFICACION DE LA CALIDAD DE VIDA '
set filter to
*CLOSE DATA 
SET DEVICE TO SCREEN
RETURN


FUNCTION VALSER
*---------------
PARAMETER FILTRO
REC=RECNO()
TOTALB=0
TOTALM=0
TOTALA=0
DO CASE
 CASE FILTRO='1'
   SET FILTER TO SECTOR='1'
 CASE FILTRO='2'
   SET FILTER TO SECTOR='2'
 CASE FILTRO='3'
   SET FILTER TO SECTOR='3'
 CASE FILTRO='4'
   SET FILTER TO SECTOR='4'
 CASE FILTRO=' '
   SET FILTER TO 
ENDCASE
GO TOP
SCAN
    = CALVIDS()
	TOTALB=TOTALB+IIF(calvid<vbajo,1,0)
    TOTALM=TOTALM+IIF(calvid>=vbajo AND calvid<=vmediano,1,0)
	TOTALA=TOTALA+IIF(calvid>valto,1,0)
ENDSCAN
SET FILTER TO
GO REC
RETURN ' '

FUNCTION VALZON
*--------------
PARAMETER FILTRO
REC=RECNO()
TOTALB=0
TOTALM=0
TOTALA=0
SET FILT TO ZONA = FILTRO
GO TOP
SCAN
    = CALVIDS()
	TOTALB=TOTALB+IIF(calvid<vbajo,1,0)
    TOTALM=TOTALM+IIF(calvid>=vbajo AND calvid<=vmediano,1,0)
	TOTALA=TOTALA+IIF(calvid>valto,1,0)
ENDSCAN
SET FILTER TO
GO REC
RETURN ' '

PROCEDURE valefic
*--------------
DEFINE WINDOW wind_0 FROM 00,00 TO 23+xis,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

SELECT maefic
SET ORDE TO 1
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
SEEK vfic
IF !FOUND() OR EMPTY(vfic)
	SET RELATION TO numfic INTO itefic
	SET SKIP TO itefic
	vtemp = RECNO()
	ACTIVATE SCREEN
	vtempo = '°°°°°°°°°°°°°°°°°°°°°°°Presione cualquier tecla para salir°°°°°°°°°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	ON KEY LABEL f10 KEYBOARD CHR(23)
	BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
		numfic :H='C¢digo',;
		grufam :H='Apellidos Grupo Familiar' ,;
		itefic.nombre :H='Composici¢n Familiar'
	vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	ON KEY LABEL f10
	SET RELA TO
	SELE maefic
ENDIF
vfic=maefic.numfic
SELE maefic
RETURN

PROCEDURE riesgo
*---------------
=CALVIDS()
DO CASE
	CASE calvid<vbajo
		vsay= 'BAJO '
	CASE calvid>=vbajo AND calvid<=vmediano
		vsay= 'MEDIO'
	CASE calvid>valto
		vsay= 'ALTO '
ENDCASE
RETURN vsay

PROCEDURE CALVIDS
*-----------------
aw=ALIAS()
SELECT parma
SEEK 'CALVID'+'A'
vbajo = parma.numdec
SEEK 'CALVID'+'B'
vmediano = parma.numdec
SEEK 'CALVID'+'C'
valto = parma.numdec
SELE (aw)
RETURN 
