** ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
** ³Programa	: Record1.Prg                                     ³
** ³Fecha		: 17/08/98                                        ³
** ³Descripci¢n	: Reporte de los Records en O/C y O/S			  ³ 
** ³			  descriminado a identificar el Rubro del		  ³
** ³			  Proveedor.									  ³
** ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
** ³  Autor	   : Tec. Prog. Norberto Recarte Romero				  ³
** ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

**-Abrir Bases de Datos - Archivos

USE Prvaux	 IN 1	order tag Prvaux3  ALIAS aux
USE OrdCom   IN 2   order tag OrdCom4  ALIAS Orden
USE Promae   IN 3   order tag Promae1  ALIAS Promae
USE OrdSer   IN 4   order tag Ordser6  ALIAS Ordser
*USE Iteoc    IN 5   order tag IteOc1   ALIAS IteOc
USE recordo	 IN 6   				   ALIAS RECORDO EXCLUSIVE
USE Parmae   IN 7   order tag Parmae1  ALIAS Parma
USE ArtMae	 IN 8	order tag ArtMae1  ALIAS Produ


*- Mensajes de aviso al usuario

Vmens01 = ' Por Nivel Especifico : REVISION '
Vmens02 = ' Registro de Proveedores con m s Ordenes Adjudicadas '
Vmens03 = ' No hay registros para procesar'

DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 ;
TITLE ' Reporte en Pantalla : Record x Ordenes ' COLOR SCHEME 10

DEFINE WINDOW LIS FROM 03,15 TO 20,65 FLOAT SHADOW;
TITLE ' Listado Records x Ordenes ' COLOR SCHEME 5

ACTIVATE WINDOW LIS
STORE 1    TO vToMes,vTipPro,vListado,vTofue,vToGen,vToPrv,vOrden
vPrv = space(4)
vAno = '98'
vFte = space(2)
vGen = space(3)
vMes = space(2)
vPer = space(2)

@ 00,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ O/C;O/S' 

@ 03,01 SAY "    Por Genericas : " GET vToGen  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOGEN,08,22)
@ 04,01 SAY "         Generica : "
@ 04,22 GET vGen  PICTURE '!!!' VALID IIF(vlistado=1,VAL_PARA(vGen,'CODGEB','C'),VAL_PARA(vGen,'CODGES','C')) WHEN VTOGEN=2 

@ 06,01 SAY "Total Proveedores : " GET vToprv  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOprv,11,22) 
@ 07,01 SAY "        Proveedor : "
@ 07,22 GET vPrv  PICTURE '!!!!' VALID VALPrv() WHEN vToPRV=2 

@ 09,01 SAY "  Todos los Meses : " GET vtoMes   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOmes,14,22)  
@ 10,01 SAY "    Mes - Periodo : "
@ 10,22 GET vMes    PICTURE '!!' VALID VAL_PARA(vMes,'FECMES','C') WHEN VTOMES=2 
@ 10,26 GET vPer    PICTURE '!!' VALID !EMPTY(vPer)   && WHEN VTOMES=2 

@ 12,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados'

@ 15,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento ...'
   vInd = SYS(3) + '.IDX'
   IF VLISTADO = 1 
      SELE RECORDO
      zap
      INDEX ON STR(IMPTOT,15)+STR(VECES,4)+NOMPRV TAG RECORD DESC

      SELECT AUX
      SET FILTER TO CODCLA=LEFT(VGEN,3)
      SELECT ORDEN
	  
      SELECT AUX
	  IF EOF()
		 DO standby WITH Vmens08
		 RETURN
	  ENDIF
	  
	  SET RELATION TO '98'+AUX.CODPRV INTO ORDEN,'98'+AUX.CODPRV INTO PROMAE,'98'+AUX.CODPRV INTO PRODU
	  suspend
      SELECT AUX
	  BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	  Orden.Periodo	:H='A¤o' ,;
	  Codcla		:H='Generica' ,;
	  Produ.descri	:H='Especifica' ,;
	  Codprv		:H='C¢digo' ,;
	  Promae.nompro	:H='Proveedor' ,;
	  Orden.Numoc	:H='N§Orden' ,;
	  Orden.nummes	:H='Unid' ,;
	  Orden.Valtot	:H='Monto S/.'
      SELE RECORDO
      suspend
      append blank
      replace periodo with Orden.Periodo,codcla with aux.codcla,descri with produ.descri,codprv with orden.codprv,nomprv with val_prv(orden.CodPrv),veces with vcantol,imptot with vcanreq
      SELE orden
      DEACTIVATE WINDOW STANDBY
      SELECT RECORDO
      GO TOP
      IF !EOF()
      	DO CASE
      		CASE ='1'
      		vtitulo = iif(vtoGen =1,'A TODA GENERICA',ALLTRIM(val_para(vGen,'CODGEB','D',22,60))) 
      		DO REPORTE WITH 2,"RecOrdcg",' Ordenes de Compra ',1,.F.,.T.
      	 	CASE='2'
         	vtitulo = iif(vtofue =1,'A TODA FUENTE',ALLTRIM(val_para(vfte,'CODFTE','D',22,60))) 
         	DO REPORTE WITH 2,"RecOrdc",' Ordenes de Compra ',1,.F.,.T.
        ENDCASE
      ELSE
		DO STANDBY WITH VMENS08
     ENDIF
   ELSE
      SELE RECORDO
      zap
      INDEX ON STR(IMPTOT,15)+STR(VECES,4)+NOMPRV TAG RECORD DESC
      SELECT ORDSER
	  INDEX ON CodPrv TO (vInd) ;    
      FOR  IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',iif(vTipPro=3,Estado = '40',iif(vTipPro=4,Estado = '20',iif(vTipPro=5,Estado = '99',Estado = '50'))))) AND ;
           iif(vtofue =1,.t.,Codfte=alltrim(vfte)) 
      SET FILTER TO iif(vtoprv =1,.t.,Codprv=alltrim(vprv)) AND IIF(vTomes=1,.t.,val(vmes)=val(nummes)) and periodo=vper
      SET INDEX TO (VIND)
      GO TOP
	  SCAN
        vCanreq=0
        vcantol=0
        vCa1 = CodPRV
        vCa2 = CodPRV
        DO WHILE vCa1 = vCa2
          vCanreq = vCanreq+ValTot
          vCantol = vcantol+1
          SKIP
          vCa1 = CodPrv
        ENDD
        SKIP -1
        sele recordo
        append blank
        replace codprv with ordser.codprv,nomprv with val_prv(ordser.CodPrv),veces with vcantol,imptot with vcanreq
        SELE ordser
      ENDSCAN
      DEACTIVATE WINDOW STANDBY
      SELECT RECORDO
      GO TOP
      IF !EOF()
         vtitulo = iif(vtofue =1,'A TODA FUENTE',ALLTRIM(val_para(vfte,'CODFTE','D',22,60))) 
       	 DO REPORTE WITH 2,"RecOrds",' Ordenes de Servicios ',1,.F.,.T.
      ELSE
       DO STANDBY WITH VMENS08
     ENDIF
   endif
   SET FILT TO
   SET INDEX TO
   CLOSE INDEX
   CLOSE DATA
   ERASE (VIND)
ENDIF
CLOSE DATA
RETURN


function valgen
*--------------
private xx
vfun = .f.
VCODGEN = iif( empty(VGEN),Vprv,padl(alltrim(Vgen),3,'0'))
xx = val_art( Vgen,.t.)
if xx
   return .t.
endif
return .F.


function valprv
*--------------
private xx
vfun = .f.
VCODPRV = iif( empty(VPRV),Vprv,padl(alltrim(Vprv),4,'0'))
xx = val_prv( Vprv,.t.)
if xx
   return .t.
endif
return .F.
