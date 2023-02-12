

USE OrdCom   IN 1   order tag OrdCom1  ALIAS Orden
USE Iteoc1   IN 2   order tag IteOc11  ALIAS IteOc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Promae   IN 4   order tag Promae1  ALIAS Promae

SELECT ORDEN

SET RELATION TO PERIODO+NUMOC INTO ITEOC
vtemp =recno()
if eof()
   do standby with 'No existen Registros a Procesar'
   return
else
   DO LisOrd
endif
SELECT ORDEN
SET RELATION TO
SET ORDE TO 1
go vtemp
*DO VISTA
RETURN


PROCEDURE lisOrd
*---------------
vOrde = ORDER()
DEFINE WINDOW LIS FROM 0,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado Ordenes de Compra' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro,vListado,vTofue,vToPrv,vtri
vCli = SPAC(4)
vAno = '99'
VFTE = '  '
vcodfte = '  '
vcodPRV = '    '
VTRI='  '
vfecini = date()
vfecfin = date()

@ 01,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ Detallado;Resumido' 

@ 04,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,07,22)  
@ 06,01 SAY "           Fuente : "
@ 06,22 GET vCodFte  PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE','C') WHEN VTOFUE=2 

@ 08,01 SAY "Todos los Proveed : " GET vToprv  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOprv,11,22) 
@ 10,01 SAY "      Proveedores : "
@ 10,22 GET vCodPrv  PICTURE '!!!!' VALID VALPrv() WHEN vToPRV=2 

@ 12,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emision;Fuente' 

@ 15,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;MenorCuant¡a;Liquidados'

@ 18,01 SAY "         Trimestre: " GET vtri picture '!!'  when vtippro = 5
@ 18,22                            GET vtri picture '!!'  when vtippro = 5

@ 20,01 SAY "           Fechas : " GET vFecIni picture '@D'  when vtippro = 6
@ 20,32                            GET vFecFin picture '@D'  when vtippro = 6

@ 22,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,NumOc,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOc))) TO (vInd) ;
      FOR  IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',iif(vTipPro=3,Estado = '40',iif(vTipPro=4,Estado = '20',iif(vTipPro=5,Estado = '50',Estado = '50'))))) AND ;
           iif(vtofue =1,.t.,Codfte=alltrim(vCODfte)) 
      SET FILT TO iif(vtoprv =1,.t.,Codprv=alltrim(vCODprv)) AND IIF(vTipPro=6,BETWEEN(FECDESP,VFECINI,VFECFIN),.T.) &&AND orden.tipord#'B'
	  SET FILT TO IIF(vTipPro=5,IIF(VTRI='01',BETWEEN(month(fecdesP),01,03),IIF(VTRI='02',BETWEEN(MONTH(FECDESP),04,06),IIF(VTRI='03',BETWEEN(MONTH(FECDESP),07,09),BETWEEN(MONTH(FECDESP),10,12)))),.T.) AND VALTOT < 35000.00
**	  FOR IIF(vTipPro=5, VALTOT < 35000.00,  ' ' )	                                                        &&IIF(VTRI='03',BETWEEN(MONTH(FECDESP),07,09),.T.) &&IIF(VTRI='03',BETWEEN(FECDESP),'07','09',BETWEEN(FECDESP),'10','12'))) AND VALTOT < 35000.00
   SET INDEX TO (VIND)
   COUNT ALL TO vTotoc     
   SUM valtot - anultot TO SUMATOT
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,'Listado Orden Compra',IIF(vTipPro=2,'Listado Orden de Compra Pendientes',IIF(vTipPro=3,'Listado de Ordenes de Compra Atendidas',IIF(vTipPro=4,'Listado Orden de Compra Afectadas',IIF(vTipPro=4,'Listado Orden de Compra Anuladas','Listado Orden de Compra Liquidadas')))))
   SET MEMOWIDTH TO 43
   IF !EOF()
     SET SKIP TO ITEOC
     	IF VTIPPRO#6
     		do case 
        		case VLISTADO=1
    	 			DO REPORTE WITH 2,"LisOrdc1",' Ordenes de Compra ',1,.F.,.T.
        		case VLISTADO=2    	
    	 			DO REPORTE WITH 2,"LisMECU",' Ordenes de Compra ',1,.F.,.T.        
     		ENDcase   
     	ELSE
     		do case 
        	case VLISTADO=1
    			DO REPORTE WITH 2,"LisOrdc",' Ordenes de Compra ',1,.F.,.T.
        	case VLISTADO=2    	
    			DO REPORTE WITH 2,"LisOrdX",' Ordenes de Compra ',1,.F.,.T.        
     		ENDcase   
     	ENDIF
     SELECT ORDEN
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILT TO
   CLOSE INDEX
   ERASE (VIND)
ENDIF
RETURN


PROCEDURE aLista
*--------------
SELECT ORDEN
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   SET RELATION TO PERIODO+NUMOC INTO ITEOC
   SET SKIP TO ITEOC
            do reporte with 2,"LisOc",' Gu¡as de Internamiento '
   SET RELATION TO
endif
select Orden
go vtemp
DO VISTA
RETURN

