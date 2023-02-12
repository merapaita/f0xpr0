USE OrdSer   IN  1  ORDER TAG OrdSer1  ALIAS OrdSe
USE ITEOS1   IN  2  ORDER TAG ITEOS11  ALIAS ITEOS1
USE Parmae   IN  3  ORDER TAG Parmae1  ALIAS Parma
USE Promae   IN  4  ORDER TAG Promae1  ALIAS Promae


SELECT ORDSE
*SCATTER MEMVAR
SET RELATION TO PERIODO+NUMOS INTO ITEOS1
vtemp =recno()
if eof()
   do standby with 'No Existen Registros a Procesar'
   return
else
   DO LISSER
endif
select Ordse
set relation to
set order to 1
go vtemp
*DO VISTA
RETURN


PROCEDURE lisSER
*---------------
vOrde = ORDER()
DEFINE WINDOW LIS FROM 0,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado Ordenes de Servicio' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro,vListado,vTofue,vToPrv, VTRI
vCli = SPAC(4)
vAno = '99'
VFTE = '  '
VTRI='  '
vcodfte = '  '
vcodPRV = '    '
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

@ 15,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;Menor Cuant¡a;Liquidados'

@ 18,01 SAY "           Fechas : " GET vtRI picture '!!'  when vtippro = 5
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
   INDEX ON IIF(vOrden=1,NumOs,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOs))) TO (vInd) ;
      FOR  IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',iif(vTipPro=3,Estado = '40',iif(vTipPro=4,Estado = '20',iif(vTipPro=5,Estado = '50',Estado = '50'))))) AND ;
           iif(vtofue =1,.t.,Codfte=alltrim(vCODfte)) 
      SET FILT TO iif(vtoprv =1,.t.,Codprv=alltrim(vCODprv)) AND IIF(vTipPro=6,BETWEEN(FECLIQ,VFECINI,VFECFIN),.T.) &&AND orden.tipord#'B'
	  SET FILT TO IIF(vTipPro=5,IIF(VTRI='01',BETWEEN(month(fecLIQ),01,03),IIF(VTRI='02',BETWEEN(MONTH(FECLIQ),04,06),IIF(VTRI='03',BETWEEN(MONTH(FECLIQ),07,09),BETWEEN(MONTH(FECLIQ),10,12)))),.T.) AND VALTOT < 35000.00
   SET INDEX TO (VIND)
   COUNT ALL TO vTotos     
   SUM valtot - anultot TO SUMATOT
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,'Listado de Ordenes de Servicio',IIF(vTipPro=2,'Listado de Ordenenes de Servicio Pendientes',IIF(vTipPro=3,'Listado de Ordenes de Servicio Atendidas',IIF(vTipPro=4,'Listado de Ordenes de Servicio Afectadas',IIF(vTipPro=5,'Listado de Ordenes de Servicio Anuladas','Listado de Ordenes de Servicio Liquidadas')))))
   SET MEMOWIDTH TO 43
   IF !EOF()
     SET SKIP TO ITEOS1
        if vtippro # 6
         do case 
          case VLISTADO=1
    	 	DO REPORTE WITH 2,"LisOrdS1",' Ordenes de Servicio ',1,.F.,.T.
          case VLISTADO=2    	
    	 	DO REPORTE WITH 2,"LisMCUS",' Ordenes de Servicio ',1,.F.,.T.        
    	 *	DO REPORTE WITH 2,"LisOrsX1",' Ordenes de Servicio ',1,.F.,.T.        
          endcase
        else
         do case
           case VLISTADO=1 
    	 	DO REPORTE WITH 2,"LisOrdS",' Ordenes de Servicio ',1,.F.,.T.
           case VLISTADO=2 
    	 	DO REPORTE WITH 2,"LisOrsX",' Ordenes de Servicio ',1,.F.,.T.        
         ENDcase   
       endif
     SELECT ORDSE
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILT TO
   CLOSE INDEX
   ERASE (VIND)
ENDIF

RETURN
