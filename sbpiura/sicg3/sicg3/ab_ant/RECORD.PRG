
USE OrdCom   IN 1   order tag OrdCom1  ALIAS Orden
USE Iteoc    IN 2   order tag IteOc1   ALIAS IteOc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma

USE OrdSer   IN 7   order  tag Ordser1  ALIAS Ordser

USE Promae   IN 4   order tag Promae1  ALIAS Promae
USE maepre   IN 5   order tag maepre1  ALIAS maepre
USE RECORD   IN 6    ALIAS RECORD EXCLUSIVE

*- Mensajes de aviso al usuario

Vmens01 = ' Gu¡as de Internamiento : REVISION '
Vmens02 = ' Registro de Gu¡as de Internamiento '
Vmens04 = 'Dicho Gu¡as de Internamiento no fue encontrado'
Vmens05 = 'No existe Gu¡as de Internamiento anterior'
Vmens06 = 'No existe Gu¡as de Internamiento siguiente'
Vmens07 = '¨ Desea ANULAR ‚ste Gu¡as de Internamiento ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Gu¡as de Internamiento ha sido anulado'
Vmens10 = 'Este Gu¡as de Internamiento ya fue Contabilizado'
Vmens11 = 'Este Gu¡as de Internamiento ha sido devuelto'

DEFINE WINDOW LIS FROM 1,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Records' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1    TO vToMes,vTipPro,vListado,vTofue,vToPrv,vOrden
vPrv = SPAC(4)
vAno = '98'
vFte = '  '
vMes = '  '
vPer = '  '

@ 00,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ O/C;O/S' 

@ 04,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOFUE,06,22)  
@ 06,01 SAY "           Fuente : "
@ 06,22 GET vFte  PICTURE '!!' VALID VAL_PARA(vFte,'CODFTE','C') WHEN VTOFUE=2 

@ 08,01 SAY "Total Proveedores : " GET vToprv  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOprv,10,22) 
@ 10,01 SAY "        Proveedor : "
@ 10,22 GET vPrv  PICTURE '!!!!' VALID VALPrv() WHEN vToPRV=2 

@ 12,01 SAY "  Todos los Meses : " GET vtoMes   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOmes,14,22)  
@ 14,01 SAY "    Mes - Periodo : "
@ 14,22 GET vMes    PICTURE '!!' VALID VAL_PARA(vMes,'FECMES','C') WHEN VTOMES=2 
@ 14,26 GET vPer    PICTURE '!!' VALID !EMPTY(vPer)   && WHEN VTOMES=2 

@ 16,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados'

@ 20,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   IF VLISTADO = 1 
      SELE RECORD
      zap
      INDEX ON STR(IMPTOT,15)+STR(VECES,4)+NOMPRV TAG RECORD DESC
      SELECT ORDEN
	  INDEX ON CodPrv TO (vInd) ;
      FOR  IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',;
      			iif(vTipPro=3,Estado = '40',iif(vTipPro=4,Estado = '20',;
      			iif(vTipPro=5,Estado = '99',Estado = '50'))))) AND ;
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
          vCantol = vcantol + 1
          IF !EOF()
             SKIP
          ENDIF   
          vCa1 = CodPrv
        ENDD
        SKIP -1
        sele record
        append blank
        replace codprv with orden.codprv,nomprv with val_prv(orden.CodPrv),veces with vcantol,imptot with vcanreq
        SELE orden
      ENDSCAN
      DEACTIVATE WINDOW STANDBY
      SELECT RECORD
      GO TOP
      IF !EOF()
         vtitulo = iif(vtofue =1,'A TODA FUENTE',ALLTRIM(val_para(vfte,'CODFTE','D',22,60))) 
       	 DO REPORTE WITH 2,"RecOrdc",' Ordenes de Compra ',1,.F.,.T.
      ELSE
       DO STANDBY WITH VMENS08
     ENDIF
   ELSE
      SELE RECORD
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
        sele record
        append blank
        replace codprv with ordser.codprv,nomprv with val_prv(ordser.CodPrv),veces with vcantol,imptot with vcanreq
        SELE ordser
      ENDSCAN
      DEACTIVATE WINDOW STANDBY
      SELECT RECORD
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
