** --------------------------------------
** INTOC1.PRG
** Integraci¢n por lotes de O/C para crear asientos de orden
** Fecha   : 15/01/97 
** Estados : 00 Pendiente de Cancelaci¢n
**           50 Cancelado
** ---------------------------------------

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens09
*Vmens01 = 'Í  Documentos  Varios ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
*Vmens02 = 'Documentos'
*Vmens08 = 'No hay registros para procesar'
*Vmens09 = '  Detalle  '
*xPos    = INT((80 - (LEN(ALLTRIM(cia)) + 2))/2)
*Vmens01 = SUBSTR(Vmens01, 1, xPos+1) + ' ' + cia + ' ' + SUBSTR(Vmens01,xPos + LEN(ALLTRIM(cia)) + 3,80)

CLOS DATA
USE Parmae    IN  4 ORDER TAG Parmae1                 ALIAS Parma
IF blo_sis(1)
   DO STANDBY WITH "Sistema Bloqueado, Consulte al Area de Sistemas"
   CLOS DATA
   RETURN 
ENDIF   

USE Compro	IN 1 ORDER TAG Compro7	ALIAS Comp
USE Diario	IN 2 ORDER TAG Diario11	ALIAS Diar
USE OrdCom	IN 3 ORDER TAG OrdCom1	ALIAS Orden
USE AstOrd  IN 7 ORDER TAG AstOrd1	ALIAS AstOrd


SELE Orden

*Agrega  = .T.
*vPatri  = .F.

*STORE 0 TO m.item, vLleno, vItem
*STORE SPACE(4) TO vNumCP

*- Mes y a¤o actual
SELECT parma

DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Fin_opcion                && finaliza

*RESTORE SCREEN FROM pta_comp
SHOW POPUP Menu
RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '  Integrando Ordenes de Compra ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
VMesOC   = IIF(FOUND(),MONTH(vmesact),MONTH(DATE()))
VAnoOC   = IIF(FOUND(),YEAR(vmesact), YEAR(DATE()))
@ 01,01 SAY "         Mes y A¤o : " 
@ 01,22 GET VMesOC  PICTURE '99' VALID VMesPES > 0 .AND. VMesPES <= 12
@ 01,27 GET vAnoOC  PICTURE '9,999'
@ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ
IF LASTKEY() = 27 OR OKCANCEL = 2
	CLOSE DATA
	RETURN
 ELSE
	VMesOC   = PADL(ALLTRIM(STR(VMESOC,2)),2,'0')
	VAnoOC   = PADL(ALLTRIM(STR(VanoOC,4)),4,'0')
	SELE Parma
	SEEK 'TIPDOC'+'ORC'      
	IF FOUND()
		vTip = ALLTRIM(Parmae.DescriAux)
		_tipo= '  O/C  '
*		wtipdcr=Val_para6('ORC','TIPDOC')
	ENDIF
	IF !HAYOC()
		SELE Comp
		SET ORDER TO tag Compro7
		SELE Diar
		SET ORDER TO tag Diario11
		DO Orden
	ENDIF
ENDIF
RETURN



PROCEDURE orden
*----------------
* Carga valores de las O/C
*-------------------------
SELECT Orden
SET FILTER TO MONTH(FecOC)=VAL(vMesOC) and periodo=vAnoOC AND estado<>'99'
SET RELATION TO Periodo+NumMes+NumOC+'O/C' INTO AstOrd ADDITIVE

GO TOP
IF EOF()
	DO standby WITH 'No existe documentos de O/C'
	RETURN .F.
 ELSE
	SCAN
		@ 5,1 SAY  '        Procesando : '+Orden.Periodo+'-'+Orden.Numoc+' '+Orden.Codfte
		m.Item = 0
		m.Periodo = Orden.Periodo
		m.Codfte  = Orden.CodFte
		m.fecEmi  = Orden.Fecoc
		m.TipDoc  = vTip
		m.Comprob = ALLTRIM(vTip)+PADL(Orden.Numoc,5,'0')+Fmes
		m.Glosa1  = 'O/C Emitida N§ '+OrdAu.Periodo+'.'+OrdAu.Numoc+' H/C N§: '+Orden.numhc+'.'+Orden.perhc
		m.Glosa2  = 'Desde fecha '+DTOC(m.Fecemi) +'  Fuente : '+m.codfte
		m.Glosa3  = Orden.docref+' Destino: '+ALLTRIM(Orden.Destino)
		m.Glosa4  = Orden.observa
		m.CodCad  = Orden.codcad
		m.TotDeb  = ROUND(Orden.valtot,2)
		m.TotHab  = ROUND(Orden.valtot,2)
		m.Periodofte = Orden.Periodo
		m.Numdocfte  = OrdAu.NUMoc
		m.Docref  = OrdAu.Numhc
		m.Tipdcr    = IIF(!EMPTY(OrdAu.NumHc),'03','  ')
		m.Nummesref = Orden.Perhc
		
		SELECT Comp
		IF f_appd()
			GATHER MEMVAR
		ENDIF
		***aqui
		*	k¤jhk¤jh¤j
		*	ï¤lkï¤lkï¤l
		********
		SELE AstOrd
		m.Item  = m.Item + 1
		m.Orden = '12'
		m.NumMov  = 1
		m.FecEmi  = vfecha,;
   		            TipAux     WITH '20',;
        	        Registro   WITH OrdAu.codprv,;
               		Montos     WITH ROUND(OrdAu.ValTot,2),;
  		            Coddep     WITH '111000',;
        	        codfte     WITH vcodfte,;
                    Docref     WITH OrdAu.Numhc,;
        	        Tipdcr     WITH IIF(!EMPTY(OrdAu.NumHc),'03','  '),;
               		Nummesref  WITH OrdAu.Perhc
       		IF l=1
       			REPLACE Diar.Tipo       WITH 'D'
           		REPLACE Diar.Cuenta     WITH '0104000000'
           	ELSE   
                REPLACE Diar.Tipo       WITH 'H'
  	            REPLACE Diar.Cuenta     WITH '0204000000'
         	ENDIF

		vItem = vItem + 1
		

***
	SELECT Diar
    FOR l=1 TO 2
   		APPEND BLANK
	ENDFOR  

***
		
		DO IntOrc
	ENDSCAN
ENDIF
RETURN

***
* Se busca el codigo del doc.
	SELE OrdAu
    vItem = vItem + 1
	SELECT Diar
    FOR l=1 TO 2
   		APPEND BLANK
	       	REPLACE Comprob    WITH ALLTRIM(wtipdcr)+'0'+OrdAu.NUMOc+Fmes,;
	       			Orden      WITH '12',;
       	           	NumMov     WITH 1,;
           		   	Item       WITH vItem,;
                    FecEmi     WITH vfecha,;
   		            TipAux     WITH '20',;
        	        Registro   WITH OrdAu.codprv,;
               		Montos     WITH ROUND(OrdAu.ValTot,2),;
  		            Coddep     WITH '111000',;
        	        codfte     WITH vcodfte,;
                    Docref     WITH OrdAu.Numhc,;
        	        Tipdcr     WITH IIF(!EMPTY(OrdAu.NumHc),'03','  '),;
               		Nummesref  WITH OrdAu.Perhc
       		IF l=1
       			REPLACE Diar.Tipo       WITH 'D'
           		REPLACE Diar.Cuenta     WITH '0104000000'
           	ELSE   
                REPLACE Diar.Tipo       WITH 'H'
  	            REPLACE Diar.Cuenta     WITH '0204000000'
         	ENDIF

		vItem = vItem + 1
	ENDFOR  
	SELE OrdAu
ENDIF

***


*** Integra ordenes una por una
*------------------------------
PROCEDURE IntOrc
*---------------
vPeriodo = Ordau.Periodo 
vNumoc   = Ordau.Numoc
SELE Ordau
@ 5,1 SAY  '        Procesando : '+Periodo+'-'+Numoc+' '+Codfte
* Se busca el codigo del doc.
IF Repro
    * Si no se encuentra  el NumRef
	*REPLACE Orden.Estado WITH '51'
    vPeriodo = Ordau.Periodo 
    vNumoc   = Ordau.Numoc
    vCodfte  = Ordau.CodFte
    vfecha   = Ordau.Fecoc  
    * Es solo un  temporal para Transpasar los datos de la o/c a los archivo de 
   	* Compro y Diario.
	wtipdcr=Val_para6('ORC','TIPDOC') 
	SELECT Comp
   	APPEND BLANK
		REPLACE  TipDoc WITH wtipdcr,;
               	 Comprob    WITH ALLTRIM(wtipdcr)+'0'+OrdAu.Numoc+Fmes,;
               	 Fecha      WITH vfecha,;
			 	 Glosa1  	WITH IIF(OrdAu.Estado="00",'O/C esta Emitida','O/C esta Atendida')+' '+OrdAu.Periodo+'.'+OrdAu.Numoc+' '+IIF(OrdAu.estado='00',' ','Numero H/C: '+Ordau.numhc+'.'+ordau.perhc),;
               	 Glosa2     WITH 'Desde '+DTOC(vfecha)+'  Hasta '+DTOC(vfecfin)+'  Fuente : '+vcodfte,;
               	 Glosa3     WITH IIF(!EMPTY(OrdAu.docref),OrdAu.docref,' ')+' Destino: '+ALLTRIM(OrdAu.Destino),;
               	 Glosa4     WITH OrdAu.observa,;
              	 CodCad     WITH OrdAu.codcad,;
              	 codfte     WITH vcodfte,;
              	 TotDeb		WITH ROUND(OrdAu.valtot,2),;
              	 TotHab		WITH ROUND(OrdAu.valtot,2),;
           	 	 Periodofte WITH OrdAu.Periodo,;
           		 Numdocfte  WITH OrdAu.NUMoc,;
             	 Docref     WITH OrdAu.Numhc,;
             	 Tipdcr     WITH IIF(!EMPTY(OrdAu.NumHc),'03','  '),;
  	         	 Nummesref  WITH OrdAu.Perhc
	SELE OrdAu
    vItem = vItem + 1
	SELECT Diar
    FOR l=1 TO 2
   		APPEND BLANK
	       	REPLACE Comprob    WITH ALLTRIM(wtipdcr)+'0'+OrdAu.NUMOc+Fmes,;
	       			Orden      WITH '12',;
       	           	NumMov     WITH 1,;
           		   	Item       WITH vItem,;
                    FecEmi     WITH vfecha,;
   		            TipAux     WITH '20',;
        	        Registro   WITH OrdAu.codprv,;
               		Montos     WITH ROUND(OrdAu.ValTot,2),;
  		            Coddep     WITH '111000',;
        	        codfte     WITH vcodfte,;
                    Docref     WITH OrdAu.Numhc,;
        	        Tipdcr     WITH IIF(!EMPTY(OrdAu.NumHc),'03','  '),;
               		Nummesref  WITH OrdAu.Perhc
       		IF l=1
       			REPLACE Diar.Tipo       WITH 'D'
           		REPLACE Diar.Cuenta     WITH '0104000000'
           	ELSE   
                REPLACE Diar.Tipo       WITH 'H'
  	            REPLACE Diar.Cuenta     WITH '0204000000'
         	ENDIF

		vItem = vItem + 1
	ENDFOR  
	SELE OrdAu
ENDIF
RETURN


FUNCTION val_para6
*----------------
PARAMETERS mvalor, filtro
SELECT parma
SEEK filtro+mvalor
IF FOUND()
   RETURN  parma.codigoaux 
ELSE
   RETURN  '  '    
ENDIF
RETURN 


PROCEDURE Fin_opcion
*-------------------
 DO Logos WITH Rotulo1,Rotulo2
 CLOSE DATA
RETURN
FUNCTION Aut_Fin


FUNCTION HAYOC
*-------------
SELE Comp
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo O/C
SEEK vtip+vMesOC+vAnoOC
mRet=.F.

IF FOUND()
	IF yesno("Ya se han procesado O/C. ¨Desea anularlas y reprocesar de nuevo?")
		DO ESPERA with 1,'Eliminando Asientos de O/C procesadas del mes ' + cMes(VAL(vMesOC))
		SELE Diar
		SET ORDER  TO tag Diario18
		SEEK vtip+vMesOC+vAnoOC
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=vMesOC AND STR(YEAR(Diar.fecemi),4)=vAnoOC
			SELECT Diar
			SET ORDER TO Diario11
		ENDIF
		
		SELE Comp
		IF !f_lock(3)
			UNLOCK
			RETURN .F.
		 ELSE
			DELETE FOR SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=vMesOC AND STR(YEAR(Comp.Fecha),4)=vAnoOC
			SELECT Comp
			SET ORDER TO Compro7
			UNLOCK
		ENDIF
		DO Espera WITH 2
		mRet=.F.
	 ELSE
		mRet=.T.
	ENDIF
ENDIF

SET ORDER TO Diario11 IN Diar
SET ORDER TO Compro7  IN Comp

RETURN mret

