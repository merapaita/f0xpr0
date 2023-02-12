*** CUADRE DE INFORMACION CONTABLE
*
** DEL C/P AL ASTPAT
clear
set talk off
set echo off
*
USE ParMae   IN  1  ORDER TAG ParMae1      ALIAS ParMa 
USE ComPag   IN  3  ORDER TAG ComPag1      ALIAS ComPag 
USE Cajas    IN 10  ORDER TAG Cajas2       ALIAS Caja
USE IteCp    IN  4  ORDER TAG IteCp1       ALIAS IteCp  
*
*- Variables de trabajo (registro a trabajar)
PUBLIC m.CodCtc,w_tipctc,w_monto,xmri,xnri,vcodprg,vcodsub,vcodact,vcodanal,mmonto,sw_z,w_ctaD,mcta10,mcond,mctaf,mctai
PUBLIC vctadeb,vctahab,vvaldeb,vvalhab,vuser_id,wuser_id
STORE SPACE(1)  TO mcta10,mcond
STORE SPACE(5)  TO vcodanal
STORE SPACE(2)  TO vcodprg,xmri
STORE SPACE(3)  TO vcodsub,vcodact
STORE SPACE(4)  TO xnri
STORE SPACE(12) TO w_ctaD,mctai,vctadeb,vctahab
STORE SPACE(12) TO mctaf
STORE 0 	    TO w_monto,mmonto,sw_z,vvaldeb,vvalhab
SCATTER MEMVAR
*
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE Clase    IN  8  ORDER TAG Clase1       ALIAS Clase   
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT compag
*
** DEL C/P AL ASTPAT
*
mesw='09'
seek mesw
do while nummes=mesw and .not. eof()
	if estado='99' OR DELETE()
		skip
		loop
	endif		
	m.nummes=nummes
	m.numcp=numcp
	m.codctc=codctc
	m.tipdoc=tipdoc
	m.incpres=incpres
	m.fecref=fecrec
	m.feccp=feccp
	m.periodo=periodo
	sele itecp
	SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)
	if not !found()
		w_part=IteCp.CodPart
	endif		
	SELE Caja
	SEEK m.codctc
	w_tipctc=Caja.Tipo
	sele astpat
	SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
	IF !FOUND()
		DO agriaut				&&&&&& ok.
	ELSE
		store 0 to vdebe,vhaber
		SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=ALLTRIM(codctc)
        	DO CASE
	          CASE SUBS(codcta,1,3)='104' OR SUBS(codcta,1,3)='108' .OR. SUBS(CODCTA,1,2)='44'
         		mmonto=Mtohab
    	      CASE SUBS(codcta,1,3)='102'
           		m.flagcon='F'
        	  CASE SUBS(codcta,1,3)='384'
           		m.flagcon='A'
           	  CASE SUBS(codcta,1,3)='385'
           		m.flagcon='E'
			ENDCASE
		ENDSCAN
		* si es corrije solo modifica
		sw=0
		SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
		IF FOUND()
			SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=ALLTRIM(codctc)
			    IF codcta='101010101000'
			    	sw=sw+1
		        	REPLA Mtodeb WITH mmonto, Fecha WITH m.feccp
		        	REPLA Mtohab WITH mmonto, Fecha WITH m.feccp
		        	tipw=tipcta
		        ENDIF
			ENDSCAN
	    endif
	    DO CASE 
    		CASE SW=1
	    	  if tipw='D'
				IF f_appd()
					REPLACE nummes WITH m.nummes,;
			        numref WITH m.numcp,;
			        tipdoc WITH 'C/P',;
			        ret    WITH 'N',;
			        codctc WITH m.codctc,;
			        fecha  WITH m.FecCP,;
			        FecHC  WITH m.FecRef,;
			        tipctc WITH w_Tipctc,;
			        codcta WITH '101010101000',;
			        Mtohab WITH mmonto,;
			        Tipcta WITH 'H',;
			        periodo WITH m.periodo
				ENDIF
			  ELSE
				IF f_appd()
					REPLACE nummes WITH m.nummes,;
			        numref WITH m.numcp,;
			        tipdoc WITH 'C/P',;
			        ret    WITH 'N',;
			        codctc WITH m.codctc,;
			        fecha  WITH m.FecCP,;
			        FecHC  WITH m.FecRef,;
			        tipctc WITH w_Tipctc,;
			        codcta WITH '101010101000',;
			        Mtodeb WITH mmonto,;
			        Tipcta WITH 'D',;
			        periodo WITH m.periodo
				ENDIF
			  ENDIF				
			CASE SW=0
				IF f_appd()
					REPLACE nummes WITH m.nummes,;
			        numref WITH m.numcp,;
			        tipdoc WITH 'C/P',;
			        ret    WITH 'N',;
			        codctc WITH m.codctc,;
			        fecha  WITH m.FecCP,;
			        FecHC  WITH m.FecRef,;
			        tipctc WITH w_Tipctc,;
			        codcta WITH '101010101000',;
			        Mtodeb WITH mmonto,;
			        Tipcta WITH 'D',;
			        periodo WITH m.periodo
				ENDIF
				IF f_appd()
					REPLACE nummes WITH m.nummes,;
			        numref WITH m.numcp,;
			        tipdoc WITH 'C/P',;
			        ret    WITH 'N',;
			        codctc WITH m.codctc,;
			        fecha  WITH m.FecCP,;
			        FecHC  WITH m.FecRef,;
			        tipctc WITH w_Tipctc,;
			        codcta WITH '101010101000',;
			        Mtohab WITH mmonto,;
			        Tipcta WITH 'H',;
			        periodo WITH m.periodo
				ENDIF
		ENDCASE
	ENDIF
	SELE COMPAG
	SKIP
ENDDO
*RETURN
**
*** DEL ASTPAT AL C/P		
*
SELE ASTPAT
seek mesw
do while nummes=mesw and .not. eof()
	if DELETE()
		skip
		loop
	endif		
	m.nummes=NUMMES
	m.numREF=NUMREF
	m.codctc=CODCTC
	SELE COMPAG
	SEEK ALLTRIM(m.nummes)+m.numREF+ALLTRIM(m.codctc)
	IF !FOUND()
		@ 05,10 SAY M.NUMMES
		@ 05,13 SAY M.NUMREF
		@ 05,20 SAY M.CODCTC
		WAIT ' '
	ENDIF
	SELE ASTPAT
	SKIP
ENDDO
RETURN	
******
				
			
			
			
	ENDIF	
	DO vis_cen
ENDIF	
USE IN 09 
USE IN 14
ON KEY
m.Reten = vret
m.PartRet = vvpartret
IF !EMPTY(m.reten)
	ACTIVATE WINDOW wind_12
	@0,1 SAY "Partida" GET m.partret PICTURE '!!!!!!'
    @0,30 SAY "Reten. "+STR(m.reten,10,2)
	READ
ENDIF	
DEACTIVATE WINDOW wind_12
UNLOCK
RETURN

FUNCTION Asig
*------------
vMonDeb=mtodeb
RETURN
PROCEDURE agrite     &&
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH m.nummes,;
	        numref WITH m.numcp,;
	        tipdoc WITH 'C/P',;
	        ret    WITH 'N',;
	        codctc WITH m.codctc,;
	        fecha  WITH m.FecCP,;
	        FecHC  WITH m.FecReF ,;
	        tipctc WITH w_tipctc,;
	        periodo WITH m.periodo
ENDIF
RETURN

PROCEDURE agriaut    && ok.
*----------------
DO CASE
   CASE m.tipdoc$'SE'    &&'SHSESS'
	   private a1,a2,a3,a4,b1,b4
	   STORE SPACE(3) TO a1,a4,b1,b4
	   STORE SPACE(2) TO a2,a3
	   SELE Clase
	   SEEK LEFT(ALLT(w_part),2)+RIGHT(ALLT(w_part),2)
	   *
	   w_ctad=Clase.CuentaD
	   a1=LEFT(w_ctad,3)
	   a4=RIGHT(w_ctad,3)
*	   a2=LEFT(ALLT(vcodprg),2)
*	   a3=SUBST(ALLT(vcodsub),2,2)
	   a2='01'
	   a3='01'
	   b1=LEFT(w_ctah,3)
	   b4=RIGHT(w_ctah,3)
	   IF a3#'06' AND a3#'10'
		   w_ctad='302'+a2+a3+a4
	       w_ctah=b1+a2+a3+b4
	   ENDIF	

   CASE m.tipdoc$'SUSS'       &&'HCRGMESUHM'
   
   		** AGREGAR ASIENTO PARA SUBSIDIOS ...............
 		w_ctad='411010100000'  		

   CASE m.tipdoc$'HCRGMEHMSHAH'       &&'HCRGMESUHM'
   
	    x1=LEFT(ALLTRIM(w_part),1)
	    x2=RIGHT(ALLTRIM(w_part),2)
	    IF x1='6'
	    	w_ctad='424010100000'   
	    ELSE
	    	SELE PARMA
	    	SEEK 'ESPGAS'+x2
	    	w_ctad=LEFT(descriaux,12)
	    	IF x2='SU'
	    		w_ctad='419010100000'
	    	ENDIF
	    ENDIF
   CASE m.tipdoc$'RERU'	     	    
		SELE ParMa
		SEEK 'CODRET'+ALLT(m.codret)
		w_ctaD=ALLT(Parma.descriaux)
   CASE m.tipdoc='SRRSAR'
	   private a1,a2,a3,a4,b1,b4
	   STORE SPACE(3) TO a1,a4,b1,b4
	   STORE SPACE(2) TO a2,a3
  	   SELE ParMa
	   SEEK 'CODRET'+ALLT(m.codret)
	   w_ctaD=allt(ParMa.descriaux)
	   a1=LEFT(w_ctad,3)
	   a4=RIGHT(w_ctad,3)
*	   a2=LEFT(ALLT(vcodprg),2)
*	   a3=SUBST(ALLT(vcodsub),2,2)
	   a2='01'
	   a3='01'
	   b1=LEFT(w_ctah,3)
	   b4=RIGHT(w_ctah,3)
	   IF a3#'06' AND a3#'10'
		   w_ctad='302'+a2+a3+a4
	       w_ctah=b1+a2+a3+b4
	   ENDIF	
ENDCASE		
SELECT astpat
IF f_appd()
   REPLACE nummes  WITH m.nummes,;
	       numref  WITH m.numcp,;
	       tipdoc  WITH 'C/P',;
	       ret     WITH 'N',;
	       codctc  WITH m.codctc,;
	       fecha   WITH m.FecCP,;
	       FecHC   WITH m.FecReF ,;
	       tipctc  WITH w_tipctc,;
	       periodo WITH m.periodo,;
	       tipcta  WITH 'D',;
	       Codcta  WITH w_ctad,;
	       mtodeb  WITH m.import
ENDIF
IF f_appd()
	REPLACE nummes  WITH m.nummes,;
	        numref  WITH m.numcp,;
	        tipdoc  WITH 'C/P',;
	        ret     WITH 'N',;
	        codctc  WITH m.codctc,;
	        fecha   WITH m.FecCP,;
	        FecHC   WITH m.FecReF ,;
	        Tipctc  WITH w_tipctc,;
	        periodo WITH m.periodo,;
	        tipcta  WITH 'H',;
	        Codcta  WITH w_ctaH,;
	        mtohab  WITH m.import
ENDIF
IF m.IncPres='S'
		IF f_appd()
			REPLACE nummes WITH m.nummes,;
		        numref WITH m.numcp,;
		        tipdoc WITH 'C/P',;
		        ret    WITH 'N',;
		        codctc WITH m.codctc,;
		        fecha  WITH m.FecCP,;
		        FecHC  WITH m.FecRef,;
		        tipctc WITH w_Tipctc,;
				codcta WITH '101010101000',;
				Mtodeb WITH mmonto,;
				Tipcta WITH 'D', periodo WITH m.periodo
		ENDIF
		IF f_appd()
			REPLACE nummes WITH m.nummes,;
		        numref WITH m.numcp,;
		        tipdoc WITH 'C/P',;
		        ret    WITH 'N',;
		        codctc WITH m.codctc,;
		        fecha  WITH m.FecCP,;
		        FecHC  WITH m.FecRef,;
		        tipctc WITH w_Tipctc,;
		        codcta WITH '101010101000',;
		        Mtohab WITH mmonto,;
		        Tipcta WITH 'H',;
		        periodo WITH m.periodo
		ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT astpat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN
PROCEDURE verap   && F5
*--------------
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+m.codctc
IF !FOUND()
	DO standby WITH 'No tiene detalle'
ELSE
	BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY ALLTRIM(m.nummes)+m.numcp   TIMEOUT 0.001 ;
		WINDOW wind_2 ;
		FIELDS;
		codcta :H='CtaCte',;
		tipcta :H='Tp',;
		mtodeb :H='Monto Debe' :p='999,999,999.99',;
		mtohab :H='Monto Haber'  :p='999,999,999.99',;
		ret    :H='Ret?' :p='!'
ENDIF
USE IN 09
SELECT ComPag
RETURN
PROCEDURE Compre
*---------------
private as
AS=ALIAS()
m.valdeb = vtotal-m.reten
m.valhab = vtotal-m.reten

m.CtaDeb = IIF(m.Codfte='00','951010100000',IIF(m.Codfte='01','952010100000','957010100000'))
m.CtaHab = IIF(m.Codfte='00','971010100000',IIF(m.Codfte='01','972010100000','977010100000'))

IF m.tipdoc$'SE'
   private a1,a2,a3,a4,b1,b4
   STORE SPACE(3) TO a1,a4,b1,b4
   STORE SPACE(2) TO a2,a3
   a1=LEFT(m.ctadeb,3)
   a4=RIGHT(m.ctadeb,3)
   a2=LEFT(ALLT(vcodprg),2)
   a3=SUBST(ALLT(vcodsub),2,2)
   b1=LEFT(m.ctahab,3)
   b4=RIGHT(m.ctahab,3)
   IF a3#'06' AND a3#'10'
	   m.ctadeb=a1+a2+a3+a4
       m.ctahab=b1+a2+a3+b4
   ENDIF	
ENDIF   
USE astpre   IN 13  ORDER TAG astpre2      ALIAS astpre  
SELECT astpre
SEEK 'D'+ALLTRIM(m.nummes)+ALLTRIM(m.numcp)+ALLTRIM(M.CTADEB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH m.ctadeb,;
		        ctadeb  WITH m.ctadeb,;
		        tipo    WITH 'D',;
		        fecref  WITH m.fecCP,;
		        CodPart WITH m.CodPart,;
		        CodCtc  WITH m.CodCtc;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
	    	    valdeb WITH m.valdeb ,;
		        ctadeb WITH m.ctadeb ,;
		        ctahab WITH SPACE(12),;
	        	valhab WITH 0 
	ENDIF
	UNLOCK
ELSE
	IF RLOCK()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH m.ctadeb,;
		        ctadeb  WITH m.ctadeb,;
		        tipo    WITH 'D',;
		        fecref  WITH m.fecCP,;
		        CodPart WITH m.CodPart,;
		        CodCtc  WITH m.CodCtc;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
	    	    valdeb WITH m.valdeb ,;
		        ctahab WITH SPACE(12),;
	        	valhab WITH 0 
	ENDIF
	UNLOCK
ENDIF
	
SEEK 'H'+ALLTRIM(m.nummes)+ALLTRIM(m.numcp)+ALLTRIM(M.CTAHAB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH m.ctahab,;
		        ctahab  WITH m.ctahab,;
		        tipo    WITH 'H',;
		        fecRef  WITH m.fecCP,;
		        CodPart WITH m.CodPart,;
		        CodCtc  WITH m.CodCtc,;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
			    valdeb WITH 0 ,;
			    valhab WITH m.valhab,;
		        ctadeb WITH SPACE(12)
	ENDIF
	UNLOCK
ELSE
	IF RLOCK()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH m.ctahab,;
		        ctahab  WITH m.ctahab,;
		        tipo    WITH 'H',;
		        fecRef  WITH m.fecCP,;
		        CodPart WITH m.CodPart,;
		        CodCtc  WITH m.CodCtc,;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
			    valdeb WITH 0 ,;
			    valhab WITH m.valhab,;
		        ctadeb WITH SPACE(12)
	ENDIF
	UNLOCK
ENDIF	
USE IN 13
ACTIVATE WINDOW wind_6
@ 01,04  SAY m.ctadeb PICTURE '!!!!!!!!!!!!' 
@ 01,18  SAY m.valdeb PICTURE '999,999,999.99' 
@ 02,12  SAY m.ctahab PICTURE '!!!!!!!!!!!!' 
@ 02,34  SAY m.valhab PICTURE '999,999,999.99' 
WAIT ' '
DEACTIVATE WINDOW wind_6
vCtadeb = m.ctadeb
vValdeb = m.Valdeb
vCtahab = m.ctahab
vValhab = m.valhab
SELE (as)
RETURN









**** RECIBOS DE INGRESOS

  SELE Caja
  SEEK ALLTRiM(m.CodCtc)
  IF FOUND()
  w_TipCtc=Caja.tipo
  wbanco = Caja.banco
  wclase = Caja.Clase
  vdeb   = Caja.CuentaH
  ENDIF
  SELE Parma
  SEEK 'TIPRI '+m.tipri
  w_ctaH=LEFT(ALLT(parma.descriaux),12)
  w_clas=IIF(LEFT(ALLT(parma.descriau2),1)='S',SUBSTR(ALLT(parma.descriau2),4,9),SPACE(9))
  w_asip=LEFT(ALLT(parma.descriau2),1)
  SELE Ri
  IF LASTKEY()#27
    vAnula = .F.
    SELECT Ri
    IF F_Appd()
       GATHER MEMVAR
       UNLOCK ALL
       SELECT Parma
       SEEK 'CORREL'+'RECING'
       REPLACE NumEnt WITH NumEnt + 1       
       SELECT Ri
       IF ALLTRIM(wclase)='T'
	   	  SELECT Parma
	      SEEK "CORREL"+"REVERS"
		  vNUMERO=PADL(NUMENT+1,4,'0')
          DO Agreg1
       ELSE
	   	  SELECT Parma
	      SEEK "CORREL"+"MOVBCO"
		  vNUMERO=PADL(NUMENT+1,4,'0')
          DO Agreg_item
       ENDIF
       SELECT IteRi
	  IF ALLT(m.tipri)='02'  &&Venta de Bases
         DO concurso
      ENDIF   	     
	  IF ALLT(m.tipri)='73'  &&Inscripciones eventos
         DO inscribe
      ENDIF   	     
       =Observa()
       SCATTER MEMVAR
       DO WHILE .T.
          SELECT IteRi
          DO IngAp
          IF ALLT(wclase)='T'  &&Cta Unica Tesoro P즒lico
          	 DO CASE
          	    CASE ALLT(m.tipri)='03'
          	         wtiprev='03'
          	    CASE ALLT(m.tipri)='12'
          	         wtiprev='01'
          	    CASE ALLT(m.tipri)='15'
          	         wtiprev='02'
          	    CASE ALLT(m.tipri)='17'
          	         wtiprev='04'
          	    CASE ALLT(m.tipri)='29'
          	         wtiprev='06'
		       	OTHER
          	         wtiprev='07'
          	ENDCASE    
   		    wAstpat = pidaprev()
		    DO Tra_hijo
	        DO IngAp2 WITH Wastpat
		  ELSE	          	    
   		  	vAstpat = pidapbd()
		    DO Trabaja_Hijo
   		    DO IngAp1 WITH vastpat
   		  ENDIF  
		  IF ALLT(w_asip)='S'
			 DO compre    
		  ENDIF   
		  IF ALLT(m.tipri)='02' .OR. ALLT(m.tipri)='73'   &&Venta de Bases/Inscripciones eventos
		     DO factura WITH ALLT(m.tipri),'1'
		  ENDIF
          IF LASTKEY() = 27
             IF YESNO( '� Cancela el Ingreso ?' )
                vAnula = .T.
                EXIT
             ENDIF
          ELSE
             IF YESNO( '� Est쟮 correctos los datos ?')
                EXIT
                =Valida()
             ENDIF
          ENDIF
       ENDDO
       IF vAnula
          DO Anula
       ENDIF
    ELSE
       GO BOTTOM
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT Ri
 *GO BOTT
 ON KEY LABEL F2 DO VisObs
 ON KEY LABEL f4 DO imp_Ri
 DO Vista
 RETURN

PROCEDURE Agreg_Item
*-------------------
SELECT Parma
SEEK "CORREL"+"MOVBCO"
vNUMERO=PADL(NUMENT+1,4,'0')
SELECT IteRi
IF F_Appd()
   REPLACE Periodo WITH m.Periodo,;
   		   NumRi   WITH m.NumRi ,;
   		   NumMes  WITH m.NumMes,;
   		   ImpParc WITH 0,;
   		   Estado  WITH m.Estado,;
   		   Fecdep  WITH m.fecRi,;
   		   docref  WITH vnumero,;
   		   tipo    WITH IIF(EMPTY(w_clas),'O','P'),;
   		   codpart WITH w_clas,;
   		   impparc WITH m.canri
   SELECT Parma
   SEEK "CORREL"+"MOVBCO"
   REPLACE NUMENT WITH NUMENT+1
   SELE ITERI
ENDIF
RETURN



PROCEDURE Elimi_Item
*-------------------
SELECT IteRi
if rlock()
   SELECT IteRi
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif

return
PROCEDURE ingap   && Ingresa asientos patrimoniales
*--------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '같같같같F5->Agregar같같같같같같같F8->Eliminar같같같같같같같F10->Terminar같같같같'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgRite
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT astpat
SEEK m.Periodo+ALLTRiM(m.nummes)+m.numRi+m.CodCtc
IF !FOUND()
	DO agRiaut
ENDIF

******* 
IF m.numri <='0663'
	SEEK m.Periodo+ALLTRiM(m.nummes)+m.numRi+m.CodCtc
	SCAN WHILE PERiODO=m.Periodo AND ALLTRiM(m.nummes)=nummes AND m.numRi=numRi AND m.CodCtc=codctc 
    	dele next 1
	ENDSCAN
	DO agriaut
ENDIF

*******

BROWSE FIELDS ;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+DescRi",codcta,2):F ,;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999.99';
	WINDOW wind_2 KEY m.Periodo+ALLTRiM(m.NumMes)+m.NumRi+m.CodCtc
STORE 0 TO vdebe, vhaber ,vRet
SEEK m.Periodo+ALLTRiM(m.nummes)+m.numRi+m.CodCtc
SCAN WHILE PERiODO=m.Periodo AND ALLTRiM(m.nummes)=nummes AND m.numRi=numRi AND m.CodCtc=codctc 
    IF IIF(TipCta='D',MtoDeb,MtoHab)=0
    	dele next 1
    else
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
    ENDIF		
ENDSCAN
IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
ON KEY
SELECT Ri
ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5
UNLOCK
ACTIVATE SCREEN
vTempo = '같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
RETURN


PROCEDURE agRite
*---------------
SELECT astpat
IF f_appd()
	REPLACE peRiodo WITH m.Periodo,;
	     	nummes WITH m.nummes,;
	        numref WITH m.numRi,;
	        tipdoc WITH 'R/I',;
	        codctc WITH m.CodCtc,;
	        fecha  WITH m.FecRi,;
	        TipCtc WITH w_TipCtc
ENDIF
RETURN

PROCEDURE agRiaut
*---------------
SELECT astpat
IF f_appd()
	REPLACE peRiodo WITH m.Periodo,;
	     	nummes WITH m.nummes,;
	        numref WITH m.numRi,;
	        tipdoc WITH 'R/I',;
	        codctc WITH m.CodCtc,;
	        fecha  WITH m.FecRi,;
	        tipctc WITH w_TipCtc,;
	        tipcta  WITH 'D',;
	        Codcta  WITH w_ctad,;
	        mtodeb  WITH m.canri
ENDIF
IF f_appd()
	REPLACE peRiodo WITH m.Periodo,;
	     	nummes WITH m.nummes,;
	        numref WITH m.numRi,;
	        tipdoc WITH 'R/I',;
	        codctc WITH m.CodCtc,;
	        fecha  WITH m.FecRi,;
	        tipctc WITH w_TipCtc,;
	        tipcta  WITH 'H',;
	        Codcta  WITH w_ctaH,;
	        mtoHab  WITH m.canri
ENDIF
RETURN


PROCEDURE EliIte
*---------------
SELECT AstPat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN
PROCEDURE ingap1   && Ingresa asientos patrimoniales para boletas de dep쥀ito
*-------------------
PARAMETERS vasiento
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '같같같같F5->Agregar같같같같같같같F8->Eliminar같같같같같같같F10->Terminar같같같같'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgRite
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)

SELE ASTPAT
xord = ORDER()
SET ORDER TO ASTPAT7

SELECT IteRi
SEEK m.Periodo+ALLTRiM(m.NumMes) + ALLTRiM(m.NumRi)
vpeRiodo = m.Periodo
vnummes  = m.nummes
vnumRi   = m.numRi
SCAN WHILE vPERiODO+ALLTRiM(vNumMes) + ALLTRiM(vNumRi) =PERiODO+ALLTRiM(NumMes) + ALLTRiM(NumRi)
	SELECT Movbco
	SEEK ALLT(IteRi.nummes)+ALLTRIM(IteRi.docRef)
	IF !FOUND()
		APPEND BLANK
		REPLACE PeRiodo 	WITH vpeRiodo ,;
				Nummes  	WITH vnummes ,;
				NummesC 	WITH vnummes ,;
				numero  	WITH Iteri.docRef ,;
				Transa  	WITH 'ABO' ,;
				Fecha   	WITH Iteri.fecdep,;
				FechaC  	WITH iteri.fecdep,;
				Tipdoc  	WITH 'B/D' ,;
				Numref  	WITH IteRi.boldep ,;
				codfte  	WITH m.codfte ,;
				codctc  	WITH m.CodCtc ,;
				nombre  	WITH 'DEPOSITO AUTOMATICO R/I '+m.numRi+'.'+m.nummes ,;
				forma   	WITH 'EF' ,;
				Monto   	WITH IteRi.impparc ,;
				codcad  	WITH m.codcad ,;
				perRi   	WITH IteRi.peRiodo,;
				nummesRi    WITH IteRi.nummes ,;
				numRi   	WITH IteRi.numRi ,;
				estado      WITH '00' ,;
				motivo      WITH '19',;				
				codban   	WITH wbanco,;
				usuario     WITH wuser_id
				
	ELSE
		REPLACE PeRiodo 	WITH IteRi.peRiodo ,;
				Fecha   	WITH iteri.fecdep ,;
				FechaC  	WITH iteri.fecdep,;
				Numref  	WITH IteRi.boldep ,;
				codfte  	WITH m.codfte ,;
				codctc  	WITH m.CodCtc ,;
				nombre  	WITH 'DEPOSITO AUTOMATICO R/I '+m.numRi+'.'+m.nummes ,;
				Monto   	WITH IteRi.impparc ,;
				codcad  	WITH m.codcad,;
				perRi   	WITH IteRi.peRiodo,;
				nummesRi    WITH IteRi.nummes ,;
				numRi   	WITH IteRi.numRi ,;
				estado      WITH '00' ,;
				motivo      WITH '19',;				
				Codban      WITH wbanco,;
				usuario     WITH wuser_id
	ENDIF		
	SELECT Astpat
	SEEK ALLT(IteRi.Nummes)+ALLTRIM(Iteri.DocRef)+m.CodCtc
	IF !FOUND()
		APPEND BLANK
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'B/D' ,; 
				fecha   WITH iteri.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH IteRi.DocRef;
				tipcta  WITH 'D',;
				codctc  WITH m.CodCtc ,;
				codcta  WITH LEFT(vasiento,12),;
				Mtodeb  WITH IteRi.impparc ,;
				tipctc  WITH w_tipctc
		APPEND BLANK
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'B/D' ,; 
				fecha   WITH iteri.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH IteRi.DocRef ,;
				tipcta  WITH 'H',;
				codctc  WITH m.CodCtc ,;
				codcta  WITH RiGHT(vasiento,12),;
				Mtohab  WITH IteRi.impparc ,;
				tipctc  WITH w_tipctc
	ELSE
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'B/D' ,; 
				fecha   WITH iteri.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH IteRi.DocRef,;
				tipcta  WITH 'D',;
				codctc  WITH m.CodCtc ,;
				codcta  WITH LEFT(vasiento,12),;
				Mtodeb  WITH IteRi.impparc ,;
				tipctc  WITH w_tipctc
		SKIP
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'B/D' ,; 
				fecha   WITH iteri.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH IteRi.DocRef,;
				tipcta  WITH 'H',;
				codctc  WITH m.CodCtc ,;
				codcta  WITH RiGHT(vasiento,12),;
				Mtohab  WITH IteRi.impparc ,;
				tipctc  WITH w_tipctc
	ENDIF
	SELECT IteRi
ENDSCAN	
SELECT Astpat
SET ORDER TO (xord)
SELE RI
RETURN
PROCEDURE ingap2   && Ingresa asientos patRimoniales para Reversiones
*-------------------
PARAMETERS vasiento
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '같같같같F5->Agregar같같같같같같같F8->Eliminar같같같같같같같F10->Terminar같같같같'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgRite
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)

SELECT Astpat
xord = ORDER()
SET ORDER TO Astpat15

SELECT IteRi
SEEK m.Periodo+ALLTRiM(m.NumMes) + ALLTRiM(m.NumRi)
vpeRiodo = m.Periodo
vnummes  = m.nummes
vnumRi   = m.numRi
SCAN WHILE vPERiODO+ALLTRiM(vNumMes) + ALLTRiM(vNumRi) =PERiODO+ALLTRiM(NumMes) + ALLTRiM(NumRi)
	SELECT Rever
	SEEK ALLT(IteRi.nummes)+Allt(Iteri.Docref)
	IF !FOUND()
		APPEND BLANK
		REPLACE PeRiodo 	WITH vpeRiodo ,;
				Nummes  	WITH vnummes ,;
				numRev  	WITH allt(Iteri.DocRef) ,;
				Fecha   	WITH IteRi.fecdep ,;
				Tipo    	WITH 'M' ,;
				TipRev  	WITH wtiprev ,;
				Glosa   	WITH 'DEPOSITO R/I '+m.numRi ,;
				ctates  	WITH m.CodCtc ,;
				Monto   	WITH IteRi.impparc ,;
				Tipdoc  	WITH 'R/I',;
				perdoc  	WITH IteRi.peRiodo,;
				mesdoc      WITH IteRi.nummes ,;
				numdoc  	WITH IteRi.numRi ,;
				estado      WITH '00' ,;
				Codctc      WITH vctc,;
				Codscta     WITH vscta,;
				concepto    WITH '   RUBRO A: REVERSIONES POR MENORES GASTOS.-             '+Ri.observ,;
				flag        WITH 'I',;
				codofg      WITH vofig,;
				usuario     WITH wuser_id
	ELSE
		REPLACE PeRiodo 	WITH vpeRiodo ,;
				Fecha   	WITH IteRi.fecdep ,;
				Tipo    	WITH 'M' ,;
				TipRev  	WITH wtiprev ,;
				Glosa   	WITH 'DEPOSITO R/I '+m.numRi ,;
				ctates  	WITH m.CodCtc ,;
				Monto   	WITH IteRi.impparc ,;
				Tipdoc  	WITH 'R/I',;
				perdoc  	WITH IteRi.peRiodo,;
				mesdoc      WITH IteRi.nummes ,;
				numdoc  	WITH IteRi.numRi ,;
				estado      WITH '00' ,;
				codscta     WITH vscta,;
				Codctc      WITH vctc,;
				concepto    WITH '   RUBRO A: REVERSIONES POR MENORES GASTOS.-             '+Ri.observ,;
				flag        WITH 'I',;
				codofg      WITH vofig,;
				usuario     WITH wuser_id
	ENDIF		
	=concepto()
	SELECT Astpat
	SEEK ALLT(IteRi.Nummes)+Iteri.DocRef+vctc
	IF !FOUND()
		APPEND BLANK
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'REV' ,; 
				fecha   WITH IteRi.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH Iteri.Docref ,;
				tipcta  WITH 'D',;
				codctc  WITH vctc ,;
				codcta  WITH LEFT(vasiento,12),;
				Mtodeb  WITH IteRi.impparc ,;
				tipctc  WITH w_tipctc,;
				numdoc  WITH vscta,;
				doc     WITH vofig
		APPEND BLANK
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'REV' ,; 
				fecha   WITH IteRi.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH Iteri.DocRef ,;
				tipcta  WITH 'H',;
				codctc  WITH vctc ,;
				codcta  WITH RiGHT(vasiento,12),;
				Mtohab  WITH IteRi.impparc ,;
				tipctc  WITH w_tipctc,;
				numdoc  WITH vscta,;
				doc     WITH vofig
	ELSE
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'REV' ,; 
				fecha   WITH IteRi.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH Iteri.Docref ,;
				tipcta  WITH 'D',;
				codctc  WITH vctc ,;
				codcta  WITH LEFT(vasiento,12),;
				Mtodeb  WITH IteRi.impparc ,;
				tipctc  WITH w_tipctc,;
				numdoc  WITH vscta,;
				doc     WITH vofig
		SKIP
		REPLACE PeRiodo WITH IteRi.peRiodo ,;
				tipdoc  WITH 'REV' ,; 
				fecha   WITH IteRi.fecdep ,;
				Nummes  WITH IteRi.Nummes ,;
				Numref  WITH Iteri.DocRef ,;
				tipcta  WITH 'H',;
				codctc  WITH vctc ,;
				codcta  WITH RiGHT(vasiento,12),;
				Mtohab  WITH IteRi.impparc,;
				tipctc  WITH w_tipctc,;
				numdoc  WITH vscta,;
				doc     WITH vofig
	ENDIF
	SELECT IteRi
ENDSCAN	
SELECT Astpat
SET ORDER TO (xord)
SELE RI
RETURN








***** MOVIMIENTO BANCARIO


PROCEDURE ingap   && F5
*--------------
DO CASE
   CASE  m.TipDoc='N/C'
		 USE AstPat   IN  6 ORDER TAG AstPat5   ALIAS AstPat
   CASE m.TipDoc='N/A'
		 USE AstPat   IN  6 ORDER TAG AstPat6   ALIAS AstPat
   CASE m.TipDoc='NOC'
		 USE AstPat   IN  6 ORDER TAG AstPat9  ALIAS AstPat
   CASE m.TipDoc='B/D'
         USE AstPat   IN  6 ORDER TAG AstPat7   ALIAS AstPat
ENDCASE         
SELE ASTPAT
SEEK ALLTRIM(m.NUMMESc)+ALLTRIM(m.numero)+ALLTRIM(m.CodCtc)
IF !FOUND()
	DO agrautbd
ENDIF
ON KEY LABEL F5 DO AgrIte
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummesc)+m.numero+alltrim(M.CODCTC) FIELDS;
	codcta :H='Cuenta',;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :p='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99'
STORE 0 TO vdebe, vhaber ,vRet
SEEK ALLTRIM(m.NUMMESc)+m.numero+ALLTRIM(m.CodCtc)
mmonto=Mtohab+mtodeb
SCAN WHILE ALLTRIM(m.NUMMESc)=NumMes AND m.numero=NumREF AND m.CodCtc=ALLTRIM(CodCtc)
    IF IIF(TipCta='D',MtoDeb,MtoHab)=0
    	DELE NEXT 1
    	MMONTO = 0
    ELSE
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
    ENDIF
ENDSCAN
IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
USE IN 6
SELECT Movb
sw_z=0
UNLOCK
RETURN

PROCEDURE agrautbd   &&
*-----------------
SELECT astpat
IF mcta10='S'
	IF f_appd()
		REPLACE nummes  WITH m.nummesC,;
		        numref  WITH m.numero,;
		        Periodo WITH m.periodo,;
	    	    tipdoc  WITH m.TipDoc,;
		        codctc  WITH m.codctc,;
		        fecha   WITH m.FechaC,;
				Tipctc  WITH w_tipctc,;
		        tipcta  WITH 'D',;
		        Codcta  WITH IIF(m.transa='ABO',w_ctad,'101010101000'),;
		        mtodeb  WITH m.monto
	ENDIF
	IF f_appd()
		REPLACE nummes  WITH m.nummesC,;
		        numref  WITH m.numero,;
		        Periodo WITH m.periodo,;
		        tipdoc  WITH m.TipDoc,;
		        codctc  WITH m.codctc,;
		        fecha   WITH m.FechaC,;
				Tipctc  WITH w_tipctc,;
		        tipcta  WITH 'H',;
		        Codcta  WITH IIF(m.transa='ABO','101010101000',w_ctad),;
		        mtoHab  WITH m.monto
	ENDIF
ELSE
  	IF  m.asiento=4 AND mcond='U'
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
			        Codcta  WITH IIF(m.transa='ABO',w_ctad,mctaf),;
			        mtodeb  WITH m.monto
			        	
		ENDIF

		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
		   		    tipcta  WITH 'H',;
		        	Codcta  WITH IIF(m.transa='ABO',mctaf,w_ctad),;
			        mtoHab  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		   		    Periodo WITH m.periodo,;
		   	    	tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
		    	    Codcta  WITH '101010101000',;
			        mtodeb  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
			        Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
		    	    tipcta  WITH 'H',;
		        	Codcta  WITH '101010101000',;
			        mtoHab  WITH m.monto
		ENDIF
	ELSE	
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
			        Codcta  WITH Mctaf,;
			        mtodeb  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
	    		    tipcta  WITH 'H',;
		        	Codcta  WITH mctaI,;
			        mtoHab  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
	    		    Periodo WITH m.periodo,;
	    	    	tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
		    	    Codcta  WITH '101010101000',;
			        mtodeb  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
			        Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
		    	    tipcta  WITH 'H',;
		        	Codcta  WITH '101010101000',;
			        mtoHab  WITH m.monto
		ENDIF
	ENDIF	
ENDIF	
RETURN

PROCEDURE agrite     &&
*---------------
SELECT AstPat
IF f_appd()
	REPLACE nummes  WITH m.nummesC,;
	        numref  WITH m.numero,;
	        Periodo WITH m.periodo,;
	        tipdoc  WITH m.TipDoc,;
	        codctc  WITH m.codctc,;
	        fecha   WITH m.FechaC,;
			Tipctc  WITH w_tipctc
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT AstPat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN
PROCEDURE agrite     &&
*---------------
SELECT AstPat
IF f_appd()
	REPLACE nummes  WITH m.nummesC,;
	        numref  WITH m.numero,;
	        Periodo WITH m.periodo,;
	        tipdoc  WITH m.TipDoc,;
	        codctc  WITH m.codctc,;
	        fecha   WITH m.FechaC,;
			Tipctc  WITH w_tipctc
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT AstPat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN

