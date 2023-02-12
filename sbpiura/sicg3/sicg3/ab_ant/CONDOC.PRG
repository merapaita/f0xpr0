*- CONDOC.PRG
*- Seguimiento de Documento Fuente
*----------------------------
USE Parmae   IN 1  ORDER Parmae1  ALIAS Parma
USE Compag   IN 2  ORDER Compag1  ALIAS Compag
USE Hojcon   IN 3  ORDER Hojcon1  ALIAS Hoja 
USE OrdCom   IN 4  ORDER OrdCom1  ALIAS Orden
USE ItePec   IN 5  ORDER ItePec12 ALIAS Itepec
USE Cajas    IN 6  ORDER Cajas1   ALIAS Caja
USE SolSer   IN 7  ORDER SolSer4  ALIAS Solser
USE Ordser   IN 8  ORDER OrdSer1  ALIAS OrdSer
USE Cheque   IN 9  ORDER Cheque1  ALIAS Cheque
USE Pecosa   IN 10 ORDER Pecosa1  ALIAS Pecosa
USE Auxil    IN 15 ORDER TAG Auxil1  ALIAS Auxil
USE Promae   IN 17 order tag Promae1 ALIAS Promae

DEFINE WINDOW Wind_1 FROM 10,20 TO 14,60  TITLE " Periodo de Consulta " FLOAT DOUBLE COLOR SCHEME 5
DEFINE WINDOW Wind_4 FROM 17,16 TO 21,64  TITLE " ±± ESC = SALIR ±± " FLOAT DOUBLE COLOR SCHEME 05
DEFINE WINDOW Wind_2 FROM 00,01 TO 23,79  FLOAT DOUBLE COLOR SCHEME 10
DEFINE WINDOW Wind_3 FROM 02,16 TO 22,64  TITLE "±± F2=BUSCAR ±±  POR NOMBRE  ±± F10=SALIR ±±" FLOAT DOUBLE COLOR SCHEME 10
DEFINE WINDOW Wind_8 FROM 06,26 TO 16,79  TITLE " Seguimiento de Documento Fuente " FLOAT DOUBLE COLOR SCHEME 05
PRIVATE vnummes,vnumcp,vcodctc,Vfeccp,Vnummesss,Vnumss

DIMENSION vsw[6]
FOR i=1 TO 6
    vsw=0
ENDFOR    
STORE  SPACE(14) TO vcodctc
STORE  SPACE(04) TO vnumcp

DO WHILE .T.
    vdBF=SYS(3)+'.DBF'
	SELE 12
	CREATE TABLE (vdbf);
	    	 (Nummes   C(2),;
	    	  Numcp    C(4),;
	    	  codctc   C(14),;
	    	  numhc    C(4),;
	    	  nummeshc C(2),;
	    	  tipdoc   C(2),;
	    	  feccp    D(8),;
	    	  glosa    C(200),;
	    	  usuario  C(8),;
	    	  fecref   D(8),;
              import   N(18,2),;
              reten    N(18,2))
	USE              
	USE (Vdbf) IN 12  EXCLUSIVE   ALIAS temp
	*ACTIVATE WINDOW Wind_8
	FOR i=1 TO 6
	     vsw[i]=0
	ENDFOR    
	STORE  SPACE(14) TO Vnumchq
	STORE  SPACE(04) TO vnumhc,vnumoc
	STORE  SPACE(02) TO vnummes,vnummeshc,vperhc,vnummesoc,vnummesss
	STORE  SPACE(03) TO vtipdoc,vfte
	STORE  SPACE(33) TO vnombre
	STORE  0 TO vimport,vreten,vliquido ,Vtiempo
	vnummes  ='0'+ALLTRIM(STR(MONTH(DATE())))
	vcodprv='    '
	okcancel = 1
	vbus=1
	do prov
	IF LASTKEY()=27
       CLOSE DATA
       RETURN
    ENDIF   
ENDDO
RELEASE WINDOW Wind_2
RELEASE WINDOW wind_8
CLOSE DATABASES
RETURN


PROCEDURE Prov
*---------------
SET CONFIR OFF
vNom = SPACE(1)
vFecini = DATE()
vFecfin = DATE()
SELE 1
activate window standby
@ 01,01 SAY 'Proveedor ->' GET vCodPrv  PICTURE '!!!!' VALID Val_prv(vCodPrv,.T.,2,27)
read
deactivate window standby
IF LASTKEY()=27
   RETURN
ENDIF   
VDESCRI=PROMAE.NOMPRO
DEFINE WINDOW Wind_6 FROM 06,26 TO 19,79  TITLE ' '+ALLTRIM(vdescri)+' ' ;
					 FOOTER " [F10] SELECCIONAR " FLOAT DOUBLE COLOR SCHEME 02
ACTIVATE WINDOW Wind_1
@ 1,1  SAY " Desde : " GET vFecini
@ 1,18 SAY "    al : " GET vFecFin
READ 
DEACTIVATE WINDOW Wind_1

IF LASTKEY()=27
   RETURN
ENDIF   

DEFINE WINDOW Xwait FROM 21,50 TO 23,75 COLOR SCHEME 05 
ACTIVATE WINDOW Xwait
@0,0 SAY " Consulta en Proceso...." COLOR W+/N*
*** Busca el proveedor o persona en compag
SELECT Compag
SET ORDER TO Compag5
SEEK ALLTRIM(vcodprv)
IF FOUND()
	SELECT Compag
	SCAN WHILE Codprv = ALLT(vCodprv)
		IF BETWEEN(Feccp,vFecini,vFecfin)
			SELECT temp
			APPEND BLANK
			REPLACE Nummes    WITH Compag.Nummes
			REPLACE Numcp     WITH Compag.Numcp
			REPLACE Codctc    WITH Compag.Codctc
     		REPLACE Nummeshc  WITH Compag.Nummeshc
			REPLACE Numhc     WITH Compag.Numhc
			REPLACE tipdoc    WITH Compag.tipdoc
			REPLACE feccp     WITH Compag.feccp
			REPLACE fecref    WITH Compag.fecref
	        REPLACE glosa     WITH Compag.glosa
			REPLACE usuario   WITH Compag.usuario
			REPLACE import    WITH Compag.import
			REPLACE reten     WITH Compag.reten
		ENDIF
		SELECT Compag
	ENDSCAN
ELSE
	RELEASE WINDOW Xwait
	DO STANDBY WITH " No hay cheques para dicho personal/proveedor/otro"
	RETURN
ENDIF	
SELE Compag
SET ORDER TO Compag1
SELE Temp 
vind1=SYS(3)+'.IDX'
INDEX ON nummes+numcp+codctc to (Vind1)
* ON KEY LABEL F10 DO Seg_doc WITH nummes,numcp,codctc
*ON KEY LABEL F10 KEYBOARD CHR(23)
DEACTIVATE WINDOW Xwait
ACTIVATE WINDOW Wind_6
BROWSE IN WINDOW Wind_6 NOEDIT NODELETE NOMENU NOAPPEND FIELD ;
	Numcp                      ,;
	Nummes					   ,;
	codctc   			:12    ,;
	FecCP                      ,;       
    X1=import-reten	    :15  :h='Liquido'
	
USE
DEACTIVATE WINDOW Wind_6
RETURN

PROCEDURE Seg_Doc
*-----------------
PARAMETERS vnummes,vnumcp,vcodctc
HIDE WINDOW wind_6
IF LASTKEY()=27 OR Okcancel = 2
   DEACTIVATE WINDOW Wind_2
   EXIT
ENDIF   
SELE temp
SEEK ALLT(vnummes)+vnumcp+ALLT(vcodctc)
    vnumhc    = temp.numhc
    vnummeshc = temp.nummeshc   
    vfeccp    = temp.feccp
    vregcp    = temp.fecref
    vimporte  = temp.import
    vreten    = temp.reten
    vliquido  = vimporte-vreten
    vtipo     = temp.tipdoc
    vregcp    = temp.fecref
    vconc1    = LEFT(temp.glosa,37)
    vconc2    = SUBS(temp.glosa,37,37)
    vconc3    = SUBS(temp.glosa,74,37)
    vconc4    = SUBS(temp.glosa,111,37)
    vconc5    = SUBS(temp.glosa,148,37)
    vconc6    = SUBS(temp.glosa,185,37)
    vusucp    = CHRTRAN(temp.usuario,'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?','ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
SELE Cheque
SEEK ALLT(vnummes)+vnumcp+ALLT(vcodctc)
IF FOUND()
	   vnumchq=cheque.numchq
	   vnombre=LEFT(cheque.nomgir,30)
	   vfecchq=cheque.fecchq
       vtiempo=vfecchq-vregcp+1
	   vsw[1]=1
	    SELE Hoja
	    SEEK ALLT(vnummeshc)+vnumhc        
		IF FOUND()
	       vtipdoc  = hoja.tipdoc
	       vperhc   = hoja.periodo
	       vnumref  = hoja.numref
    	   vfte     = hoja.codfte
    	   vfechc   = hoja.fechc
    	   vreghc   = hoja.user_fc
    	   vusuhc   = hoja.user
           vtiempo  =vfecchq-vreghc+1
  	       vsw[2]=1
		   DO CASE
		      CASE ALLTRIM(vtipdoc)='O/C'
					SELE ORDEN
					SEEK vperhc+vnumref+ALLTRIM(vfte)
			    	IF FOUND()
			           vnumoc   = orden.numoc
			           vnummesoc= SUBSTR(orden.codcal,3,2)
			           vfecoc   = orden.fecoc
  				       vregoc   = orden.user_fc
  				       vtiempo  = vfecchq-vregoc+1
  				       vusuoc   = orden.user
  					   vsw[3]=1
			           SELE itepec
		    	       SEEK vnumoc+ALLT(vnummesoc)
			           IF FOUND()
			               vnumpe   = itepec.numpec
        	               vnummespe= SUBSTR(itepec.codcal,3,2)		               
        	               vperpe   = itepec.periodo
	  					   vsw[4]=1
	  					   SELE Pecosa
	  					   SEEK vperpe+vnumpe+ALLT(vfte)
	  					   IF FOUND()
	        	              vfecpe   = pecosa.fecemi
	  				          vregpe   = pecosa.user_fc
  					          vusupe   = pecosa.user
			    	          vtiempo  = vfecchq-vregpe+1
  					       ENDIF    
		    	       ENDIF
			       ENDIF 
			  CASE ALLTRIM(vtipdoc)='O/S'
				    SELE ORDSER
					SEEK vperhc+vnumref+ALLTRIM(vfte)
			    	IF FOUND() 
			           vnumos   =ordser.numos
			           vnummesos=SUBSTR(DTOC(ordser.Fecos),4,2)
			           vfecos   =ordser.fecos
  				       vregos   =ordser.user_fc
				       vusuos   =ordser.user
			           vtiempo  =vfecchq-vregos+1
			           vfecliq  =ordser.fecliq
  					   vsw[5]=1
			           SELE Solser
		    	       SEEK vnumos+ALLT(vnummesos)
		    	       IF FOUND()
						   vnumss  =solser.numss
				           vnummesss=SUBSTR(DTOC(Solser.Fecss),4,2)
				           vfecss   =solser.fecss
	  				       vregss   = solser.user_fc
  					       vususs   = solser.user
			    	       vtiempo  =vfecchq-vregss+1
	  					   vsw[6]=1
			           ENDIF
		    	   ENDIF 
	       ENDCASE   
        ENDIF
    ENDIF
	DEACTIVATE WINDOW wind_8      	
	ACTIVATE WINDOW Wind_2
	@  0,08 SAY "Doc. Fte"
	@  0,37 SAY "Fec.Doc."
	@  0,50 SAY "Fec.Ope."
	@  0,63 SAY "Usuario"
	@  1,08 SAY REPL('=',8)
	@  1,37 SAY REPL('=',8)
	@  1,50 SAY REPL('=',8)
	@  1,63 SAY REPL('=',6)
	DO CASE
	   CASE ALLT(vtipdoc)='O/S'
		   @  3,01 SAY "N§ S/S      : "+ALLT(vnummesss)+'.'+vnumss
		   @  3,38 SAY IIF(vsw[5]=1,vfecss,' ')
		   @  3,51 SAY IIF(vsw[5]=1,vregss,' ')
		   @  3,64 SAY IIF(vsw[5]=1,vususs,' ')
		   @  4,04 SAY '' COLOR RGB(255,0,0,255,255,225)
    	   @  5,01 SAY "N§ O/S      : "+ALLT(vnummesos)+'.'+vnumos+'.'+vfte
		   @  5,38 SAY IIF(vsw[6]=1,vfecos,' ')
		   @  5,51 SAY IIF(vsw[6]=1,vregos,' ')
		   @  5,64 SAY IIF(vsw[6]=1,vusuos,' ')
		   @  6,04 SAY '' COLOR RGB(255,0,0,255,255,225)
	   CASE ALLT(vtipdoc)='O/C'
		   @  3,01 SAY "N§ PECOSA   : "+ALLT(vnummespe)+'.'+vnumpe
		   @  3,38 SAY IIF(vsw[3]=1,vfecpe,' ')
		   @  3,51 SAY IIF(vsw[3]=1,vregpe,' ')
		   @  3,64 SAY IIF(vsw[3]=1,vusupe,' ')
		   @  4,04 SAY '' COLOR RGB(255,0,0,255,255,225)
    	   @  5,01 SAY "N§ O/C      : "+ALLT(vnummesoc)+'.'+vnumoc+'.'+vfte
		   @  5,38 SAY IIF(vsw[4]=1,vfecoc,' ')
		   @  5,51 SAY IIF(vsw[4]=1,vregoc,' ')
		   @  5,64 SAY IIF(vsw[4]=1,vusuoc,' ')
		   @  6,04 SAY '' COLOR RGB(255,0,0,255,255,225)
	ENDCASE
	IF vsw[2]=1
		@  7,01 SAY "N§ H/C      : "+ALLT(vnummeshc)+'.'+vnumhc+'  '+IIF(vsw[3]=1 OR vsw[5]=1,' ',ALLT(vtipdoc))
		@  7,38 SAY vfechc
	    @  7,51 SAY vreghc
	    @  7,64 SAY vusuhc
        @  8,04 SAY '' COLOR RGB(255,0,0,255,255,225)
	ENDIF
	@  9,01 SAY "N§ C/P      : "+ALLT(vnummes)  +'.'+vnumcp+'.'+ALLT(vcodctc)
	@  9,38 SAY vfeccp  PICTURE "@D"
    @  9,51 SAY vregcp
    @  9,64 SAY vusucp
    @ 10,04 SAY '' COLOR RGB(255,0,0,255,255,225)
	@ 11,01 SAY "N§ Cheque   : "+ALLT(vnumchq)  
	@ 11,38 SAY IIF(vsw[1]=1,vfecchq,' ')
    @ 11,51 SAY vregcp
    @ 11,64 SAY vusucp
    @ 12,04 SAY '' COLOR RGB(255,0,0,255,255,225)
	@ 13,01 SAY "Girado a    : "+vnombre
	@ 13,46 SAY "Monto Bruto :" 
	@ 13,62 SAY vimporte   PICTURE "@Z 9,999,999.99"
	@ 14,02 TO 21,41
	@ 15,03 SAY vconc1 
	@ 15,46 SAY "Retenci¢n   :" 
	@ 15,62 SAY vreten     PICTURE "9,999,999.99"
	@ 16,03 SAY vconc2 
	@ 17,03 SAY vconc3 
	@ 17,46 SAY "Liquido     :" 
	@ 17,62 SAY vliquido   PICTURE "@Z 9,999,999.99" COLOR RGB(1,0,0,200,111,115)
	@ 18,03 SAY vconc4 
	@ 19,03 SAY vconc5 
	@ 20,03 SAY vconc6 
	@ 19,46 SAY "Duraci¢n    :" 
	@ 19,65 SAY vtiempo   PICTURE "999" COLOR RGB(1,0,0,200,111,115)
	@ 19,68 SAY IIF(vtiempo=1,"  D¡a","  D¡as") COLOR RGB(1,0,0,200,111,115)
	@ 21,62 GET okcancel FUNCTION '*TH \!\<OK' DEFAULT 1 SIZE 1,11,8
	READ
CLEA 
DEACTIVATE WINDOW Wind_2
*CTIVATE WINDOW Wind_8
RETURN

PROCEDURE Salir
*---------------
PARAMETERS vSal
IF LASTKEY()=-9
	vsal = .T.
ENDIF
RETURN .T.

PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.

FUNCTION disppro
*---------------
PARAMETERS xtipprv,xcodotr,xtipdoc
PRIVATE xnompre
DO CASE
    CASE xtipprv="P"
         xnompre=val_Auxi(ALLT(Compag.Codprv),'20','V')
    CASE xtipdoc$"RESR"
		 xnompre=VAL_PARA(Compag.codret,'CODRET','V',22,40)	
	OTHER  	   
	     xnompre=compag.nompre
ENDCASE
RETURN xnompre

PROCEDURE BUSCA
*-----------------
SET EXAC OFF
ACTIVATE WINDOW Wind_4
VNOM = SPACE(20)
@ 1,1 SAY " Nombre : " GET vNom 
READ && VALID salir(vsalir)
DEACTIVATE WINDOW Wind_4
vReg = RECNO()
SEEK ALLTRIM(vnom)
IF !FOUND()
	GO vReg
ENDIF
SET EXACT ON
RETURN
