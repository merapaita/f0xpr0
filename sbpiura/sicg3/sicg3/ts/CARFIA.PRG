* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ CarFia.Prg  10/07/96                                L: 0640  ³	
* ³ Registro de Cartas Fianza                                    ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ³ ACTUALIZACION  :  PILAR PACHERRE NAVARRO                     ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Estados
*- 00 En Curso
*- 10 Liberado
*- 20 Renovado
*- 99 Eliminado

*- Apertura de files
USE CarFia  IN 1 ORDER CarFia1 		ALIAS CarF
USE ParMae  IN 2 ORDER ParMae1 		ALIAS Parma
USE Obra    IN 3 ORDER obra2		ALIAS obra 
USE Auxil   IN 4 ORDER TAG Auxil1   ALIAS Auxil
SELE CarF
GO BOTTOM

*- Mensajes del Sistema

vmens01 = ' °®F2¯Renueva   ®F4¯Libera   ®F6¯Imprime Oficios°'
Vmens02 = ' Revisi¢n de Cartas Fianzas     Presione F10 para concluir   '
Vmens04 = 'Dicha carta no fue encontrada.'
Vmens05 = 'No existe Carta Fianza anterior.'
Vmens06 = 'No existe Carta Fianza siguiente.'
Vmens07 = '¨Est  seguro que desea ANULAR esta Carta?'
Vmens08 = 'No hay registros para procesar'

*- Variables de trabajo (registro a trabajar)
PUBLIC vTipCF,oriw,vtocli,vnuevo,sw,VUSER_ID,wuser_id,VORDEN
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
STORE 1  TO vTipCF,oriw,vtocli,VORDEN
STORE 0  TO sw
STORE SPACE(4)  TO vnuevo
SCATTER MEMVAR

*- Inicia proceso

*SAVE SCREEN TO OldPant

DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL 
DO Pantalla                  && Muestra pantalla inicial
DO vista
*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

*RESTORE SCREEN FROM OldPant
SHOW POPUP Menu
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN

 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW wind_4 FROM 00,00 TO 23,79 DOUBLE ;
	TITLE 'SELECCIONAR' COLOR SCHEME 2

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi

 ON KEY LABEL F2 DO RenCF
 ON KEY LABEL F4 DO LibCF
*ON KEY LABEL F6 DO CARCF
 ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR

 @  1, 2 SAY "           N£mero :"
 @  2, 2 SAY " Fecha Digitaci¢n :"
 @  4, 2 SAY "            Banco :"
 @  5, 2 SAY "  N§ Carta Fianza :"
 @  7, 2 SAY "    Fecha Emisi¢n :"
 @  8, 2 SAY "Fecha Vencimiento :"
 @ 09, 2 SAY "          Importe :" 
 @ 11, 2 SAY "           Origen :"
 @ 13, 2 SAY "             Tipo :"
 @ 14, 2 SAY "     Raz¢n Social :"
 @ 15, 2 SAY "       Referencia :"
 @ 17, 2 SAY "         Garant¡a :"
 @ 18, 2 SAY "     Observaciones:"
RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 
 SELECT CarF
 SCATTER MEMVAR
 
 IF EOF()
    DO Pantalla
    RETURN
 ENDIF
 @  0,62 SAY IIF(m.Estado='00',' EN CURSO   ',IIF(m.estado='10','  LIBERADA  ',IIF(m.estado='20','  RENOVADA  ','  ANULADA   '))) COLOR SCHEME 02
 @  1,22 SAY m.numero
 @  2,22 SAY m.Fecrec
 @  4,22 SAY Val_Para(m.CodBan,"BANCOS",'V',22,40)
 @  5,22 SAY m.NumDoc
 @  7,22 SAY m.FecEmi
 @  8,22 SAY m.FecVen
 @ 09,22 SAY m.importe PICTURE '999,999,999,999.99'
 @ 11,22 SAY m.origen PICTURE "!"
 @ 13,22 SAY m.tipo PICTURE "!"
 @ 14,22 SAY IIF(m.tipo='C',val_Auxi(ALLT(m.CodPrv),'50','V'),IIF(m.tipo='P',val_Auxi(ALLT(m.CodPrv),'20','V'),val_Auxi(ALLT(m.CodPrv),'09','V')))
 @ 15,22 SAY m.codobr  PICTURE "@!"
* @ 15,22 GET m.refer   PICTURE "@S50"   FUNCTION "!" when m.origen='A'
 @ 15,27 SAY IIF(m.origen='G',Val_Fun("obra","Codobra","LEFT(Descri,50)",ALLTRIM(m.Codobr)),m.refer)
 @ 17,22 SAY VAL_PARA(m.garantia,'GARANT','V', 22,20)
 @ 18,22 SAY m.Observ    PICTURE '@S50'
 IF !EMPTY(m.carlib)
    @ 20,02 SAY "  Cargo Liberaci¢n:"
    @ 20,43 SAY "  Fecha Liberaci¢n:"
    @ 20,22 SAY m.carlib COLOR SCHEME 02
    @ 20,63 SAY m.feclib COLOR SCHEME 02
    sw=1
 ELSE
    @ 20,02 CLEA  TO 20,78   
 ENDIF
 IF !EMPTY(m.carren)    
    @ 20,02 SAY "  Carta Renovaci¢n:"
    @ 20,43 SAY "  Fecha Renovaci¢n:"
    @ 20,22 SAY m.carren  COLOR SCHEME 05
    @ 20,63 SAY m.fecren  COLOR SCHEME 05
 ELSE
    IF sw=0
       @ 20,02 CLEA TO 20,78   
    ENDIF
 ENDIF    
 
RETURN


PROCEDURE Revis
*--------------
SELECT CarF

 IF EOF()
   DO standby WITH vMens08
   RETURN
 ENDIF

 ACTIVATE SCREEN
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
 numero,;
 x1=IIF(tipo='C',val_Auxi(ALLT(CodPrv),'50','V'),IIF(TIPO='P',VAL_AUXI(ALLT(CODPRV),'20','V'),val_Auxi(ALLT(CodPrv),'09','V'))) :H='Proveedor' :25,;
 x2=Val_Para(CodBan,"BANCOS",'V',22,40) :H='Banco' :12,;
 fecemi :H='Fecha',;
 x3=IIF(ORIGEN='G',Val_Fun("obra","Codobra","LEFT(Descri,50)",ALLTRIM(Codobr)),REFER) :H='Obra' :50
 ON KEY LABEL F10

 DO Vista
  
RETURN


PROCEDURE Busca
*--------------
 IF EOF()
   DO standby WITH vMens08
   RETURN
 ENDIF

 Vtemp = RECNO()
 DEFINE WINDOW WBusca FROM 10,12 TO 14,68 DOUBLE COLOR SCHEME 6
 ACTIVATE WINDOW WBusca
 
 STORE SPACE(4) TO Vbusca
 
 @ 1,3 SAY 'N§ Carta Fianza :' GET Vbusca PICT '@!' COLOR SCHEME 7
 READ
 RELEASE WINDOW WBusca
 
 IF EMPTY(Vbusca) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK vBusca
   
   IF !FOUND()
     DO standby WITH vMens04
     GOTO Vtemp
   ELSE
     DO vista
   ENDIF
   
 ENDIF
 
RETURN


PROCEDURE Anter
*--------------
 IF EOF()
   DO standby WITH vMens08
   RETURN
 ENDIF
 
 SELECT CarF
 IF !BOF()
   SKIP -1
 ENDIF
 
 IF BOF()
   GO TOP
   DO standby WITH vMens05
 ELSE
   DO vista
 ENDIF
 
RETURN


PROCEDURE Proxi
*--------------
 IF EOF()
   DO standby WITH vMens08
   RETURN
 ENDIF
 
 SELECT CarF
 IF !EOF()
   SKIP
 ENDIF
 
 IF EOF()
   DO standby WITH vMens06
   GO BOTTOM
 ELSE
   DO vista
 ENDIF
 
RETURN


PROCEDURE Corri
*--------------
 IF EOF()
   DO standby WITH vMens08
   RETURN
 ENDIF
  IF Estado = '99'  && Anulada
   DO STANDBY WITH "Esta Carta Fianza ha sido anulada"
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 @  1,22 SAY m.numero 
 @  2,22 SAY m.FecRec
 @  4,22 GET m.CodBan  PICTURE "!!!" VALID Val_Para(m.CodBan,"BANCOS"," ",22,25)
 @  5,22 GET m.NumDoc  PICTURE "@!" VALID NoRepCF() 
 @  7,22 GET m.FecEmi
 @  8,22 GET m.FecVen  VALID (m.fecven>=m.fecemi)
 @ 09,22 GET m.importe PICTURE '999,999,999,999.99'
 @ 11,22 GET m.origen  VALID(m.origen$'GA')
 @ 13,22 GET m.Tipo    VALID(m.tipo$'COP')
 @ 14,22 GET m.CodPrv  PICTURE "!!!!!!"   VALID val_AuxiD(ALLT(m.CodPrv),'50',"A",26)  WHEN m.Tipo='C' AND limpia()
 @ 14,22 GET m.CodPrv  PICTURE "!!!!!!"   VALID val_AuxiD(ALLT(m.CodPrv),'09',"A",26)  WHEN m.Tipo='O' AND limpia()
 @ 14,22 GET m.CodPrv  PICTURE "!!!!"     VALID val_AuxiD(ALLT(m.CodPrv),'20',"A",26)  WHEN m.Tipo='P'
 @ 15,22 GET m.codobr  PICTURE "@!"    VALID Val_fun1('obra','Codobra','descri',m.codobr,1,'cptobra4',15,28) when m.origen='G' and limpi1()
 @ 15,22 GET m.refer   PICTURE "@S50"    FUNCTION "!" when m.origen='A'
 @ 17,22 GET m.garantia  PICTURE "!!" VALID VAL_PARA(m.garantia,'GARANT',' ', 22,20)
 @ 18,22 GET m.Observ   PICTURE '@S50' FUNCTION "!"
 m.Estado=Estado
 m.usuario=wuser_id
 READ
 IF YesNo("Esta Correcto?")
    SELECT CarF
    IF F_Lock(1)
       GATHER MEMVAR
       UNLOCK
    ELSE
       DO StandBy WITH " No pudo grabar archivo "
    ENDIF
 ENDIF

 DO vista
 
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW Wind_0
 SELECT CarF
 DO Pantalla
 SCATTER MEMVAR BLANK
 m.Estado  ='00'
 STORE DATE() TO m.fecemi,m.fecrec,m.fecven
 SELECT Parma
 SEEK 'CORREL'+'CTAFIA'
 m.Numero = PADL(NumEnt+1,4,'0')
 m.observ = 'MEMO N§    -'+SUBSTR(STR(YEAR(DATE()),4),3,2)+' '+'CTAR PIURA-GRO-SRGO'+SPACE(70)
 m.memolib = 'MEMO N§    -'+SUBSTR(STR(YEAR(DATE()),4),3,2)+' '+'CTAR PIURA-GRO-SRGO'
 m.refer  = 'LICITACION PUBLICA N§     -'+SUBSTR(STR(YEAR(DATE()),4),3,2)+' '+'CTAR PIURA-GA'+SPACE(6)
 SELECT CarF
 @  1,22 GET m.numero  WHEN .F.
 @  2,22 SAY m.FecRec
 @  4,22 GET m.CodBan  PICTURE "!!!"    VALID Val_Para(m.CodBan,"BANCOS"," ",22,25)
 @  5,22 GET m.NumDoc  PICTURE "@!"     VALID NoRepCF() 
 @  7,22 GET m.FecEmi
 @  8,22 GET m.FecVen  VALID (m.fecven>=m.fecemi)
 @ 09,22 GET m.importe PICTURE '999,999,999,999.99'
 @ 11,22 GET m.origen  FUNCTION "M G,A"   
 @ 13,22 GET m.Tipo    FUNCTION "M C,O,P"   
 @ 14,22 GET m.CodPrv  PICTURE "!!!!!!"   VALID val_AuxiD(m.CodPrv,'50',"A",26)  WHEN m.tipo='C' AND limpia()
 @ 14,22 GET m.CodPrv  PICTURE "!!!!"     VALID val_AuxiD(m.CodPrv,'20',"A",26)  WHEN m.tipo='P' AND limpia()
 @ 14,22 GET m.CodPrv  PICTURE "!!!!!!"   VALID val_AuxiD(m.CodPrv,'09',"A",26)  WHEN m.Tipo='O'
 @ 15,22 GET m.codobr  PICTURE "@!"     VALID Val_fun1('obra','Codobra','descri',ALLT(m.codobr),1,'cptobra4',15,28) when m.origen='G' and limpi1()
 @ 15,22 GET m.refer   PICTURE "@S50"   FUNCTION "!" when m.origen='A'
 @ 17,22 GET m.garantia  PICTURE "!!" VALID VAL_PARA(m.garantia,'GARANT',' ', 22,20)
 @ 18,22 GET m.Observ   PICTURE '@S50' FUNCTION "!"
 m.usuario=wuser_id
 READ VALID val_READ() 
 IF LASTKEY() = 27
   DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
   DO Vista
   RETURN
 ENDIF
 DO CASE
 	CASE M.ORIGEN='G'
 		M.REFER=SPACE(50)
	CASE M.ORIGEN='A'
		M.CODOBR=SPACE(4)
		M.GARANTIA=SPACE(2)		
 ENDCASE						 		
    SELECT CarF
    IF F_Appd()            && Crea y graba nuevo registro
       GATHER MEMVAR
       *- Actualiza correlativo
       SELECT Parma
       SEEK 'CORREL' + 'CTAFIA'
       REPLACE NumEnt WITH Nument+1
    ELSE
       DO StandBy WITH " No pudo grabar archivo "
    ENDIF
    UNLOCK
 DO vista
RETURN


PROCEDURE Elimi
*--------------
 IF EOF()
   DO standby WITH vMens08
   RETURN
 ENDIF
 STORE YESNO(vMens07) TO vElimina
 IF vElimina
    SELECT CarF
    IF F_Lock(1)
       REPLACE Estado WITH '99'
       REPLACE usuario WITH wuser_id
    ENDIF
 ENDIF
 DO vista
RETURN


PROCEDURE Lista
*--------------
 SELECT CarF
 IF EOF()
   DO standby WITH vMens08
   RETURN
 ENDIF
 
 vTemp = RECNO()
 GO TOP
 vIniCF=numero
 GO BOTTOM
 vFinCF=numero
 DEFINE WINDOW CarFia FROM 3,10 TO 23,70 TITLE " Reporte de Cartas Fianzas " DOUBLE COLOR SCHEME 05 
 ACTIVATE WINDOW CarFia
 @ 01,01 SAY "Tipo de Listado: " GET vTipCF function '^ En Curso;Vencidas;Renovadas;Liberadas;Anuladas;General' 
 @ 04,01 SAY "   Ordenado Por: " GET vorden function '^ Numero de Carta Fianza;Vencimiento;Banco-Contratista' 
 @ 08,01 SAY " Todas las C/F : " GET vTOCLI  FUNCTION '*RNH \<Todas;\<Por Rangos' SIZE 1,10,6 
 @ 10,01 SAY "  N£mero  Desde: " GET vIniCF  WHEN vOrden#2 .and. vtocli=2
 @ 11,01 SAY "          Hasta: " GET vFinCF  WHEN vOrden#2 .and. vtocli=2
 @ 13,01 SAY "  Vencim. Desde: " GET vIniF  DEFAULT DATE() WHEN vOrden=2 .and. vtocli=2
 @ 14,01 SAY "          Hasta: " GET vFinF  DEFAULT DATE() wHEN vOrden=2 .and. vtocli=2
 @ 15,01 SAY "  Originada Por: " GET oriw function '^ Gerencia Operaciones;Abastecimiento' 
 @ 17,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
 IF LASTKEY()=27 OR okcancel=2
    RELEASE WINDOW CarFia
    GO vTemp
    RETURN
 ENDIF
 
 RELEASE WINDOW CarFia
 vIdx = SYS(3)+'.IDX'
 DO CASE
     CASE vtocli=2  &&Por Rangos
	      DO CASE
		     CASE vorden=1
		     	  if oriw=1
					  INDEX ON numero TO (vIdx) FOR BETWEEN(numero, vINICF, vFINCF) AND Vertip() and origen='G'
				  else
					  INDEX ON numero TO (vIdx) FOR BETWEEN(numero, vINICF, vFINCF) AND Vertip() and origen='A'
				  endif				  					  
				  DO Reporte WITH 2,"LisCF2"," Cartas Fianzas",1,.F.,.T.
			 CASE vOrden=2
			 	  if oriw=1
					  INDEX ON DTOC(FecVen,1) TO (vIdx) FOR BETWEEN(FecVen,vIniF,vFinF)  AND Vertip() and origen='G'
				  else
					  INDEX ON DTOC(FecVen,1) TO (vIdx) FOR BETWEEN(FecVen,vIniF,vFinF)  AND Vertip() and origen='A'
				  endif				  
				  DO Reporte WITH 2,"LisCF2"," Cartas Fianzas",1,.F.,.T.
		     CASE vorden=3
		     	  if oriw=1
					  INDEX ON codban+tipo+codprv+numero TO (vIdx) FOR BETWEEN(numero, vINICF, vFINCF) AND Vertip() and origen='G'
				  else
					  INDEX ON codban+tipo+codprv+numero TO (vIdx) FOR BETWEEN(numero, vINICF, vFINCF) AND Vertip() and origen='A'
				  endif
				  DO Reporte WITH 2,"LisCF1"," Cartas Fianzas",1,.F.,.T.
		 ENDCASE
     CASE vtocli=1  &&Por Rangos
	      DO CASE
		     CASE vorden=1
				  INDEX ON numero TO (vIdx) FOR Vertip()
				  DO Reporte WITH 2,"LisCF2"," Cartas Fianzas",1,.F.,.T.
			 CASE vOrden=2
				  INDEX ON DTOC(FecVen,1) TO (vIdx) FOR Vertip()
				  DO Reporte WITH 2,"LisCF2"," Cartas Fianzas",1,.F.,.T.
		     CASE vorden=3
				  INDEX ON codban+tipo+codprv+numero TO (vIdx) FOR BETWEEN(numero, vINICF, vFINCF) AND Vertip()
				  DO Reporte WITH 2,"LisCF1"," Cartas Fianzas",1,.F.,.T.
		 ENDCASE
ENDCASE		 
SET INDEX TO
SET ORDER TO CarFia1 
GOTO vTemp
CLOSE INDEX 
ERASE &vIdx
RETURN


FUNCTION NoRepCF
*----------------
PRIVATE vFun, vRec
   vRec = IIF(EOF(),-1,RECNO())
   SET ORDER TO CarFia2
   SEEK m.codban+m.numdoc
   IF FOUND() AND estado='99'
      Vfun=.F.
   ELSE
      vfun=.T.
   ENDIF      
   SET ORDER TO CarFia1   
   IF vRec<0
      GO BOTTOM
   ELSE
      GO vRec
   ENDIF
RETURN vFun



PROCEDURE RenCF
*--------------
   SELE CARF
   SCATTER MEMVAR
   DEFINE WINDOW RenCF FROM 5,2 TO 18,78 COLOR SCHEME 10 ;
     TITLE "Renovaci¢n de Carta Fianza       Esc:Cancela"
   DO CASE
	   CASE m.Estado="00" &&En Curso
		   m.memolib = 'MEMO N§    -'+SUBSTR(STR(YEAR(DATE()),4),3,2)+' '+'CTAR-RG-GO-SGO'+SPACE(70)
		   STORE DATE() TO m.fecrec,m.fecren,m.fecven
		   STORE SPACE(25) TO m.numdoc
		   ACTIVATE WINDOW RenCF
		   @  2, 2 SAY " Fecha Renovaci¢n :"
		   @  4, 2 SAY "  N§ Carta Fianza :"
		   @  6, 2 SAY "   Fecha Registro :"
		   @  8, 2 SAY "Fecha Vencimiento :"
		   @ 10, 2 SAY "Memo de Renovaci¢n:"
		   @  2,22 GET m.FecRen  
		   @  4,22 GET m.Numdoc
		   @  6,22 SAY m.FecRec
		   @  8,22 GET m.Fecven
		   @ 10,22 GET m.memolib PICTURE '@S50'
		   READ
		   m.fecemi=m.fecren
		   RELEASE WINDOW RenCF
		   IF LASTKEY()=27
		      RETURN
		   ENDIF
		   IF YesNo("Confirme")
		      SELECT Parma
		      SEEK "CORREL"+"CTAFIA"
		      vnuevo = PADL(Parma.NumEnt+1,4,'0')
		      SELECT CarF
		      REPLACE Carren WITH vnuevo,;
		              Estado WITH '20',;
		              fecren WITH m.fecren,;
		              memolib with m.memolib
				      STORE CTOD(SPACE(8)) TO m.fecren        
			          Vrec=RECNO()	      
		      IF F_Appd()
    		     m.Estado = '00'
		         m.numero = vnuevo
		         m.observ = m.memolib
		         m.memolib=space(50)
    		     GATHER MEMVAR
		         SELECT Parma
    		     REPLACE NumEnt WITH NumEnt + 1
	    	     SELECT CarF
	    	  ENDIF
	    	  GO vrec
		      UNLOCK ALL
		      DO Vista
		   ENDIF
	   CASE ALLTRIM(m.Estado)="20"
		     DO STANDBY WITH "La Carta fianza ya ha sido renovada"
		     IF YesNo("Corrige")
		  	   ACTIVATE WINDOW RenCF
			   @  2, 2 SAY "Fecha Renovaci¢n :"
			   @  4, 2 SAY "Nro. CF Renovac. :"
			   @  6, 2 SAY "  Fecha Registro :"
			   @  8, 2 SAY "Fecha Vencimiento :"
			   @ 10, 2 SAY "Memo de Renovac. :"
			   @  2,22 GET m.FecRen  
			   @  4,22 GET m.Numdoc
			   @  6,22 SAY m.FecRec
			   @  8,22 GET m.Fecven
			   @ 10,22 GET m.memolib PICTURE '@S50'
			   READ
			   m.fecemi=m.fecren
			   RELEASE WINDOW RenCF
			   IF LASTKEY()=27
			      RETURN
			   ENDIF
			   SELECT CarF
		       REPLACE fecren WITH m.fecren
		       STORE CTOD(SPACE(8)) TO m.fecren        
			   Vrec=RECNO()	      
 		       SEEK m.carren
 		       IF FOUND()
 		          m.memolib=space(50)
    		      GATHER MEMVAR
	    	   ENDIF
    		   SELECT CarF
			   UNLOCK ALL
			   GO vrec
			ENDIF
	ENDCASE
	if  yesno("Imprime Cargo")
		DO Reporte WITH 2,"LisCAR","Cargo Liberaci¢n",1,.F.,.T.
	endif
	DO Vista
RETURN

PROCEDURE LibCF
*--------------
    SELE Carf
    SCATTER MEMVAR
    DEFINE WINDOW LibCF FROM 5,2 TO 14,78 COLOR SCHEME 10 ;
       TITLE ' Liberaci¢n de Cartas Fianzas '
    DO CASE
	    CASE m.Estado="00"
		         m.memolib = 'MEMO N§    -'+SUBSTR(STR(YEAR(DATE()),4),3,2)+' '+'CTAR-RG-GO-SGO'
				 SELECT Parma
				 SEEK 'CORREL'+'CARLIB'
				 m.carlib = PADL(NumEnt+1,4,'0')
				 SELE carf
			    ACTIVATE WINDOW LibCF
			    @ 2, 2 SAY " N£mero de Cargo: " GET m.carlib DEFAULT SPACE(4)
			    @ 4, 2 SAY "Fecha Liberaci¢n: " GET m.Feclib DEFAULT DATE()
			    @ 6, 2 SAY " Memo Liberaci¢n: " get m.memolib  picture '@S50'
			    READ
			    RELEASE WINDOW LibCF
			    IF LASTKEY()=27
			       RETURN
			    ENDIF
			   IF YesNo("Confirme")
				    SELECT CarF
				    REPLACE Estado WITH "10",;
				            Carlib WITH m.carlib,;
				            FecLib WITH m.FecLib,;
				            memolib with m.memolib
			       SELECT Parma
			       SEEK 'CORREL' + 'CARLIB'
			       REPLACE NumEnt WITH Nument+1
			       SELE carf
			   ENDIF
*				if  yesno("Imprime Cargo")
*					DO Reporte WITH 2,"LisCAR","Cargo Liberaci¢n",1,.F.,.T.
*				endif
		 CASE ALLTRIM(M.Estado)="10"
  	          m.memolib = 'MEMO N§    -'+SUBSTR(STR(YEAR(DATE()),4),3,2)+' '+'CTAR-RG-GO-SGO'
			  DO STANDBY WITH "La Carta fianza ya ha sido liberada"
			  IF YesNo("Corrige")
				    ACTIVATE WINDOW LibCF
				    @ 2, 2 SAY " N£mero de Cargo: " GET m.carlib DEFAULT SPACE(5)
				    @ 4, 2 SAY "Fecha Liberaci¢n: " GET m.FecLib DEFAULT DATE()
				    @ 6, 2 SAY " Memo Liberaci¢n: " get m.memolib  picture '@S50'
				    READ
				    RELEASE WINDOW LibCF
				    IF LASTKEY()=27
				       RETURN
				    ENDIF
				    SELECT CarF
				    REPLACE Estado WITH "10",;
				            carlib WITH m.carlib,;
				            FecLib WITH m.FecLib,;
				            memolib with m.memolib
			   ENDIF
	 ENDCASE 
	if  yesno("Imprime Cargo")
		DO Reporte WITH 2,"LisCAR","Cargo Liberaci¢n",1,.F.,.T.
	endif
    DO Vista
RETURN

function VerTip
*--------------
PRIVATE vFun
vFun= ( (vTipCF=1 .AND. Estado="00") OR (vTipCF=2 AND FecVen<DATE() .AND. Estado<>"99") OR ;
       (vTipCF=3 AND Estado='10') OR (vTipCF=4 AND Estado='20') OR (vTipCF=5 AND Estado='99') OR (vtipCf=6 AND !EMPTY(estado)) )
RETURN vFun


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
RETURN


PROCEDURE Fin_opcion
*-------------------
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_2
  RELEASE    WINDOW wind_3
  RELEASE    WINDOW wind_4
  RELEASE    MENU   mMenu
  CLOSE DATA
RETURN

FUNCTION limpia
*--------------
@14,22 SAY SPACE(50)
RETURN

FUNCTION limpi1
*--------------
@15,26 SAY SPACE(50)
RETURN
*
