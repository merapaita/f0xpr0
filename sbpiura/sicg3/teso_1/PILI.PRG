* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegREV.PRG  30/05/96                             L: 0803     ³	
* ³ Registro de Reversiones                                      ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

USE  Parmae   IN 1   ORDER TAG parmae1        ALIAS Parma
USE  Cajas    IN 2   ORDER TAG Cajas1         ALIAS Caja
USE  Reversio IN 3   ORDER TAG Rever1         ALIAS Rever
USE  AstPat   IN 4   ORDER TAG AstPat15       ALIAS AstPat
USE  Cuentas  IN 5   ORDER TAG cuentas1       ALIAS cuenta
USE  Auxil    IN 15  ORDER TAG Auxil2         ALIAS Auxil
USE  SubCtas  IN 11  ORDER TAG Subctas1       ALIAS Subcta

STORE SPACE(2)  TO vMes,vMesT
vmens04 = 'Dicha Reversi¢n no fue encontrada'
vmens08 = 'No hay registros para procesar'
vmens10 = 'La reversi¢n ya est  anulada'
vmens02 = ' °®F2¯Observaciones     ®F4¯Imprime REV      ®F9¯Corrige Asientos.°'
SELECT Rever
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK
PUBLIC W_TIPCTC,deme,VUSER_ID,wuser_id
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista
ON KEY LABEL F2 DO VISOBS
ON KEY LABEL F4 DO imp_rev

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Termina

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW wind_cte0 FROM 00,00 TO 24,79 DOUBLE ;
 TITLE ' REVERSIONES AL TESORO  ' FOOTER '±F2 observaciones ±  ±F4 Imprime Doc.   ±F9 Corrige Asientos±  ' COLOR SCHEME 10 


 DEFINE WINDOW Wind_2 FROM 15,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Asientos Patrimoniales - Reversiones' ;
 COLOR SCHEME 10


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 ON KEY LABEL F4 DO imp_rev
 ON KEY LABEL F9 DO cor_as
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW wind_cte0
 CLEAR	
 @  1, 2  SAY "    N§ Reversi¢n : " 
 @  3, 2  SAY "             Mes : " 
 @  3,50  SAY          "Periodo : " 
 @  5, 2  SAY "           Fecha : "
 @  7, 2  SAY "Oficina Giradora.: "
 @  9, 2  SAY "  Tipo Reversi¢n.: "
 @ 11, 2  SAY "     Observaci¢n : " 
 @ 13, 2  SAY "Cta. Tes.P£blico : "
 @ 15, 2  SAY "Cuenta Corriente : "
 @ 17, 2  SAY "      Sub_Cuenta : "
 @ 17, 50 SAY "        N§ SIAF : "
 @ 19, 2  SAY " Doc. Referencia : "
 @ 19, 50 SAY          "N§.Doc. : "
 @ 21, 2 SAY "           Monto : "
@  22,00 SAY PADC(ALLTRIM(vmens02),79,' ') COLOR W+/B
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW wind_cte0
 SELECT REVER
 IF EOF()
    DO Pantalla
    RETURN
 ENDIF
 SCATTER MEMVAR
 ON KEY LABEL F9 DO cor_as
 @  0,50 SAY IIF(m.tipo='M','Por Menores Gastos','Partidas de Ingresos') COLOR SCHEME 02
 @  1,22 SAY m.NumRev
 @  3,22 SAY m.Nummes
 @  3,29 SAY Val_Para(m.NumMes,'FECMES','D',3,12) 
 @  3,60 SAY m.periodo
 @  5,22 SAY m.Fecha
 @  7,22 SAY m.CodOfg
 @  7,29 SAY IIF(!EMPTY(m.codofg),Val_Para(m.codofg,'OFIGIR','D',3,23) ,' ')
 @  9,22 SAY IIF(m.tipo='M',m.TipRev,'              ') 
 @  9,29 SAY IIF(m.tipo='M',Val_Para(m.tiprev,'CODREV','D',3,35),'                                  ')
 @ 11,22 SAY m.Glosa PICTURE '@S56' 
 @ 13,22 SAY m.ctates
 @ 13,38 SAY Val_Fun("Caja","Codctc","Descri",ALLTRIM(m.Ctates))
 @ 15,22 SAY m.CodCtc
 @ 15,38 SAY IIF(!EMPTY(m.codctc),Val_Fun("Caja","Codctc","Descri",ALLTRIM(m.Codctc)) ,' ')
 @ 17,22 SAY m.CodSCta
* @ 17,38 SAY Val_Fun("SubCta","Subcta","Descri",ALLTRIM(m.CodScta)) 
 @ 17,60 SAY NUMSIAF
 @ 19,22 SAY m.TipDoc
 @ 19,29 SAY Val_Para(m.tipdoc,'TIPDOC','D',3,23) 
 @ 19,60 SAY m.NumDoc+'.'+m.mesdoc+'/'+m.perdoc
 @ 21,22 SAY m.Monto   PICTURE "999,999,999,999.99"
 RETURN

PROCEDURE Revis
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
   RETURN
 ENDIF
 vTemp = RECNO()
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10    KEYBOARD CHR(23)
 BROWSE WINDOW wind_cte0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
 NumMes  :H='Mes' ,;
 NumRev  :H='N£mero',;
 X1=IIF(tipo='M','Menores Gastos','Partida de Ingresos') :15    :H='Tipo',;
 Fecha   :H='Fecha',;
 CodCtc  :H='CtaCte' ,;
 CodSCta :H='SubCta' ,; 
 Monto   :H='Monto':P='999,999,999,999.99'
 ON KEY LABEL F10
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO vtemp
 ENDIF
 SHOW MENU mMenu
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
VTEMP=RECNO()
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
ACTIVATE WINDOW standby
@ 1,01 SAY 'Ingrese N£mero : ' GET vRE PICTURE '@!' DEFAULT SPACE(4)
@ 1,30 SAY 'Mes:' GET vMes
READ
DEACTIVATE WINDOW standby
IF EMPTY(vRE) .or. LASTKEY()=27
    RETURN
ELSE
    vRE = PADL(ALLTRIM(vRE),4,'0')
    SEEK ALLTRIM(vMes)+ALLTRIM(vRE)
    IF !FOUND()
       DO standby WITH Vmens04
       GOTO Vtemp
    ELSE
       DO Vista
    ENDIF
ENDIF
RETURN


PROCEDURE Anter
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT REVER
 IF !BOF()
   SKIP -1
 ENDIF
 IF BOF()
   GO TOP
   DO standby WITH "Inicio de archivo: no existe Contratante anterior"
 ELSE
   DO Vista
 ENDIF
 RETURN


PROCEDURE Proxi
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT REVER
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH "Fin de archivo: no existe Contratante siguiente"
   GO BOTTOM
 ELSE
   DO Vista
 ENDIF
 RETURN


PROCEDURE Corri
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF

 IF Estado = '99'
    DO StandBy WITH "El Documento esta Anulado"
    RETURN
 ENDIF

 SCATTER MEMVAR
 ACTIVATE WINDOW wind_cte0
 DO PANTALLA
   @  1,22 SAY m.NumRev
   @  3,22 GET m.NumMes   PICTURE '!!'   VALID Val_Para(m.NUmMes,'FECMES',' ',22,9)  DISABLE
   @  3,60 GET m.periodo  PICTURE '!!'   
   @  5,22 GET m.Fecha    VALID(MONTH(m.fecha)=VAL(m.nummes))
   @  7,22 GET m.CodOfg   PICTURE '!!'   VALID Val_Para(m.CodOfg ,'OFIGIR',' ',22,25)
   @  9,22 GET m.TipRev   PICTURE '!!'   VALID Val_Para(m.TipRev ,'CODREV',' ',22,25)    WHEN m.tipo='M'
   @ 11,22 GET m.Glosa    PICTURE '@S56'  FUNCTION "!"     &&VALID vctc()
   @ 13,22 GET m.Ctates   PICTURE "@!"   VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",m.Ctates,1,13,22) 
   @ 15,22 GET m.CodCtc   PICTURE "@!"   VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",m.CodCTc,1,15,22)    &&AND cheqscta()
   @ 17,22 GET m.CodSCta  PICTURE "@!"   VALID Val_Fun("Subcta","SubCta","SubCta+' '+Descri",m.CodSCTa,1,17,22)
   @ 17,60 GET m.NUMSIAF 
   @ 19,22 GET m.TipDoc   PICTURE "@!"   VALID Val_Para(m.TipDoc,"TIPDOC","C") 
   @ 19,60 GET m.NumDoc   PICTURE "@!" 
   @ 19,65 GET m.Mesdoc   PICTURE '!!'  VALID val_para(m.MesDoc,'FECMES',' ',32,9) 
   @ 19,68 GET m.perdoc
   @ 21,22 GET m.Monto    PICTURE "999,999,999,999.99"
   READ VALID Val_Read()
   m.usuario=wuser_id
 SELE CAJA
 SEEK m.codctc
 w_tipctc=CAJA.Tipo
 SELE Rever
   IF LASTKEY() <> 27
      =concepto()
    IF m.tipo='M'
	    DO Ingap
	ENDIF
      SELECT Rever
      GATHER MEMVAR
   ENDIF
 UNLOCK
 SELECT REVER
 ON KEY LABEL F2 DO VISOBS
 ON KEY LABEL F4 DO imp_rev
 DO Vista                    && Muestra nuevos datos
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
IF escolor
	DEFINE POPUP xcot  FROM 17,55 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP xcot  FROM 17,55 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF xcot PROMPT ' de \<Menores Gastos   '
DEFINE BAR 2 OF xcot PROMPT ' de \<Part. de Ingresos'
ON SELECTION POPUP xcot  DEACTIVATE POPUP
ACTIVATE POPUP xcot
DO CASE
   CASE BAR() = 1
		deme = .T.
		dein = .F.
   CASE BAR() = 2
		deme = .F.
		dein = .T.
   OTHERWISE
		deme = .F.
		dein = .F.
ENDCASE
 RELEASE POPUP xcot

 SELECT REVER
 SCATTER MEMVAR BLANK

 SELECT Parma
 SEEK 'CORRELREVERS'
 m.NumRev = PADL(NumEnt+1,4,'0')
 SELECT REVER
 DO Pantalla
 m.Fecha =DATE()
 m.FecBol=DATE()
 m.tipdoc='R/I'
 @   1,22 GET m.NumRev
 @   3,22 GET m.Nummes   PICTURE '!!'   VALID Val_Para(m.NUmMes,'FECMES',' ',22,9) 
 @   3,60 GET m.periodo
 @   5,22 GET m.Fecha    VALID(MONTH(m.fecha)=VAL(m.nummes))
 @   7,22 GET m.CodOfg   PICTURE '!!'   VALID Val_Para(m.CodOfg  ,'OFIGIR',' ',22,25)
 @   9,22 GET m.TipRev   PICTURE '!!'   VALID Val_Para(m.TipRev ,'CODREV',' ',22,25)    WHEN deme=.T.
 @  11,22 GET m.Glosa    PICTURE '@S56' FUNCTION "!" VALID vctc()
 @  13,22 GET m.Ctates   PICTURE "@!"   VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",m.Ctates,1,13,22) 
 @  15,22 GET m.Codctc   PICTURE "@!"   VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",m.Ctates,1,15,22)      &&AND cheqscta()
 @  17,22 GET m.CodSCta  PICTURE "@!"   VALID Val_Fun("Subcta","SubCta","SubCta+' '+Descri",m.CodSCTa,1,17,22)
 @  17,60 GET m.numsiaf
 @  19,22 GET m.TipDoc   PICTURE "@!"   VALID Val_Para(m.TipDoc,"TIPDOC","C") 
 @  19,60 GET m.NumDoc   PICTURE "@!" 
 @  19,65 GET m.Mesdoc   PICTURE '!!'  VALID val_para(m.MesDoc,'FECMES',' ',32,9) 
 @  19,68 GET m.perdoc
 @  21,22 GET m.Monto
 READ VALID Val_Read()
 m.usuario=wuser_id
 SELE Caja
 SEEK m.codctc
 w_tipctc=caja.Tipo
 SELE Rever
 IF LASTKEY() <> 27
 	IF dein
 	    m.tipo='P'
 	ELSE    
 		m.tipo='M'
 	ENDIF
    SELECT REVER
    =concepto()
    IF m.tipo='M'
	    DO Ingap
	ENDIF
    IF F_appd()
        GATHER MEMVAR
    ENDIF   
    SELECT Parma
    SEEK 'CORRELREVERS'
    REPLACE NumEnt WITH NumEnt + 1
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    GO BOTTOM
 ENDIF
SELECT REVER
ON KEY LABEL F2 DO VISOBS
ON KEY LABEL F4 DO imp_rev
DO VISTA
RETURN


PROCEDURE ANULA
*--------------
 SELECT REVER
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF Estado = '99' 
   DO STANDBY WITH  vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR esta Reversi¢n ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
   REPLACE ESTADO WITH '99'
   REPLACE usuario WITH wuser_id
   DO Vista
 ENDIF
 UNLOCK
 sele astpat
 SEEK ALLTRIM(m.nummes)+m.numREV+alltrim(m.codctc)
    IF FOUND()
    	scan while nummes=alltrim(m.nummes) and numref=m.numREV and codctc=m.codctc and tipdoc="REV" 
		    IF RLOCK() .OR. F_LOCK(1)
    		   DELETE NEXT 1
	   		ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF	    
 SELE REVER    
 RETURN


PROCEDURE Lista  &&Reportes varios
*--------------
SELECT REVER
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
DEFINE WINDOW lis FROM 3,10 TO 18,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1  TO vtocta,vestado,vlista
STORE SPACE(14) TO vCta,vCuenta
STORE SPACE(2)  TO vAno,vMes
STORE SPACE(4)  TO vCli
VCTA=M.CODCTC
VANO=M.NUMMES
VCLI=M.NUMrev
STORE DATE() TO vfecini, vfecfin
@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento;Menores Gastos;Partida Ingresos'
@ 05,01 SAY "     N§ Documento : "
@ 05,22 GET vano    WHEN vlista=1  PICTURE '!!'
@ 05,25 GET vcli    WHEN vlista=1  PICTURE '!!!!' 
@ 07,01 SAY "   Cta. Corriente : "
@ 07,22 GET vcta  WHEN  vlista=1 OR VLISTA=2 OR vlista=3 VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,10,07)
@ 09,01 SAY " Fecha de Emisi¢n : "
@ 10,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7   WHEN vlista>1 
@ 10,32 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7 WHEN vlista>1
@ 12,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   SELECT Rever
   GO TOP
   vInd = SYS(3) + '.IDX'
   vbas = SYS(3) + '.DBF'
   DEACTIVATE WINDOW STANDBY
   DO CASE
      CASE vLista=2 OR vLista=3
       	  IF vLista=2
       	     SET FILT TO Rever.Tipo='M' AND Rever.codctc=vcta AND BETWEEN(Rever.fecha,vfecini,vfecfin) 
       	     IF EOF()
       	        DO StanDBY with VMENS08
				SELECT REVER
				SET FILT TO
				IF vTemp>0
				   GO vTemp
				ENDIF
				DO vista
				RETURN
			 ENDIF	
             DO REPORTE WITH 2,"LisRev2",' Reporte de Reversiones por Menores Gastos'
          ELSE
       	     SET FILT TO Rever.Tipo='P' AND Rever.codctc=vcta AND BETWEEN(Rever.fecha,vfecini,vfecfin) 
       	     IF EOF()
       	        DO StanDBY with VMENS08
				SELECT REVER
				SET FILT TO
				IF vTemp>0
				   GO vTemp
				ENDIF
				DO vista
				RETURN
			 ENDIF	
      	     DO REPORTE WITH 2,"LisRev2",' Reporte de Reversiones por Partida de Ingresos'
          ENDIF  
      CASE vLista=1
	      SEEK ALLT(vano)+vcli
		  IF FOUND()
		      COPY NEXT 1 TO (vbas)
		      USE (Vbas) IN 0 ALIAS RE
		      SELE RE
		      INDEX ON RE.NumMes+RE.Numrev+RE.codctc TO (vind)
*       	  SET RELATION TO RE.NumMes+RE.Numrev+RE.codctc INTO AstPat
	       	  SET MEMOWIDTH TO 75
		   	  DO REPORTE WITH 2,"T-6",' Reporte de Reversiones '
*    	  SET RELATION OFF INTO astpat
	  	      USE
	  	      ERASE (vbas)
	      ELSE
              DO Standby WITH vmens08
          ENDIF         
   	  ENDCASE
ENDIF
SELECT REVER
SET ORDER TO TAG REVER1
IF vTemp>0
   GO vTemp
ENDIF
DO vista
RETURN


PROCEDURE ingap   && Ingresa asientos patrimoniales
*--------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgrIte
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numrev+m.codctc
IF !FOUND()
	DO agrite
ENDIF
BROWSE NOAPPEND NODELETE NOMENU WINDOW WIND_2 KEY ALLTRIM(m.nummes)+m.numrev+m.codctc FIELDS ;
	codcta :H='Cuenta',;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999.99'

STORE 0 TO vdebe, vhaber ,vRet
SEEK ALLTRIM(m.nummes)+m.numrev+m.codctc
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numrev=numrev AND m.codctc=codctc
IF IIF(TipCta='D',MtoDeb,MtoHab)=0
 	dele next 1
ELSE
	vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
	vhaber= vhaber + IIF(tipcta='H',mtohab,0)
ENDIF		
ENDSCAN

IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
ON KEY
SELECT REVER

ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5

UNLOCK
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
 
RETURN


PROCEDURE agrite
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes  WITH m.nummes,;
	        numref  WITH m.numrev,;
	        tipdoc  WITH 'REV',;
	        codctc  WITH m.codctc,;
	        fecha   WITH m.Fecha,;
	        TIPCTC WITH W_TIPCTC,;
	        Periodo WITH m.periodo
	        
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT astpat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN


FUNCTION ValRes
*--------------
 SELECT REVER
 vTemp=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK vMes+vCli
 IF !FOUND()
    SET ORDER TO Rever1 
    HIDE MENU mMenu
    ACTIVATE SCREEN
    vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo    
    ON KEY LABEL F10 KEYBOARD CHR(23)    
    BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
    NumMes :H='Mes',;
    NumRev :H=' N§ Rev.' ,;
    Fecha  :H='Fecha' ,;
    Monto :H='Total Reversi¢n',;
    Observ :H='Observaci¢n',;
    CodCtc :H='CtaCte'

    vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo
    IF LASTKEY()=27
       GO Vtemp
    ENDIF

    SHOW MENU mMenu
    ON KEY LABEL F10
    SET RELATION TO
    GO Vtemp
 ENDIF
 vMes=NumMes
 vCli=NumRev

 RETURN .t.


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  ON KEY LABEL F2
  ON KEY LABEL F4
  ON KEY LABEL F9
  ON KEY
  RETURN


PROCEDURE Termina
*----------------
  CLOSE DATA
  ON KEY LABEL F2
  RELEASE    WINDOW wind_cte0
  RELEASE    WINDOW wind_cte1
  RELEASE    MENU   mMenu
  ACTIVATE   SCREEN
  RESTORE    SCREEN FROM Principal
  RETURN

PROCEDURE valfecha
*-----------------
an=RIGHT(STR(YEAR(DATE()),4),2)
vme = VAL(m.nummes)+1
me = PADL(ALLTRIM(STR(vme,2)),2,'0')
vfec = '01/&ME/&AN'
m.feccp = CTOD(vfec) - 1
RETURN .T.


*FUNCTION cheqscta
**---------------
SELE CAJA
SEEK m.codctc
IF FOUND()
	m.codscta=ALLT(Caja.CodScta)
ENDIF	
RETURN 


FUNCTION Vctc
**---------------
SELE CAJA
LOCATE FOR clase='T'
IF FOUND()
	m.ctates=ALLT(Caja.Codctc)
ENDIF	
RETURN 

FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)


FUNCTION concepto
*---------------
vAlias = ALIAS()
SELECT Rever
SET MEMOWIDTH TO 50
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Concepto")
   DEFINE WINDOW Concepto FROM 03,14 TO 20,66 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF
IF WVISIBLE("Concepto")
   ACTIVATE WINDOW concepto SAME
ELSE
   ACTIVATE WINDOW concepto NOSHOW
ENDIF
MODIFY MEMO concepto WINDOW concepto
IF !WVISIBLE("Concepto")
   ACTIVATE WINDOW concepto
ENDIF
RELEASE WINDOW concepto
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci¢n '
ENDIF
SELECT (vAlias)
RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
SELECT Rever
IF !WEXIST("concepto")
   DEFINE WINDOW concepto FROM 03,14 TO 20,66 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("concepto")
   ACTIVATE WINDOW concepto SAME
ELSE
   ACTIVATE WINDOW concepto NOSHOW
ENDIF

MODIFY MEMO concepto NOEDIT WINDOW concepto

IF !WVISIBLE("concepto")
   ACTIVATE WINDOW concepto
ENDIF
RELEASE WINDOW concepto
RETURN .T.


PROCEDURE imp_REV
*---------------
   vInd = SYS(3) + '.IDX'
   vbas = SYS(3) + '.DBF'
   SEEK ALLT(m.nummes)+ALLT(m.numrev)
		  IF FOUND()
		      COPY NEXT 1 TO (vbas)
		      USE (Vbas) IN 0 ALIAS RE
		      SELE RE
		      INDEX ON RE.NumMes+RE.Numrev+RE.codctc TO (vind)
	       	  SET RELATION TO RE.NumMes+RE.Numrev+RE.codctc INTO AstPat
	       	  SET MEMOWIDTH TO 75
		   	  DO REPORTE WITH 2,"T-6",' Reporte de Reversiones '
	    	  SET RELATION OFF INTO astpat
	  	      USE
	  	      ERASE (vbas)
	      ELSE
              DO Standby WITH vmens08
          ENDIF         
	SELECT REVER
	SET ORDER TO TAG REVER1
	DO vista
RETURN	


PROCEDURE cor_as  &&Corrige Asientos Patrimoniales
*---------------
IF m.tipo='M'
SELE Caja
SEEK ALLTRiM(m.CodCtc)
IF FOUND()
   w_TipCtc=CAJA.TIPO
ENDIF
SELE rever
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgrIte
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numrev+m.codctc
IF !FOUND()
	DO agrite
ENDIF
BROWSE NOAPPEND NODELETE NOMENU WINDOW WIND_2 KEY ALLTRIM(m.nummes)+m.numrev+m.codctc FIELDS ;
	codcta :H='Cuenta',;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999.99'

STORE 0 TO vdebe, vhaber ,vRet
SEEK ALLTRIM(m.nummes)+m.numrev+m.codctc
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numrev=numrev AND m.codctc=codctc
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
ON KEY LABEL F4 DO imp_rev
SELECT REVER

ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5

UNLOCK
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
SELE Rever
ENDIF
DO vista
RETURN