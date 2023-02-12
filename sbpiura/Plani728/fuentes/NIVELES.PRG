**----------------------------------------------------------------------------------
** PERMAE.PRG  Administra la lista de Personal
** 03/1999
** SISTEMA SBPP
** PIURA
** A.S. SEGUNDO CORREA SANDOVAL
**---------------------------------------------------------------------------------------------
USE  parmae   IN 1   ORDER TAG parmae1        ALIAS Parma
USE  maeniv   IN 2   ORDER TAG maeniv1        ALIAS Nivel
USE  iteniv   IN 3   ORDER TAG iteniv1        ALIAS Iteniv

SELECT nivel
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

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

 DEFINE WINDOW wind_cte0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE ' TABLA DE NIVELES DE LA S.B.P.' COLOR SCHEME 10 ;
 FOOTER ''

 DEFINE WINDOW wind_cte1 FROM 00,00 TO 11,79 DOUBLE ;
 TITLE ' Revisi¢n de Niveles de la S.B.P' COLOR SCHEME 10

 DEFINE WINDOW wind_2P   FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle : Aumentos(+) , Descuentos (-)' COLOR SCHEME 10 ;
 FOOTER '± ®Esc¯ Sale ±'

 DEFINE WINDOW wind_2Pa  FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' Niveles : Aumentos / Descuentos ' COLOR SCHEME 10 ;

 DEFINE WINDOW wind_2Pb  FROM 07,02 TO 18,78 DOUBLE ;
 TITLE ' Detalle : Ordenes Compra ' COLOR SCHEME 10 ;
 FOOTER '± ®Esc¯ Sale ±'

 DEFINE WINDOW wind_2Pc  FROM 07,02 TO 18,78 DOUBLE ;
 TITLE ' Detalle : Ordenes Servicio ' COLOR SCHEME 10 ;
 FOOTER '± ®Esc¯ Sale ±'

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
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi

 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW wind_cte1

 @  1, 2 SAY "Codigo del Nivel : "
 @  2, 2 SAY "     Descripci¢n : "
 @  4, 2 SAY "  Total Ingresos : "
 @  4,40 SAY "         L¡quido : "
 @  5, 2 SAY "   Total Egresos : "
 @  7, 2 SAY "  Empleador(CEM) : "
 @  8, 2 SAY "  Empleador(IES) : "

RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW wind_cte1
 SELECT Nivel
 IF EOF()
    DO Pantalla
    RETURN
 ENDIF
 SCATTER MEMVAR
 @  1, 22 SAY m.niv_per
 @  2, 22 SAY val_para(m.niv_per,'NIVPER','D',22)
 @  4, 22 SAY m.tot_ing
 @  4, 62 SAY (m.tot_ing - m.tot_des) PICTURE '999,999.99' COLOR SCHEME 15
 @  5, 22 SAY m.Tot_des
 @  7, 22 SAY m.apo_01 PICTURE '999.99'
 @  8, 22 SAY m.apo_02 PICTURE '999.99'
 @  7, 2 SAY "  Empleador(CEM) : " 
 DO VISTA_HIJO
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

 ON KEY LABEL F10  KEYBOARD CHR(23)

 BROWSE WINDOW wind_cte1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
 	niv_per :H='Nivel' ,;
 	xx=val_para(niv_per,'NIVPER','D',22) :H='Descripci¢n'

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
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 ACTIVATE WINDOW standby
 STORE space(2) TO Vbusca
 @ 1,2 SAY 'C¢digo: ' GET Vbusca PICT '99999999' && COLOR SCHEME 7
 READ
 DEACTIVATE WINDOW standby
 IF LASTKEY() # 27
    SEEK Vbusca
    IF .NOT. FOUND()
        DO standby WITH "Dicho Nivel no fue encontrado"
     	GOTO Vtemp
   	ELSE
     	DO Vista
   	ENDIF
 ENDIF
 DO VISTA
 RETURN

PROCEDURE Anter
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT nivel
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
 SELECT Nivel
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
 SCATTER MEMVAR
 vtem= recno() 
 DO PANTALLA
 @  1, 22 GET m.niv_per PICTURE '99' VALID Val_para(m.niv_per,'NIVPER','C', 22)
 @  2, 22 SAY val_para(m.niv_per,'NIVPER','D',22)
 @  4, 22 SAY m.tot_ing
 @  5, 22 SAY m.Tot_des
 @  7, 22 SAY m.apo_01 PICTURE '999.99'
 @  8, 22 SAY m.apo_02 PICTURE '999.99'
 READ VALID Val_Read()
 IF LASTKEY() <> 27
    select Nivel
    GATHER MEMVAR
    =Trabaja_Hijo()
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    GO BOTTOM
 ENDIF
 UNLOCK
 SELECT nivel
 DO Vista                    && Muestra nuevos datos
 RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT Nivel
 SCATTER MEMVAR BLANK
 DO Pantalla
 @  1, 22 GET m.niv_per PICTURE '99' VALID Val_para(m.niv_per,'NIVPER','C', 22)
 @  2, 22 SAY val_para(m.niv_per,'NIVPER','D',22)
 @  4, 22 SAY m.tot_ing
 @  5, 22 SAY m.Tot_des
 @  7, 22 SAY m.apo_01 PICTURE '999.99'
 @  8, 22 SAY m.apo_02 PICTURE '999.99'
 READ VALID Val_Read()
 IF LASTKEY() <> 27
    SELECT Nivel
    IF F_appd()
		GATHER MEMVAR
		= Trabaja_hijo()
    endif    
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    GO BOTTOM
 ENDIF
 SELECT Nivel
 DO VISTA
 RETURN

PROCEDURE Elimi
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
   RETURN
 ENDIF
 DELETE NEXT 1
 UNLOCK
 DO Vista
 RETURN

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN

PROCEDURE Termina
*----------------
  CLOSE DATA
  RELEASE    MENU   mMenu
  ACTIVATE   SCREEN
  RESTORE    SCREEN FROM Principal
  RETURN

PROCEDURE Lista
*--------------
SELECT PERMAE
IF EOF()
   RETURN
ENDIF
VTEMP = RECNO()
go top
DO LisPER
SELECT pERmae
GO VTEMP
RETURN

PROCEDURE LisPER
*---------------
IF EOF()
   DO STANDBY WITH vMens08
   RETURN
ENDIF
vTemP = RECNO()

DEFINE WINDOW LIS FROM 2,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Proveedores' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1        TO vToCli,vOrden,vTipPro,vEstado,vTipLis
VCODPER = SPAC(8)

@ 01,01 SAY "   Total Personal : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
@ 03,01 SAY "         Personal : "
@ 03,22 GET VCODPER    WHEN VTOCLI=2  PICTURE '99999999' VALID Val_busper(vcodper,1)

@ 05,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ L.E.;Cta. Ahor.;Ape_Nom'  WHEN VTOCLI =  1

@ 08,01 SAY "           Estado : " GET vEstado  FUNCTION '^ Todos;Aptos;Anulados' WHEN VTOCLI =  1

@ 17,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   IF VTOCLI = 1
	  DO CASE   	
	  	SET ORDER TO vorder
      	CASE vEstado=2
      		SET FILTER TO estado='00'
        CASE vEstado=3
      		SET FILTER TO estado='99'
      ENDCASE                 
   ELSE 
   	  SET FILTER TO Lib_ele=vcodper
   ENDIF
   DEACTIVATE WINDOW STANDBY
   IF !EOF()
		do reporte  with 2, 'LISPER', ' Lista del Personal',1,.F.,.T.
   ELSE
      DO STANDBY WITH 'Archivo Vac¡o ...'
   ENDIF
   SET FILTER TO
ENDIF
SELECT PERMAE
SET ORDE TO 2
GO VTEMP
RETURN



PROCEDURE Trabaja_Hijo
*---------------------
 AS=ALIAS()
 ACTIVATE SCREEN
 hide menu mmenu
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO Agreg_niv
 ON KEY LABEL F8  DO Elimi_niv
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELE iteniv
 SEEK m.niv_per

 if !found()
    DO Agreg_Niv
 endif

 BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2PA KEY m.niv_per FIELD ;
    Tip_adn : H='+/-',;
    cod_adn : H='Aum/Desc':V=Val_para(cod_adn,IIF(Tip_adn='+','AUMLEY','DESLEY'),'cod_adn') AND valores(),;
	xx=Val_para(cod_adn,IIF(Tip_adn='+','AUMLEY','DESLEY'),'Z',15):H='Descripci¢n',;
    tot_adn : H='Valor',;
    porcent : H='Porcentaje':R,;
    suJ_des : H='Suj. a Desc'

 DEACTIVATE WINDOW WIND_2Pa
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 store 0 to vaumento,vdescuento, vsuj_des,vsuj_desP
 store 0 to vaumentoP,vdescuentoP ,v26
 SELE iteniv
 SEEK m.niv_per
 SCAN WHILE m.niv_per=niv_per
 	IF Tip_adn='+'
 	   IF porcent='N'
 	   	  IF cod_adn <> '26'
 	   	  	vaumento   = vaumento   + tot_adn
 	   	  ELSE
 	   	   	V26 = tot_adn
 	   	  ENDIF
 	      IF suj_des = 'N'
 	      	 vsuj_des = vsuj_des + tot_adn
 	      ENDIF
 	   ELSE
   	   	  vaumentop   = vaumentoP   + tot_adn
   	      IF suj_des = 'N'
      	 	 vsuj_desp = vsuj_desp + tot_adn
 	      ENDIF
 	   ENDIF
 	ELSE 
 	   IF porcent='N'
 	   	  vdescuento = vdescuento + tot_adn
 	   ELSE
   	   	  vdescuento   = vdescuento + tot_adn
 	   ENDIF
	ENDIF
	
 ENDSCAN
 vaumento   = vaumento + (vaumento*vaumentop)/100
 vdescuento = (((vaumento+V26)-vsuj_des)*vdescuento)/100
 STORE 0 TO vEmpl01, vEmpl02
 SELECT PARMA
 seek 'APORTA'
 IF FOUND()
 	SCAN WHILE tipo = 'APORTA'
 		IF allt(codigo) = '01'
 			vEmpl01 = vEmpl01 + numdec
 		ELSE
 			vEmpl02 = vEmpl02 + numdec
 		ENDIF
 	ENDSCAN
	vEmpl01 = ((vaumento-vsuj_des)*vEmpl01)/100
	vEmpl02 = ((vaumento-vsuj_des)*vEmpl02)/100
 ENDIF
 UNLOCK ALL
 SHOW MENU MMENU
 ON KEY LABEL F5
 ON KEY LABEL F8
 ON KEY LABEL F10

 SELECT (AS)
 REPLACE tot_ing WITH vaumento+v26
 REPLACE tot_des WITH vdescuento
 REPLACE apo_01  WITH vEmpl01 
 REPLACE apo_02  WITH vEmpl02
 RETURN


PROCEDURE Vista_hIJO
*---------------------
 hide popup all
 SELECT iteniv
 SEEK m.niv_per
 BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.NIV_PER  TIMEOUT 0.001 ;
   WINDOW Wind_2Pa;
   FIELDS;
    Tip_adn : H='+/-':P="@M '+,-'" :R,;
    cod_adn : H='Aum/Desc':R,;
    xx=Val_para(cod_adn,IIF(Tip_adn='+','AUMLEY','DESLEY'),'Z',20):H='Descripci¢n',;
    tot_adn : H='Valor':R,;
    porcent : H='Porcentaje':R

 SELECT Nivel
 RETURN

PROCEDURE Agreg_niv
*-----------------
IF F_appd()
  REPLACE niv_per WITH m.niv_per, SUJ_DES WITH 'S'
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Elimi_niv
*-------------------
PRIVATE VFUN
VFUN=.F.
SELEC iteniv
delete next 1
VFUN=.T.
RETURN VFUN

FUNCTION valores
*---------------
IF parma.numdec>0
	REPLACE tot_adn with parma.numdec, porcent with 'S'
ELSE
	REPLACE tot_adn with parma.nument, porcent with 'N'
ENDIF
RETURN .T.