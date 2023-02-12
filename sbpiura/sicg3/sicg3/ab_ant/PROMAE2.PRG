**----------------------------------------------------------------------------------
** PROMAE2.PRG  Administra la lista de Proveedores
** 08- 07 -97
** PIURA
**---------------------------------------------------------------------------------------------

USE  parmae   IN 1   ORDER TAG parmae1        ALIAS Parma
USE  promae   IN 2   ORDER TAG promae1        ALIAS Promae
USE  prvaux   IN 3   ORDER TAG prvaux1        ALIAS aux
USE  artmae   IN 4   ORDER TAG artmae1        ALIAS produ
USE  auxil    IN 5   ORDER TAG auxil1         ALIAS auxil
USE  ordcom   IN 6   ORDER TAG ordcom4        ALIAS orden
USE  ordser   IN 7   ORDER TAG ordser6        ALIAS ordser

SELECT PROMAE
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
 TITLE ' CATALOGO DE PROVEEDORES' COLOR SCHEME 10 ;
 FOOTER '± ®F2¯ Muestra Clasificaci¢n ± ®F11¯ O/C ± ®F12¯ O/S ±'

 DEFINE WINDOW wind_cte1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE ' Revisi¢n de Proveedores ' COLOR SCHEME 10

 DEFINE WINDOW wind_2P   FROM 12,10 TO 22,69 DOUBLE ;
 TITLE ' Detalle : Clasificaci¢n ' COLOR SCHEME 10 ;
 FOOTER '± ®Esc¯ Sale ±'

 DEFINE WINDOW wind_2Pa  FROM 14,04 TO 22,75 DOUBLE ;
 TITLE ' Clasificaci¢n del Proveedor ' COLOR SCHEME 10 ;

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
 ACTIVATE WINDOW wind_cte0
 @  0, 2 SAY "          Estado : "
 @  1, 2 SAY "          C¢digo : "
 @  1,23 SAY "-"
 @  1,40 SAY "   Fecha Ingreso : "

 @  3, 2 SAY "          Nombre : "
 @  4, 2 SAY "       Direcci¢n : "
 @  5, 2 SAY "        Tel‚fono : "
 @  5,40 SAY "             Fax : "
 @  6, 2 SAY "           R.U.C.: "
 @  6,40 SAY "           Sigla : "

 @  8, 2 SAY "   Lic.Municipal : "
 @  8,40 SAY "   Reg.Prom.PYME : "
 @  9, 2 SAY "   Reg.Unificado : "
 @  9,40 SAY "  Reg.Industrial : "
 @ 10, 2 SAY "      Testimonio : "
 @ 10,40 SAY "      C.Apertura : "

 @ 12, 2 SAY "  Nombre R.Legal : "
 @ 13, 2 SAY "  Direcc.R.Legal : "
 @ 14, 2 SAY "     RUC R.Legal : "
 @ 15, 2 SAY "    L.E. R.Legal : "
 @ 16, 2 SAY "  Telef. R.Legal : "

 @ 18, 2 SAY "  Tipo Operaci¢n : "
 @ 19, 2 SAY "   Act.Econ¢mica : "

 @ 20, 2 SAY "          Ambito : "
 @ 21, 2 SAY "          M‚rito : "
 @ 21,40 SAY "Doc. Legalizados : " 
RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW wind_cte0
 ON KEY LABEL F12 DO SERVI
 ON KEY LABEL F11 DO ORDEN

 SELECT Promae
 IF EOF()
    DO Pantalla
    RETURN
 ENDIF
 ON KEY LABEL F2 DO VISTA_PRO
 SCATTER MEMVAR
 @  0,60 SAY Verest(m.Estado)
*@  0,22 SAY m.regul
 @  1,22 SAY m.TipPro
 @  1,24 SAY m.Codprv
 @  1,60 SAY m.Fecing

 @  3,22 SAY m.Nompro
 @  4,22 SAY m.Dirpro
 @  5,22 SAY m.Telpro
 @  5,60 SAY m.Faxpro
 @  6,22 SAY m.Numruc
* @  6,60 SAY m.Sigla

 @  8,22 SAY m.LicMun
 @  8,60 SAY m.RegPPYME
 @  9,22 SAY m.RegUnf
 @  9,60 SAY m.RegInd
 @ 10,22 SAY m.Test
 @ 10,60 SAY m.Apert

 @ 12,22 SAY m.Rl_nom
 @ 13,22 SAY m.Rl_dir
 @ 14,22 SAY m.Rl_ruc
 @ 15,22 SAY m.Rl_le
 @ 16,22 SAY m.Rl_tel

 @ 18,22 SAY Val_para (m.TipOpe,'TIPOPE','V')
 @ 19,22 SAY Val_para (m.Acteco,'ACTECO','V')
 @ 20,22 SAY Val_para (m.Ambito,'AMBITO','V')
 @ 21,22 SAY m.Clasif
 @ 21,60 SAY m.Legali
 
 RETURN


PROCEDURE Revis
*--------------
 ON KEY LABEL F2 
 ON KEY LABEL F3 DO BUSCA_P
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
   RETURN
 ENDIF
 vTemp = RECNO()
 IF escolor
    DEFINE POPUP Busmenu FROM 15,50  SHADOW COLOR &L_COL
 ELSE
    DEFINE POPUP Busmenu FROM 15,50  COLOR SCHEME C_POPUP
 ENDIF
 DEFINE BAR 1 OF Busmenu PROMPT ' ordenado por:  \<C¢digo  '
 DEFINE BAR 2 OF Busmenu PROMPT ' ordenado por:  \<Nombre  '
 DEFINE BAR 3 OF Busmenu PROMPT ' ordenado por:  \<R.U.C   '
 DEFINE BAR 4 OF Busmenu PROMPT ' ordenado por:  representante \<Legal '
 ON SELECTION POPUP Busmenu DEACTIVATE POPUP
 ACTIVATE POPUP Busmenu
 DO CASE
     CASE BAR()=1
          set orde to 1
     CASE BAR()=2
          set orde to 2
     CASE BAR()=3
          set orde to 3
     CASE BAR()=4
          set orde to 4
 ENDCASE     
 IF LASTKEY()=27
    SET ORDE TO 1
    DO VISTA
    RETURN
 ENDIF     
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo

 ON KEY LABEL F10  KEYBOARD CHR(23)

 BROWSE WINDOW wind_cte1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
 tippro  :H='T' ,;
 CodPrv  :H='C¢digo' ,;
 NomPro  :H='Nombre',;
 Dirpro  :H='Direcci¢n',;
 NumRuc  :H='RUC' ,;
 rl_nom  :H='Representante Legal'

 ON KEY LABEL F10

 SET ORDER TO 1
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 IF LASTKEY()=27
   GOTO vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F3 
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 IF escolor
    DEFINE POPUP Busmenu FROM 15,54  SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP Busmenu FROM 15,54  COLOR SCHEME C_POPUP
  ENDIF
  
  DEFINE BAR 1 OF Busmenu PROMPT ' por \<C¢digo  '
  DEFINE BAR 2 OF Busmenu PROMPT ' por \<Nombre  '
  DEFINE BAR 3 OF Busmenu PROMPT ' por \<R.U.C   '
  DEFINE BAR 4 OF Busmenu PROMPT ' por repres. \<Leg '
  ON SELECTION POPUP Busmenu DEACTIVATE POPUP

  ACTIVATE POPUP Busmenu
  SELECT PROMAE
  Vtemp = RECNO()
  DO CASE
     CASE BAR()=1
       ACTIVATE WINDOW standby
       STORE '0000' TO Vbusca
       @ 1,2 SAY 'C¢digo: ' GET Vbusca PICT '!!!!' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = UPPER(ALLTRIM(vbusca))
       ENDIF
     CASE BAR() = 2
       ACTIVATE WINDOW standby
       STORE SPACE(40) TO Vbusca
       @ 1,1 SAY ' Nombre: ' GET Vbusca PICTURE '@S30' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = TRIM(UPPER(vBusca))
          SET ORDER TO 2
       ENDIF
     CASE BAR() = 3
       ACTIVATE WINDOW standby
       STORE SPACE(8) TO Vbusca
       @ 1,1 SAY ' N§ RUC: ' GET Vbusca PICTURE '!!!!!!!!' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = TRIM(UPPER(vBusca))
          SET ORDER TO 3
       ENDIF
     CASE BAR() = 4       
       ACTIVATE WINDOW standby
       STORE SPACE(40) TO Vbusca
       @ 1,1 SAY ' Nombre Rep.Legal: ' GET Vbusca PICTURE '@S20' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = TRIM(UPPER(vBusca))
          SET ORDER TO 4
       ENDIF
       
     OTHERWISE
       RETURN
 ENDCASE

 IF EMPTY( Vbusca ) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK Vbusca
   IF .NOT. FOUND()
     DO standby WITH "Dicho Proveedor no fue encontrado"
     GOTO Vtemp
   ELSE
     DO Vista
   ENDIF
 ENDIF
 SET ORDER TO 1
 DO VISTA
 RETURN


PROCEDURE Busca_P            && Realiza b£squeda directa
*--------------
 AS=ORDER()
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 IF escolor
    DEFINE POPUP Busmenu FROM 17,54  SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP Busmenu FROM 17,54  COLOR SCHEME C_POPUP
  ENDIF
  
  DEFINE BAR 1 OF Busmenu PROMPT '  \<C¢digo  '
  DEFINE BAR 2 OF Busmenu PROMPT '  \<Nombre  '
  DEFINE BAR 3 OF Busmenu PROMPT '  \<R.U.C   '

  ON SELECTION POPUP Busmenu DEACTIVATE POPUP

  ACTIVATE POPUP Busmenu
  SELECT PROMAE
  Vtemp = RECNO()
  DO CASE
     CASE BAR()=1
       SET ORDE TO 1
       ACTIVATE WINDOW standby
       STORE '0000' TO Vbusca
       @ 1,2 SAY 'C¢digo: ' GET Vbusca PICT '!!!!' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = UPPER(ALLTRIM(vbusca))
       ENDIF
     CASE BAR() = 2
       ACTIVATE WINDOW standby
       STORE SPACE(40) TO Vbusca
       @ 1,1 SAY ' Nombre: ' GET Vbusca PICTURE '@S30' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = TRIM(UPPER(vBusca))
          SET ORDER TO 2
       ENDIF
     CASE BAR() = 3
       ACTIVATE WINDOW standby
       STORE SPACE(8) TO Vbusca
       @ 1,1 SAY ' N§ RUC: ' GET Vbusca PICTURE '!!!!!!!!' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = TRIM(UPPER(vBusca))
          SET ORDER TO 3
       ENDIF
     OTHERWISE
       RETURN
    ENDCASE

 IF EMPTY( Vbusca ) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK Vbusca
   IF .NOT. FOUND()
     DO standby WITH "Dicho Proveedor no fue encontrado"
     GOTO Vtemp
   ENDIF
 ENDIF
 SET ORDER TO AS
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo

 RETURN



PROCEDURE Anter
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT Promae
 IF !BOF()
   SKIP -1
 ENDIF
 IF BOF()
   GO TOP
   DO standby WITH "Inicio de archivo: no existe Proveedor anterior"
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
 SELECT Promae
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH "Fin de archivo: no existe Proveedor siguiente"
   GO BOTTOM
 ELSE
   DO Vista
 ENDIF
RETURN



PROCEDURE Corri
*--------------
*OK=VE_PASSW('MRA')    
*IF !OK
*   RETURN
*ENDIF 
  
 ON KEY LABEL F2
 ON KEY LABEL F11
 ON KEY LABEL F12

 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF

 IF Estado = 'AN'
   * El Contratista esta anulado
   if !yesno('El Proveedor esta Anulado,desea continuar?')
      RETURN
   endif   
 ENDIF

 SCATTER MEMVAR
 vtem= recno() 
 ACTIVATE WINDOW wind_cte0
 DO PANTALLA
* @  0,22 GET m.regul  PICTURE '@M û, '
 @  1,22 GET m.TipPro PICTURE '!' Valid Val_para (m.TipPro ,'TIPPRO','C',22,20)
 @  1,24 GET m.Codprv 
 @  1,60 GET m.Fecing

 @  3,22 GET m.Nompro
 @  4,22 GET m.Dirpro
 @  5,22 GET m.Telpro
 @  5,60 GET m.Faxpro
 @  6,22 GET m.Numruc VALID NUMRUC()
* @  6,60 GET m.Sigla
 @  8,22 GET m.LicMun
 @  8,60 GET m.RegPPYME	FUNCTION "M NO,SI"
 @  9,22 GET m.RegUnf
 @  9,60 GET m.RegInd
 @ 10,22 GET m.Test
 @ 10,60 GET m.Apert

 @ 12,22 GET m.Rl_nom
 @ 13,22 GET m.Rl_dir
 @ 14,22 GET m.Rl_ruc
 @ 15,22 GET m.Rl_le
 @ 16,22 GET m.Rl_tel

 @ 18,22 GET m.TipOpe PICTURE '!!!!!'  VALID Val_para (m.TipOpe,'TIPOPE',' ',22,20)
 @ 19,22 GET m.Acteco PICTURE '!!!!!!' VALID Val_para (m.Acteco,'ACTECO',' ',22,20)
 @ 20,22 GET m.Ambito PICTURE '!!!'    VALID Val_para (m.Ambito,'AMBITO',' ',22,25)
 @ 21,22 GET m.Clasif PICTURE '!'
 @ 21,60 GET m.Legali PICTURE '@M û, '
    
 READ VALID Val_Read()
 IF LASTKEY() <> 27
    SELECT AUX
    DO WHILE .T.
       Ok = Trabaja_prov()
       IF LASTKEY() # 27
          IF YESNO('¨ Conforme la modificaci¢n?')
             EXIT
          ENDIF
       ELSE
          IF YESNO('¨ Cancela la modificaci¢n ?')
             ok = .F.
             EXIT
          ENDIF
       ENDIF
   ENDDO
   IF Ok .AND. LASTKEY() # 27
      select promae
      m.Giro = IIF(LEFT(m.Acteco,2)='PJ','JURIDICA','NATURAL')
      DO CASE 
   	     CASE m.GIRO='JURIDICA'
 	    	      DO CASE
 	    	          CASE !EMPTY(m.TEST) AND !EMPTY(m.RL_LE) AND !EMPTY(m.REGUNF) AND !EMPTY(m.NUMRUC) AND m.Legali='û'
 	    	                m.Estado = 'VG'
 	    	          CASE  EMPTY(m.TEST) AND  EMPTY(m.RL_LE) AND  EMPTY(m.REGUNF) AND  EMPTY(m.NUMRUC) 
 	    	                m.Estado = 'PD'
 	    	          OTHER       
 	    	          		m.Estado = 'RG'
 	    	      ENDCASE
         CASE m.GIRO='NATURAL' 
               DO CASE 
                  CASE !EMPTY(m.RL_LE) AND !EMPTY(m.REGUNF) AND !EMPTY(m.NUMRUC)  AND m.Legali='û'
 	   	                m.Estado = 'VG'    	              
                  CASE  EMPTY(m.RL_LE) AND  EMPTY(m.REGUNF) AND  EMPTY(m.NUMRUC) 
 	   	                m.Estado = 'PD'       	              
 	   	          OTHER      
 	   	          		m.Estado = 'RG' 	    	          
               ENDCASE   
	     CASE EMPTY(m.GIRO)
              m.Estado = 'PD'
      ENDCASE         
      SELECT PROMAE
      if recno() <> vtem
*         do standby with 'Error..!'
         GO VTEM
      ENDIF   
     * else    
         GATHER MEMVAR
         SCATTER MEMVAR
         SELECT auxil
         SEEK '20'+m.codprv
         if found()
	         if rlock()
	         	replace descri with m.nompro,direccion with m.dirpro,codigo with m.CodPrv
	         endif	
	         GATHER MEMVAR
	     ENDIF    
      *endif   
   ENDIF
 ENDIF
* ELSE
*    DO STANDBY WITH 'Proceso cancelado'
*    GO BOTTOM
* ENDIF
 UNLOCK
 SELECT PROMAE
 DO Vista                    && Muestra nuevos datos
 RETURN

PROCEDURE ORDEN
***************
private ali,vkey
ali=alias()
vkey = SUBSTR(ALLTRIM(STR(YEAR(FECING))),3,2)+promae.codprv
select orden
SEEK vkey
if !found()
	do standby with 'No registra Ordenes de Compra'
 else
	BROWSE ; 
	NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY vkey ;
	WINDOW Wind_2pb ;
	FIELDS;
	numoc       : H= ' O/C' ,;
	Codfte      : H= 'Fte'  ,;
	fecoc       : H= 'FecOc',;
	fecdesp     : H= 'Liquid' ,;
	valtot      : H= 'Total ' ,;
	user        : H= 'Ingreso' ,;
	user_fc     : H= 'FecIng'
endif
sele (ali)
do vista
return



PROCEDURE SERVI
***************
private ali,vkey
ali=alias()
vkey = SUBSTR(ALLTRIM(STR(YEAR(FECING))),3,2)+promae.codprv
select ordser
SEEK vkey
if !found()
	do standby with 'No registra Ordenes de Servicios'
 else
	BROWSE ; 
		NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY vkey ;
		WINDOW Wind_2pC ;
		FIELDS;
		numos       : H= ' O/S' ,;
		Codfte      : H= 'Fte'  ,;
		fecos       : H= 'FecOs',;
		fecliq      : H= 'Liquid' ,;
		valtot      : H= 'Total ' ,;
		user        : H= 'Ingreso' ,;
		user_fc     : H= 'FecIng'
endif
sele (ali)
do vista
return


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
*OK=VE_PASSW('MRA')    
*IF !OK
 *  RETURN
*ENDIF

 ON KEY LABEL F2
 ON KEY LABEL F11
 ON KEY LABEL F12

 SELECT Promae
 SCATTER MEMVAR BLANK
 SELECT Parma
 SEEK 'CORRELPROVEE'
 = valpro ( Parma.NumEnt + 1 )
 SELECT Promae
 DO Pantalla
 STORE 'VG'  TO m.Estado
 m.fecing=date()
* @  0,22 GET m.regul  PICTURE '@M û, '
 @  1,22 GET m.TipPro PICTURE '!' Valid Val_para (m.TipPro ,'TIPPRO','C',22,20)
 @  1,24 GET m.Codprv DISABLE
 @  1,60 GET m.Fecing VALID !EMPTY(m.FecIng)

 @  3,22 GET m.Nompro
 @  4,22 GET m.Dirpro
 @  5,22 GET m.Telpro
 @  5,60 GET m.Faxpro
 @  6,22 GET m.Numruc VALID Numruc()
* @  6,60 GET m.Sigla

 @  8,22 GET m.LicMun
 @  8,60 GET m.RegPPYME	FUNCTION "M NO,SI"
 @  9,22 GET m.RegUnf
 @  9,60 GET m.RegInd
 @ 10,22 GET m.Test
 @ 10,60 GET m.Apert

 @ 12,22 GET m.Rl_nom
 @ 13,22 GET m.Rl_dir
 @ 14,22 GET m.Rl_ruc
 @ 15,22 GET m.Rl_le
 @ 16,22 GET m.Rl_tel

 @ 18,22 GET m.TipOpe PICTURE '!!!!!'   VALID Val_para (m.TipOpe,'TIPOPE',' ',22,20)
 @ 19,22 GET m.Acteco PICTURE '!!!!!!'  VALID Val_para (m.Acteco,'ACTECO',' ',22,20)
 @ 20,22 GET m.Ambito PICTURE '!!!'     VALID Val_para (m.Ambito,'AMBITO',' ',22,25)
 @ 21,22 GET m.Clasif PICTURE '!'
 @ 21,60 GET m.Legali PICTURE '@M ,û'
 
 READ VALID Val_Read()
 IF LASTKEY() <> 27
    SELECT PROMAE
*    IF F_appd()
       SELECT AUX
       DO WHILE .T.
          Ok = Trabaja_prov()
          IF LASTKEY() # 27
             IF YESNO('¨ Confirme el ingreso ?')
                EXIT
             ENDIF
          ELSE
             IF YESNO('¨ Cancela el ingreso ?')
                ok = .F.
                EXIT
             ENDIF
          ENDIF
       ENDDO
       IF Ok .AND. LASTKEY() # 27
          SELECT PROMAE       
          m.Giro = IIF(LEFT(m.Acteco,2)='PJ','JURIDICA','NATURAL')
	      DO CASE 
   	   		CASE m.GIRO='JURIDICA'
 	    	      DO CASE
 	    	          CASE !EMPTY(m.TEST) AND !EMPTY(m.RL_LE) AND !EMPTY(m.REGUNF) AND !EMPTY(m.NUMRUC)  AND m.Legali='û'
 	    	                m.Estado = 'VG'
 	    	          CASE  EMPTY(m.TEST) AND  EMPTY(m.RL_LE) AND  EMPTY(m.REGUNF) AND  EMPTY(m.NUMRUC) 
 	    	                m.Estado = 'PD'
 	    	          OTHER       
 	    	          		m.Estado = 'RG'
 	    	      ENDCASE
        	 CASE m.GIRO='NATURAL' 
               DO CASE 
                  CASE !EMPTY(m.RL_LE) AND !EMPTY(m.REGUNF) AND !EMPTY(m.NUMRUC) AND m.Legali='û'
 	   	                m.Estado = 'VG'    	              
                  CASE  EMPTY(m.RL_LE) AND  EMPTY(m.REGUNF) AND  EMPTY(m.NUMRUC) 
 	   	                m.Estado = 'PD'       	              
 	   	          OTHER      
 	   	          		m.Estado = 'RG' 	    	          
               ENDCASE   
		     CASE EMPTY(m.GIRO)
              m.Estado = 'PD'
	      ENDCASE    
          SELECT PROMAE            
          IF F_appd()
	         GATHER MEMVAR
	      endif    
	      SCATTER MEMVAR
          SELECT AuxiL
          IF F_appd()
      	      REPLACE tipo with '20',codigo with m.CodPrv,descri with m.nompro,direccion with m.dirpro
	      endif    
	      unlock
          SELECT Parma
          SEEK 'CORRELPROVEE'
          REPLACE NumEnt WITH NumEnt + 1
       ENDIF
   * ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    GO BOTTOM
 ENDIF
SELECT PROMAE
DO VISTA
RETURN

PROCEDURE NUMRUC
*---------------
private vfun
AX=RECNO()
vfun = .t.
SET ORDE TO 3
SEEK M.NUMRUC
IF AX#RECNO()
	IF FOUND() OR EMPTY(M.NUMRUC) OR  LEN(ALLTRIM(M.NUMRUC))<8 
		DO STANDBY WITH 'El Contratista ya est  registrado,Tiene el mismo RUC,o est  en Blanco/Nulo'
		vfun=.f.
	ENDIF
ENDIF	
set order to 1	
return vfun

PROCEDURE Trabaja_Prov
*---------------------
 AS=ALIAS()
 ACTIVATE SCREEN
 hide menu mmenu
 ACTIVATE WINDOW WIND_2Pa
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO Agreg_pro
 ON KEY LABEL F8  DO Elimi_pro
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELE aux
 SET ORDE TO PRVAUX1
 SEEK m.codprv
 if !found()
    DO Agreg_Pro
 endif

 BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2PA KEY m.codprv FIELD ;
    CodCla : H='Clase' :V=VALART(CODCLA):F:W=EMPTY(CodCla),;
    xDescri  = Val_Fun('Produ','CodArt','Descri',CodCla): H= 'Descripci¢n' :60 :W=.F.

 DEACTIVATE WINDOW WIND_2Pa
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 
 SCAN WHILE m.codprv=codprv
    IF RLOCK()
      IF EMPTY(CodCla)
        DELETE NEXT 1
      ENDIF
    ENDIF
 ENDSCAN

UNLOCK ALL

SHOW MENU MMENU
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10

 SELECT (AS)
 RETURN


PROCEDURE Vista_Pro
*------------------
 SELE AUX
 SET ORDE TO 1
 SEEK m.codprv

 if !found()
    DO STANDBY WITH 'Este Contratista no tiene Clasificaci¢n'
 else
   ACTIVATE WINDOW WIND_2P
   HIDE MENU mMenu
   BROWSE NOEDIT NOAPPEND NODELETE NOCLEAR KEY m.codprv FIELD ;
       CodCla : H='Clase' ,;
       xDescri  = Val_Fun('Produ','CodArt','Descri',CodCla): H= 'Descripci¢n' :44 :W=.F.  ,;
       xTipprv=iif(left(codcla,1)='B','Bien','Serv')  :H='Tipo' :W=.F.
 endif
 DEACTIVATE WINDOW WIND_2P
 SHOW MENU mMenu
 SELECT PROMAE
 RETURN



PROCEDURE Elimi
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
   RETURN
 ENDIF

 IF ESTADO = 'AN'
   DO standby WITH "El Contratista ya est  anulado"
 ENDIF

 IF escolor
   DEFINE POPUP Elimenu FROM 17,54  SHADOW COLOR &L_COL
 ELSE
   DEFINE POPUP Elimenu FROM 17,54  COLOR SCHEME C_POPUP
 ENDIF
 DEFINE BAR 1 OF Elimenu PROMPT ' \<Anulaci¢n  '
 DEFINE BAR 2 OF Elimenu PROMPT ' \<Rehabilita '

 ON SELECTION POPUP Elimenu DEACTIVATE POPUP
 ACTIVATE POPUP Elimenu
 DO CASE
   CASE BAR() = 1
		IF Rlock()      
           REPLACE ESTADO WITH 'AN',fecanu with date()
        ENDIF   		
   CASE BAR() = 2
        IF ESTADO # 'AN'
		   DO standby WITH "El Contratista NO est  anulado"   
		ELSE   
			IF RLOCK() 
    	       REPLACE ESTADO WITH 'PD',fecanu with ctod('  -   -  ')
        	ENDIF   		
        ENDIF 	
 ENDCASE
 RELEASE POPUP Elimenu
 UNLOCK
 DO Vista
 RETURN

PROCEDURE Agreg_Pro
*-----------------
IF F_appd()
  REPLACE codprv WITH m.codprv
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Agreg_Aux
*-----------------
sele Auxil
IF F_appd()
  REPLACE CODIGO WITH m.codprv,TIPO with '20',descri with m.nompro,direccion with m.dirpro
  RETURN .T.
ENDIF
RETURN .F.


PROCEDURE Elimi_Pro
*-------------------
SELECT Aux
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif

return

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN

PROCEDURE Termina
*----------------
  CLOSE DATA
  ON KEY LABEL F2
  RELEASE    WINDOW wind_cte0
  RELEASE    WINDOW wind_cte1
  ON KEY LABEL F12
  ON KEY LABEL F11
  RELEASE    MENU   mMenu
  ACTIVATE   SCREEN
  RESTORE    SCREEN FROM Principal
  RETURN

PROCEDURE Lista
*--------------
SELECT PROMAE
IF EOF()
   RETURN
ENDIF
VTEMP = RECNO()
go top
DO LisPro
SELECT promae
GO VTEMP
ON KEY LABEL F2 DO VISTA_PRO
RETURN

FUNCTION VALDIR
*--------------
m.Dir_cob = m.Dir_cte
RETURN .T.

function valprv
*--------------
private xx,VFUN
vfun = .f.
VCODPRV = iif( empty(VCODPRV),Vcodprv,padl(alltrim(Vcodprv),4,'0'))
ZZ = val_prv( Vcodprv,.t.)
if ZZ
   return .t.
endif
return .F.

function Xvalprv
*--------------
parameter vcli
private xx, vfun
vfun = .f.
vcli= iif( empty(vcli),vcli,padl(alltrim(vcli),4,'0'))
xx = val_prv( vcli,.t.,3,1)
if !EMPTY(xx)
   return .t.
endif
return vfun

FUNCTION VALPRO
*--------------
parameter vcodprv
private vfun
vfun = .t.
m.codprv=padl(alltrim(str(vcodprv,4)),4,'0')
if m.codprv = '0000' or empty(m.codprv)
   vfun = .f.
endif
return vfun

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,YY,ZZ,vFun
vFun = .t.
AS=ALIAS()
if empty(_Cod)
   ZZ=VAL_PARA(_Cod,'CODGE','C')
   IF LASTKEY()=27
      RETURN .T.
   ENDIF
   IF ZZ
      XX = Val_Art(CodCla,.F.)
      IF XX
         SELECT Aux
         REPLACE CodCla WITH Produ.Codart
         vFun = .T.
      ENDIF
   ENDIF
ELSE
   vFun = .t.
ENDIF
SELECT (AS)
unlock all
ON KEY LABEL F2  DO VISTA_PRO
ON KEY LABEL F5  DO Agreg_pro
ON KEY LABEL F8  DO Elimi_pro
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN vFun

FUNCTION VALLIS
*--------------
PARAMETERS _Cod
PRIVATE XX,YY,ZZ,vFun
vFun = .t.
AS=ALIAS()
ZZ=VAL_PARA(_Cod,'CODGEB','C')
IF ZZ
   XX = Val_Art(CodCla,.T.)
   IF XX
      vFun = .T.
   ENDIF
ENDIF
SELECT (AS)
RETURN vFun

PROCEDURE LisPro
*---------------
  IF escolor
    DEFINE POPUP Lismenu FROM 17,54 MARGIN SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP Lismenu FROM 17,54 MARGIN COLOR SCHEME C_POPUP
  ENDIF
  public ZZ
  DEFINE BAR 1 OF Lismenu PROMPT ' \<Gen‚rico     '
  DEFINE BAR 2 OF Lismenu PROMPT ' por \<Cat logo '
  DEFINE BAR 3 OF Lismenu PROMPT ' por \<Contratista'

  ON SELECTION POPUP Lismenu DEACTIVATE POPUP

  ACTIVATE POPUP Lismenu
  vTemp=recn()
  DO CASE
     CASE BAR()=1
          DO LISPRO1
     CASE BAR()=2
          IF escolor
             DEFINE POPUP Lismenu FROM 17,54 MARGIN SHADOW COLOR &L_COL
          ELSE
             DEFINE POPUP Lismenu FROM 17,54 MARGIN COLOR SCHEME C_POPUP
          ENDIF
          DEFINE BAR 1 OF Lismenu PROMPT '\<Grupo gen‚rico'
          DEFINE BAR 2 OF Lismenu PROMPT '\<Clase         '
          ON SELECTION POPUP Lismenu DEACTIVATE POPUP
          ACTIVATE POPUP Lismenu
          DO CASE
             CASE BAR()=1
                  SELECT PROMAE
				  SET FILTER TO LEGALI='û' AND ESTADO='VG'
				  COUNT ALL TO VCD             
                  SELECT Aux
                  SET RELATION TO CODPRV INTO PROMAE
                  SET SKIP TO PROMAE
                  vTem=recno()
                  SET UNIQ ON
                  vInd = SYS(3) + '.IDX'
                  INDEX ON LEFT(CODCLA,3)+CODPRV TO (vInd) FOR !EMPTY(PROMAE.CODPRV)
                  SET INDEX TO (VIND)
                  IF !YESNO(' Todos los Grupos Gen‚ricos ?')
                     ACTIVATE WINDOW STANDBY
                     _Cod='   '
                     @ 1,1 say 'Ingrese C¢digo :' get _Cod PICTURE '!!!' valid VAL_PARA(_Cod,'CODGE','C')
                     read
                     DEACTIVATE WINDOW STANDBY
                     Cod=Parma.Codigo
                     SET FILT TO CODCLA=ALLTRIM(COD) AND PROMAE.LEGALI='û' AND PROMAE.ESTADO='VG' AND PROMAE.REGUL='û'
                     COUNT ALL TO VCD
                  ENDIF
                  GO TOP
                  IF !EOF()
                     vtitulo = 'Por Grupo Gen‚rico'
                     DO REPORTE WITH 2, "GenPrv1", " Lista por Grupo Gen‚rico " ,1,.F.,.T. &&ELSE
                     DO STANDBY WITH "No se tiene existencias de esta l¡nea"
                  ENDIF
                  SET UNIQ OFF
                  SET FILTER TO
                  CLOSE INDEX
                  SELE PROMAE
                  SET FILT TO
                  SELE AUX
                  SET ORDER TO 1
                  go Bott
                  ERASE (VIND)
             CASE BAR()=2
                  SELECT PROMAE
				  SET FILTER TO LEGALI='û' AND ESTADO='VG' &&Prueba
				  COUNT ALL TO VCD
                  SELECT Aux
                  SET RELATION TO CODPRV INTO PROMAE
                  SET ORDER TO 2                  JFG
                  SET SKIP TO PROMAE
                  IF !YESNO(' Toda la Clasificaci¢n ?')
                      activate window standby
                      _Cod='       ' &&space(7)
                      @ 1,1 say 'Ingrese C¢digo :' get _Cod PICTURE '!!!.!!!' valid Val_ART1(_Cod,.T.)
                      read
                      deactivate window standby
                      SET FILTER TO CODCLA=ALLTRIM(_COD) AND PROMAE.LEGALI='û' AND PROMAE.ESTADO='VG' AND PROMAE.REGUL='û'
                      COUNT ALL TO VCD                      
                  ENDIF
                  GO TOP                  
                  IF !EOF()
                     vtitulo = 'Por Clase'
                     SET FILTER TO ESTADO='VG'
                     SELECT Aux
					 SET RELATION TO CODPRV INTO PROMAE
					 SET ORDER TO 2                  JFG
                     DO REPORTE WITH 2, "GenPrv", " Lista por Catalogos " ,1,.F.,.T.
                     SET FILTER TO
                  ELSE
                     DO STANDBY WITH "No se tiene existencias de esta l¡nea"
                  ENDIF
                  SET FILTER TO
                  SET ORDER TO 1
                  SELE PROMAE
                  SET FILT TO
          ENDCASE
     CASE BAR()=3 &&Por Contratista
          DEFINE WINDOW LIS FROM 3,15 TO 21,65 FLOAT DOUBLE TITLE 'Listado Contratistas' COLOR SCHEME 5
          ACTIVATE WINDOW LIS
          STORE 1        TO vToCli,vOrden,vTipPro,vEstado,VtipPro
          VCODPRV = SPAC(4)
          @ 01,01 SAY "Total Contratista : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
          @ 03,01 SAY "      Contratista : "
          @ 03,22 GET VCODPRV   WHEN VTOCLI=2  PICTURE '!!!!' VALID Valprv()

          @ 05,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Nombre;C¢digo;Ingreso;RUC' WHEN vTOCLI = 1

          @ 08,01 SAY "           Estado : " GET vEstado  FUNCTION '^ Aptos;No aptos'  WHEN vTOCLI = 1

          @ 11,01 SAY " Tipo Contratista : " GET vTipPro  FUNCTION '^ Todos;Bienes;Servicios' WHEN vTOCLI = 1

          @ 14,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
          READ CYCLE
          RELEASE WINDOW LIS
          IF OKCANCEL = 1
             ACTIVATE WINDOW STANDBY
             @ 01,04 SAY ' Espere un momento ... '
             vInd = SYS(3) + '.IDX'
             IF vTOCLI = 2
                VCD = 1
                SELECT PROMAE &&****
			    SET FILTER TO LEGALI='û' AND ESTADO='RG'
			    COUNT ALL TO VCD
		        SELECT Aux
    	        SET ORDER TO PRVAUX1
    	        SET FILT TO CodPrv = VCODPRV
        	    SET RELA TO CODPRV INTO PROMAE
            	SET SKIP TO PROMAE
             Else
                SELECT PROMAE
			    SET FILTER TO LEGALI='û' AND ESTADO='VG'
			    COUNT ALL TO VCD
		        SELECT Aux
    	        SET ORDER TO PRVAUX1
        	    SET RELA TO CODPRV INTO PROMAE
            	SET SKIP TO PROMAE
			 endif               	 
             GO TOP
             IF VTOCLI = 1
        	     IF vEstado=1
    	            INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,DTOS(Promae.FECing))) TO (vInd);
	                      FOR IIF(vToCli=1,.T.,CodPrv=VCODPRV) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,promae.TipPro='B',promae.TipPro='S'))
            	          SET FILT TO IIF(SUBSTR(PROMAE.ACTECO,1,2)='PJ',(!EMPTY(PROMAE.TEST) AND !EMPTY(PROMAE.RL_LE) AND !EMPTY(PROMAE.REGUNF) AND !EMPTY(PROMAE.NUMRUC)), (!EMPTY(PROMAE.RL_LE) AND !EMPTY(PROMAE.REGUNF) AND !EMPTY(PROMAE.NUMRUC)) );
        	              AND !EMPTY(PROMAE.GIRO) AND IIF(VTOCLI=1,PROMAE.LEGALI='û',.T.) AND IIF(VTOCLI=1,PROMAE.ESTADO='VG',.T.)
    	         ELSE
                	INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,DTOS(Promae.FECing))) TO (vInd) ;
                    	  FOR IIF(vToCli=1,.T.,CodPrv=VCODPRV) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,promae.TipPro='B',promae.TipPro='S'))
	                      SET FILT TO IIF(SUBSTR(PROMAE.ACTECO,1,2)='PJ',(EMPTY(PROMAE.TEST) or EMPTY(PROMAE.RL_LE) or EMPTY(PROMAE.REGUNF) or EMPTY(PROMAE.NUMRUC)), (EMPTY(PROMAE.RL_LE) or EMPTY(PROMAE.REGUNF) or EMPTY(PROMAE.NUMRUC)) );
	                      OR EMPTY(PROMAE.GIRO) AND PROMAE.LEGALI#'û' AND PROMAE.ESTADO#'VG'
             	ENDIF
             	SET INDEX TO (vInd)
             	GO TOP
			 ENDIF	
			 DEACTIVATE WINDOW STANDBY
             IF !EOF()
                DO REPORTE WITH 2, "GenPrv2A", " Lista por Contratista " ,1,.F.,.T.
             ELSE
                DO STANDBY WITH " No se tiene existencias de esta l¡nea "
             ENDIF
             SET RELATION TO
             SET FILTER TO
             CLOSE INDEX
             ERASE (VIND)
           ENDIF
           SELECT PROMAE
           SET ORDE TO 1
     OTHERWISE
  ENDCASE
 RELEASE POPUP LisMenu
 RETURN

PROCEDURE LisPro1
*----------------
IF EOF()
   DO STANDBY WITH vMens08
   RETURN
ENDIF
vTemo = RECNO()

DEFINE WINDOW LIS FROM 2,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Contratistas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1        TO vToCli,vOrden,vTipPro,vEstado,vTipLis
VCODPRV = SPAC(4)

@ 01,01 SAY " Total Contratista : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
@ 03,01 SAY "       Contratista : "
@ 03,22 GET VCODPRV    WHEN VTOCLI=2  PICTURE '!!!!' VALID Valprv()

@ 05,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Nombre;C¢digo;Ingreso;RUC'  WHEN VTOCLI =  1

@ 08,01 SAY "           Estado : " GET vEstado  FUNCTION '^ Todos;Aptos;No aptos;Pendientes;Anulados' WHEN VTOCLI =  1

@ 11,01 SAY " Tipo Contratista : " GET vTipPro  FUNCTION '^ Todos;Bienes;Servicios' WHEN VTOCLI =  1

@ 14,01 SAY "     Tipo Listado : " GET vTipLis  FUNCTION '^ General;Resumido;INAP;Control O/C;Control O/S' WHEN  VTOCLI =  1 &&vestado#4 AND

@ 17,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento ...'
   vInd = SYS(3) + '.IDX'
   DO CASE
      CASE vTipLis=1 OR vTipLis=3
         DO CASE
            CASE vEstado=1 
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECing),NumRuc))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv=VCODPRV) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro='B',TipPro='S')) 
            CASE vEstado=2 
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECing),NumRuc))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv=VCODPRV) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro='B',TipPro='S')) and estado='VG' AND LEGALI='û'
                *SET FILT TO IIF(SUBSTR(ACTECO,1,2)='PJ',(!EMPTY(TEST) AND !EMPTY(RL_LE) AND !EMPTY(REGUNF) AND !EMPTY(NUMRUC)), (!EMPTY(RL_LE) AND !EMPTY(REGUNF) AND !EMPTY(NUMRUC)) ) AND !EMPTY(GIRO)
            CASE vEstado=3
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,IIF(VORDEN=3,DTOS(FECing),NumRuc))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = VCODPRV) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro='B',TipPro ='S'))  AND ESTADO='RG'
                *SET FILT TO IIF(SUBSTR(ACTECO,1,2)='PJ',(EMPTY(TEST) or EMPTY(RL_LE) or EMPTY(REGUNF) or EMPTY(NUMRUC)), (EMPTY(RL_LE) or EMPTY(REGUNF) or EMPTY(NUMRUC)) ) or EMPTY(GIRO)
            CASE vEstado=4
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,IIF(VORDEN=3,DTOS(FECing),NumRuc))) TO (vInd) ;
                 FOR ESTADO='PD'
            CASE vEstado=5
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,IIF(VORDEN=3,DTOS(FECing),NumRuc))) TO (vInd) ;
                 FOR ESTADO='AN'
         ENDCASE
      CASE vTipLis=2
         DO CASE
            CASE vEstado=1 &&para general
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,Promae.CodPrv,IIF(vorden=3,DTOS(promae.fecing),promae.NumRuc))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = VCODPRV) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' ))
            CASE vEstado=2 &&para aptos (vigentes)
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,promae.CodPrv,IIF(vorden=3,DTOS(promae.fecing),promae.NumRuc))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = VCODPRV) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' )) AND ESTADO='VG' AND LEGALI='û'
                *SET FILT TO IIF(SUBSTR(ACTECO,1,2)='PJ',(!EMPTY(TEST) AND !EMPTY(RL_LE) AND !EMPTY(REGUNF) AND !EMPTY(NUMRUC)), (!EMPTY(RL_LE) AND !EMPTY(REGUNF) AND !EMPTY(NUMRUC)) ) AND !EMPTY(GIRO)
            CASE vEstado=3 &&para no aptos (regularizar)
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,IIF(VORDEN=3,DTOS(FECing),NumRuc))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = VCODPRV) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' )) AND ESTADO='RG'
                *SET FILT TO IIF(SUBSTR(ACTECO,1,2)='PJ',(EMPTY(TEST) or EMPTY(RL_LE) or EMPTY(REGUNF) or EMPTY(NUMRUC)), (EMPTY(RL_LE) or EMPTY(REGUNF) or EMPTY(NUMRUC)) ) or EMPTY(GIRO)
            CASE vEstado=4 &&para los pendientes.
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,IIF(VORDEN=3,DTOS(FECing),NumRuc))) TO (vInd) ;
                 for estado='PD'
            CASE vEstado=5
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,IIF(VORDEN=3,DTOS(FECing),NumRuc))) TO (vInd) ;
                 for estado='AN'
         ENDCASE                 
      CASE vTipLis=4
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECing),NumRuc))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = VCODPRV) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' )) and ESTADO # 'VG' AND busprvoc() 
      CASE vTipLis=5                       
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECing),NumRuc))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = VCODPRV) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' )) and ESTADO # 'VG' AND busprvos()
   ENDCASE
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   IF !EOF()
      IF vTipLis=1
          vTitul=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' por Bienes ',' por Servicio '))
            do case 
               case vestado=1
                    vTitulo = ' Contratistas en General '
               case vestado=2
                    vTitulo = ' Contratistas Aptos '               
               case vestado=3
                    vTitulo = ' Contratistas no Aptos '               
               case vestado=4
                    vTitulo = ' Contratistas Pendientes '               
               case vestado=5
                    vTitulo = ' Contratistas Anulados '               
            endcase   
			do reporte  with 2, 'LISPRO', ' Lista de Contratistas ',1,.F.,.T.
	  ELSE
         IF vTipLis=2
            do case 
               case vestado=1
                    vTitulo = ' Contratistas en General '
               case vestado=2
                    vTitulo = ' Contratistas Aptos '               
               case vestado=3
                    vTitulo = ' Contratistas no Aptos '               
               case vestado=4
                    vTitulo = ' Contratistas Pendientes '               
               case vestado=5
                    vTitulo = ' Contratistas Anulados '               
            endcase   
            do reporte  with 2, 'LISPRO2', ' Lista de Contratistas ',1,.F.,.T.
         ELSE
            If vTipLis=3
               vTitulo=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' por Bienes ',' por Servicio '))
               do case 
                  case vestado=1
                    vTitu = ' Contratistas en General '
                  case vestado=2
                    vTitu = ' Contratistas Aptos '               
                  case vestado=3
                    vTitu = ' Contratistas no Aptos '               
                  case vestado=4
                    vTitu = ' Contratistas Pendientes '               
                  case vestado=5
                    vTitu = ' Contratistas Anulados '               
               endcase   
               ACTIVATE WINDOW STANDBY
               AN=STR(YEAR(DATE()),4)
               MES = '00'
               @ 1,10 SAY 'Ingrese Per¡odo -> ' GET AN PICTURE '!!!!'
               @ 1,35 GET MES PICTURE '!!'
               read
               deactivate window standby
               do reporte  with 2, 'LISPROX', ' Lista de Proveedor ',1,.F.,.T.
            else
               if vTipLis=4
                  do reporte  with 2, 'prvoc', ' Lista de Proveedor ',1,.F.,.T.  
               else
                  do reporte  with 2, 'prvos', ' Lista de Proveedor ',1,.F.,.T.
               endif
            endif   
         ENDIF
      ENDIF
   ELSE
      DO STANDBY WITH 'Archivo Vac¡o ...'
   ENDIF
   SET FILTER TO
   CLOS INDEX
   ERASE (VIND)
ENDIF
SELECT PROMAE
SET ORDE TO 1
GO VTEMP
RETURN

FUNCTION Val_Art1   && Articulos
*---------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo

_campo = VARREAD()

mAlias = ALIAS()
SELECT Produ
_OldWnd = WOUTPUT()

IF !mEdita
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
ELSE
   IF EMPTY(xcod)
      SET ORDER TO 1
      ACTIVATE WINDOW STANDBY
      @ 1,14 SAY 'Espere un Momento ....' color W*
      GO TOP
      IF EOF()
         DEACTIVATE WINDOW STANDBY
         ACTIVATE SCREEN
         SET FILTER TO
         v_fun = .f.
      ELSE
         DEACTIVATE WINDOW STANDBY
         ACTIVATE SCREEN
         ON KEY LABEL F10 KEYBOARD CHR(23)
         ON KEY LABEL F2 DO FunBus
         DEFINE WINDOW _BusArt FROM 2,02 TO 22,77
         BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU FIELDS;
                CodArt   :H='C¢digo'    :w=EMPTY(SUBSTR(CODART,5,3)),;
                Descri   :H='Nombre':60 :w=empty(descri)
         ON KEY LABEL F10
         ON KEY LABEL F2
         RELEASE WINDOW _BusArt
         SET FILTER TO

         IF !EMPTY(_OldWnd)
            ACTIVATE WINDOW &_OldWnd
         ENDIF

         IF Lastkey()=27
            V_FUN = .f.
         ELSE
            xcod = CodArt
            IF mMsg
               @ _x,_y SAY Descri
            ENDIF
            SELECT (mAlias)
            IF !_tipo
               REPLACE &_campo WITH  xcod
            ENDIF
            v_fun = .T.
        ENDIF
      ENDIF
   ELSE
      SEEK xcod
      IF mMsg .AND. FOUND()
         @ _x,_y SAY Descri
      ENDIF
      v_fun = FOUND()
   ENDIF
ENDIF
SET ORDER TO 1
SELECT (mAlias)
RETURN v_fun



FUNCTION BuscArt
*----------------
 PARAMETER VCODIGO
 as=alias()
 sele produ
 set orde to 1
 Seek ALLTRIM(VCODIGO)
 if !found()
    vFun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
 else
    vfun=produ.descri
 endif
 sele (as)
 return vfun



PROCEDURE busprvoc
*-----------------
private ali,vkey
ali=alias()
vkey = '96'+promae.codprv
select orden
SEEK vkey
if found()
   vfun = .t.
else   
   vfun = .f.
endif   
sele (ali)
return vfun


PROCEDURE busprvos
*-----------------
private ali,vkey
ali=alias()
vkey = '96'+promae.codprv
select ordser
SEEK vkey
if found()
   vfun = .t.
else   
   vfun = .f.
endif  
sele (ali)
return vfun