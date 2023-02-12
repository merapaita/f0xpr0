* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ CatMov.PRG  11/06/96                                L: 0719  ³	
* ³ Cat logo de Conceptos de Movimientos de Bancos               ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

USE Parmae   IN 1  ORDER TAG Parmae1  ALIAS Parma
USE Ingreso  IN 10  ORDER TAG Ingreso1 		ALIAS IngR
USE Cuentas  IN 7   ORDER TAG Cuentas1 ALIAS cuenta
*- Variables de trabajo (registro a trabajar)
SELECT Parma
SET FILT TO Tipo='MOTIVO'
GO BOTTOM
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Mensajes de aviso al usuario
Vmens01 = ' ± Registro de Rubro de Retenciones ± '
Vmens02 = 'Revisi¢n de Rubros'
Vmens05 = 'No existe Rubro anterior'
Vmens06 = 'No existe Rubro siguiente'
Vmens08 = 'No hay registros para procesar'
PUBLIC vtip,vcon,vtran,vfte,vasiento1,vasiento2,Vcla,vpar
STORE  SPACE(10) TO vasiento1,vasiento2
STORE  SPACE(09) TO vpar
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
DO Fin_opcion
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79  ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_c1 FROM 00,00 TO 23,79 ;
 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
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
 ACTIVATE WINDOW Wind_0
 CLEAR
 @ 01,02 SAY "      C¢digo del Rubro :"
 @ 03,02 SAY "           Descripci¢n :"
 @ 06,02 SAY "                Fuente :"
 @ 08,02 SAY "           Transacci¢n  :"
 @ 10,02 SAY "           N§ Asientos :"
 @ 12,02 SAY "                  Tipo :"
 @ 14,02 SAY " Condici¢n del Asiento :"
 @ 16,02 SAY "Asiento Autom tico [1] :"
 @ 18,02 SAY "Asiento Autom tico [2] :"
 @ 20,02 SAY " Clasificador [S/N] :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Parma
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR
 @ 01,27 SAY m.codigo
 @ 03,27 SAY m.Descri
 DO CASE
    CASE ALLT(m.codigoaux)='PRP'
         vfte=1
    CASE ALLT(m.codigoaux)='TRN'
         vfte=2
    OTHERWISE     
         vfte=3
 ENDCASE         
 DO CASE
    CASE LEFT(m.descriaux,2)='A '
         vtran=1
    CASE LEFT(m.descriaux,2)='C '
         vtran=2
    CASE LEFT(m.descriaux,2)='O '
         vtran=3
    CASE LEFT(m.descriaux,2)='AC' OR LEFT(m.descriaux,2)='CA'
         vtran=4
 ENDCASE         
 DO CASE
    CASE SUBSTR(m.descriaux,5,1)='S'
         vtip=1
    CASE SUBSTR(m.descriaux,5,1)='N'
         vtip=2
    OTHER
         vtip=3
 ENDCASE         
 DO CASE
    CASE SUBSTR(m.descriaux,8,1)='U'
         vcon=1
    CASE SUBSTR(m.descriaux,8,1)='C'
         vcon=2
    CASE SUBSTR(m.descriaux,8,1)='A'
         vcon=3
 ENDCASE         
 vcla=SUBST(ALLT(m.descriaux),11,1)
 vpar=IIF(vcla='S',SUBST(ALLT(m.descriaux),14,9),' ')
 @ 06,27 GET vfte  FUNCTION '*RNH \<Ing.Propios;\<Tes.P£blico;\<Ambos'  		 SIZE 1,10,2 	 COLOR SCHEME 11 WHEN .F.
 @ 08,27 GET vtran FUNCTION '*RNH \<Abono;\<Cargo;\<Orden;Abo\<No-Cargo' 	     SIZE 1,10,2     COLOR SCHEME 11 WHEN .F.
 @ 10,27 SAY m.nument PICTURE "9"
 IF m.nument#0
	 @ 12,27 GET vtip FUNCTION '*RNH \<Juega s¢lo con la 10;\<Con otras cuentas' SIZE 1,10,2 COLOR SCHEME 11 WHEN .F.
	 @ 14,27 GET vcon FUNCTION '*RNH \<Unico;\<En funci¢n del Gasto;\<Doble' 	 SIZE 1,10,2 COLOR SCHEME 11 WHEN .F.
 ELSE
     vtip=3 
     @ 12,27 SAY SPACE(50)	 
     @ 14,27 SAY SPACE(50)	 
 ENDIF
 IF vtip=2
	 @ 16,27 SAY LEFT(m.descriau2,10)
 	 @ 16,40 SAY val_fun('Cuenta','Cuenta',"DescRi",LEFT(m.descriau2,10))
  ELSE
     @ 16,27 SAY SPACE(10)	 
 ENDIF
 IF vcon=2 OR vcon=3
	 @ 18,27 SAY SUBSTR(m.descriau2,12,10)
 	 @ 18,40 SAY val_fun('Cuenta','Cuenta',"DescRi",SUBS(m.descriau2,12,10))
 ELSE
     @ 18,27 SAY SPACE(10)	 
 ENDIF    
 @ 20,30 SAY IIF(SUBS(m.descriaux,11,1)='S',SUBSTR(ALLT(m.descriaux),14,9),SPACE(30))
 READ
 RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE Parma
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 codigo  :H='Rubro' ,;
 descri  :30 :H='Detalle' ,;
 x1=IIF(ALLT(codigoaux)='X','PRP-TRN',IIF(ALLT(codigoaux)='PRP','PRP','TRN')) :7 :H='Fuente',;
 x2=LEFT(descriau2,10) :10 :H='Asiento[1]',;
 x3=SUBSTR(descriau2,12,10) :10 :H='Asiento[2]',;
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 DO Vista
 RETURN

PROCEDURE Busca
*--------------
 SELECT Parma
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF
 Vtemp = RECNO()
 STORE SPACE(02) TO vGen
 ACTIVATE WINDOW standby
 @ 1,03 SAY "   C¢digo del Rubro :" GET vGen
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vGen) .OR. LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK 'MOTIVO'+vGen
    IF !FOUND()
       DO standby WITH "Dicho Rubro no est  registrado"
       GOTO Vtemp
    ELSE
       DO Vista
    ENDIF
 ENDIF
 RETURN


PROCEDURE Anter
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF !BOF()
    SKIP -1
 ENDIF
 IF BOF()
    GO TOP
    DO standby WITH Vmens05
 ELSE
    DO Vista
 ENDIF
 RETURN


PROCEDURE Proxi
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH Vmens06
   GO BOTTOM
 ELSE
   DO Vista
 ENDIF
 RETURN


PROCEDURE Corri
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF

 ACTIVATE WINDOW Wind_0
 SELECT Parma
 DO pantalla
 SCATTER MEMVAR
 m.tipo='MOTIVO'
 DO CASE
    CASE ALLT(m.codigoaux)='PRP'
         vfte=1
    CASE ALLT(m.codigoaux)='TRN'
         vfte=2
    OTHERWISE     
         vfte=3
 ENDCASE         
 DO CASE
    CASE LEFT(m.descriaux,2)='A '
         vtran=1
    CASE LEFT(m.descriaux,2)='C '
         vtran=2
    CASE LEFT(m.descriaux,2)='O '
         vtran=3
    CASE LEFT(m.descriaux,2)='AC' OR LEFT(m.descriaux,2)='CA'
         vtran=4
 ENDCASE         
 DO CASE
    CASE SUBSTR(m.descriaux,5,1)='S'
         vtip=1
    CASE SUBSTR(m.descriaux,5,1)='N'
         vtip=2
 ENDCASE         
 DO CASE
    CASE SUBSTR(m.descriaux,8,1)='U'
         vcon=1
    CASE SUBSTR(m.descriaux,8,1)='C'
         vcon=2
    CASE SUBSTR(m.descriaux,8,1)='A'
         vcon=3
 ENDCASE  
 vasiento1=LEFT(m.descriau2,10)
 vasiento2=SUBSTR(m.descriau2,12,10)       
 vcla=SUBSTR(m.descriaux,11,1)       
 vpar=SUBSTR(m.descriaux,14,09)       
 IF RLOCK() OR F_LOCK(1)
	 @ 01,27 GET m.Codigo DISABLE
     @ 03,27 GET m.Descri PICTURE '@S36'
	 @ 06,27 GET vfte  FUNCTION '*RNH \<Ing.Propios;\<Tes.P£blico;\<Ambos'  		 SIZE 1,10,2 	 COLOR SCHEME 11 
	 @ 08,27 GET vtran FUNCTION '*RNH \<Abono;\<Cargo;\<Orden;Abo\<No-Cargo' 	     SIZE 1,10,2     COLOR SCHEME 11 
	 @ 10,27 GET m.nument PICTURE "9"
	 READ
	 IF m.nument#0
		 @ 12,27 GET vtip FUNCTION '*RNH \<Juega s¢lo con la 10;\<Con otras cuentas' SIZE 1,10,2 COLOR SCHEME 11 
		 @ 14,27 GET vcon FUNCTION '*RNH \<Unico;\<En funci¢n del Gasto;\<Doble' 	 SIZE 1,10,2 COLOR SCHEME 11 
	 ELSE
    	 vtip=3 
    	 @ 12,27 SAY SPACE(50)	 
	     @ 14,27 SAY SPACE(50)	 
	 ENDIF
	 READ
	 IF vtip=2
	   @ 16,27 GET vasiento1 PICTURE "9999999999" 
	 ELSE
    	 @ 16,27 SAY SPACE(10)	 
	 ENDIF
	 IF vcon=2 OR vcon=3
	   @ 18,27 GET vasiento2 PICTURE "9999999999" 
	 ELSE
    	 @ 18,27 SAY SPACE(10)	 
	 ENDIF    
    @ 20,27 GET vcla FUNCTION 'M N,S'
    @ 20,30 GET vpar PICTURE "!.!.!.!!!"  VALID VAL_INGE(SUBSTR(vpar,11,2),LEFT(vpar,9),' ')  WHEN Vcla='S'
     READ VALID Val_Read()
     IF LASTKEY() # 27          && Si se presion¢ Esc, no graba
        m.codigoaux=IIF(vfte=1,'PRP',IIF(vfte=2,'TRN','X'))
        DO CASE 
           CASE vtran=1 AND vtip=1 AND vcon=1
                m.descriaux='A --S--U--'+vcla+'--'+vpar
           CASE vtran=1 AND vtip=1 AND vcon=2
                m.descriaux='A --S--C--'+vcla+'--'+vpar
           CASE vtran=1 AND vtip=1 AND vcon=3
                m.descriaux='A --S--A--'+vcla+'--'+vpar
           CASE vtran=1 AND vtip=2 AND vcon=1
                m.descriaux='A --N--U--'+vcla+'--'+vpar
           CASE vtran=1 AND vtip=2 AND vcon=2
                m.descriaux='A --N--C--'+vcla+'--'+vpar
           CASE vtran=1 AND vtip=2 AND vcon=3
                m.descriaux='A --N--A--'+vcla+'--'+vpar
           CASE vtran=1 AND vtip=3 AND vcon=1
                m.descriaux='A --X--U--'+vcla+'--'+vpar
           CASE vtran=1 AND vtip=3 AND vcon=2
                m.descriaux='A --X--C--'+vcla+'--'+vpar
           CASE vtran=1 AND vtip=3 AND vcon=3
                m.descriaux='A --X--A--'+vcla+'--'+vpar

           CASE vtran=2 AND vtip=1 AND vcon=1
                m.descriaux='C --S--U--'+vcla+'--'+vpar
           CASE vtran=2 AND vtip=1 AND vcon=2
                m.descriaux='C --S--C--'+vcla+'--'+vpar
           CASE vtran=2 AND vtip=1 AND vcon=3
                m.descriaux='C --S--A--'+vcla+'--'+vpar
           CASE vtran=2 AND vtip=2 AND vcon=1
                m.descriaux='C --N--U--'+vcla+'--'+vpar
           CASE vtran=2 AND vtip=2 AND vcon=2
                m.descriaux='C --N--C--'+vcla+'--'+vpar
           CASE vtran=2 AND vtip=2 AND vcon=3
                m.descriaux='C --N--A--'+vcla+'--'+vpar
           CASE vtran=2 AND vtip=3 AND vcon=1
                m.descriaux='C --X--U--'+vcla+'--'+vpar
           CASE vtran=2 AND vtip=3 AND vcon=2
                m.descriaux='C --X--C--'+vcla+'--'+vpar
           CASE vtran=2 AND vtip=3 AND vcon=3
                m.descriaux='C --X--A--'+vcla+'--'+vpar
                
           CASE vtran=3 AND vtip=1 AND vcon=1
                m.descriaux='O --S--U--'+vcla+'--'+vpar
           CASE vtran=3 AND vtip=1 AND vcon=2
                m.descriaux='O --S--C--'+vcla+'--'+vpar
           CASE vtran=3 AND vtip=1 AND vcon=3
                m.descriaux='O --S--A--'+vcla+'--'+vpar
           CASE vtran=3 AND vtip=2 AND vcon=1
                m.descriaux='O --N--U--'+vcla+'--'+vpar
           CASE vtran=3 AND vtip=2 AND vcon=2
                m.descriaux='O --N--C--'+vcla+'--'+vpar
           CASE vtran=3 AND vtip=2 AND vcon=3
                m.descriaux='O --N--A--'+vcla+'--'+vpar
           CASE vtran=3 AND vtip=3 AND vcon=1
                m.descriaux='O --X--U--'+vcla+'--'+vpar
           CASE vtran=3 AND vtip=3 AND vcon=2
                m.descriaux='O --X--C--'+vcla+'--'+vpar
           CASE vtran=3 AND vtip=3 AND vcon=3
                m.descriaux='O --X--A--'+vcla+'--'+vpar

           CASE vtran=4 AND vtip=1 AND vcon=1
                m.descriaux='AC--S--U--'+vcla+'--'+vpar
           CASE vtran=4 AND vtip=1 AND vcon=2
                m.descriaux='AC--S--C--'+vcla+'--'+vpar
           CASE vtran=4 AND vtip=1 AND vcon=3
                m.descriaux='AC--S--A--'+vcla+'--'+vpar
           CASE vtran=4 AND vtip=2 AND vcon=1
                m.descriaux='AC--N--U--'+vcla+'--'+vpar
           CASE vtran=4 AND vtip=2 AND vcon=2
                m.descriaux='AC--N--C--'+vcla+'--'+vpar
           CASE vtran=4 AND vtip=2 AND vcon=3
                m.descriaux='AC--N--A--'+vcla+'--'+vpar
           CASE vtran=4 AND vtip=3 AND vcon=1
                m.descriaux='AC--X--U--'+vcla+'--'+vpar
           CASE vtran=4 AND vtip=3 AND vcon=2
                m.descriaux='AC--X--C--'+vcla+'--'+vpar
           CASE vtran=4 AND vtip=3 AND vcon=3
                m.descriaux='AC--X--A--'+vcla+'--'+vpar
                
       ENDCASE
       DO CASE
          CASE !EMPTY(vasiento1) AND EMPTY(vasiento2)  
          	   m.descriau2=vasiento1	 
          CASE !EMPTY(vasiento1) AND !EMPTY(vasiento2)  
          	   m.descriau2=vasiento1+','+vasiento2
       ENDCASE   
       SELECT Parma
       GATHER MEMVAR
    ENDIF
    DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT Parma
 DO Pantalla
 SCATTER MEMVAR BLANK
 m.tipo='MOTIVO'
 @ 01,27 GET m.Codigo PICTURE '99' VALID vret() 
 @ 03,27 GET m.Descri PICTURE '@S36'
 STORE 1 TO vfte,vtran,vtip,vcon
 	 @ 06,27 GET vfte  FUNCTION '*RNH \<Ing.Propios;\<Tes.P£blico;\<Ambos'  		 SIZE 1,10,2 	 COLOR SCHEME 11 
	 @ 08,27 GET vtran FUNCTION '*RNH \<Abono;\<Cargo;\<Orden;Abo\<No-Cargo' 	     SIZE 1,10,2     COLOR SCHEME 11 
	 @ 10,27 GET m.nument PICTURE "9"
	 READ
	 IF m.nument#0
		 @ 12,27 GET vtip FUNCTION '*RNH \<Juega s¢lo con la 10;\<Con otras cuentas' SIZE 1,10,2 COLOR SCHEME 11 
		 @ 14,27 GET vcon FUNCTION '*RNH \<Unico;\<En funci¢n del Gasto;\<Doble' 	 SIZE 1,10,2 COLOR SCHEME 11 
	 ELSE
    	 vtip=3 
    	 @ 12,27 SAY SPACE(50)	 
	     @ 14,27 SAY SPACE(50)	 
	 ENDIF
	 READ
	 IF vtip=2
	   @ 16,27 GET vasiento1 PICTURE "9999999999" 
	 ELSE
    	 @ 16,27 SAY SPACE(10)	 
	 ENDIF
	 IF vcon=2 OR vcon=3
	   @ 18,27 GET vasiento2 PICTURE "9999999999" 
	 ELSE
    	 @ 18,27 SAY SPACE(10)	 
	 ENDIF    
     @ 20,27 GET vcla FUNCTION 'M N,S'
     @ 20,30 GET vpar PICTURE "!.!.!.!!!"  VALID VAL_INGE(SUBSTR(vpar,11,2),LEFT(vpar,9),' ')  WHEN Vcla='S'
     READ VALID Val_Read()
     IF LASTKEY() # 27          && Si se presion¢ Esc, no graba
	     m.codigoaux=IIF(vfte=1,'PRP',IIF(vfte=2,'TRN','X'))
	     DO CASE 
    	    CASE vtran=1 AND vtip=1 AND vcon=1
        	     m.descriaux='A --S--U--'+vcla+'--'+vpar
	        CASE vtran=1 AND vtip=1 AND vcon=2
    	         m.descriaux='A --S--C--'+vcla+'--'+vpar
	        CASE vtran=1 AND vtip=1 AND vcon=3
    	         m.descriaux='A --S--A--'+vcla+'--'+vpar
	        CASE vtran=1 AND vtip=2 AND vcon=1
    	         m.descriaux='A --N--U--'+vcla+'--'+vpar
	        CASE vtran=1 AND vtip=2 AND vcon=2
    	         m.descriaux='A --N--C--'+vcla+'--'+vpar
	        CASE vtran=1 AND vtip=2 AND vcon=3
    	         m.descriaux='A --N--A--'+vcla+'--'+vpar
	        CASE vtran=1 AND vtip=3 AND vcon=1
    	         m.descriaux='A --X--U--'+vcla+'--'+vpar
	        CASE vtran=1 AND vtip=3 AND vcon=2
    	         m.descriaux='A --X--C--'+vcla+'--'+vpar
	        CASE vtran=1 AND vtip=3 AND vcon=3
    	         m.descriaux='A --X--A--'+vcla+'--'+vpar

	        CASE vtran=2 AND vtip=1 AND vcon=1
    	         m.descriaux='C --S--U--'+vcla+'--'+vpar
	        CASE vtran=2 AND vtip=1 AND vcon=2
	             m.descriaux='C --S--C--'+vcla+'--'+vpar
	        CASE vtran=2 AND vtip=1 AND vcon=3
    	         m.descriaux='C --S--A--'+vcla+'--'+vpar
	        CASE vtran=2 AND vtip=2 AND vcon=1
    	         m.descriaux='C --N--U--'+vcla+'--'+vpar
	        CASE vtran=2 AND vtip=2 AND vcon=2
    	         m.descriaux='C --N--C--'+vcla+'--'+vpar
	        CASE vtran=2 AND vtip=2 AND vcon=3
    	         m.descriaux='C --N--A--'+vcla+'--'+vpar
	        CASE vtran=2 AND vtip=3 AND vcon=1
    	         m.descriaux='C --X--U--'+vcla+'--'+vpar
	        CASE vtran=2 AND vtip=3 AND vcon=2
    	         m.descriaux='C --X--C--'+vcla+'--'+vpar
	        CASE vtran=2 AND vtip=3 AND vcon=3
    	         m.descriaux='C --X--A--'+vcla+'--'+vpar
                
	        CASE vtran=3 AND vtip=1 AND vcon=1
    	         m.descriaux='O --S--U--'+vcla+'--'+vpar
	        CASE vtran=3 AND vtip=1 AND vcon=2
    	         m.descriaux='O --S--C--'+vcla+'--'+vpar
	        CASE vtran=3 AND vtip=1 AND vcon=3
    	         m.descriaux='O --S--A--'+vcla+'--'+vpar
	        CASE vtran=3 AND vtip=2 AND vcon=1
    	         m.descriaux='O --N--U--'+vcla+'--'+vpar
	        CASE vtran=3 AND vtip=2 AND vcon=2
    	         m.descriaux='O --N--C--'+vcla+'--'+vpar
	        CASE vtran=3 AND vtip=2 AND vcon=3
    	         m.descriaux='O --N--A--'+vcla+'--'+vpar
	        CASE vtran=3 AND vtip=3 AND vcon=1
    	         m.descriaux='O --X--U--'+vcla+'--'+vpar
	        CASE vtran=3 AND vtip=3 AND vcon=2
    	         m.descriaux='O --X--C--'+vcla+'--'+vpar
	        CASE vtran=3 AND vtip=3 AND vcon=3
    	         m.descriaux='O --X--A--'+vcla+'--'+vpar

	        CASE vtran=4 AND vtip=1 AND vcon=1
    	         m.descriaux='AC--S--U--'+vcla+'--'+vpar
        	CASE vtran=4 AND vtip=1 AND vcon=2
            	 m.descriaux='AC--S--C--'+vcla+'--'+vpar
	        CASE vtran=4 AND vtip=1 AND vcon=3
    	         m.descriaux='AC--S--A--'+vcla+'--'+vpar
	        CASE vtran=4 AND vtip=2 AND vcon=1
    	         m.descriaux='AC--N--U--'+vcla+'--'+vpar
	        CASE vtran=4 AND vtip=2 AND vcon=2
    	         m.descriaux='AC--N--C--'+vcla+'--'+vpar
	        CASE vtran=4 AND vtip=2 AND vcon=3
    	         m.descriaux='AC--N--A--'+vcla+'--'+vpar
	        CASE vtran=4 AND vtip=3 AND vcon=1
	             m.descriaux='AC--X--U--'+vcla+'--'+vpar
    	    CASE vtran=4 AND vtip=3 AND vcon=2
        	     m.descriaux='AC--X--C--'+vcla+'--'+vpar
	        CASE vtran=4 AND vtip=3 AND vcon=3
    	         m.descriaux='AC--X--A--'+vcla+'--'+vpar
	     ENDCASE
	     DO CASE
    	    CASE !EMPTY(vasiento1) AND EMPTY(vasiento2)  
	    	   	   m.descriau2=vasiento1	 
	         CASE !EMPTY(vasiento1) AND !EMPTY(vasiento2)  
    	      	   m.descriau2=vasiento1+','+vasiento2
	     ENDCASE   
	     
       SELECT Parma
       IF F_Appd()
         GATHER MEMVAR
       ELSE
         GO BOTTOM
       ENDIF
	 ELSE
	   DO STANDBY WITH 'Proceso cancelado, no se graba nada'
	   GO BOTTOM
	 ENDIF
 UNLOCK ALL
 SELECT Parma
 DO Vista
 RETURN


PROCEDURE Elimi
*--------------
 SELECT Parma
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ELIMINAR FISICAMENTE ‚ste Rubro ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
   DELETE NEXT 1
   GO BOTTOM
   DO Vista
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Lista
*--------------
SELECT Parma
Vind=SYS(3)+'.idx'
vtemp =RECNO()
IF EOF()
   DO standby WITH vmens08
   RETURN
ELSE
	IF escolor
		DEFINE POPUP xcot  FROM 18,55 SHADOW COLOR &l_col
	ELSE
		DEFINE POPUP xcot  FROM 18,55 COLOR SCHEME c_popup
	ENDIF
	DEFINE BAR 1 OF xcot PROMPT 'Por \<C¢digo     '
	DEFINE BAR 2 OF xcot PROMPT 'Por \<Descripci¢n'
	ON SELECTION POPUP xcot  DEACTIVATE POPUP
	ACTIVATE POPUP xcot
	DO CASE
	   CASE BAR() = 1
	        INDEX ON val(codigo) TO (Vind) FOR tipo='MOTIVO' 
		    DO reporte WITH 2,"CatMovC",' Cat logo de Conceptos de Mov.Bcos '
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
			SET ORDER  TO Parmae1
	   CASE BAR() = 2
	        SET ORDER TO Parmae2
		    DO reporte WITH 2,"CatMovD",' Cat logo de Conceptos de Mov.Bcos '
			SET ORDER  TO Parmae1
	ENDCASE
ENDIF
GO vtemp
DO Vista
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN



FUNCTION vret
*------------
SELE Parma
SEEK m.tipo+m.codigo
IF FOUND()
   DO STANDBY WITH 'Rubro de Retenci¢n ya existe' 
   RETURN  .F.
ENDIF
RETURN .T.

FUNCTION Val_IngE
*---------------
  PARAMETERS mValor, Filtro, mVaRiable, mCol, mLong
  PRiVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVaRiable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRi
  ENDCASE
  mAlias  = ALIAS()
  SELECT IngR
  SEEK Filtro+mValor

  IF !FOUND() AND !mVaRiable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
         SET FILTER TO CodIng >= Filtro
      ENDIF
      GO TOP
      IF EOF() 
         DO STANDBY WITH 'No existen partidas definidas'
         SET FILTER TO
         sele (malias)
         return &&.f.
      endif
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRiM(CODING)+'.'+ALLTRiM(SUBING)+' '+SUBSTR(DESING,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = IngR.CodIng 
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )
 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVaRiable=' '   && En edici¢n
      @ ROW(),mCol+24  SAY mValor
      @ ROW(),mCol+37 SAY mDescr
      vpar     =mvalor
      RETURN .T.
    CASE mVaRiable='A'   && En edici¢n SOLO DESCRiPCION
      @ ROW(),mCol SAY mDescr
      RETURN
    CASE mVaRiable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVaRiable='D'   && En vista
      RETURN mDescr
    CASE mVaRiable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVaRiable='C'   && Solo codigo
      RETURN .T.
    CASE mVaRiable='T'
      &mVaRiable = mValor
      @ ROW(),mCol+7 SAY mDescr
      RETURN  mValor
    OTHERWISE   
      vpar = mvalor
      RETURN .T.
  ENDCASE