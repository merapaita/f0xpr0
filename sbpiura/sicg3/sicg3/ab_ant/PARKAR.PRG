*--------------------------------------------------------------------------
* ParKar.Prg
* Parte de Modificaci¢n de Kardex.
* Este programa a sido implementado al Sistema del periodo 97' y 98'
* La opci¢n a sido incluida en el Menu de Almacen.
*--------------------------------------------------------------------------

parameter vopcion 

*- Abriendo Archivos
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE calen    IN 2   order tag calen3   ALIAS calen
USE parkar   IN 3   order tag parkar1  ALIAS parkar
USE ItePec   IN 4   order tag ItePec1  ALIAS Itepec
USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE IteArt   IN 6   order tag IteArt1  ALIAS Iteart
USE maepre   IN 10  order tag maepre1  ALIAS maepre
USE itepar   IN 11  order tag itepar1  ALIAS ITEPAR          
USE Obra     IN 12  order tag obra1    ALIAS obra
USE Promae   IN 14  order tag Promae1  ALIAS Promae
USE USUARIO  IN 20  ORDER TAG USUARIO1 ALIAS USU


*- Mensajes de aviso al usuario
Vmens01 = ' Pecosas : REVISION '
Vmens02 = 'Registro de Pecosa'
Vmens04 = 'Dicho Pecosa no fue encontrado'
Vmens05 = 'No existe Pecosa anterior'
Vmens06 = 'No existe Pecosa siguiente'
Vmens07 = '¨ Desea Anular ‚ste Pecosa ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Pecosa ha sido anulado'
Vmens10 = 'El Pecosa ya est  Atendido'
Vmens11 = 'El Pecosa ha sido devuelto'
Vmens12 = 'El Pecosa ya tiene O/C'

PUBLIC VANT,valobra,gh,vnp
SELECT parkar
on key label f7 do liquidar
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
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

procedure liquidar
*-----------------
SELEct PARKAR
SCATTER MEMVAR
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF Estado = '50'
   * El parkar ya tiene O/C
   DO STANDBY WITH 'Parte ya liquidada'
*  RETURN
 ENDIF
 
SELECT parkar
ACTIVATE WINDOW Wind_1

@ 0,60 GET m.FecDesp   PICTURE '@D'
read

IF LASTKEY() # 27 && Si se presion¢ Esc, no graba
   SELECT Itepec
   SEEK m.periodo+m.numpec 
   SCAN WHILE m.Periodo = Periodo and m.Numpec = Numpec  
        IF RLOCK()
           REPLA FECDESP WITH M.FECDESP,ESTADO WITH '50'
        ENDIF   
   ENDSCAN
   SELE PARKAR
   M.ESTADO = '50'
   GATHER MEMVAR
endif
SELE PARKAR
do vista   
return

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular   Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: Parkar  ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: Parkar   ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nular '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD anula  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_1
 CLEAR
 @  0,40 SAY "Fecha Liquidaci¢n :"
 @  1, 2 SAY "            Fecha :"
 @  1,40 SAY "    N£mero PteMod :"
 @  4, 2 SAY " F.Financiamiento :"
 @  4,40 SAY "             Tipo :"
 @  5, 2 SAY "         Programa :"
 @  6, 2 SAY "      Subprograma :"
 @  7, 2 SAY " Activid/Proyecto :"
 @  8, 2 SAY "          Destino :"
 @  9, 2 SAY "    Observaciones :"

 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT parkar
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 SCATTER MEMVAR

 @  0,20 SAY vEstPec(m.Estado) COLOR SCHEME 02
 @  0,60 SAY m.FecDesp
 @  1,22 SAY m.FecPec
 @  1,60 SAY m.periodo
 @  1,63 SAY m.NumPec
 @  1,68 SAY m.TIPPEC
 IF m.Tippec = 'P'
   @  2, 2 SAY "      Dependencia :"
   @  3, 2 SAY "             Atte :"
   @  2,22 SAY val_para(m.CodDep,'CODDEP','A',22,50,7)
   @  3,22 SAY m.atte
 ELSE
   @ 2, 2 SAY "        Proveedor :" 
   @ 2,22 SAY IIF(EMPTY(m.codprv),'Sin Codigo'+space(40),val_prv(m.Codprv))
   @ 3,22 say spac(56)
 ENDIF   
 @  4,22 SAY Val_Para(m.CodFte,'CODFTE','V',26,20)
 @  4,60 SAY val_para(m.Tipfun,'TIPFUN','A',60,15,2)
 IF alltrim(m.Tipfun)='I' 
	 @  5,22 SAY IIF(!EMPTY(substr(m.CodCal, 8,2)),val_para(substr(m.CodCal, 8,2),'CODPRG','V',26,40),SPAC(56))
	 @  6,22 SAY IIF(!EMPTY(substr(m.CodCal,10,3)),VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40),SPAC(56))
	 @  6, 2 SAY IIF(ALLTRIM(m.TipFun)='F','        Actividad :','         Proyecto :')
	 @  7,22 SAY IIF(!EMPTY(substr(m.CodCal,13,3)),VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4),SPAC(56))
     @  7,25 SAY '.'
	 @  7,26 SAY IIF(!EMPTY(substr(m.CodCal,16,2)),VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5),SPAC(56))
 else
    @  5,22 clear to 7,79
 endif	
 @  8,22 SAY m.destino picture '@S56'
 @  9,22 SAY m.Observa             && PRUEBA

 DO VISTA_HIJO

 RETURN

PROCEDURE VISTA_HIJO
*-------------------
private vest
hide popup all
SELECT ItePec
GO TOP
BROWSE ; 
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.numpec TIMEOUT 0.0001 ;
   WINDOW Wind_2 ;
   FIELDS;
   Tippec      : H= 'C' ,;
   numREF      : H= 'Ref.' ,;   
   CodArt      : H= 'C¢digo' :V=VALART():F:W=EMPTY(CodArt),;
   Descri      : H= 'Descripci¢n' :32 :W=.F.  ,;
   Coduni      : H= 'Uni'      :W=.F. :3 ,;
   tipreb      : h= 'R' ,;
   Canreq      : H= 'Cantd' :P='99,999.99' :W=!EMPTY(CodArt) ,;
   COSTOT      : H= 'Total' :P='99,999.99' :w=.f.
 
   SELE parkar
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
ON KEY LABEL F7 
ON KEY LABEL F9
SELE parkar
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO PERIODO+NUMPEC INTO ITEPEC
 SET SKIP TO ITEPEC
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumPec :H=' N§ ' ,;
 Est = IIF(Itepec.Estado= '00','Pend',IIF(Itepec.Estado = '30',itepec.NumOc,IIF(Estado='99','Anul',IIF(Itepec.Estado='50','Aten','S/Ct')))) :H='Estd' ,;
 Codfte :H='fte ' ,;
 FecPec :H='Fecha' ,;
 Coddep :H='DEP',;
 Itepec.CanReq : H='Cantid':P='99,999.99',;
 itepec.Descri :H='Detalle '
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SELE parkar
 SET RELA TO
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
ON KEY LABEL F7 
ON KEY LABEL F9
vtemp    = RECNO()
vPeriodo = RIGHT(DTOC(DATE()),2)
vNum_pec = '    '
vCod_fte = '   '
ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero parkar: ' GET vPeriodo PICTURE '!!'
 @ 1,27 SAY '-' GET vNum_Pec PICTURE '!!!!' VALID vBusca()
READ
DEACTIVATE WINDOW standby

 IF EMPTY(vNum_pec) .or. LASTKEY()=27
 	ON KEY LABEL F9 DO vista_det
    RETURN
 ELSE
    SEEK vPeriodo + vNum_pec &&+ alltrim(vcod_fte)
    IF !FOUND()
       DO standby WITH Vmens04
       GOTO Vtemp
    ELSE
       DO Vista
    ENDIF
 ENDIF
ON KEY LABEL F9 DO vista_det
RETURN

PROCEDURE vBusca
*---------------
vnum_pec=padl(alltrim(vnum_pec),4,'0')
retur .t.

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
SELE PARKAR
SCATTER MEMVAR
PUBLIC vMes,vPart,Ob
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF Estado = '99'
   * Anulada
   DO STANDBY WITH Vmens09
   RETURN
 ENDIF
 IF Estado = '70'
   * El parkar ha sido devuelto
   DO STANDBY WITH Vmens11
   RETURN
 ENDIF
 IF Estado = '50'
   * El parkar ya tiene O/C
   DO STANDBY WITH Vmens12
   RETURN
 ENDIF
 
 IF ESTADO = '30'
    DO STANDBY WITH 'El parkar ya tiene O/C' 
    return
 endif	
 
 IF ESTADO = '40'
    DO STANDBY WITH 'El parkar ya est  despachado '
    return
 endif    

 *****
SELECT parkar

ACTIVATE WINDOW Wind_1
DO PANTALLA
Ob = .F.
vCodPrg  = substr(m.codcal,08,2)
vCodSub  = substr(m.codcal,10,3)
vProyec  = substr(m.codcal,13,3) 
vCodact  = substr(m.codcal,13,2) 
vSubpry  = substr(m.codcal,16,4)
  
	@ 1,22 GET m.FecPec    PICTURE '@D'
	@ 1,60 GET m.Periodo   PICTURE '!!'    DISABLE
	@ 1,63 SAY '-'
	@ 1,63 GET m.NumPec    PICTURE '!!!!'  DISABLE
    @ 1,67 GET m.Tippec    picture '!' valid m.tippec$'PO' 	DISABLE
	@ 2,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_PARA( m.CodDep,'CODDEP',' ',22,40,7) AND VALATTE()
	@ 3,22 GET m.atte      VALID !EMPTY(m.CodDep)
	@ 4,22 GET m.CodFte    PICTURE '!!!'  VALID VAL_PARA(m.CodFte ,'CODFTE',' ',22,20) 
	@ 4,60 GET m.tipfun    PICTURE '!'    VALID VAL_PARA(m.tipfun,'TIPFUN',' ',60,15,2) &&and af_proy()
	@ 5,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) WHEN alltrim(m.tipfun)='I'
	@ 6,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40) WHEN alltrim(m.tipfun)='I'
	@ 7,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
	@ 7,26 GET vSubPry     PICTURE '!!'   VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
	@ 8,22 GET m.Codobra   PICTURE '!!!!'  VALID VAL_OBRA(m.CodObra,m.periodo+Alltrim(vCodprg)+alltrim(vcodsub)+alltrim(vProyec)+alltrim(vSubPry),'m.CodObra',' ',22,40) WHEN ALLTRIM(M.TIPFUN)='I' 
    @ 8,27 GET m.TipObr    PICTURE '!'    VALID VAL_para(m.tipobr ,'TIPOBR','C',26,40) AND AFDEST() WHEN !EMPTY(m.CodObra) and ALLTRIM(M.TIPFUN)='I'  
	@ 8,29 GET m.destino   picture '@S49' 
	@ 9,22 GET m.Observa   picture "@S40"
    READ
    
    IF LASTKEY() # 27 && Si se presion¢ Esc, no graba
       DO WHILE .T.
          Ok = Trabaja_Hijo()
          IF LASTKEY() # 27
             IF YESNO('¨ Conforme la correcci¢n ?')
                EXIT
             ENDIF
          ELSE
             IF YESNO('¨ Cancela la correcci¢n ?')
                ok = .F.
                EXIT
             ENDIF
          ENDIF
       ENDDO
       ** verifica si estan correctos
       m.codcal = m.periodo+padl(alltrim(str(month(m.fecpec),2)),2,'0') +ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+alltrim(vProyec)+alltrim(vSubPry)
       SELECT Itepec
       SEEK m.periodo+m.numpec &&+alltrim(m.Codfte)
       vorer=1
	   SCAN WHILE m.Periodo = Periodo and m.Numpec = Numpec  
		    IF RLOCK()
		      IF EMPTY(CODART) OR EMPTY(DESCRI) or  canreq=0
	  			 DELETE NEXT 1
    		  ELSE
		         REPLACE CodCal  WITH m.CodCal , TipPec with m.TipPec , Codfte with m.CodFte 
		      ENDIF
		    ENDIF
       ENDSCAN

       IF Ok .AND. LASTKEY() # 27
          SELECT parkar
		  m.user = sys(0)
          m.user_fc = date()
          m.user_TP = 'C'

          GATHER MEMVAR
       ELSE
          SELECT Itepec
       ENDIF
    ELSE
       *- No pudo agregar la liquidaci¢n
       SELECT parkar
    ENDIF
 DO Vista                    && Muestra nuevos datos
UNLOCK
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
SELECT PARKAR
SCATTER MEMVAR BLANK

vCodPrg  = space(2)  
vCodSub  = space(3)    
vProyec  = space(3) 
vSubpry  = space(2)
vCodAct  = space(3)
m.Fecpec  = DATE()
m.periodo = STR(YEAR(DATE())-1900,2)
Ob = .F.
ACTIVATE WINDOW Wind_1 

@  1,22 GET m.FecPec    PICTURE '@D'
@  1,60 GET m.Periodo   PICTURE '!!' 
read
 
IF LASTKEY()=27
   DO VISTA
   RETURN
endif    
 @  1,63 SAY '-'
 @  1,63 GET m.NumPec    PICTURE '!!!!' valid SIHAY() 
 @  1,68 GET m.Tippec    picture '!' valid m.tippec$'PO' 
 if m.tippec='P'
    @  2, 2 SAY "      Dependencia :"
    @  3, 2 SAY "             Atte :"
    @  2,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_PARA(m.CodDep,'CODDEP',' ',22,40,7) &&AND VALATTE()
    @  3,22 GET m.atte      VALID !EMPTY(m.CodDep)
 else
    @  2, 2 SAY "        Proveedor :" 
    @  2,22 GET m.CodPrv  PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,2,27) 
    @  3, 2 SAY space(70)
 endif    

 @  4,22 GET m.CodFte    PICTURE '!!!'  VALID VAL_PARA(m.CodFte ,'CODFTE',' ',22,20) AND SIHAY()
 @  4,60 GET m.tipfun    PICTURE '!'    VALID VAL_PARA(m.tipfun,'TIPFUN',' ',60,15,2) &&and af_proy() 

 @  5,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) WHEN alltrim(m.tipfun)='I'
 @  6,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40) WHEN alltrim(m.tipfun)='I'
 @  7,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
 @  7,26 GET vSubPry     PICTURE '!!'   VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
 @  8,22 GET m.Codobra   PICTURE '!!!!'  VALID VAL_OBRA(m.CodObra,m.periodo+Alltrim(vCodprg)+alltrim(vcodsub)+alltrim(vProyec)+alltrim(vSubPry),'m.CodObra',' ',22,40) WHEN ALLTRIM(M.TIPFUN)='I' 
 @  8,27 GET m.TipObr    PICTURE '!'    VALID VAL_para(m.tipobr ,'TIPOBR','C',26,40) AND AFDEST() WHEN !EMPTY(m.CodObra) and ALLTRIM(M.TIPFUN)='I'  

 @  8,29 GET m.destino   picture '@S49' 
 @  9,22 GET m.Observa   picture '@S40' 

 READ VALID VAL_READ()

 IF LASTKEY() # 27 
    m.codcal = m.periodo+padl(alltrim(str(month(m.fecpec),2)),2,'0') +ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+alltrim(vProyec)+alltrim(vSubPry)
    m.Estado = '00'
    m.fecemi = date()
    DO WHILE .T.
       Ok = Trabaja_Hijo()
       IF LASTKEY() # 27
          IF YESNO('¨ Confirme el ingreso ?')
             EXIT
          ENDIF
       ELSE
          DO STANDBY WITH ' Cancelado el Ingreso ..'
          ok = .F.
          EXIT
       ENDIF
    ENDDO
       
    IF Ok .AND. LASTKEY() # 27
        * verifica si estan correctos
	    SELECT Itepec
	    SEEK m.periodo+m.numpec
	    vNumOrd = 0
	    SCAN WHILE m.Periodo = Periodo and m.Numpec = Numpec 
		     IF F_LOCK(1)
			    IF EMPTY(CODART) OR EMPTY(DESCRI) 
		  	       DELETE NEXT 1
	    		ELSE
	    		   vnumord = vnumord + 1 
			       REPLACE CodCal  WITH m.CodCal,TipPec with m.tippec 
			    ENDIF
			 ENDIF
		ENDSCAN

		SELECT parkar
		IF F_APPD()
		   if !empty(m.Numpec)
		      m.user = sys(0)
  		      m.user_fc = date()
              m.user_TP = 'I'
              GATHER MEMVAR
           ENDIF   
        endif
    ELSE
        SELECT Itepec
        SEEK m.periodo+m.numpec 
        SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec 
             IF F_LOCK(1)
                DELETE NEXT 1
             ENDIF
        ENDSCAN
        UNLOCK
        SELE parkar
    ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
   SELE parkar
 ENDIF
 SELECT parkar
 DO Vista
 RETURN


PROCEDURE VAL_PE
*---------------
m.NumPec = Padl(alltrim(m.NumPec),4,'0')
return

PROCEDURE SIHAY
*--------------
PRIVATE AS
AS=RECNO()
SEEK m.periodo+m.NumPec 
if found() AND RECNO()#AS
   do standby with 'El parkar ya esta registrado'
   return .f.
endif   
return

PROCEDURE VALATTE
*----------------
SELECT Cuadro
SEEK  m.Periodo+m.Coddep

if found()
   m.atte = IIF(EMPTY(m.atte) or m.atte='No se Registra Solicitante..,revise',Cuadro.Atte,m.atte)
else
   m.atte = 'No se Registra Solicitante..,revise'
endif

SELECT parkar
return

PROCEDURE Trabaja_Hijo
*---------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELE ITEPEC
 SEEK m.periodo+m.numpec 

 IF !FOUND()
     DO Agreg_item
 ENDIF

 BROWSE NOREFRESH NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY m.periodo+m.numpec FIELD ;
     Tippec      : H= 'C' :V=(tippec$'OP'):F  ,;
     numref      : H= 'Refer' :P='!!!!' ,;     
     CodArt      : H= 'C¢digo' :V=VAL_ARTC(codArt) AND antr() :F ,;
     Descri      : H= 'Descripci¢n' :39 :W=.F.,;
     Coduni      : H= 'Uni' :W=.F. :5 ,;
     tipreb      : h= 'R' :V=(tipreb$'+-') ,;
     Canreq      : H= 'Cantd' :P='99,999.99' ,;
     COSTOT      : H= 'Total'
UNLOCK ALL

 ON KEY LABEL F5
 ON KEY LABEL F8
 ON KEY LABEL F10

 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT parkar

 RETURN .T.

procedure antr
*-------------
vAnt = itepec.Canreq
return .t.

PROCEDURE Valor
*--------------
sele itecn
seek m.Periodo+alltrim(m.Coddep)+ALLTRIM(ITEPEC.CodArt)
vPer = Trimestre(m.Fecpec)
vDispon = Itecn.Nec_&vPer - Itecn.Ped_&vPer

If Itepec.Canreq > vDispon + vant
   do standby with IIF(vdispon+vant=0,'Ya no Existe Art¡culos para este Trimestre','Ud. s¢lo puede pedir m ximo '+ALLTRIM(str(vdispon+VANT,5))+' para este Trimestre')
endif
  if rlock()
     replace Ped_&vper with Ped_&vper + ItePec.Canreq - vant
  endif
sele itepec
return .t.


PROCEDURE Anula
*---------------
 SELECT parkar
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR ‚ste parkar ?')
 vEstado = .t.
 SELECT Itepec
 SEEK m.periodo+m.numpec &&+ alltrim(m.codfte)
 SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec and alltrim(codfte)= codfte
      IF ESTADO='30' or estado='20' 
         DO STANDBY WITH 'La parkar ya tiene generada O/C o S/C,no se puede anular'
         vEstado = .f.            
         EXIT
 	 ENDIF
 ENDSCAN
 if vestado
    IF velimina 
	    SELECT Itepec
	    SEEK m.periodo+m.numpec &&+ alltrim(m.codfte)
	    SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec and alltrim(codfte)= codfte
		 if rlock()
		    replace estado with '99' 
         endif
	    endscan
   		sele parkar
	    REPLACE ESTADO WITH '99' ,FECVER WITH DATE()
    endif
 endif   
 sele parkar    
 DO Vista
 
 UNLOCK
 RETURN

PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON
SELECT parkar
VCON = RECNO()
SCATTER MEMVAR
fdbf = sys(3)+'*.DBF'
fidx = sys(3)+'*.IDX'
USE IN 5
USE IN 6
USE IN 7
copy stru to (fdbf)
use (fdbf) in 5  alias def exclu
select def
index on periodo+numpec to (fidx)
append blank
gather memvar
SET RELATION TO periodo + numpec INTO itepec
SET SKIP TO ITEPEC
if eof()
   do standby with vmens08
   return
else
  do reporte with 2,"parkar",' Pe.co.sa ',2
endif
SET RELATION TO 
USE IN 5
USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE IteArt   IN 6   order tag IteArt1  ALIAS Iteart
USE cdrnec   IN 7   order tag Cdrnec1  ALIAS cuadro
USE itecn    IN 8   order tag itecn3   ALIAS itecn
SELECT parkar
if vopcion=2
   set filter to coddep=substr(vcoddep,1,vnumdep)
endif   
GO VCON
DO VISTA
RETURN


PROCEDURE Agreg_Item
*-----------------
SELE ITEPEC
IF F_appd()
  REPLACE Periodo WITH m.Periodo ,;
          NumPec  WITH m.NumPec ,;
          Estado  WITH '00' ,;
          CodCal  WITH m.CodCal ,;
		  CodFte  with m.Codfte ,;	
          Tipfun  WITH m.Tipfun ,;
          Tippec  WITH m.tippec
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Elimi_Item
*-------------------
SELECT ItePec
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif
return

PROCEDURE Corri_Item
*-----------------
REPLACE CodCal  WITH m.CodCal
RETURN .T.

PROCEDURE LISTA
*--------------
ON KEY LABEL F7 
ON KEY LABEL F9
select parkar
vtemp =recno()
SET RELATION TO PERIODO+NUMPEC+CODFTE INTO ITEPEC
SET SKIP TO ITEPEC
if eof()
   do standby with vmens08
   return
else
   DO LISPEC
endif
select parkar
SET RELATION TO
if vopcion=2
   set filter to coddep=substr(vcoddep,1,vnumdep)
endif   
DO VISTA
RETURN

PROCEDURE LisPec
*---------------
vOrde = ORDER()
vrec  = RECNO()
DEFINE WINDOW LIS FROM 0,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado parkars' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vToPec,vToMes,vToFue,vTodep,vOrden,vtiplis,vTipRep
vNumpec = SPAC(4)
vFte    = SPAC(3)
vCodmes = SPAC(2)
vPeriod = SPAC(2)
vCoddep = SPAC(6)
vCodFte = SPAC(3)

@ 01,01 SAY "Todas las parkars : " GET vToPec  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToPec,2,22)  and assig()
@ 02,01 SAY "           parkar : "
@ 02,22 GET vNumpec  PICTURE '!!!!' WHEN vToPec = 2  &&VALID Padl(alltrim(vNumpec),4,'0')
@ 02,27 get vPeriod  Picture '!!'   WHEN vToPec = 2 
@ 02,30 SAY '-'
@ 02,31 GET vFte     PICTURE '!!!' VALID VAL_PARA(vfte,'CODFTE','C') AND ValPecO() WHEN vToPec = 2

@ 04,01 SAY "  Todos las Meses : " GET vTomes  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTomes,5,22)  WHEN vToPec = 1
@ 05,01 SAY "              Mes : "
@ 05,22 GET vCodmes  PICTURE '!!'  VALID VAL_PARA(vCodMes,'FECMES','C') WHEN vToPec = 1 AND vTomes = 2

@ 07,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTofue,8,22)  WHEN vToPec = 1
@ 08,01 SAY "           Fuente : "
@ 08,22 GET vCodFte  PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE','C')  WHEN vToPec = 1 and vTofue =2

@ 10,01 SAY "Todas las Dependc : " GET vToDep  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTodep,11,22)  WHEN vToPec = 1
@ 11,01 SAY "      Dependencia : "
@ 11,22 GET vCodDep  PICTURE '!!!!!!' VALID VAL_PARA(vCodDep,'CODDEP','C')  WHEN vToPec = 1 and vTodep =2

@ 13,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Dependencia;Emision'  WHEN vToPec = 1

@ 16,01 SAY "           Estado : " GET vTipLis  FUNCTION '^ Todos;Pendientes;Atendidos;Liquidados'  WHEN vToPec = 1

@ 19,01 SAY "           Estado : " GET vTipRep  FUNCTION '^ Resumido;Detallado'  WHEN vToPec = 1

@ 22,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1 AND LASTKEY()#27
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,Periodo+NumPec,IIF(vOrden=2,CodDep,DTOS(FECemi))) TO (vInd) ;
      FOR IIF(vToPec=1,.T.,Periodo+NumPec+CodFte = vPeriod+vNumpec+vFte) .AND. IIF(vTipLis=1,.T.,iif(vTipLis=2,Estado = '00',iif(vTipLis=3,Estado = '40',Estado='50')))
   SET FILTER TO iif(vToMes=1,.t.,IIF(vTipLis=4,month(fecdesp),month(fecpec))=val(vCodMes)) and iif(vToMes=1,.t.,month(fecpec)=val(vCodMes)) AND iif(vToFue=1,.t.,Codfte=alltrim(vCodfte)) and ;
		 		 iif(vToDep=1,.t.,CodDep=alltrim(vCodDep)) 
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipLis=1,' en General ',IIF(vTipLis=2,' Pendientes ',' Atendidos '))
   IF !EOF()
      if vtopec = 1
         if vTiprep = 1
	         do reporte with 2,"LisPecX",' Pe.co.sa ',1,.F.,.T.
	     else
	         do reporte with 2,"LisPec",' Pe.co.sa ',1,.F.,.T.
	     endif    
	  else
	     do reporte with 2,"LisPec1",' Pe.co.sa '
	  endif   
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILT TO
   CLOSE INDEX
   ERASE (VIND)
ENDIF
SELECT parkar
SET ORDE TO (VORDE)
GO TOP
GO VREC
RETURN

PROCEDURE ASSIG
*--------------
vNumpec = parkar.NUMPEC
vPeriod = parkar.PERIODO
vfte    = parkar.codfte
RETURN  .T.


PROCEDURE VALPECO              && Revisi¢n de BD en browse
*--------------
SELE parkar
IF EOF()
    DO standby WITH Vmens08
    RETURN
ENDIF
vNumpec=Padl(alltrim(vNumpec),4,'0')
SEEK '96'+vNumPec &&+alltrim(vFte)
IF !FOUND()
     SET FILTER TO Codfte = alltrim(vFte)	
	 SET RELATION TO PERIODO+NUMPEC INTO ITEPEC
	 SET SKIP TO ITEPEC
	 vtemp = RECNO()
	 HIDE MENU mMenu
	 ACTIVATE SCREEN
	 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	 DO Logos WITH Rotulo1,vTempo
	 ON KEY LABEL F10 KEYBOARD CHR(23)
	 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	 NumPec :H=' N§ ' ,;
	 Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
	 Codcal :H='Calendario' ,;
	 FecPec :H='Fecha' ,;
	 Coddep :H='DEP',;
	 Itepec.CanReq : H='Cantidad':P='99,999',;
	 itepec.Descri :H='Detalle '
	 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	 DO Logos WITH Rotulo1,vTempo
	 SELE parkar
	 SET RELATION TO
	 set filter to
ENDIF
vNumpec = Numpec
vFte    = Codfte
SHOW MENU mMenu
ON KEY LABEL F10
SELE parkar
RETURN

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  ON KEY LABEL F7 
  ON KEY LABEL F9
  ON KEY LABEL F4  
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  ON KEY LABEL F7 
  ON KEY LABEL F9
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

function valpec
*-----------------
parameter vnumpec,alis
alis = alias()
private vfun
vfun = .t.
m.numpec=padl(alltrim(str(vnumpec,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif
SELECT Parma
SEEK 'CORREL' + 'parkar'
REPLACE NumEnt WITH NumEnt + 1
select (alis)
return vfun

function valult
*--------------
parameter vrec
vrec = recno()
select parkar
set orde to parkar1
go bott
vnumpec = val(numpec)+1
m.numpec=padl(alltrim(str(vnumpec,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif

function repasa
*--------------
parameter vrec,vfun,vali,AS
vfun = .t.
vrec = recno()
vali = alias()
select parkar
set orde to parkar1
SET FILTER TO 
*SEEK '960482'
SEEK '961500'
AS = RECNO()
IF FOUND()
  go as
ENDIF  
*numr = 482
numr = 1500
do while .t.
   if val(numpec)=numr
      numr = numr + 1
      skip
      loop
   else 
      exit   
   endif   
enddo

m.numpec=padl(alltrim(str(numr,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
else
   IF F_APPD() 
     replace periodo with m.periodo,numpec with m.numpec, estado with '00', user_tp with 'E',user with sys(0)
     gh = recno()
     vnp = m.numpec
   ENDIF 
   UNLOCK
endif

SELECT Parma
SEEK 'CORREL' + 'parkar'
REPLACE NumEnt WITH NumR 
sele (vali)
return vfun


FUNCTION XVALART
*--------------
PARAMETERS _Cod
DC=ALIAS()
PRIVATE XX,YY,ZZ,vFun
vFun = .F.
SELEC ITEART
vTemp = RECNO()
SEEK 'B'+ALLTRIM(ITEPEC.CODART)
IF FOUND() AND !EMPTY(ITEPEC.CODART)
   SELECT Itecn
   seek m.Periodo+alltrim(m.Coddep)+itepec.Codart
   if found()
      SELECT itepec
         IF RLOCK()
          REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri,;
          tipcdr  WITH 'S'
          ENDIF
      vFun = .T.
   else
      do standby with 'Este producto no est  registrado en el Cuadro de Necesidades'
      SELE ITEPEC
      IF RLOCK()
        *REPLACE ITEPEC.CodArt  WITH spac(11) && CAMBIAR PARA VALIDAR LOS PROD. QUE NO ESTAN EN CN
         REPLACE TIPCDR WITH 'N'
      ENDIF
      vFun = .T.
  endif
ELSE
   SELE itepec
   ZZ=VAL_PARA(CodArt,'CODGEB','C')
   IF LASTKEY()=27
      RETURN .T.
   ENDIF
   IF ZZ
      XX = Val_Art(_Cod,.F.)
      IF XX
         YY = Val_ArtDet(SUBSTR(Alltrim(PRODU.CODART),2,6),.F.)
         IF YY and m.Tipdoc='S'
            SELECT Itecn
            seek m.Periodo+alltrim(m.Coddep)+itepec.Codart
            if !found()
               do standby with 'Este producto no est  registrado en el Cuadro de Necesidades'
               SELE ITEPEC
               if Rlock()
                   REPLACE ;
                   coduni  WITH Iteart.coduni,;
                   preuni  WITH Iteart.preuni,;
                   descri  WITH Iteart.descri,;
                   TIPCDR WITH 'N'
               endif
               vFun = .T.
            else
               SELECT Itepec
               if rlock()
                  REPLACE ;
                   coduni  WITH Iteart.coduni,;
                   preuni  WITH Iteart.preuni,;
                   descri  WITH Iteart.descri,;
                   TIPCDR WITH 'S'
               endif
               vFun = .T.
            endif
         ELSE
           if f_lock(1)
               REPLACE ITEPEC.codart  WITH spac(11)
           endif
           vFun = .F.
        ENDIF
      ELSE
        if f_lock(1)
            REPLACE ITEPEC.codart  WITH spac(11)
        endif
        vFun = .F.
      ENDIF
   ELSE
      if f_lock(1)
        REPLACE ITEPEC.codart  WITH spac(11)
      endif
      vFun = .F.
   ENDIF
ENDIF
ON KEY
ON KEY LABEL F5  DO Agreg_item
ON KEY LABEL F8  DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELEC itepec

RETURN vFun

PROCEDURE Trimestre
*------------------
parameter vFecha

DO CASE
   CASE MONTH(vFecha) = 1  OR MONTH(vFecha) = 2   OR  MONTH(vFecha) = 3
        vTrim = '1'
   CASE MONTH(vFecha) = 4  OR MONTH(vFecha) = 5   OR  MONTH(vFecha) = 6
        vTrim = '2'
   CASE MONTH(vFecha) = 7  OR MONTH(vFecha) = 8   OR  MONTH(vFecha) = 9
        vTrim = '3'
   CASE MONTH(vFecha) = 10 OR MONTH(vFecha) = 11  OR  MONTH(vFecha) = 12
        vTrim = '4'
ENDCASE
return vtrim

FUNCTION Observa
*---------------
vAlias = ALIAS()
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle parkar ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERVa WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF

RELEASE WINDOW Observa
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci¢n '
ENDIF

RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,18 TO 20,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle parkar ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERVa NOEDIT WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
RETURN .T.

FUNCTION Val_ArtC  && Articulos
*------------------
PARAMETER xcod,_tipo,_x,_y
oN KEY LABEL F5  
ON KEY LABEL F8  

** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo
_campo = VARREAD()
ORD=ORDER()
mAlias = ALIAS()
SELECT IteArt
GO TOP
_OldWnd = WOUTPUT()
v_Fun=.f.
v_ent =.f.

IF !mEdita
   SET ORDER TO ITEART3
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
   v_ent = found()
ENDI
   IF EMPTY(xcod) OR !V_ENT
      SET ORDER TO ITEART2
      GO TOP
      ACTIVATE SCREEN
      ON KEY LABEL F10 KEYBOARD CHR(23)
      ON KEY LABEL F2 DO FunBusDet
      ON KEY LABEL F5
      ON KEY LABEL F8
      DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
      BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
        CodArt   :H='C¢digo'     ,;
        Descri   :H='Nombre':60   ,;
        CodUni   :H='Unidad':7   
      ON KEY LABEL F10
      ON KEY LABEL F2
      RELEASE WINDOW _BusArt
      SET ORDER TO 1

      IF Lastkey()=27
         V_FUN = .f.
         v_ent = .f.
      ELSE
         xcod = CodArt
         xDes = Descri
         xuni = coduni
         IF mMsg
            @ _x,_y SAY Descri
         ENDIF
         SELECT (mAlias)
         IF !_tipo
            REPLACE &_campo WITH  xcod &&,itepec.descri with xdes ,itepec.coduni with xuni
         ENDIF
         v_fun = .T.
         v_ent = .t.
      ENDIF
   ENDIF
*ENDIF
vTipCdr='N'
IF v_ent and alltrim(m.tipfun)='F' and m.tippec='S'
   SELECT Itecn
   seek m.Periodo+alltrim(m.Coddep)+ALLTRIM(xCod)
   IF !FOUND()
      do standby with 'Este producto no est  registrado en el Cuadro de Necesidades'      
      vTipCdr='N'
   else
      vTipCdr='S'
   ENDIF
ENDIF   
SELECT itepec
IF RLOCK()
   REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri,;
          TIPCDR  WITH vTipCdr
ENDIF

SELECT (mAlias)
SET ORDE TO (ORD)
ON KEY LABEL F5  DO Agreg_item
ON KEY LABEL F8  DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
IF !V_ENT
   RETURN v_FUN
ELSE
   RETURN v_ENT
ENDIF   


PROCEDURE VUSUA
***************
PARAMETER CSYS
PRIVATE ALI
ALI = ALIAS()
VKEY = ALLTRIM(CSYS)
SELE USU
SEEK VKEY
VFUN = NOMBRE
SELE (ALI)
RETURN VFUN

FUNCTION BuscArt
*----------------
 as=alias()
 sele produ
 set orde to 1
 seek 'B'+LEFT(ITEPEC.CODART,6)
 if !found()
    vFun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
 else
    vfun=UPPER(produ.descri)
 endif
 sele (as)
return vfun


FUNCTION val_DEP
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong , mdist
PRIVATE malias
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
		mdist = 6
	CASE PARAMETERS() = 5
		mdist = 6
ENDCASE
malias  = ALIAS()

SELECT parma
SEEK filtro+mvalor

IF !FOUND() .AND. !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	SET FILTER TO tipo = filtro AND CODIGO = substr(vcoddep,1,vnumdep)
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
		ENDIF
		RETURN
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = parma.codigo
mcuenta= parma.descriau2
mdescr = SUBSTR( parma.descri, 1, mlong )
mdescriaux = SUBSTR( parma.descriaux, 1, mlong)
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable=' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable='D'   && En vista
		RETURN mdescr
	CASE mvariable='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE
