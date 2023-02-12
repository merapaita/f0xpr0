*-------------------------------------------------------------------------
* RegCal.Prg
* Registra Calendarios
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE parmae   IN 1   ORDER TAG parmae1  ALIAS parma
USE calen    IN 2   ORDER TAG calen3   ALIAS calen
USE maepre   IN 3   ORDER TAG maepre1  ALIAS maepre
USE maepar   IN 4   ORDER TAG maepar1  ALIAS presu
USE itepar   IN 5   ORDER TAG itepar2  ALIAS itepar
USE clase    IN 6   ORDER TAG clase1   ALIAS clase

*- Mensajes de aviso al usuario
vmens01 = 'Registro de Calendario'
vmens02 = ' Calendario : REVISION '
vmens04 = 'Dicho Calendario no fue encontrado'
vmens05 = 'No existe Calendario anterior'
vmens06 = 'No existe Calendario siguiente'
vmens07 = '¨ Desea Anular ‚ste Calendario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Calendario ha sido anulado'
vmens10 = 'El Calendario ya est  Atendido'
vmens11 = 'El Calendario ha sido devuelto'

SELECT calen
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO pantalla                  && Muestra pantalla inicial
DO vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO ven_accion
DO WHILE ven_accion
	ACTIVATE SCREEN
	ACTIVATE MENU mmenu
ENDDO

DO fin_opcion

RETURN


PROCEDURE inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente                             Listar  Termina '
DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_0 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_1 FROM 00,00 TO 11,79  DOUBLE ;
	TITLE vmens02 COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 12,00 TO 23,79 DOUBLE ;
	TITLE 'Detalle: Calendario' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_3 FROM 05,01 TO 15,78 DOUBLE ;
	TITLE '² ®F10¯ Escoge ²' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_4 FROM 05,08 TO 15,70 DOUBLE ;
	TITLE '² ®F10¯ Escoge ²' ;
	COLOR SCHEME 10

DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis   OF mmenu PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mmenu PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mmenu PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mmenu PROMPT '\<Siguiente'  AT 24,25
DEFINE PAD lista   OF mmenu PROMPT '\<Listar '    AT 24,63
DEFINE PAD termi   OF mmenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mmenu DO revis
ON SELECTION PAD busca  OF mmenu DO busca
ON SELECTION PAD anter  OF mmenu DO anter
ON SELECTION PAD proxi  OF mmenu DO proxi
ON SELECTION PAD lista  OF mmenu DO lista
ON SELECTION PAD termi  OF mmenu DO termi

RETURN


PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_0
CLEAR

@  1, 2 SAY "              Mes :"
@  1,40 SAY "          Periodo :"
@  2, 2 SAY "          Volumen :"
@  3, 2 SAY "           Sector :"
@  4, 2 SAY "           Pliego :"

@  6, 2 SAY " F.Financiamiento :"
@  7, 2 SAY "         Programa :"
@  8, 2 SAY "      SubPrograma :"
@  9, 2 SAY "     Tipo Funci¢n :"

@ 19, 2 SAY "          Partida :"
@ 20, 2 SAY "       Asignaci¢n :"
@ 20,40 SAY "     Saldo al mes :"
RETURN

PROCEDURE vista     && Coloca valores de BD en variables y pinta datos
*--------------
SELECT calen
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@  0,60 SAY IIF( m.estado= '00','Emitido  ',IIF( m.estado = '20','         ',IIF(m.estado='99','Anulado  ',IIF(m.estado='50','Atendido ',' ------- '))))
@  1,22 SAY val_para(m.nummes  ,'FECMES','V',26,20)
@  1,60 SAY m.periodo
@  2,22 SAY '02    GOBIERNOS REGIONALES'
@  3,22 SAY '01    PRESIDENCIA DEL CONSEJO DE MINISTROS'
@  4,22 SAY '001   REGION GRAU'
@  6,22 SAY val_para(m.codfte  ,'CODFTE','V',26,40)
@  7,22 SAY val_para(m.codprg  ,'CODPRG','V',26,40)
@  8,22 SAY VAL_SUBP(m.codsubpr,'CODSUB'+m.codprg,'V',22,40)
@  9,22 SAY val_para(m.tipfun  ,'TIPFUN','V',22,15)
@ 11,01 CLEAR TO 13,78
IF ALLTRIM(m.tipfun)='F'
	IF !EMPTY(m.codact)
		@ 11, 2 SAY "        Actividad : "
		@ 11,22 SAY m.codact
		@ 12, 2 SAY "                           "
		@ 13, 2 SAY "                           "
	ENDIF
ELSE
	IF !EMPTY(m.codproy)
		PUBLIC vproyecto
		=buscprg()
		@ 11, 2 SAY "         Proyecto : "
		@ 12, 2 SAY "      Subproyecto : "
		@ 11,22 SAY m.codproy
		@ 11,28 SAY LEFT(vproyecto,50)
		@ 12,22 SAY m.codsupry
		@ 12,28 SAY m.dessupry
		IF !EMPTY(m.codobra)
			@ 13, 2 SAY "             Obra : "
			@ 13,22 SAY m.codobra
		ENDIF
	ENDIF
ENDIF

@ 15,2  CLEAR TO 18,60

IF !EMPTY(m.descri)
	@ 15, 2 SAY "      Descripci¢n :"
	@ 15,22 SAY SUBSTR(m.descri,1,56)
	@ 16,22 SAY SUBSTR(m.descri,57,56)
ENDIF
IF ALLTRIM(m.tipfun)='I'
	@ 18, 2 SAY "        Ubicaci¢n :"
	@ 18,22 SAY m.ubicac
ENDI
@ 19,22 SAY m.codpart
@ 19,28 SAY val_part(SUBSTR(m.codpart,4,2),LEFT(m.codpart,2),'D',28,50)
@ 20,22 SAY m.valpart PICTURE '999,999,999.99'
@ 20,60 SAY m.valpre  PICTURE '999,999,999.99'
RETURN


PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE calen
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	codpart :H='Part.' ,;
	xx = val_para( nummes ,'FECMES','D',2,3) :H='MES' ,;
	tipfun  :H='Tp' ,;
	codfte  :H='F Fto',;
	codprg  :H='Prg',;
	codsubpr :H='SubPrg',;
	codact  :H='Act',;
	codproy :H='Proy',;
	codsupry:H='Spry',;
	codobra :H='Obra',;
	descri :H='Obs'
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
*Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE calen
DO vista
RETURN


PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vperiodo = RIGHT(DTOC(DATE()),2)
AS=ORDER()
vnum_mes = 0
vnummes  = '  '
vcodfte  = '   '
vtipfun  = ' '
vcodprg  = '  '
vcodsubpr= '   '

DEFINE WINDOW lis FROM 11,12 TO 20,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lis


@  1,2 SAY ' Periodo : ' GET vperiodo   PICTURE '!!'
@  2,2 SAY '     Mes : ' GET vnummes    PICTURE '!!'  VALID val_para(vnummes  ,'FECMES',' ',14,30)
@  3,2 SAY '  Fuente : ' GET vcodfte    PICTURE '!!!' VALID val_para(vcodfte,'CODFTE',' ',14,30)
@  4,2 SAY 'Programa : ' GET vcodprg    PICTURE '!!'  VALID val_para(vcodprg,'CODPRG',' ',14,30)
@  5,2 SAY 'SubProg. : ' GET vcodsubpr  PICTURE '!!!' VALID VAL_SUBP(vcodsubpr,'CODSUB'+vcodprg,' ',14,30)
@  6,2 SAY '   Nivel : ' GET vtipfun    PICTURE '!'   VALID val_para(vtipfun,'TIPFUN',' ',14,30)

READ

DEACTIVATE WINDOW lis

IF EMPTY(vnummes) .OR. LASTKEY()=27
	RETURN
ELSE
	SET ORDER TO calen4
	SEEK vperiodo+ALLTRIM(vnummes)+ALLTRIM(vcodfte)+ALLTRIM(vcodprg)+ALLTRIM(vcodsubpr)+ALLTRIM(vtipfun)
	IF !FOUND()
		DO standby WITH vmens04
		SET ORDER TO (AS)
		GOTO vtemp
	ELSE
		SET ORDER TO (AS)
		DO vista
	ENDIF
ENDIF


PROCEDURE vbusca
*---------------
vnum_pec=PADL(ALLTRIM(STR(vnum_pec,4)),4,'0')
RETUR .T.

PROCEDURE anter
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !BOF()
	SKIP -1
ENDIF
IF BOF()
	GO TOP
	DO standby WITH vmens05
ELSE
	DO vista
ENDIF
RETURN


PROCEDURE proxi
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !EOF()
	SKIP
ENDIF
IF EOF()
	DO standby WITH vmens06
	GO BOTTOM
ELSE
	DO vista
ENDIF
RETURN

PROCEDURE lista
*--------------
USE repocal  IN 7                      ALIAS REPO   EXCLUSIVE
USE repcal1  IN 8                      ALIAS rep1   EXCLUSIVE
SELE REPO
ZAP
SELE rep1
ZAP
SELECT calen
vtemp =RECNO()
vperiodo = RIGHT(STR(YEAR(DATE()),4),2)
vnummes  = '  '
vcodfte  = '   '
vtipfun  = ' '
vcodprg  = '  '
vcodpart = '  .  '
IF yesno (" Marco Presupuestal ?")
	*------Listado Marco Presupuestal----
	DEFINE WINDOW lis_1 FROM 08,13 TO 15,67 DOUBLE ;
		TITLE ' °° Calendario Marco Presupuestal °° ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1
	@  1,2 SAY '  Periodo : ' GET vperiodo   PICTURE '!!'
	@  2,2 SAY '  Fte Fin : ' GET vcodfte    PICTURE '!!!' VALID val_para(vcodfte,'CODFTE',' ',15,30)
	@  3,2 SAY '  Programa: ' GET vcodprg    PICTURE '!!'  VALID val_para(vcodprg,'CODPRG',' ',15,30)
	@  4,2 SAY '    Nivel : ' GET vtipfun    PICTURE '!'   VALID val_para(vtipfun,'TIPFUN',' ',15,30)
	READ
	DEACTIVATE WINDOW lis_1
	IF LASTKEY()= 27
		DO vista
		RETURN
	ENDIF

	IF EOF()
		DO standby WITH vmens08
		RETURN
	ELSE
		SELE rep1
		vind = SYS(3) + '.IDX'
		INDEX ON periodo+codfte+codprg+codsubpr+IIF(ALLTRIM(vtipfun)='I',codproy+codsupry,codact)+codpart TO (vind)
		SELE calen
		SET FILTER TO codfte=ALLTRIM(vcodfte)  AND periodo=ALLTRIM(vperiodo) AND codprg=ALLTRIM(vcodprg) AND tipfun = ALLTRIM(vtipfun)
		GO TOP
		SCAN
			SCATTER MEMVAR
			vrep2 = m.periodo+m.codfte+m.codprg+m.codsubpr+IIF(ALLTRIM(vtipfun)='I',m.codproy+m.codsupry,m.codact)
		    STORE 0 TO 	m.C_01,m.C_02,m.C_03,m.C_04,m.C_05,m.C_06,m.C_07,m.C_08,m.C_09,m.C_10,m.C_11,m.C_12
			SELECT rep1
			SEEK ALLTRIM(vrep2)
			IF !FOUND()
				vcod='C_'+ALLTRIM(m.nummes)
				m.&vcod=m.valpart
				APPEND BLANK
				GATHER MEMVAR
				vkey=ALLTRIM(m.periodo)+ALLTRIM(m.codfte)+ALLTRIM(m.codprg)+ALLTRIM(m.codsubpr)+IIF(ALLTRIM(vtipfun)='I',ALLTRIM(m.codproy)+ALLTRIM(m.codsupry),alltrim(m.codact))
				SELE itepar
				GO TOP
				SEEK vkey
				IF FOUND()
					SELE rep1
					REPLACE valpres WITH itepar.valpart
					* RETURN
				ENDIF
				m.valpart=0
			ELSE
				vcod='C_'+ALLTRIM(m.nummes)
			 *	IF &vCod>0
			    IF ALLTRIM(vtipfun)='F'
		 		   &vcod = &vcod + m.valpart
			 	   append blank
		 		   gather memvar
		 		   ENDIF
			*	else   
					IF RLOCK()
						REPLACE &vcod WITH m.valpart
					ENDIF
		 		endif		
			 	m.valpart = 0
*			ENDIF
			SELECT calen
		ENDSCAN
		SELE rep1
		SET INDEX TO (vind)
		IF ALLTRIM(VTIPFUN)='I'
			DO reporte WITH 2,"LisCal3",' A nivel de Marco Presupuestal '
		ELSE			
			DO reporte WITH 2,"LisCal4",' A nivel de Marco Presupuestal '		
		endif	
		CLOSE DATA
		ERASE (vind)
		USE parmae   IN 1   ORDER TAG parmae1  ALIAS parma
		USE calen    IN 2   ORDER TAG calen3   ALIAS calen
		USE maepre   IN 3   ORDER TAG maepre1  ALIAS maepre
		USE maepar   IN 4   ORDER TAG maepar1  ALIAS presu
		USE itepar   IN 5   ORDER TAG itepar2  ALIAS itepar
		USE clase    IN 6   ORDER TAG clase1   ALIAS clase
	ENDIF
	SELECT calen
	SET ORDE TO 1
	RETURN

ELSE

	DEFINE WINDOW lis_1 FROM 08,13 TO 15,67 DOUBLE ;
		TITLE ' °° Listado Calendario °° ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1
	@  1,2 SAY '  Periodo : ' GET vperiodo   PICTURE '!!'
	@  2,2 SAY '  Fte Fin : ' GET vcodfte    PICTURE '!!!' VALID val_para(vcodfte,'CODFTE',' ',15,30)
	@  3,2 SAY '      Mes : ' GET vnummes    PICTURE '!!'  VALID val_para(vnummes,'FECMES',' ',15,30)
	@  4,2 SAY '    Nivel : ' GET vtipfun    PICTURE '!'   VALID val_para(vtipfun,'TIPFUN',' ',15,30)
	READ
	DEACTIVATE WINDOW lis_1

	IF LASTKEY()= 27
		DO vista
		RETURN
	ENDIF

	IF EOF()
		DO standby WITH vmens08
		RETURN
	ELSE
		IF vtipfun = 'F'

			SELE REPO
			vind = SYS(3) + '.IDX'
			INDEX ON periodo+nummes+tipfun+codfte+codprg+codsubpr+codact TO (vind)
			SELE calen
			SET FILTER TO codfte=ALLTRIM(vcodfte) AND nummes=ALLTRIM(vnummes) AND periodo=ALLTRIM(periodo) AND tipfun=ALLTRIM(vtipfun)
			GO TOP
			SCAN
				SCATTER MEMVAR
				vrep2 = m.periodo+m.nummes+m.tipfun+m.codfte+m.codprg+m.codsubpr+m.codact
				SELECT REPO
				SEEK ALLTRIM(vrep2)
				IF !FOUND()
					vcod='C_'+LEFT(m.codpart,2)	
					APPEND BLANK
					m.&vcod=m.valpart
					GATHER MEMVAR
					m.&vcod=0
				ELSE
					vcod='C_'+LEFT(m.codpart,2)
					IF RLOCK()
						REPLACE &vcod WITH m.valpart
					ENDIF
				ENDIF
				SELECT calen
			ENDSCAN
			SELE REPO
			GO  TOP
			SET INDEX TO (vind)
			DO reporte WITH 2,"LisCal1",' A nivel Funcionamiento '
			CLOSE DATA
			ERASE (vind)
			USE parmae   IN 1   ORDER TAG parmae1  ALIAS parma
			USE calen    IN 2   ORDER TAG calen3   ALIAS calen
			USE maepre   IN 3   ORDER TAG maepre1  ALIAS maepre
			USE maepar   IN 4   ORDER TAG maepar1  ALIAS presu
			USE itepar   IN 5   ORDER TAG itepar2  ALIAS itepar
			USE clase    IN 6   ORDER TAG clase1   ALIAS clase
			SELECT calen
			SET ORDE TO 1
			RETURN
		ELSE
			SET ORDER TO calen7
			SET FILTER TO codfte=ALLTRIM(vcodfte) AND nummes=ALLTRIM(vnummes) AND periodo=ALLTRIM(periodo) AND tipfun=ALLTRIM(vtipfun)
			
			DO reporte WITH 2,"LisCal2",' A nivel de Proyectos de Inversi¢n '
		ENDIF
	ENDIF
ENDIF
SET FILTER TO
SELE calen
GO vtemp
DO vista
RETURN


PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
RELEASE    WINDOW wind_c1
RELEASE    MENU   mmenu
RESTORE SCREEN FROM principal
RETURN


PROCEDURE valcal
*-----------------
PARAMETER vnumcal,vsino
PRIVATE vfun
vfun = .T.
IF ALLTRIM(vsino)='S'
	m.codobra=PADL(ALLTRIM(STR(vnumcal,2)),2,'0')
	IF m.codobra = '00' OR EMPTY(m.codobra)
		vfun = .F.
	ENDIF
ENDIF
RETURN vfun

PROCEDURE buscprg
*----------------
*BUSCAR ACTIVIDAD
PRIVATE vkey
vkey=ALLTRIM(m.periodo)+ALLTRIM(m.codprg)+ALLTRIM(m.codsubpr)+ALLTRIM(m.codproy)
SELECT maepre
SET ORDER TO 2
SEEK vkey
vproyecto = maepre.descri
SELECT calen
RETURN (vproyecto)


PROCEDURE buscpro
*----------------
*BUSCAR PROYECTO
*BROW
RC=RECNO()
PRIVATE vkey
vkey=ALLTRIM(periodo)+ALLTRIM(codprg)+ALLTRIM(codsubpr)+ALLTRIM(codproy)
SELECT maepre
SET ORDER TO 2
SEEK vkey
vproyecto = maepre.descri
SELECT calen
GO RC
*BROW
RETURN (vproyecto)

FUNCTION sumaprg
*---------------
PARAMETER PART
totact = 0
prg = repo.codprg
vrec = RECNO()
GO TOP
SUM &part TO TOTAL FOR codprg = prg
GO vrec
RETURN TOTAL

FUNCTION sumact
*--------------
PARAMETER PART
prg  = repo.codprg+repo.codsubpr
vrec = RECNO()
GO TOP
SUM &part TO total1 FOR repo.codprg+repo.codsubpr = prg AND !EMPTY(codact)
GO vrec
RETURN total1


FUNCTION totact
*--------------
total2 = c_01+c_02+c_03+c_04+c_05+c_06+c_07+c_08+c_09+c_10+c_11+c_12+c_14
RETURN total2

FUNCTION totnew
*----------------
totnew = sumact('C_01')+sumact('C_02')+sumact('C_03')+sumact('C_04')+sumact('C_05')+sumact('C_06')+sumact('C_07')+sumact('C_09')+sumact('C_10')+sumact('C_11')+sumact('C_12')+sumact('C_14')
RETURN totnew

FUNCTION sumtotprg
*-----------------
vkey = codprg
vrec = RECNO()
GO TOP
SUM valpart TO suma FOR codprg = vkey
GO vrec
RETURN suma

FUNCTION sumasuprg
*---------------
vkey = codprg + codsubpr
vrec = RECNO()
GO TOP
SUM valpart TO sumaq FOR codprg+codsubpr = vkey
GO vrec
RETURN sumaq

FUNCTION sumasubpy
*---------------
vkey = codprg + codsubpr + codproy + codsupry + codpart
vrec = RECNO()
GO TOP
SUM valpart TO sumt FOR codprg+codsubpr+codproy+codsupry + codpart = vkey
GO vrec
RETURN sumt

FUNCTION totsupry
*--------------
totalm = C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12
RETURN totalm

FUNCTION totsalspry
*--------------
PUBLIC totals
totals = valpres-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12)
RETURN totals

FUNCTION sumsupry
*-----------------
PARAMETER PART
prg =rep1.codprg+rep1.codsubpr+rep1.codproy
vrec = RECNO()
GO TOP
SUM &part TO sumam FOR rep1.codprg+rep1.codsubpr+rep1.codproy=prg
GO vrec
RETURN sumam

FUNCTION sumspytot
*---------------
vkey = codprg + codsubpr+codproy + codpart
vrec = RECNO()
GO TOP
SUM valpart TO sumt FOR codprg+codsubpr+codproy+codpart = vkey
GO vrec
RETURN sumt

FUNCTION sumspyau
*---------------
vkey = codprg + codsubpr+codproy
vrec = RECNO()
GO TOP
SUM valpres TO sumau FOR codprg+codsubpr+codproy = vkey
GO vrec
RETURN sumau

FUNCTION salpry
*---------------
vkey = codprg + codsubpr+codproy
vrec = RECNO()
GO TOP
SUM valpart TO sum1 FOR codprg+codsubpr+codproy = vkey
GO TOP
SUM valpres TO sum2 FOR codprg+codsubpr+codproy = vkey
GO vrec
RETURN sum2-sum1

FUNCTION salsprg
*---------------
vkey = codprg + codsubpr
vrec = RECNO()
GO TOP
SUM valpart TO sum1 FOR codprg+codsubpr = vkey
GO TOP
SUM valpres TO sum2 FOR codprg+codsubpr = vkey
GO vrec
RETURN sum2-sum1

FUNCTION salprg
*---------------
vkey = codprg
vrec = RECNO()
GO TOP
SUM valpart TO sum1 FOR codprg = vkey
GO TOP
SUM valpres TO sum2 FOR codprg = vkey
GO vrec
RETURN sum2-sum1

FUNCTION sumspgau
*---------------
vkey = codprg + codsubpr
vrec = RECNO()
GO TOP
SUM valpres TO sumsg FOR codprg+codsubpr = vkey
GO vrec
RETURN sumsg

FUNCTION sumspg
*-----------------
PARAMETER PART
prg =rep1.codprg+rep1.codsubpr
vrec = RECNO()
GO TOP
SUM &part TO sumsg FOR rep1.codprg+rep1.codsubpr=prg
GO vrec
RETURN sumsg

FUNCTION sumspgtot
*---------------
vkey = codprg + codsubpr
vrec = RECNO()
GO TOP
SUM valpart TO sumt FOR codprg+codsubpr = vkey
GO vrec
RETURN sumt

FUNCTION sumprgau
*---------------
vkey = codprg
vrec = RECNO()
GO TOP
SUM valpres TO sumsg FOR codprg = vkey
GO vrec
RETURN sumsg

FUNCTION sumar_prg
*-----------------
PARAMETER PART
prg =rep1.codprg
vrec = RECNO()
GO TOP
SUM &part TO sumsg FOR rep1.codprg=prg
GO vrec
RETURN sumsg

FUNCTION sumprgtot
*---------------
vkey = codprg
vrec = RECNO()
GO TOP
SUM valpart TO sumt FOR codprg = vkey
GO vrec
RETURN sumt

FUNCTION sumSACtot
*---------------
vkey = CODSUBPR+codACT
vrec = RECNO()
GO TOP
SUM valpart TO sumt FOR CODSUBPR+codACT = vkey
GO vrec
RETURN sumt

PROCEDURE valpreS
*----------------
as = alias()
select itepar
set orde to iif(alltrim(vtipfun)='I',2,7)
vkeyF=alltrim(vPeriodo)+alltrim(vCodprg)+alltrim(vCodfte)+alltrim(vTipfun)+rep1.codsubpr+rep1.codact
vkeyI=alltrim(vPeriodo)+alltrim(vCodfte)+alltrim(vCodprg)+rep1.Codsubpr+rep1.codproy+rep1.codsupry
seek iif(alltrim(vtipfun)='I',vKeyI,vKeyF)
vtotal = 0
vcresu = 0
scan while iif(alltrim(vtipfun)='F',Periodo+CodPrg  +CodFte+TipFun  +CodSubpr+CodAct = vkeyF,Periodo+CodFte  +CodPrg+CodSubpr+CodProy +CodSuPry=vkeyI)
    if right(itepar.codpart,2) # '00'
  		vtotal = vtotal + valpart 
		vcresu = vcresu + cresup
	endif		
endscan
sele (as)
return (vtotal+vcresu)      


PROCEDURE TOTALPRG
*-----------------
PARAMETER VHAY
vkey = &VHAY
TOTALT = 0
vrec = RECNO()
GO TOP
SCAN FOR &VHAY=VKEY
	 totalm = C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12     
	 TOTALT=TOTALT+TOTALM
ENDSC
GO TOP
GO vrec
RETURN TOTALT
