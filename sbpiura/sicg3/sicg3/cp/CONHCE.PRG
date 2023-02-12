* ---------------------------------------------------
** SALPRE.PRG
** Reporte Ejecucion ESTADOS
** ---------------------------------------------------
* Abrir Archivos de trabajo
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maepre   IN 2  ORDER TAG maepre1  ALIAS maepre
USE IteHc    IN 3  ORDER TAG Itehc1   ALIAS Itehc
USE HOJCON   IN 4  ORDER TAG HOJCON1  ALIAS HOJA

PUBLIC vCodSub,vCodAct,vProyec,vSubpry,vCalend 
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Presupuesto'
vmens02 = ' Presupuesto : REVISION '
vmens04 = 'Dicho Presupuesto no fue encontrado'
vmens05 = 'No existe Presupuesto anterior'
vmens06 = 'No existe Presupuesto siguiente'
vmens07 = '¨ Desea Anular ‚ste Presupuesto ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Presupuesto ha sido anulado'
vmens10 = 'El Presupuesto ya est  Atendido'
vmens11 = 'El Presupuesto ha sido devuelto'

TOT = 0
TOT1 = 0
DO Inicia
DO salida
CLOSE DATA
RETURN


PROCEDURE Inicia
*--------------

PRIVATE VTEXP
ord   = ORDER()
vteXp = RECNO()
store 0 to vTotal
STORE 1 TO vtipo
vUniges = '01'
vUnieje = '001'
store space(2) to vperiodo,vcodfte,VCODFUN,VCALEND
store space(3) to vcodprg
store space(5) to vcodcom
	*------ SALDO PRESUPUESTAL -----------

	DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
		TITLE ' °°  Saldo Presupuestal °° ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1
	@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
    @  1,2 say '         Mes : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)
	@  3,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    VALID val_para(vUniGes,'UNIGES',' ',18,30)
	@  4,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 	VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)

	@  6,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		
	@  7,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 	VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) 
	
	@  9,2 SAY ' X Componete : ' GET vTipo   	FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
	@ 10,2 SAY '  Componente : ' GET vcodcom  	PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)	when vTipo=1

	@ 12,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	VALID val_para(vcodfte,'CODFTE',' ',18,30)

	READ VALID val_read()

	DEACTIVATE WINDOW lis_1

	IF LASTKEY()= 27
		RETURN
	ENDIF
	SELE itehc
	SET RELATION TO NUMMES+NUMHC INTO HOJA
	SET SKIP TO HOJA
	IF EOF()
		DO standby WITH vmens08
	ELSE
   		DO ESPERA1 WITH 1,"Espere un momento por favor"

		vkey = ALLT(vUniGes) + ALLT(vUniEje) + IIF(!EMPTY(VCODfun),ALLT(vCodFun),'') +IIF(!EMPTY(VCODprg),ALLT(vCodPrg),'') + ALLT(vCodFte)
		*- Abriendo Archivos
		vInd = SYS(3) + '.DBF'
		COPY STRU TO (vInd)
		use (vind) in 6 alias itehc1 EXCLUSIVE
		vind = SYS(3) + '.IDX'
		SELEC itehc1
		INDEX ON LEFT(codart,5)+LEFT(codpart,2)+codfte+NUMHC TO (vind)
		
		SELEC itehc
		SET FILTER TO NUMMES=allt(vcalend) and IIF(!EMPTY(ALLT(vcodcom)),codcom=allt(vcodcom),.T.) and Codfte=allt(vcodfte) AND ;
			UniGes=ALLT(vUniGes) and UNIeje=ALLT(vUniEje) and estado # '99' AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
		GO TOP
		SCAN
			SCATTER MEMVAR
			sele maepre
			seek Vperiodo+itehc.uniges+itehc.unieje+itehc.codcad
			vkey1= uniges + unieje +IIF(!EMPTY(VCODfun),CodFun,'') +IIF(!EMPTY(VCODprg),CodPrg,'') + ITEHC.CODFTE
			SELE itehc
			IF vKey = vKey1
				SELEC itehc1
				vkey2 = maepre.codfun+maepre.codprg+LEFT(ITEHC.codpart,2)+ITEHC.codfte+ITEHC.NUMHC
				SEEK vkey2
				IF FOUND()
					REPLACE valpart with valpart+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
				ELSE
					APPEND BLANK
					GATHER MEMVAR
					replace codart with maepre.codfun+maepre.codprg
					replace descri with hoja.nombre
					replace modif  with hoja.numref
					REPLACE valpart with IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
				ENDIF	
			ENDIF
			SELECT ITEHC
		ENDSCAN		
		SELE ITEHC1
		GO TOP	
	   	do espera1 with 2
		IF EOF()
			DO standby WITH 'No existe Registros para procesar'
		ELSE
		USE COMPAG IN 7 ORDER TAG COMPAG4
		SET RELATION TO itehc1.nummes+ itehc1.numhc INTO COMPAG ADDITIVE
			if vtipo=2
				DO reporte WITH 2,"ConHc1",' Consolidado de Afectaci¢n ',1,.F.,.T.
			else
				vind = SYS(3) + '.IDX'
				INDEX ON LEFT(CODART,5)+CODCOM+LEFT(CODPART,2)+CODFTE+codcad+NUMHC TO (vind)
				set relation to '97'+UNIGES+UNIEJE+CODCAD into maepre
				GO TOP
				DO reporte WITH 2,"ConHcE3",' Consolidado de Afectaci¢n x Componente',1,.F.,.T.
			endif
			vind = SYS(3) + '.IDX'
			INDEX ON LEFT(codpart,2)+LEFT(codart,5) TO (vind)
			GO TOP
			DO reporte WITH 2,"ConHc2",' Consolidado de GENERAL x Componente ',1,.F.,.T.
		ENDIF
	ENDIF
return


PROCEDURE Salida
*---------------
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN

FUNCTION totfun
*--------------
PARAMETER vkEY
vrecno = RECNO()
SUM valpart TO vsuma FOR LEFT(codart,2)=vkey
GO vrecno
RETURN vsuma

FUNCTION totprg
*--------------
PARAMETER vkEY
vrecno = RECNO()
SUM valpart TO vsuma FOR LEFT(codart,5)=vkey
GO vrecno
RETURN vsuma

FUNCTION totCOM
*--------------
PARAMETER vkEY
vrecno = RECNO()
SUM valpart TO vsuma FOR LEFT(codart,5)+CODCOM=vkey
GO vrecno
RETURN vsuma

FUNCTION DESTINO
*---------------
PARAMETER VKEY
VRECNO = RECNO()
VALIAS = ALIAS()
SELE HOJA
SEEK VKEY
IF FOUND()
	SELE (VALIAS)
	go vrecno
	RETURN HOJA.DESTINO
ELSE
	SELE (VALIAS)
	go vrecno
	RETURN 'No Tiene'
ENDIF	