*------------------------------------------------------------------
* InsSoc.Prg
* Registra Socios
* Autor: Analista Segundo Correa Sandoval
*--------------------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maesoc   IN 2  ORDER TAG maesoc1  ALIAS socio
USE itecob   IN 3  ORDER TAG itecob3  ALIAS iteCOB
USE maecob   IN 4  ORDER TAG maeCOB1  ALIAS COBRA
*-
*- Mensajes de aviso al usuario
vmens01 = 'No hay Registros'
vmens02 = 'Reporte de Liquidaci¢n'
SELECT itecob
SET RELATIO TO CODCON INTO SOCIO
SET SKIP TO SOCIO
GO TOP
DO LISTA
DO fin_opcion
RETURN

PROCEDURE lista
*--------------
vAlias = Alias()
vorder = Order()
vrecno = Recno()
STORE 1  TO vToCli,vOrden,vTipPro,vLista,vTofue,vTomes
STORE date() to vfecini,vfecfin
vCli = SPACE(4)

DEFINE WINDOW LIS FROM 6,15 TO 18,65 FLOAT DOUBLE TITLE 'Liquidaci¢n de Cobradores' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 01,01 SAY "       Todos : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,1,22)

@ 03,01 SAY "      C¢digo : " 
@ 03,22 GET vCli    PICTURE '!!!!!!' VALID Val_Hc() when vTOCLI=2

@ 05,01 SAY "   Cobranzas : " GET vToFue  FUNCTION '*RNH \<Todas;\<Por fechas' SIZE 1,10,6  VALID VALTOD(VTOfue,5,15) 
@ 07,01 SAY "      Fechas : "
@ 07,22 GET vfecini  PICTURE '@D'   COLOR SCHEME 7 WHEN vtofue=2 
@ 07,32 GET vfecfin  PICTURE '@D'   COLOR SCHEME 7 VALID (vfecfin >= vfecini)  WHEN vtofue=2 

@ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	IF vtocli=2
		SET FILTER TO codcob=vcli and iif(vtofue=2,BETWEEN(fecpag,vfecini,vfecfin),.t.)
	Else
		SET FILTER TO iif(vtofue=2,BETWEEN(fecpag,vfecini,vfecfin),.t.)
	endif
	IF !EOF()
		do reporte with 2,"liqcob",'Liquidaci¢n de Promotores',2
	ELSE
		DO STANDBY WITH vmens01
	ENDIF
ENDIF
SET FILTER TO
SET RELATION TO
SELECT (vAlias)
SET ORDER TO (vorder)
RETURN

PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RESTORE SCREEN FROM principal
RETURN

PROCEDURE VAL_HC             && Revisi¢n de BD en browse
*---------------
 ON KEY LABEL F9
 SELECT COBRA
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK vCli
 IF !FOUND()
    Vtemp = RECNO()
    ACTIVATE SCREEN
    DEFINE WINDOW wind_0 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10
    vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
    ON KEY LABEL F10 KEYBOARD CHR(23)
    BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	codIGO    :H='Codigo',;
	apelli    :H='Apellido',;
	nombre    :H='Nombre'
    vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
    IF LASTKEY()=27
       GO BOTT
    ENDIF
 ENDIF
 vcli = CODIGO
 ON KEY LABEL F10
 ON KEY LABEL F9 DO VISTA_DET
 SELECT ITECOB
RETURN
