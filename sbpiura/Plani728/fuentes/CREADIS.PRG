**----------------------------------------------------------------------
** Backup. prg                                  Sistema Uni
** Copia los archivos del sistema
** Autor: Luis Castillo.
**----------------------------------------------------------------------
USE parmae   IN 1 ORDER TAG parmae1  ALIAS parma
USE personal IN 2 ORDER TAG persona7 ALIAS permae
USE totapor  IN 3 ORDER TAG totapor1 ALIAS totapoR
USE abono    IN 4 					 ALIAS abono exclusive
SELEC abono
zap

SELE Parma
SEEK "FECSIS01"

IF FOUND()
	cMes=LEFT(Descri,2)
	cAno = SUBSTR(Descri,4,4)
ENDIF


ACTIVATE WINDOW STANDBY
_drive = 'A:'
@ 01,01 SAY "En que drive est  sacando la Copia:  " GET _drive PICTURE '@M A:,B:,C:,F:'
READ
DEACTIVATE WINDOW STANDBY
IF LASTKEY() # 27
	if _drive = 'C:' OR _drive = 'F'
		do backup1 with 1
	 else
		DO STANDBY WITH 'Inserte el diskette en el drive ' + _drive
		IF LASTKEY()=27 OR .NOT. IsDisket(_Drive)
			RETURN
		ENDIF
		_Ano = cAno
		_MES = cMes
		ACTIVATE WINDOW STANDBY
		@ 01,00 SAY "Periodo a Copiar:" GET _Mes Valid Val_para(_Mes,'FECMES',' ',18)
		@ 01,31 GET cAno
		READ VALID Val_Read()
		
		DEACTIVATE WINDOW STANDBY
		SELECT PERMAE
		SET RELATION TO LIB_ELE+cAno+cMes INTO TOTAPOR
*		SET FILTER TO ALLT(NRO_CTA)#'     ' AND ALLT(NRO_CTA)#'0000000000' AND !EMPTY(LIB_ELE) AND !EMPTY(APE_NOM) AND LIB_ELE#'00000' AND REC_PER#'2' AND MES_VAC#ALLT(_MES) AND (Estado='10')																	&& PARA DEVENGADOS
		SET FILTER TO ALLT(NRO_CTA)#'     ' AND ALLT(NRO_CTA)#'0000000000' AND !EMPTY(LIB_ELE) AND !EMPTY(APE_NOM) AND LIB_ELE#'00000' AND REC_PER#'2' AND MES_VAC#ALLT(_MES) AND (Estado='03' or estado='08') AND (totapoR.tot_ing-totapoR.tot_des)#0			&& PARA PENSIONISTAS
		GO TOP
*SUSP		
		SCAN
			scatter memvar
			m.Cuenta  = m.Nro_Cta
			m.importe = totapoR.tot_ing - totapoR.tot_des
			SELECT Abono
			append Blank
			GATHER MEMVAR
			SELECT permae
		ENDSCAN
		SELECT ABONO
		USE
		SELECT permae
*		HIDE POPUP ALL
*		RESTORE SCREEN FROM Pantalla
		@ 10,10 CLEAR TO 14,60
		@ 10,10,14,60 BOX
		@ 12,12 SAY "Espere un momento, se est  sacando Copia"
		!FOXSWAP COPY ..\DATA\ABONO.DBF &_DRIVE
*		RESTORE SCREEN FROM Principal
		SHOW POPUP MENU
	endif
ENDIF
CLOS DATA

RETURN
