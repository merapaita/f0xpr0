** -----------------------------------------------------------------
** APEMES.PRG
** Proceso de Reapertua del Mes 
** AUTOR : UDEP - FMC
** Fecha : 27/02/97
** NOTA  : Graba en el campo Estado del compro.dbf con "  ";
**         Estos documentos ya no podr n ser corregidos
** Last UpDate : Ing. Fernando Mu¤oz Cho 27/02/97
** Beneficencia
** -----------------------------------------------------------------

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03
Vmens01 = 'Proceso de Apertura mensual'
Vmens02 = '  Apertura mensual  '
Vmens03 = '   Procesando  '
Vmens04 = 'Mayorizaci¢n no efectuada...'
Vmens05 = 'Saldos en Moneda Nacional no est n cuadrados'
Vmens07 = 'Falta emitir el Libro Diario...'
Vmens08 = 'Falta emitir el Libro Mayor ...'
Vmens09 = 'Falta emitir el Balance de Comprobaci¢n.'
Vmens10 = 'Falta emitir el Balance General Modelo II'
Vmens11 = 'Falta emitir el Estado de Perdidas y Ganancias'
Vmens12 = 'Falta enviar informaci¢n de cuenta corriente'
Vmens13 = 'Falta emitir el Balance General Modelo I'

OK=VE_PASSW('BENEF')
IF !OK
	RETURN
ENDIF

=PopError('Aseg£rese que NADIE m s use el sistema para poder realizar el proceso de Apertura de Mes sin ning£n problema.',.F. )

USE Compro   IN 1  ORDER TAG Compro7                  ALIAS Comp EXCLUSIVE
USE Parmae   IN 7  ORDER TAG Parmae1                  ALIAS parma 

*- Inicia proceso
SELECT parma
SEEK "CONTAB"+"CIERRE"
IF !Found()
	DO Standby WITH "Parametro no Inicializado. Favor Avisar al area de Sistemas."
 ELSE
	_Cierre = LEFT(descri,6)
	vCierre = CTOD("01/"+RIGHT(_Cierre,2)+"/"+ LEFT(_Cierre,4))
	vCierre = LastDay(MONT(vCierre),YEAR(vCierre))
	vfecha = DATE()
ENDIF

DO Inicia                    && Define ventanas, t¡tulos

IF Se_Abre()
	IF Proceso()
		SELECT parma
		SEEK 'CONTAB' + 'CIERRE'
		IF FOUND()
			IF F_LOCK(1)
				REPLACE parma.Descri WITH IIF(MONT(vCierre)=1,STR(YEAR(vCierre)-1,4),STR(YEAR(vCierre),4)) + IIF(MONTH(vCierre)=1,"12",PADL(MONTH(vCierre)-1,2,'0')) 
				REPLACE parma.fecha  WITH vFecha
			ENDIF
			
			SEEK 'MESANO' + 'ACTUAL'
			IF FOUND()
				IF F_LOCK(1)
					vnuevo = STR(YEAR(vCierre),4)+PADL(MONT(vCierre),2,'0')
					REPLACE Descri WITH vnuevo
					UNLOCK
				ENDIF
			 ELSE
				DO StandBy WITH "Parametro no Inicializado. Favor de avisar al area de Sistemas"
			ENDIF
			DO Standby WITH 'Proceso de Apertura de Mes finalizado ...!'
		 ELSE
			DO StandBy WITH "Parametro no Inicializado. Favor de avisar al area de Sistemas"
		ENDIF
		
	ENDIF
ENDIF
DO Fin_opcion
RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------

ACTIVATE SCREEN
DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
TITLE Vmens01 COLOR SCHEME 10

DEFINE WINDOW Wind_1 FROM 15,23 TO 20,57 DOUBLE ;
TITLE Vmens02 COLOR SCHEME 10

DEFINE WINDOW Wind_2 FROM 06,10 TO 09,70  ;
TITLE Vmens03 COLOR SCHEME 10

ACTIVATE SCREEN
RETURN

FUNCTION Se_Abre
*-----------------
SELECT parma

IF BETW(MONTH(vCierre),1,12)
	IF yesno("Esta Ud. Seguro de Aperturar el Mes "+mfecha(MONTH(vCierre),YEAR(vCierre)))
		RETURN .T.
	 ELSE
		RETURN .F.
	ENDIF
 ELSE
	DO StandBy WITH "Periodo " + mfecha(MONTH(vCierre),YEAR(vCierre)) + " no Existe"
ENDIF

RETURN .F.

FUNCTION Proceso
*---------------

DO ESPERA WITH 1,'Apertura del mes '+ mfecha(MONTH(vCierre),YEAR(vCierre)) +' en proceso ...'
SELECT Comp
REPLACE Estado WITH "  " FOR MONTH(FECHA)=MONTH(vCierre)
DO ESPERA WITH 2

RETURN .T.


PROCEDURE Fin_opcion
*-------------------
CLOSE DATABASE
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
RELEASE    WINDOW wind_2
RETURN

