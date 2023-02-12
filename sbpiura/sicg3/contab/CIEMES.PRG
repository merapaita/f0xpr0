** -----------------------------------------------------------------
** CIERRE.PRG
** Proceso de Cierre Mensual
** AUTOR : UDEP - JFG
** Fecha : 28/11/95
** NOTA  : Graba en el campo Estado del compro.dbf con "20";
**         Estos documentos ya no podr n ser corregidos
** Last UpDate : Ing. Fernando Mu¤oz Cho 27/02/97
** Beneficencia
** -----------------------------------------------------------------

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03
Vmens01 = 'Proceso de cierre mensual'
Vmens02 = '  Cierre mensual  '
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

=PopError('Aseg£rese que NADIE m s use el sistema para poder realizar el proceso de Cierre de Mes sin ning£n problema.',.F. )

*SET STEP ON

USE Compro IN 1 ORDER TAG Compro7 ALIAS Comp EXCLUSIVE
USE Parmae IN 7 ORDER TAG Parmae1 ALIAS parma 

*- Inicia proceso
SELECT parma
SEEK "CONTAB"+"CIERRE"
IF !Found()
	DO Standby WITH "Parametro no Inicializado. Favor Avisar al area de Sistemas."
 ELSE
	_Cierre = LEFT(descri,6)
	vCierre = CTOD("01/"+IIF(RIGHT(_Cierre,2)="12","01",PADL(ALLTRIM(STR(VAL(RIGHT(_Cierre,2))+1)),2,'0'))+"/"+ IIF(RIGHT(_Cierre,2)="12",STR(VAL(LEFT(_Cierre,4))+1,4),LEFT(_Cierre,4)))
	vCierre = LastDay(MONT(vCierre),YEAR(vCierre))
	vfecha = DATE()
ENDIF

DO Inicia                    && Define ventanas, t¡tulos

IF Se_cierra()
	IF Proceso()
		SELECT parma
		SEEK 'CONTAB' + 'CIERRE'
		IF FOUND()
			IF F_LOCK(1)
				REPLACE parma.Descri WITH STR(YEAR(vCierre),4)+PADL(MONTH(vCierre),2,'0')
				REPLACE parma.fecha     WITH vFecha
			ENDIF
			
			SEEK 'MESANO' + 'ACTUAL'
			
			IF FOUND()
				IF F_LOCK(1)
					IF MONTH(vCierre) = 12
						* Enero del Siguiente A¤o.
						vnuevo = STR(YEAR(vCierre)+1,4)+ '01'
					 ELSE
						vnuevo = STR(YEAR(vCierre),4) + PADL(ALLTRIM(STR(MONTH(vCierre)+1)),2,'0')
					ENDIF
					REPLACE Descri WITH vnuevo
				ENDIF
			 ELSE
				DO Standby WITH "Parametro no Inicializado. Favor Avisar al area de Sistemas."
			ENDIF
			DO Standby WITH 'Proceso de Cierre de Mes finalizado ...!'
		 ELSE
			DO Standby WITH "Parametro no Inicializado. Favor Avisar al area de Sistemas."
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


FUNCTION Se_cierra
*-----------------
*ACTIVATE WINDOW Wind_0

 SELECT parma
 
* SEEK 'OPERATMAYOR'
* IF ALLTRIM(parma.Descri) # 'Si'
*   DO Standby WITH Vmens04
*   RETURN .F.
* ENDIF
*SEEK 'LISCONDIARI'
*IF ALLTRIM(parma.Descri) # 'Si'
*  DO Standby WITH Vmens07
*  RETURN .F.
* ENDIF
*SEEK 'LISCONMAYOR'
*IF ALLTRIM(parma.Descri) # 'Si'
*  DO Standby WITH Vmens08
*  RETURN .F.
*ENDIF
*SEEK 'LISCONCOMPR'
*IF ALLTRIM(parma.Descri) # 'Si'
*  DO Standby WITH Vmens09
*  RETURN .F.
*ENDIF
*SEEK 'LISCONBALI'
*IF ALLTRIM(parma.Descri) # 'Si'
*  DO Standby WITH Vmens13
*  RETURN .F.
*ENDIF
*SEEK 'LISCONBALII'
*IF ALLTRIM(parma.Descri) # 'Si'
*  DO Standby WITH Vmens10
*  RETURN .F.
*ENDIF
*SEEK 'LISCONESTAD'
*IF ALLTRIM(parma.Descri) # 'Si'
*  DO Standby WITH Vmens11
*  RETURN .F.
*ENDIF

IF yesno("Esta Ud. Seguro de Cerrar el Mes "+ mFecha(MONTH(vCierre),YEAR(vCierre)))
	RETURN .T.
 ELSE 
	RETURN .F.
ENDIF
RETURN .F.


FUNCTION Proceso
*---------------
DO ESPERA WITH 1,'Cierre del mes en proceso ...'  
SELECT Comp
REPLACE Estado WITH "20" FOR MONTH(FECHA)=MONTH(vCierre)
DO ESPERA WITH 2

RETURN .T.


PROCEDURE Fin_opcion
*-------------------
CLOSE DATABASE
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
RELEASE    WINDOW wind_2
RETURN

