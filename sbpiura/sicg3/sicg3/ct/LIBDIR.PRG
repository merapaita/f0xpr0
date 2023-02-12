** --------------------------------------
** DIARCONT.PRG
** Reporte Diario de Contabilidad Mensual
** AUTOR : UDEP - JECHS
** --------------------------------------

* Abrir Archivos de trabajo
USE Parmae  IN 1  ORDER TAG Parmae1         ALIAS parma
USE SaldoNa IN 2  ORDER TAG SaldoNa1        ALIAS SalN
USE Diario  IN 3  ORDER TAG Diario1         ALIAS Diar
USE Compro  IN 4  ORDER TAG Compro7         ALIAS Comp
USE Cuentas IN 5  ORDER TAG Cuentas1        ALIAS Cuen
USE Auxil   IN 6  ORDER TAG Auxil1          ALIAS Auxi

STORE 0 TO xano, xMes
DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
DEFINE WINDOW DiarCont FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '  Libro Diario de Contabilidad  ' COLOR SCHEME 5
ACTIVATE WINDOW DiarCont
  
xbalance = 'Mensual'
xdigitos = 2

SELECT parma
SEEK 'MESANOACTUA'
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
xAno     = IIF(FOUND(), YEAR(vmesact),   YEAR(DATE()))
vAjustado="Ajustado   "

@ 01,01 SAY '               Mes : ' GET xMes     PICTURE '99' WHEN xBalance = 'Mensual';
           VALID xMes > 0 .AND. xMes <= 12
@ 03,01 SAY '               A¤o : ' GET xAno                           PICTURE '9,999'
@ 05,01 SAY '          Ajustado : ' GET vAjustado  PICTURE '@M Ajustado,No Ajustado'


@ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

IF LASTKEY() # 27 AND OKCANCEL = 1
	vind = SYS(3) + '.idx'
	Vind1= SYS(3) + '.idx'
	xruta=SET('PATH')
	DO CASE
		CASE xbalance = 'Mensual'
			IF xMes = MONTH(vmesact) .AND. xAno = YEAR(vmesact)
				SELECT SalN
				INDEX ON SalN.Cuenta FOR RIGHT(SalN.Cuenta,8) = '00000000';
					.AND. SalN.Mes = IIF(xMes = 1, 12,xMes-1) .AND.;
				SalN.Ano = IIF(xMes = 1, xAno-1,XAno) TO (vind)
				*USE SaldoNA INDEX (vind) ALIAS SalN
				
				IF (ALLTRIM(vAjustado)="Ajustado")
					* SUM DSaldoNa, HSaldoNa TO vdebe, vHaber
					SUM DSumana+dajumes, HSumana+hajumes TO vdebe, vHaber
				 ELSE
					* SUM DSaldoNa, HSaldoNa TO vdebe, vHaber
					SUM DSumana, HSumana TO vdebe, vHaber
				ENDIF
				
				SELECT Comp
				SELECT Diar
				DO ESPERA WITH 1, 'Diario de Contabilidad en proceso; espere...'
				SET RELATION TO Diar.Cuenta  INTO Cuen
				SET RELATION TO Diar.Comprob+Diar.ComAdi INTO Comp ADDITIVE
				INDEX ON Comprob+ComAdi TO (vind1) FOR MONTH(Diar.FecEMI) = MONTH(vmesact) AND YEAR(Diar.FecEMI) = YEAR(vmesact) ;
					AND IIF(ALLTRIM(vAjustado)="Ajustado",.T.,LEFT(Diar.Comprob,2)<>"64")
				DO ESPERA WITH 2
				DO REPORTE WITH 2, "DiarCont", 'Diario de Contabilidad Mensual',1,.F.,.T.
				SELECT parma
				SEEK 'LISCONDIACON'
				bloquea = .T.
				DO Fox_lock WITH 1, Bloquea
				IF Bloquea .AND. FOUND()
					REPLACE parma.Descri WITH 'Si'
				ENDIF
				CLOSE DATA
			 ELSE
				vpath   = '..\'+ALLTRIM(STR(xmes,2))+STR(xano,2)
				vdir    = '..\'+ALLTRIM(STR(xmes,2)) + STR(xano,2) + '\'
				vbackup = ALLTRIM(STR(xmes,2)) + STR(xano,2)+'.zip'
				IF !FILE(vdir + vbackup)
					DO Standby WITH 'Archivo : de BackUp en Disco del mes ' ;
						+ ALLTRIM(STR(xmes)) + ' '+ALLTRIM(STR(xano,2)) + ' no existe'
					RETURN
				ENDIF
				!CD &vpath >NUL
				!FOXSWAP PKUNZIP -o &vbackup Parmae.dbf SaldoNa.dbf Diario.dbf Compro.dbf Cuentas.dbf >NUL
				!FOXSWAP PKUNZIP -o &vbackup Parmae.cdx SaldoNa.cdx Diario.cdx Compro.cdx Cuentas.cdx >NUL
				!CD ..\FUENTES
				CLOSE DATA
				SET PATH TO &vpath
				USE Parmae  IN 1  ORDER TAG Parmae1     ALIAS parma
				USE SaldoNa IN 2                        ALIAS SalN
				USE Diario  IN 3                        ALIAS Diar
				USE Compro  IN 4  ORDER TAG Compro7     ALIAS Comp              
				USE Cuentas IN 5  ORDER TAG Cuentas1    ALIAS Cuen
				SELECT SalN
				INDEX ON SalN.Cuenta FOR LEN(ALLTRIM(SalN.Cuenta)) <= 2;
						.AND. SalN.Mes = IIF(xMes = 1, 12,xMes-1) .AND.;
						SalN.Ano = IIF(xMes = 1, xAno-1,XAno) TO (vind)
				
				IF (ALLTRIM(vAjustado)="Ajustado")
					* SUM DSaldoNa, HSaldoNa TO vdebe, vHaber
					SUM DSumana+dajumes, HSumana+hajumes TO vdebe, vHaber
				 ELSE
					* SUM DSaldoNa, HSaldoNa TO vdebe, vHaber
					SUM DSumana, HSumana TO vdebe, vHaber
				ENDIF
				
				SELECT Comp
				vCodFte=SUBSTR(CodCal,5,3)
				SELECT Diar
				SET RELATION TO Diar.Cuenta  INTO Cuen
				SET RELATION TO Diar.Comprob+Diar.ComAdi+Diar.CodFte INTO Comp ADDITIVE
				INDEX ON Comprob+ComAdi+Codfte TO (vind1) FOR MONTH(Diar.FecEMI) = MONTH(vmesact) AND YEAR(Diar.FecEMI) = YEAR(vmesact);
						AND IIF(ALLTRIM(vAjustado)="Ajustado",.T.,LEFT(Diar.Comprob,2)<>"64")
				DO REPORTE WITH 2, "DiarCont", 'Diario de Contabilidad Mensual',1,.F.,.T.
				CLOSE DATA
				SET PATH TO &xruta
				vdbf = vdir + '*.dbf'
				vidx = vdir + '*.idx'
				vcdx = vdir + '*.cdx'
				!ERASE &vdbf >NUL
				!ERASE &vidx >NUL
				!ERASE &vcdx >NUL
			ENDIF
	ENDCASE
	CLOSE DATABASE
	ERASE (vind)
	ERASE (vind1)
ENDIF
DEACTIVATE WINDOW DiarCont

IF !escolor
	RESTORE SCREEN FROM principal
ENDIF
SHOW POPUP menu, pop_03
RETURN


PROCEDURE Salida
*---------------
RELEASE    WINDOW DiarCont
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


FUNCTION Fin
*-----------
IF !EOF()
   SKIP
   IF EOF()
      SKIP - 1
      RETURN .T.
   ENDIF
   SKIP - 1
   RETURN .F.
ENDIF
RETURN .T.
