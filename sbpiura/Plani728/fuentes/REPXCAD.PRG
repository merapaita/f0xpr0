*--------------------------------------------------------------------------
* RepxCad.Prg
* Reporte de de Pagos por Cadenas Funcionales
* Autor:M.R.A.
* Parametro : nOpcion 1 - Personal Activo
*					  2 - Personal Jubilado
*--------------------------------------------------------------------------
PARAMETERS nOpcion

PUBLIC vTipCad,OkCancel,cMes_Sue,cMes_Vac,cPeriodo,xPer,vIdx1

xPer  = SYS(3)+".dbf"
vIDX1 = SYS(3)+".Idx"

CLOS DATA
USE TotApor	 IN 1 
USE Personal IN 2 ORDER TAG Persona7 ALIAS Pers
USE Parmae	 IN 3 ORDER TAG PARMAE1  ALIAS Parma

SELE Pers
COPY TO xPer
Use xPer ALIAS Pers
INDEX ON Lib_Ele TO (vIDX1)
SET RELATION TO "RECPER"+Rec_Per INTO Parma

SELE TotApor
SET RELATION TO Lib_ele INTO Pers

SELE Parma
SEEK "FECSIS"
VMES = LEFT(Parma.Descri,2)
vAno = SUBS(parma.descri,4,4)

DO Escoge

IF LASTKEY()=27 OR OkCancel=2
	CLOS DATA
	RETURN
ENDIF


IF OkCancel = 1 
	IF allt(vmes)#cMes_Sue AND allt(vAno)#cPeriodo
		DO STANDBY WITH 'MES A REPORTAR ES DIFERENTE AL DEL SISTEMA'
		CLOS DATA
		OKCANCEL = 2
		RETURN
	ENDIF
	vtipo = IIF(nOpcion=2,' - VACACIONES - ',' - RETRIBUCIONES - ')+UPPER(cMes(VAL(cMes_Sue)))+' DE '+ STR(YEAR(DATE()),4)
	DO CASE
		CASE vTipCad=1
			cFteFto='1'
			SELECT TotApor
			INDEX ON Pers.Rec_Per+Pers.CodCad+Lib_Ele TO XX
			SET FILTER TO Mes_Sue=cMes_Sue AND Periodo=cPeriodo AND !EMPTY(Pers.CodCad) AND (Pers.Rec_Per=cFteFto OR Pers.Reservada OR Pers.Reservada1) AND IIF(nOpcion=1,Pers.Mes_Vac#cMes_Sue,Pers.Mes_Vac=cMes_Sue) AND !(Pers.Con_Per="3" OR Pers.Con_Per="4")
			
* PARCHE PARA LA OPCION DE RESERVADA1 DESHABILITADO

*			SELE Pers
*			REPLACE Rec_Per WITH "1" for Rec_Per="2"
			SELECT TotApor
		CASE vTipCad=2
			cFteFto='2'
			SELECT TotApor
			INDEX ON Pers.Rec_Per+Pers.CodCad+Lib_Ele TO XX
			SET FILTER TO Mes_Sue=cMes_Sue AND Periodo=cPeriodo AND !EMPTY(Pers.CodCad) AND (Pers.Rec_Per=cFteFto AND !Pers.Reservada AND !Pers.Reservada1) AND IIF(nOpcion=1,Pers.Mes_Vac#cMes_Sue,Pers.Mes_Vac=cMes_Sue) AND !(Pers.Con_Per="3".OR. Pers.Con_Per="4")
	ENDCASE
ENDIF

DO REPORTE WITH 2,'LisCad2', ' Reporte por Cadenas',2,.F.,.T.
IF yesno('¨Impreme reporte desglozado?')
	DO REPORTE WITH 2,'LisCad3', ' Reporte por Cadenas',2,.F.,.T.
ENDIF
DO Salida

PROCEDURE Escoge
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE 'Listado de Planillas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO vTipCad,OkCancel
cMes_Sue = vMes
cPeriodo = vAno
@ 02,01 SAY " Cadena a Reportar : " GET vTipCad  FUNCTION '^ Donac. Transf.;Direc. Recaud.'
@ 06,01 SAY "   Mes a Reportar : "
@ 06,22 GET cMes_Sue  Valid Val_para(cMes_Sue,'FECMES',' ',27)
@ 06,25 GET cPeriodo
@ 08,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
cMes_Vac = cMes_Sue
RELEASE WINDOW LIS
RETURN

PROCEDURE Salida
*---------------
CLOS DATA
RELEASE WIND ALL
ERASE (vIdx1)
ERASE (xPer)


