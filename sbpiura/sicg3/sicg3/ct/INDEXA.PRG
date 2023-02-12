*--------------------------------------------------------------
* PROGRAMA	  : Indexa.prg
* OBJETIVO	  : Rutina de Organizaci¢n de archivos
*           	crea ¡ndices compuestos y compacta la data.
* Autor   	  : UDEP
* LAST UPDATE : 24 de Febrero de 1997
*--------------------------------------------------------------
PARAMETER _fil
PRIVATE _auto, _path, ss, pos

ss=SET('PATH')
pos = AT(";",ss)
_path=IIF(pos=0,ss,SUBSTR(ss,1,AT(";",ss)-1))

_auto = IIF( PARAMETER()>0,.T.,.F.)

CLOSE DATA

IF !_auto

   IF escolor
      DEFINE POPUP _yt FROM 4,53 SHADOW MARGIN COLOR &L_COL
   ELSE
      DEFINE POPUP _yt FROM 4,53 MARGIN COLOR SCHEME C_POPUP
   ENDIF

   DEFINE BAR  1 OF _yt PROMPT '\<Todos                 '   
   DEFINE BAR  2 OF _yt PROMPT '\<Notas de Contabilidad '
   DEFINE BAR  3 OF _yt PROMPT 'P\<Lan de cuentas       '
   DEFINE BAR  4 OF _yt PROMPT '\<Saldos                '
   DEFINE BAR  5 OF _yt PROMPT 'Au\<Xiliares            '
   DEFINE BAR  6 OF _yt PROMPT 'D\<ocumentos Fuente     '
   DEFINE BAR  7 OF _yt PROMPT 'Tabl\<As                '
   DEFINE BAR  8 OF _yt PROMPT 'Par m\<Etros            '
   DEFINE BAR  9 OF _yt PROMPT '\-'
   DEFINE BAR 10 OF _yt PROMPT '\<Hist¢rico-Contabilidad'
   DEFINE BAR 11 OF _yt PROMPT '\<Asientos Aut./ Pat./ Pre'
   DEFINE BAR 12 OF _yt PROMPT '\<Presupuesto           '
   DEFINE BAR 13 OF _yt PROMPT 'Otras A\<Reas           '
   
   ON SELECTION POPUP _yt DO organiza WITH BAR()
   ACTIVATE POPUP _yt

   RELEASE POPUP _yt

ELSE
   DO organiza WITH _num(_fil)
ENDIF

CLOS DATA
RETURN


PROCEDURE organiza
*-----------------
PARAMETER cual

DEFINE WINDOW Working   FROM 02,40 TO 23,76 FLOAT   COLOR SCHEME 1
ACTIVATE WINDOW Working

CLOSE DATA
SET EXCLUSIVE ON
SET TALK ON
_con = SET('CONSOLE')
SET CONSOLE ON

ACTIVATE WINDOW Working

CLEAR

DO CASE
	CASE cual=1 .OR. cual=2
		   USE Compro EXCLUSIVE
		   PACK
		   =ordena("Comprob+ComAdi+ALLTRIM(STR(MONTH(Fecha)))+ALLTRIM(CodCaD)","compro1")
		   =ordena("DTOC(fecha,1)","compro2")
		   =ordena("estado+comprob","compro3")
		   =ordena("comprob","compro4")
		   =ordena("DocRef+DocAdi","compro5")
		   =ordena("comprob+ComAdi+CodCTC","compro6")
		   =ordena("comprob+ComAdi+CodCaD","compro7")   
		   =ordena("SUBSTR(Comprob,1,2)+PADL(ALLTRIM(STR(MONTH(Fecha),2)),2,'0')+SUBSTR(STR(YEAR(Fecha),4),3,2)","Compro8")
		   =ordena("SUBSTR(Comprob,1,2)+DTOC(Fecha)","Compro9")
		   =ordena("SUBSTR(Comprob,1,2)+PADL(ALLTRIM(STR(MONTH(Fecha),2)),2,'0')+STR(YEAR(Fecha),4)+codcaD","Compro10")
		   =ordena("TIPDOC+SUBSTR(Comprob,4,4)+PADL(ALLTRIM(STR(MONTH(Fecha),2)),2,'0')+SUBSTR(STR(YEAR(Fecha),4),3,2)","Compro11")
		   =ordena("SUBSTR(Comprob,1,2)+STR(DAY(Fecha),2)+STR(MONTH(FECHA),2)+STR(YEAR(Fecha),4)","Compro12")
		   =ordena("ALLTRIM(Comprob)+ComAdi+PADL(MONTH(Fecha),2,'0')+STR(YEAR(Fecha),4)","compro13")
		   USE
  
		   USE Diario EXCLUSIVE
		   PACK
	   
		   * USE Diario EXCLUSIVE
		   =ordena("comprob+STR(item,4)","Diario1")
		   =ordena("comprob+cuenta","Diario2")
		   =ordena("cuenta","Diario3")
		   =ordena("TipAux+registro","Diario4")
		   =ordena("DocRef","Diario5")
		   =ordena("comprob+ComAdi+CodFte+CodCTC","Diario6")
		   =ordena("TipDcR+DocRef+DocAdi+TipAux+registro+DTOS(FecEmi)","Diario7","!EMPTY(DocRef)")
		   =ordena("DocRef+DocAdi","Diario8")
		   =ordena("Comprob+ComAdi+cuenta+TipAux+registro","Diario9")
		   =ordena("DocRef+DocAdi+cuenta+TipAux+registro","Diario10")
		   =ordena("Comprob+ComAdi+CodFte","Diario11")
		   =ordena("comprob+ComAdi+CodCTC","Diario12")
		   =ordena("comprob+ComAdi","Diario13")
		   =ordena("SUBSTR(Comprob,1,2)+PADL(ALLTRIM(STR(MONTH(Fecemi),2)),2,'0')+SUBSTR(STR(YEAR(Fecemi),4),3,2)","Diario14")
		   =ordena("SUBSTR(Comprob,1,2)+DTOC(Fecemi)","Diario15")   
		   =ordena("DTOC(Fecemi,1)","Diario16")   
		   =ordena("Comprob+ComAdi+CodFte+Tipo+SUBSTR(Cuenta,1,2)","Diario17")
		   =ordena("SUBSTR(Comprob,1,2)+PADL(ALLTRIM(STR(MONTH(Fecemi),2)),2,'0')+STR(YEAR(Fecemi),4)+codfte","Diario18")
		   =ordena("Comprob+comadi+codfte+tipo+LEFT(cuenta,1)","Diario19","Tipo='D' .AND. LEFT(Cuenta,1)='4'")
		   =ordena("ALLTRIM(comprob)+ComAdi+STR(YEAR(FecEmi),4)+CodFte","DIARIO20")

		   USE
		   
		   USE Glosas EXCLUSI
		   PACK
		   =ordena("ano+tipo+orden","Glosas1")
		   USE
		   
		   USE PolAux EXCLUSIVE
		   PACK
		   =ordena("periodo+NumPol+TipPol","Polaux1")
		   USE
		   
	CASE  Cual=1 .OR. Cual=3
		  USE Cuentas EXCLUSIVE
		  PACK
		  =ordena("Cuenta","Cuentas1")
		  =ordena("DContra","Cuentas2","!EMPTY(DContra)")
		  =ordena("HContra","Cuentas3","!EMPTY(HContra)")
		  =ordena("Cuenta","Cuentas4","UPPER(Detalle)='S'")
		  =ordena("UPPER(Descri)","Cuentas5")
		  =ordena("Cuenta","Cuentas6","Cuenta='9'")  
		  =ordena("TIPO+Cuenta","Cuentas7")  
		  USE
		  
	CASE cual=1 .OR. cual=4
		   USE saldona EXCLUSIVE
		   PACK
		   =ordena("cuenta+STR(Mes,2)+STR(Ano,4)","Saldona1")
		   =ordena("cuenta","Saldona2")
		   =ordena("cuenta+STR(Ano,4)","Saldona3")
           =ordena("CUENTA+STR(Ano,4)+STR(Mes,2)","Saldona4")
		   USE
   
   		   USE SalBcos EXCLUSIVE
		   PACK
		   =ordena("CodCtc+Periodo+Nummes","SalBcos1")
		   USE
   
		   USE SalMesN EXCLUSIVE
		   PACK
		   =ordena("Cuenta","SalMesN1")
		   USE

		   USE Invent EXCLUSIVE
		   PACK
		   =ordena("Cuenta+PADL(Mes,2,'0')+STR(Ano,4)","Invent1")
		   USE
		   
	CASE cual=1 .OR. cual=5
	
		   USE Auxil  EXCLUSIVE
		   PACK
		   =ordena("tipo+codigo","Auxil1")
		   =ordena("codigo","Auxil2")
		   =ordena("Banco + Moneda + Codigo","auxil3","TipBan")
		   =ordena("tipo","auxil4")
		   =ordena("tipo+banco","auxil5")
		   =ordena("descri","auxil6","tipo='20'")
		   =ordena("tipo+ALLTRIM(codigo)","auxil7")  
		   =ordena("codigo","auxil8","tipo='20'")
		   =ordena("descri","auxil9","tipo='30'")
		   =ordena("tipo+descri","auxil10")
		   USE   
   
		   USE SalAux  EXCLUSIVE
		   PACK
		   =ordena("tipo+codigo+STR(Ano,2)+STR(Mes,2)+Cuenta","SalAux1")
		   =ordena("tipo+codigo+STR(mes,2)+STR(ano,2)+Cuenta","SalAux2")
		   =ordena("tipo+codigo+STR(Ano,2)+STR(Mes,2)","SalAux3")
		   =ordena("cuenta+tipo+codigo+STR(mes,2)+STR(ano,2)","SalAux4")
		   USE
   
		   USE EstFin EXCLUSIVE
		   PACK
		   =ordena("CodRep+cuenta+DebHab","EstFin1")   
		   =ordena("ALLTRIM(fila)+ALLTRIM(codrep)+ALLTRIM(columna)","EstFin2")
		   USE
      
		   USE RepFin EXCLUSIVE
		   PACK
		   =ordena("CodRep+cuenta","RepFin1")
		   =ordena("ALLTRIM(CodRep)+ALLTRIM(fila)+ALLTRIM(columna)","RepFin2")
		   =ordena("Periodo+Nummes+Cuenta","RepFin3")
		   =ordena("CodRep+Periodo+Nummes","RepFin4")
		   =ordena("ALLTRIM(CodRep)+ALLTRIM(fila)+ALLTRIM(columna)+periodo","RepFin5")
  		   =ordena("fila+CodRep+columna","RepFin6")
		   
		   USE
   
   		   USE CRepFin EXCLUSIVE
		   PACK
		   =ordena("CodRep+cuenta","CRepFin1")
		   =ordena("ALLTRIM(codrep)+ALLTRIM(fila)+ALLTRIM(columna)","CRepFin2")
		   =ordena("Periodo+Nummes+Cuenta","CRepFin3")
		   =ordena("CodRep+Periodo+Nummes","CRepFin4")
		   USE
   
		   USE Saldini EXCLUSIVE
		   PACK
		   =ordena("ALLTRIM(CodRep)+CODDES+ALLTRIM(STR(Periodo))","SaldIni1")
		   =ordena("ALLTRIM(CodRep)+ALLTRIM(fila)+ALLTRIM(columna)","SaldIni2")
		   USE
   
		   USE DetDes  EXCLUSIVE
		   PACK
		   =ordena("ALLTRIM(CodRep)+CodDes","DetDes1")
		   USE
   
		   USE AutFin EXCLUSIVE
		   =ordena("Tipdoc+Cuenta","Autfin1")
		   USE
	CASE cual=1 .OR. cual=6

			USE ordser EXCLU			&& Ordenes de Servicio
			PACK
			=ordena('periodo+numos+codfte','ordser1')
			=ordena('periodo+CodFte+numos','ordser2')
			* PARA CONTABILIDAD
			=ordena('periodo+CodFte+numos','ordser3',"estado='50'")
			=ordena('ALLTRIM(CodCal)+CodPart','ordser4',"TipFun='I'")
			=ordena('ALLTRIM(CodCal)+CodAnal','ordser5',"TipFun='F'")
			=ordena("CodPrv","OrdSer6")
			USE
			
			
			USE ordCom EXCLU			&& *Ordenes DE Compra
			PACK
			=ordena('periodo+numOC+codfte','ordCom1')
			=ordena('NumOC','ordCom2')
			=ordena('periodo+CodFte+NumOC','OrdCom3')
			=ordena('Periodo+CodPrv','ordCom4')
			=ordena('Periodo+NumOC+CodFte','ordCom5',"Estado='50'")
			=ordena('PerHC+NumHC','ordCom6')
			USE
			
			USE IteOC EXCLU					&& Items de Ordenes de Compra
			PACK
			=ordena('periodo+NumOC+CodFte','IteOc1')
			=ordena('periodo+NumOC+CodFte','IteOc2',"Estado='00'")
			=ordena('periodo+CodFte+NumOC','IteOc3')
			=ordena('ALLTRIM(UPPER(Descri))+DTOC(FecOC)','IteOc4')
			=ordena('ALLTRIM(UPPER(Descri))','IteOc5')
			=ordena('ALLTRIM(CodCal)+CodPart','IteOc6',"TipFun='I'")
			=ordena('ALLTRIM(CodCal)+CodAnal','IteOc7',"TipFun='F'")
			=ordena('Periodo+NumOC+CodFte+CodAnal','IteOc8')
			USE
			
			USE HojCon EXCLU				&& Hojas de Afectaci¢n presupuestaria
			PACK
			=ordena('NumMes+NumHc','HojCon1')
			=ordena('NumMes+NumHc','HojCon3',"ESTCON='  '.AND.ESTADO#'99'")
			=ordena('NumMes+NumHc','Reg_E51')
			USE
			
			USE IteHc EXCLU					&& Items de Hojas de Afectaci¢n
			PACK
			=ordena('NumMes+NumHC','IteHC1')
			=ordena('CodPart+CodCla','IteHC2')
			=ordena('CodAnal+CodCla','IteHC3')
			=ordena('NumMes+NumHC','IteHC4',"ESTADO='92'")
			=ordena('NumMes+TipFun+CodFte+CodPrg+CodSubpr+CodProy+CodSupry+CodPart+CodCla','IteHC5',"TIPFUN='I'")
			=ordena('NumMes+TipFun+CodFte+CodPrg+CodSubpr+CodAct+COdAnal+CodCla','IteHC6',"TipFun='F'")
			USE
			
			USE Compag EXCLU				&& Comprobantes de Pago
			PACK
			=ordena('ALLTRIM(NumMes)+NumCP+ALLTRIM(CodCtc)','ComPag1')
			=ordena('ALLTRIM(NumMes)+CCNOMBRE+ALLTRIM(CodCtc)','ComPag2')
			=ordena('ALLTRIM(NumMes)+NumCP+ALLTRIM(CodCtc)','ComPag3',"ESTADO#'51'.AND.ESTADO#'99'")
			=ordena('NumMesHC+NumHC','ComPag4')
			=ordena('ALLTRIM(CodPrv)','ComPag5')
			=ordena('ALLTRIM(CodEmp)','ComPag6')
			USE
			
			USE IteCp EXCLU					&& Comprobantes de Pago
			PACK
			=ordena('ALLTRIM(NumMes)+NumCP+ALLTRIM(CodCtc)','IteCP1')
			=ordena('CodPart+NumMes','IteCP2')
			USE
			
			USE Cheque EXCLU				&& Cheques
			PACK
			=ordena('NumMes+NumCP+CodCtc','Cheque1')
			=ordena('CodCtc+ALLTRIM(NumChq)','Cheque2')
			=ordena('Periodo+NumMes+NumCP+CodCtc','Cheque3')
			=ordena('ALLTRIM(NumChq)','Cheque4')
			USE
			
			USE Reten  EXCLU				&& Retenciones
			PACK
			=ordena('NumMes+NumCP+CodCtc','Reten1')
			=ordena('CodCtc+CodRet','Reten2')
			=ordena('NumMes+NumCP+CodCtc+CodRet','Reten3','EMPTY(ConPago)')
			=ordena('CodCtc+CodRet','Reten4','EMPTY(ConPago)')
			=ordena('MesCPPG+NumCPPG+CodCtc','Reten5')
			USE

			USE Poliza EXCLUSIVE			&& Polizas
			PACK
			=ordena("Periodo+NumPol","Poliza1","TipPol='E'")
			=ordena("Periodo+NumPol","Poliza2","TipPol='S'")
			=ordena("Periodo+NumPol+TipPol","Poliza3")
			USE
			
			USE ItePol EXCLUSIVE			&& Items De Polizas
			PACK
			=ordena("Periodo+NumPol+Numref+CodCla","ItePol1","TipPol='E'")
			=ordena("Periodo+NumPol+Numref+CodCla","ItePol2","TipPol='S'")
			=ordena("Periodo+NumPol+TipPol","ItePol3")
			USE
			
			USE Recing EXCLU				&& Recibos de Ingreso
			PACK
			=ordena('Periodo+ALLTRIM(Nummes)+ALLTRIM(NumRI)','RecIng1')
			=ordena('Periodo+ALLTRIM(Nummes)+ALLTRIM(NumRI)','RecIng2',"ESTADO#'51'")
			=ordena("NumRi","RecIng3")
			=ordena('numri','numri')
			USE
			
			USE IteRI  EXCLU				&& Items de Recibos de Ingreso
			PACK
			=ordena('Periodo+ALLTRIM(NumMes)+ALLTRIM(NumRI)','IteRI1')
			=ordena('ALLTRIM(NumMes)+ALLTRIM(BolDep)','IteRI2')
			USE
			
			USE MovBco EXCLUSIVE			&& Movimientos Bancarios
			PACK
			=ordena("NumMes+Numero","MovBco1")
			=ordena("ALLTRIM(TipDoc)+ALLTRIM(Numero)+ALLTRIM(CodCtc)","MovBco2")
			=ordena("ALLTRIM(TipDoc)+ALLTRIM(Numero)","MovBco3")
			=ordena("Fecha","MovBco4")
			=ordena("CodCtc+ALLTRIM(Numero)","MovBco5","estado='40' AND Tipdoc='N/C'")
			=ordena("CodCtc+ALLTRIM(Numero)","MovBco6","estado='40' AND Tipdoc='N/A'")
			=ordena("NumMes+numero+CodCTC","MovBco7","estado#'51'.AND.TipDoc='B/D'")
			=ordena("NumMes+numero+CodCTC","MovBco8","estado#'51'.AND.(TipDoc='N/C' OR TipDoc='NOC')")
			=ordena("NumMes+numero+CodCTC","MovBco9","estado#'51'.AND.TipDoc='N/A'")
			=ordena("NumMes+numero+CodCTC","MovBco10","ESTADO#'51'.AND.TIPDOC='NOC'")
			=ordena("PADL(ALLTRIM(STR(MONTH(fecha))),2,'0')+ALLTRIM(TipDoc)+STR(monto,18,2)+ALLTRIM(CodCTC)","MovBco12")
			=ordena("ALLTRIM(periodo)+ALLTRIM(NumMes)+ALLTRIM(NumRI)","MovBco15")
			=ordena("ALLTRIM(PERIODO)+ALLTRIM(NUMMESRI)+ALLTRIM(NUMRI)","MovBco16")
			=ordena("NUMMES+NUMERO","MovBco17","ESTADO#'51'.AND.TIPDOC='N/C'")
			=ordena("NUMMES+NUMERO","MovBco18","ESTADO#'51'.AND.TIPDOC='B/D'")
			USE
			
	CASE cual=1 .OR. cual=7
		   USE Parmae EXCLUSIVE
		   PACK
		   =ordena("tipo+codigo+CodigoAux","parmae1")
		   =ordena("tipo+CodigoAux","parmae2")
		   USE
		   
		   *USE Usuario EXCLUSIVE
		   *PACK
		   *=ordena("usuario","Usuario1")
		   *USE
		   *USE IteUsu EXCLUSIVE 
		   *PACK
		   *=ordena("UsuCla+modulo","IteUsu1")
		   *USE  
		   
		   USE factores EXCLUSIVE
		   PACK
		   =ordena("Periodo+Tipo+Mes","factores1")
		   =ordena("Tipo+Periodo+Mes","factores2")
		   USE
		   
		  USE Folios EXCLUSI
		  PACK
		  =ordena("Reporte+Mes+Periodo+Tipdoc+Orden+Cuenta","Folios1")
*		  =ordena("Reporte+Mes+Periodo+Orden+Cuenta","Folios2")
		  USE

	
	CASE cual=1 .OR. cual=8
	* AUN EN FUNCION DE LO QUE EL GRUPRO CONFIRME LA ESTRUCTURA DE LOS INDICES
	
	CASE cual=1 .OR. cual=10
		   SELE A
		   USE HCompro EXCLUSIVE
		   PACK
		   =ordena("Comprob+ComAdi+ALLTRIM(STR(MONTH(Fecha)))+ALLTRIM(CodCal)","HCompro1")
		   =ordena("DTOC(fecha,1)","HCompro2")
		   =ordena("estado+comprob","HCompro3")
		   =ordena("comprob","HCompro4")
		   =ordena("DocRef+DocAdi","Hcompro5")
		   =ordena("Comprob+ComAdi+CodCTC","Hcompro6")
		   =ordena("comprob+ComAdi+SUBSTR(CodCal,5,3)+DTOC(Fecha)","Hcompro7")   
		   =ordena("SUBSTR(Comprob,1,2)+PADL(ALLTRIM(STR(MONTH(Fecha),2)),2,'0')+SUBSTR(STR(YEAR(Fecha),4),3,2)","HCompro8")  
		   =ordena("SUBSTR(Comprob,1,2)+DTOC(Fecha)","HCompro9")
		   =ordena("SUBSTR(Comprob,1,2)+PADL(ALLTRIM(STR(MONTH(Fecha),2)),2,'0')+SUBSTR(STR(YEAR(Fecha),4),3,2)+SUBSTR(codcal,5,3)+Tipfun","HCompro10")
		   =ordena("SUBSTR(Comprob,1,2)+PADL(ALLTRIM(STR(MONTH(Fecha),2)),2,'0')+SUBSTR(STR(YEAR(Fecha),4),3,2)+Tipfun","HCompro11")
		   =ordena("TIPDOC+SUBSTR(Comprob,4,4)+PADL(ALLTRIM(STR(MONTH(Fecha),2)),2,'0')+SUBSTR(STR(YEAR(Fecha),4),3,2)","HCompro13")
		   =ordena("SUBSTR(Comprob,1,2)+STR(DAY(Fecha),2)+STR(MONTH(FECHA),2)+STR(YEAR(Fecha),4)","HCompro14")
		   USE
   
		   SELE B
		   USE HDiario EXCLUSIVE
		   PACK
		   USE HDiario EXCLUSIVE
		   =ordena("comprob+STR(item,4)","HDiario1")
		   =ordena("comprob+cuenta","HDiario2")
		   =ordena("cuenta","HDiario3")
		   =ordena("TipAux+registro","HDiario4")
		   =ordena("DocRef","HDiario5")
		   =ordena("comprob+ComAdi+CodFte+CodCTC","HDiario6")
		   =ordena("TipDcR+DocRef+DocAdi+TipAux+registro+DTOS(FecEmi)","HDiario7","!EMPTY(DocRef)")
		   =ordena("DocRef+DocAdi","HDiario8")
		   =ordena("Comprob+ComAdi+cuenta+TipAux+registro","HDiario9")
		   =ordena("DocRef+DocAdi+cuenta+TipAux+registro","HDiario10")
		   =ordena("Comprob+ComAdi+CodFte+DTOC(Fecemi)","HDiario11")
		   =ordena("comprob+ComAdi+CodCTC","HDiario12")
		   =ordena("comprob+ComAdi","HDiario13")
		   =ordena("SUBSTR(Comprob,1,2)+PADL(ALLTRIM(STR(MONTH(Fecemi),2)),2,'0')+SUBSTR(STR(YEAR(Fecemi),4),3,2)","HDiario14")
		   =ordena("SUBSTR(Comprob,1,2)+DTOC(Fecemi)","HDiario15")   
		   =ordena("DTOC(Fecemi,1)","HDiario16")   
		   =ordena("Comprob+ComAdi+CodFte+Tipo+SUBSTR(Cuenta,1,2)","HDiario17")
		   =ordena("SUBSTR(Comprob,1,2)+PADL(ALLTRIM(STR(MONTH(Fecemi),2)),2,'0')+SUBSTR(STR(YEAR(Fecemi),4),3,2)+codfte+tipfun","HDiario18")
		   =ordena("SUBSTR(Comprob,1,2)+PADL(ALLTRIM(STR(MONTH(Fecemi),2)),2,'0')+SUBSTR(STR(YEAR(Fecemi),4),3,2)+Tipfun","HDiario19")
		   =ordena("Comprob+comadi+codfte+tipo+LEFT(cuenta,1)","HDiario20","Tipo='D' .AND. LEFT(Cuenta,1)='4'")
		   USE
		   
	CASE cual=1 .OR. cual=11
			USE asiaut EXCLUSIVE
			PACK
			=ordena("tipdoc+destino+tipfun+codart","asiaut1")
			=ordena("codart+tipdoc+tipfun+destino","asiaut2")
			=ordena("codart+tipdoc+tipfun","asiaut3")
			USE
			
			USE Astpat EXCLUSIVE
			PACK
			=ordena("NumMes+NumRef+CodCtc","AstPat1")
			=ordena("NumMes+NumRef","AstPat2","TipDoc='H/C'")
			=ordena("NumMes+NumRef+CodCtc","AstPat3","TipDoc='C/P'")
			=ordena("ALLTRIM(NumMes)+NumRef+CodCtc","AstPat4")
			=ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat5","TipDoc='N/C' OR TipDoc='NOC'")
			=ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat6")
*			=ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat6","TipDoc='N/A'")
			=ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat7","TipDoc='B/D'")
			=ordena("CodCta","AstPat8")
			=ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat9","TipDoc='NOC'")
			=ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat10")
			=ordena("ALLTRIM(Periodo)+ALLTRIM(NumMesRI)+ALLTRIM(TipDoc)+ALLTRIM(NumRI)+NumRef","AstPat11")
			=ordena("Periodo+ALLTRIM(NumMes)+NumRef+CodCtc","AstPat14","TipDoc='R/I'")
			=ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat15","TipDoc='REV'")  
			=ordena("NumMes+NumRef+CodCtc","AstZPat18","TipDoc='H/A'")
			=ordena("NumMes+CodCTC+NumRef+NumDoc","AstPat19")
			=ordena("ALLTRIM(Periodo)+ALLTRIM(NumMes)+ALLTRIM(TipDoc)+ALLTRIM(NumRI)+NumRef","AstPat30","TipDoc='B/D'")
			=ordena("ALLTRIM(periodo)+ALLTRIM(NumMes)+ALLTRIM(TipDoc)+ALLTRIM(NumRI)","AstPat31")
			=ordena('NumMes+ALLTRIM(NumRef)','astpat32',"TIPDOC='N/C'")
			=ordena("NumMes+ALLTRIM(Numref)","AstPat33","TipDoc='B/D'")
			=ordena("NumMes+ALLTRIM(Numref)","AstPat34","TipDoc='C/P'")
			=ordena("NumMes+ALLTRIM(Numref)","AstPat35")
			=ordena("Periodo+NumMes+NUmRef+TipDoc+TipCta+CodCta","AstPat36")
			=ordena('numri','numri')
			USE
			
			USE AstPre   EXCLUSIVE
			PACK
			=ordena("Tipo+NumMes+NumRef+Cuenta","AstPre1","Tipdoc='H/C'")
			=ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre2","Tipdoc='C/P'")
			=ordena("Tipo+NumMes+NumRef+Cuenta","AstPre3","Tipdoc='H/M'")
			=ordena("tipo+Cuenta","AstPre4")
			=ordena("NumMes+NumRef+TipDoc+Tipo","AstPre5")
			=ordena("tipo+periodo+NumMes+NumRef+cuenta+CodCTC","AstPre6","TipDoc='R/I'")
			=ordena("NumMes+NumRef+CodCTC","AstPre7","TipDoc='C/P'")
			=ordena("tipo+cuemta","AstPre8","estado#'99'")
			=ordena("NumMes+NumRef+CodCTC","AstPre9","TipDoc='N/C'")
			=ordena("NumMes+NumRef+CodCTC","AstPre17","TipDoc='C/P'")
			=ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre18","Tipdoc='H/A'")
			=ordena("NumMes+NumRef+CodCtc","AstPre19","Tipdoc='H/A'")
			=ordena("Periodo+NumMes+NumRef+CodCTC","AstPre20","TipDoc='R/I'")
			=ordena("tipo+periodo+NumMes+NumRef+cuenta+CodCtc","AstPre25","TipDoc='N/C'")
			=ordena("NumMes+ALLTRIM(NumRef)","AstPre26","Tipdoc='C/P'")
			USE

	CASE cual=1 .OR. cual=12		   

			USE Calen  EXCLUSIVE
			PACK
			=ordena("Periodo+NumMes+CodFte+CodFun+CodPrg+CodSubpr+Codproy+CodSupry+CodObra","Calen1")
			=ordena("Periodo+NumMes+CodFte+CodFun+CodPrg+CodSubpr+Codact+Codpart","Calen2")
			=ordena("Periodo+NumMes+CodFte+CodFun+CodPrg+CodSubpr+Codact+Codproy+CodSupry+CodObra","Calen3")
			=ordena("Periodo+NumMes+CodFte+CodFun+CodPrg+CodSubpr+Tipfun","Calen4")
			=ordena("Periodo+Nummes+CodFun+CodPrg+CodFte+CodPart+CodSubpr+CodAct","Calen5","TipFun='F'")
			=ordena("Periodo+NumMes+CodFun+CodPrg+CodFte+CodPart+CodSubpr+CodProy+CodSuPry+CodObra","Calen6","TipFun='I'")
			=ordena("Periodo+NumMes+CodFun+CodPrg+CodFte+TipFun+CodSubpr+CodProy+CodSuPry+CodObra","Calen7")
			=ordena("Periodo+CodFun+CodPrg+CodFte+CodSubpr+CodProy+CodSuPry","Calen8")
			=ordena('CodFun+CodPrg+CodSubpr+CodAct+CodFte+Periodo+NumMes','Caf',"TipFun='F'")
			USE
			
			USE MaePar EXCLUSIVE
			PACK
			=ordena("Periodo+CodFun+CodPrg+CodFte+TipFun","MaePar1")
			=ordena("Periodo+CodFun+CodPrg+TipFun","MaePar2")
			USE
			
			USE ItePar EXCLUSIVE
			PACK
			=ordena("Periodo+ CodFun+CodPrg+CodFte  +TipFun  +CodPart +CodSubpr","ItePar1")
			=ordena("Periodo+CodFte +CodFun+CodPrg  +CodSubpr+CodProy +CodSuPry","ItePar2","TipFun='I'")
			=ordena("Periodo+CodFun +CodPrg+CodFte  +TipFun  +CodPart +CodSubpr +CodAct","ItePar3","TipFun='F'")
			=ordena("Periodo+CodFun +CodPrg+CodFte  +TipFun  +CodPart +CodSubpr +CodProy +CodSuPry","ItePar4","TipFun='I'")
			=ordena("CodFun +CodPrg +CodFte+CodSubpr+CodProy +CodSuPry+CodPart","ItePar5")
			=ordena("CodFun +CodPrg +CodFte+CodSubpr+CodAct  +CodPart","ItePar6")
			=ordena("Periodo+CodFun +CodPrg+CodFte  +TipFun  +CodSubpr+CodAct","ItePar7","TipFun='F'")
			=ordena("Periodo+CodFun +CodPrg+CodSubpr+CodProy +CodSuPry+CodPart","ItePar8","TipFun='I'")
			=ordena("Periodo+CodFun +CodPrg+TipFun  +CodPart +CodSubpr +CodProy +CodSuPry","ItePar9","TipFun='I'")
			=ordena("CodFte +CodFun +CodPrg+CodSubpr+CodProy +CodPart","ItePar10")
			=ordena("CodFte +CodFun +CodPrg+CodSubpr+CodAct  +CodPart","ItePar11")
			USE
			
			USE MaePre EXCLUSIVE
			PACK
			=ordena("Periodo+CodFun +CodPrg+CodSubPr","MaePre1")
			=ordena("Periodo+CodFun +CodPrg+CodSubPr+CodProy","MaePre2","TipFun='I' AND !EMPTY(CodProy)")
			=ordena("Periodo+CodFun +CodPrg+CodSubPr+CodAct", "MaePre3","TipFun='F' AND !EMPTY(CodAct)")
			=ordena("ALLTRIM(CodPrg)+ALLTRIM(Codprg)+ALLTRIM(CodSubPr)+ALLTRIM(CodProy)","MaePre4")
			=ordena("Periodo+CodFun +CodPrg+CodSubPr+CodProy+Codact","MaePre5")
			=ordena("Tipfun +Periodo+CodFun+CodPrg+CodSubPr+CodProy","MaePre6")
			=ordena("TipFun +Periodo+CodFun+CodPrg+CodSubPr+CodAct", "MaePre7")
			USE
			
			USE Clase  EXCLUSIVE
			PACK
			=ordena("Catego+CodPart+Modali+CodSubp","Clase1","Tipo='01' and tipdep='P'")
			* Tipo 01 y 02 so para diferencialrlos en el
			* momento para la asignacion correcta en Contabilidad
			* 01 => para todos menos almacenes
			* 02 => Solo para almacenes
			=ordena("Tipo+Catego+CodPart+Modali+CodSubp","Clase2")
			=ordena('codpart1','clase3',"Tipo='01'.AND.TipDep='P'")
			USE
			
			USE Ingreso  EXCLUSIVE
			PACK
			*  =ordena("CodIng+SubIng","Ingreso1")
			=ordena("CodIng","Ingreso1")
			USE
			
			USE MaeParI  EXCLU
			PACK
			=ordena('Periodo+CodFun+CodPrg+CodFte+TipFun','MaeParI1')
			=ordena('Periodo+CodFun+CodPrg+TipFun','MaeParI2')
			USE
			
			USE IteParI  EXCLU
			PACK
			=ordena('Periodo+CodFun+CodPrg+CodFte+TipFun+CodPart+CodSubpr','IteParI1')
			=ordena('Periodo+CodFte+CodFun+CodPrg+CodSubpr+CodProy+CodSubpr','IteParI2',"TipFun='I'")
			=ordena('PERIODO+CODFUN+CODPRG+CODFTE+TIPFUN+CODPART+CODSUBPR+CODACT','IteParI3',"TipFun='F'")
			=ordena('PERIODO+CODFUN+CODPRG+CODFTE+TIPFUN+CODPART+CODSUBPR+CODPROY+CODSUPRY','IteParI4','TIPFUN="I"')
			=ordena('CODFUN+CODPRG+CODFTE+CODSUBPR+CODPROY+CODSUPRY+CODPART','IteParI5')
			=ordena('CODFUN+CODPRG+CODFTE+CODSUBPR+CODACT+CODPART','IteParI6')
			=ordena('PERIODO+CODFUN+CODPRG+CODFTE+TIPFUN+CODSUBPR+CODACT','IteParI7',"TipFun='F'")
			=ordena('PERIODO+CODFUN+CODPRG+CODSUBPR+CODPROY+CODSUPRY+CODPART','IteParI8',"TIPFUN='I'")
			=ordena('PERIODO+CODFUN+CODPRG+TIPFUN+CODPART+CODSUBPR+CODPROY+CODSUPRY','IteParI9',"TIPFUN='I'")
			=ordena('CODFTE+CODFUN+CODPRG+CODSUBPR+CODPROY+CODPART','IteParI10')
			=ordena('CODFTE+CODFUN+CODPRG+CODSUBPR+CODACT+CODPART','IteParI11')

			
	CASE cual=1 .OR. cual=13
		   


   USE ArtMae EXCLUSIVE
   PACK
   =ordena("LEFT(CodArt,7)","ArtMae1")
   =ordena("UPPER(ALLTRIM(Descri))","ArtMae2" )
   USE

   USE IteArt EXCLUSIVE
   PACK
   =ordena("TipArt+CodArt","IteArt1")
   =ordena("UPPER( ALLTRIM(Descri))","IteArt2" )
   =ordena("CodArt","IteArt3")
   =ordena("Cuenta","IteArt4")   
   USE

   * CUADRO DE NECESIDADES
   *----------------------

   * USE CdrNec EXCLUSIVE
   * PACK
   * =ordena("Periodo+CodDep","CdrneC1")
   * USE
   * USE IteCn EXCLUSIVE
   * PACK
   * =ordena("Periodo+CodDep","IteCn1")
   * =ordena("CodArt","IteCn2")
   * =ordena("Periodo+CodDep+CodArt","IteCn3")
   * USE

*PECOSAS
*---------

   USE Pecosa EXCLUSIVE
   PACK
   =ordena("Periodo+NumPec+Codfte","Pecosa1")
   =ordena("Periodo+Codfte+NumPec","Pecosa2")    
   =ordena("Periodo+CodDep","Pecosa3")    
   * Se usa en Contabilidad  PECOSA4
   =ordena("Periodo+NumPec+Codfte","Pecosa4","Estado='50'")
   USE

   USE Itepec EXCLUSIVE
   PACK
   =ordena("Periodo+NumPec+Codfte","Itepec1")
   =ordena("CodArt+Periodo+NumPec","Itepec2","empty(NumSc)")
   =ordena("CodCal+CodArt","Itepec3","estado='20'")
   =ordena("CodCal+CodPrv","Itepec4","!empty(CodPrv)")
   =ordena("Periodo+NumPec+Codfte","Itepec5","estado#'50'")
   =ordena("Periodo+NumPec+Codfte","Itepec6","estado='20'")
   =ordena("Periodo+NumOC+Newfte+Codart","Itepec7")
   =ordena("ALLTRIM(UPPER(Descri))","Itepec8")
   =ordena("Periodo+Codfte+Numpec","Itepec9") 
   =ordena("CodArt","Itepec10")   
   =ordena("Periodo+Numsc+CodFte+CodArt","Itepec11")
   USE



   USE AuxCot EXCLUSIVE
   PACK
   =ordena("Periodo+NumSc","AuxCot1","TipAct='B'")
   =ordena("Periodo+NumSc","AuxCot2","TipAct='S'")
   USE
   
   


*PROVEEDORES
*-----------

   USE Promae EXCLUSIVE
   PACK
   =ordena("CodPrv","ProMae1")
   =ordena("UPPER(ALLTRIM(NomPro))","Promae2")
   =ordena("NUMRUC","Promae3")
   =ordena("RL_NOM","Promae4")    
   USE
   
   USE PrvAux EXCLUSIVE
   PACK
   =ordena("CodPrv","PrvAux1")
   =ordena("CodCla","PrvAux2")
   =ordena("left(CodCla,3)","PrvAux3")
   USE

*PARTE DE ANULACION
*------------------

   USE PteAnu EXCLUSIVE
   PACK
   =ordena("NumPa","PteAnu1","TipDoc='O/C' and Tipo = 'A'")
   =ordena("NumPa","PteAnu2","TipDoc='O/S' and Tipo = 'A'")
   =ordena("NumPa","PteAnu3","TipDoc='O/C' and Tipo = 'R'")
   =ordena("NumPa","PteAnu4","TipDoc='O/S' and Tipo = 'R'")
   =ordena("NumPa","PteAnu5","TipDoc='O/C' and Tipo = 'M'")
   =ordena("NumPa","PteAnu6","TipDoc='O/S' and Tipo = 'M'")
   =ordena("PerHC+NumHC","Pteanu7","TipDoc='O/C'.AND.TIPO='A'")
   =ordena("PerHC+NumHC","PteAnu8","TipDoc='O/S'.AND.TIPO='A'") 
   =ordena("PerHC+NumHC","PteAnu9")
   USE


*AUTORIZACION DE GIRO
*--------------------

  ?'AutGir'
  USE AutGir EXCLUSIVE
  PACK
  =ordena("ALLTRIM(NumMes)+ALLTRIM(NumAG)","AutGir1")
  =ordena("ALLTRIM(Codctc)","AutGir2")
  =ordena("Periodo+NumMes+CodFte+TipFun","AutGir3")
  USE

   ?'IteAut'
   USE IteAut EXCLUSIVE
   PACK
   =ordena("ALLTRIM(NumDoc)+ALLTRIM(NuAuGi)","IteAut1")
   =ordena("ALLTRIM(CodSCta)","IteAut2")
   USE





  ?'SalPar'
  USE SalPar EXCLUSIVE
  PACK
  =ordena("Codpart+NumMes","SalPar1")
  USE


  ?'Cajas'
  USE Cajas EXCLUSIVE
  PACK
  =ordena("ALLTRIM(CodCtc)","Cajas1")
  =ordena("ALLTRIM(CodCtc)","Cajas2","Tipo='CC'")
  =ordena("ALLTRIM(CodCtc)","Cajas3","Tipo='RE'")
  =ordena("ALLTRIM(DocRef)","Cajas4")
  USE
  

  ?'BcoCta'
  USE BcoCta EXCLUSIVE
  PACK
  =ordena("mes+CodCtc","BcoCta1")
  =ordena("mes+ALLTRIM(CodCtc)","BcoCta2")
  USE

  ?'DetCta'
  USE DetCta EXCLUSIVE
  PACK
  =ordena("Mes+CodCtc+STR(Monto,9,2)+TipDoc+STR(Dia,2)","DetCta1")
  =ordena("Mes+CodCtc+str(dIA,2)+STR(Sec,6)","DetCta2")
  =ordena("SUBSTR(DTOC(FecBco,1),5,2)+CodCtc+STR(Sec,6)","DetCta3")    && Para fines de c lculo
  =ordena("TRANSFORM(Monto,'9,999,999.99')","DetCta4")
  =ordena("Mes+ALLTRIM(CodCtc)","DetCta5")
  =ordena("CodCtc+NumChq","DetCta6")
  =ordena("Mes+ALLTRIM(TipDoc)+ALLTRIM(CodCtc)+ALLTRIM(NumChq)","DetCta7")
  =ordena("Mes+ALLTRIM(TipDoc)+STR(Monto,18,2)+ALLTRIM(CodCtc)","DetCta8")
  USE
  
*+

*+  

  *-------------------------------------------------------------
  * No esta Contemplado, para la continuaci{on del proyecto de a
  * cargo de Victor Paredes 
  *-------------------------------------------------------------
  * USE Hojmod EXCLUSIVE
  * PACK
  * =ordena("NumMes+NumHm","Hojmod1")
  * =ordena("NumMeshc+NumHC","Hojmod2")
  * =ordena("NumMes+NumHm","Hojmod3","Estado='00'")
  * USE

  USE personal EXCLUSIVE
  PACK
  =ordena("codigo","personal1")
  =ordena("descri","personal2")  
  USE

*  ?'Factura'
*  USE Factura EXCLUSIVE
*  PACK
*  =ordena("NumSer+NumFac","Factura1")
*  USE

  USE IteCre EXCLUSIVE
  PACK
  =ordena("periodo+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+ALLTRIM(tipfun)","IteCre1")
  =ordena("periodo+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+ALLTRIM(tipfun)+CodPrg +CodFte  +CodSubpr+CodProy +CodSuPry+CodPart","IteCre2")
  =ordena("periodo+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+ALLTRIM(tipfun)+CodPrg +CodFte  +CodSubpr+CodAct  +CodPart","IteCre3")
  =ordena("periodo+CodFte+CodPrg+CodSubPr+CodProy+CodSuPry+CodPart","IteCre4","TipFun='I'")
  =ordena("periodo+CodFte+CodPrg+CodSubPr+CodAct+CodPart","IteCre5","TipFun='F'")
  USE

 *-Archivo de Items de Transferencia de Partidas
*+
  USE IteTra EXCLUSIVE
  PACK
  =ordena("periodo+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)++ALLTRIM(tipfun)","IteTra1")
  =ordena("periodo+CodFte +CodPrg  +CodSubpr +CodProy + CodSuPry + CodPart","IteTra2","TipFun='I'")
  =ordena("periodo+CodFte +CodPrg  +CodSubpr +CodAct  +CodPart","IteTra3","TipFun='F'")
  *=ordena("ALLTRIM(TipDoc)+ALLTRIM(NumDoc)","IteTra1")
  USE
  
ENDCASE

*------------

CLOSE DATA   
RELEASE WINDOW Working
SET TALK OFF
SET EXCLUSIV OFF
RETURN


FUNCTION ordena
*--------------
PARAMETER key,cdx,condi
*cdx = _path + cdx
IF PARAMETERS()<3
  INDEX ON &key TAG (cdx)
ELSE
   INDEX ON &key TAG (cdx) FOR &condi
ENDIF
RETURN ""


FUNCTION _num
*------------
PARAMETER _FF

   _ff = UPPER(_ff)
   DO CASE
   CASE _ff="TODOS"
      _ffun = 1
   CASE _ff="COMPRO"
      _ffun = 2
   CASE _ff="DIARIO"
      _ffun = 3
   CASE _ff="CUENTAS"
      _ffun = 4
   CASE _ff="SALDOS"
      _ffun = 5
   CASE _ff="AUXIL"
      _ffun = 6
   CASE _ff="PARMAE"
      _ffun = 7
   CASE _ff="CTOCTO"
      _ffun = 8
   CASE _ff="ESTRUC"
      _ffun = 9
   CASE _ff="INGFLU"
      _ffun = 10
   CASE _ff="USUARIO"
      _ffun = 11
   OTHERWISE
      _ffun = 0
   ENDCASE

RETURN _ffun
