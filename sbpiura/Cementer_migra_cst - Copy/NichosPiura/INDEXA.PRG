** ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
** ³ Programa	: Indexa.prg									   ³
** ³ 			  Utilitario de Creaci¢n de Indices                ³
** ³ Fecha		: 18/08/98                                         ³
** ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PARAMETER confirma
if parameters()=0
   confirma=.T.
else
   confirma=.F.
endif
CLOSE DATA
IF confirma
   IF !YESNO('¨Desea indexar archivos?')
      RETURN
   ENDIF
ENDIF
DO ORGANIZA
clos data
RETURN

PROCEDURE organiza
*-----------------
PARAMETER cual

CLOSE DATA

bases  = 24
actual = 0
do progreso with 1,'Progreso de Indexaci¢n:'

*- 1
	USE Cement EXCLUSIVE
	PACK
	REIN
	=ordena("CodCem","Cement1")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 2
	USE Cuartel EXCLUSIVE
	PACK
	REIN
	=ordena("CodCem+CodCuar","Cuartel1")
	=ordena("CodCem+NomCuar","Cuartel2")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual
	
*- 3
	USE Nichos EXCLUSIVE
	PACK
	REIN
	=ordena("CodCem+CodCuar+Fila1","Nichos1")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual
	
*- 4
	USE Difunto EXCLUSIVE
	PACK
	REIN
	=ordena("CodDif","Difunto1")
	=ordena("ALLTRIM(UPPER(ApePat)) + ALLTRIM(UPPER(ApeMat)) + ALLTRIM(UPPER(NomDif))","Difunto2")
	=ordena("CodMau","Difunto3")
	=ordena("CodCli","Difunto4")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual
	
*- 5
	USE OcupFut EXCLUSIVE
	PACK
	REIN
	=ordena("CodOcu","OcupFut1")
	=ordena("ALLTRIM(UPPER(ApePatOc)) + ALLTRIM(UPPER(ApeMatOc)) +ALLTRIM(UPPER(NomOcu))","OcupFut2")
	=ordena("CodCli","OcupFut3")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 6
	USE Mausoleo EXCLUSIVE
	PACK
	REIN
	=ordena("CodMau","Mausoleo1")
	=ordena("Familia","Mausoleo2")
	=ordena("Codcli","Mausoleo3")    
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 7
	USE Traslado EXCLUSIVE
	PACK
	REIN
	=ordena("CodTras","Traslado1")
	=ordena("CodDifAn","Traslado2")
	=ordena("CodDifNw","Traslado3")    
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 8
	USE Transfer EXCLUSIVE
	PACK
	REIN
	=ordena("CodTrn","Transfer1")
	=ordena("CodOcuAn","Transfer2")
	=ordena("CodOcuNw","Transfer3")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 9
	USE Ventas EXCLUSIVE
	PACK
	REIN
	=ordena("CodVta","Ventas1")
	=ordena("CodCli","Ventas2")
	=ordena("CodAvl","Ventas3")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 10
	USE Creditos EXCLUSIVE
	PACK
	REIN
	=ordena("CodCre","Creditos1")
	=ordena("CodCli","Creditos2")
	=ordena("CodAvl","Creditos3")
	=ordena("CodDif","Creditos4")
	=ordena("CodMau","Creditos5")
	=ordena("CodOcu","Creditos6")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 11
	USE Subvenc EXCLUSIVE
	PACK
	REIN
	=ordena("CodSub","Subvenc1")
	=ordena("CodEnt+CodSub","Subvenc2")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 12
	USE Donac EXCLUSIVE
	PACK
	REIN
	=ordena("CodDon","Donac1")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 13
	USE PagACta EXCLUSIVE
	PACK
	REIN
	=ordena("CodPAC","PagACta1")
	=ordena("CodCli","PagACta2")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 14
	USE PagSub EXCLUSIVE
	PACK
	REIN
	=ordena("CodPS","PagSub1")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 15
	USE ItePS EXCLUSIVE
	PACK
	REIN
	=ordena("CodPS","ItePS1")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 16
	USE CanDon EXCLUSIVE
	PACK
	REIN
*	=ordena("PERIODO+CODART+CORREL+CORAUX","Kardexv1","ESTADO#'99'")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 17
	USE KarCre EXCLUSIVE
	PACK
	REIN
	=ordena("CodCre+Correl","KarCre1","Estado#'99'")
	=ordena("TipMov+CodMov","KarCre2")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 18
	USE Caja EXCLUSIVE
	PACK
	REIN
	=ordena("TipCaj+CorIng","Caja1")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 19
	USE Clientes EXCLUSIVE
	PACK
	REIN
	=ordena("CodCli","Clientes1")
	=ordena("NomCli","Clientes2")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 20
	USE Parmae EXCLUSIVE
	PACK
	REIN
	=ordena("Tipo+ALLTRIM(Codigo)+CodigoAux","Parmae1")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 21
	USE Usuario EXCLUSIVE
	PACK
	REIN
	=ordena("Usuario","Usoario1")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*USE IteUsu EXCLUSIVE
*PACK
*=ordena("UsuCla+Modulo","IteUsu1")
*=ordena("Sistema+Modulo","IteUsu2")
*USE

*- 22
	USE IteUsuOp EXCLUSIVE
	PACK
	REIN
	=ordena("ALLTRIM(User)+Sistema+Modulo+Opcion","IteUsuOp1")
	=ordena("ALLTRIM(User)+CodAcc","IteUsuOp2")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 23
	USE MaeAcc EXCLUSIVE
	PACK
	REIN
	=ordena("Sistema+Modulo+Opcion","MaeAcc1")
	=ordena("CodAcc","MaeAcc2")
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual

*- 24

	USE Bitacora EXCLUSIVE
	PACK
	REIN
	USE
	actual=actual+1
	do progreso with 2,'',bases,actual


CLOSE DATA   
WAIT WINDOW 'Fin de proceso de indexaci¢n' TIMEOUT 3
DO PROGRESO WITH 3

RETURN

FUNCTION ordena
*--------------
PARAMETER key,cdx,condi
IF PARAMETERS()<3
   INDEX ON &key TAG (cdx)
ELSE
   INDEX ON &key TAG (cdx) FOR &condi
ENDIF
RETURN ""

