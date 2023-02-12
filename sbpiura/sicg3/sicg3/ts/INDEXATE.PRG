*------------------------------------------------------
* PROGRAMA: IndexaTE.prg
* OBJETIVO: Rutina de Organizaci¢n archivos
*           Crea ¡ndices compuestos y compacta la data.
*-------------------------------------------------------
PARAMETER _fil

*PRIVATE _auto
*_auto = IIF( PARAMETER()>0,.T.,.F.)

PRIVATE _auto, _path, ss, pos
ss=SET('PATH')
pos = AT(";",ss)
_path=IIF(pos=0,ss,SUBSTR(ss,1,AT(";",ss)-1))
_auto = IIF( PARAMETER()>0,.T.,.F.)

CLOSE DATA

IF !WEXIST("msg2user")
   DEFINE WINDOW MSG2USER FROM 19,27 TO 23,77
ENDIF

IF !_auto

  IF escolor
    DEFINE POPUP _yt  FROM 9,55          SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP _yt  FROM 9,55          COLOR SCHEME C_POPUP
  ENDIF

  DEFINE BAR  1 OF _yt PROMPT "\<a TODOS"
* DEFINE BAR  2 OF _yt PROMPT "\<b Tesorer¡a Regional"
* DEFINE BAR  3 OF _yt PROMPT "\<c Tesorer¡a Sede" 
  DEFINE BAR  2 OF _yt PROMPT "\<b Par metros"

  ON SELECTION POPUP _yt DO ORGANIZA WITH BAR()
  ACTIVATE POPUP _yt

  RELEASE POPUP _yt

ELSE
  x = _num(_fil)
  DO organiza WITH x
ENDIF

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


IF cual=5   && Tesoreria no entra

*  ?'AutGiro'
*  USE AutGiro EXCLUSIVE
*  PACK
*  =ordena("ALLTRIM(Nummes)+Correl","AutGiro1")
*  =ordena("ALLTRIM(CodScta)","AutGiro2")
*  USE

*  ?'IteAut'
*  USE IteAut EXCLUSIVE
*  PACK
*  =ordena("alltrim(Nummes)+Correl","IteAut1")
*  =ordena("ALLTRIM(CodSCta)","IteAut2")
*  =ordena("ALLTRIM(CodScta)+Codpart+nummes","Iteaut3")    
*  USE
  
*  ?'AuGiRe'
*  USE AuGiRe EXCLUSIVE
* PACK
*  =ordena("ALLTRIM(Nummes)+ALLTRIM(Numdoc)","AuGiRe1")
*  =ordena("ALLTRIM(CodSCta)","AuGiRe2")
*  =ordena("ALLTRIM(Carref)","AuGiRe3")
*  USE

*  ?'ItAuRe'
*  USE ItAuRe EXCLUSIVE
*  PACK
*  =ordena("ALLTRIM(Nummes)+ALLTRIM(Numdoc)","itaure1")
*  =ordena("ALLTRIM(CodSCta)","ItAuRe2")
*  =ordena("Nummes+ctcofg","ItAuRe3")
*  USE


  ?'AutGirP'
  USE AutGiRP EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+ALLTRIM(Numdoc)","AuTGiRp1")
  =ordena("ALLTRIM(CodSCta)","AuTGiRp2")
  =ordena("ALLTRIM(Carref)","AuTGiRp3")
  USE

  ?'IteauP'
  USE IteauP EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+ALLTRIM(Numdoc)","iteaup1")
  =ordena("ALLTRIM(CodSCta)","Iteaup2")
  =ordena("Nummes+ctcofg","Iteaup3")
  USE

  ?'RevSnu'
  USE RevSnu EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+NumRev","RevSnu1")
  USE

  ?'IteRevSn'
  USE IteRevSn EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+NumRev","IteRevSn1")
  USE

  ?'CarOrd'
  USE CarOrd EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+NumCar","CarOrd1")
  USE

  ?'IteCaOrd'
  USE IteCaOrd EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+NumCar","IteCaOrd1")
  USE

  ?'ChqGer'
  USE ChqGer EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+Numero","Chqger1")
  USE

  ?'IteChqGe'
  USE IteChqGe EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+Numero","IteChqge1")
  =ordena("ALLTRIM(Nummes)+Numero+codctc","Itechqge2")
  USE

  ?'TransFon'
  USE TransFon EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+Numero","transfon1")
  USE

  ?'IteTf'
  USE IteTf EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+Numero","IteTf1")
  =ordena("ALLTRIM(Nummes)+Numero+codctc","IteTf2")
  USE

  ?'AutSim'
  USE AutSim EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+Numero","Autsim1")
  USE

  ?'Iteauts'
  USE Iteauts EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Nummes)+Numero","Iteauts1")
  =ordena("ALLTRIM(Nummes)+Numero+codctc","Iteauts2")
  USE

  
ENDIF

IF cual=1   && Tesoreria 
  ?'Compag'
 *-Archivo de Comprobante de Pago
  USE ComPag EXCLUSIVE
  PACK
  =ordena("ALLTRIM(NumMes)+NumCp+ALLTRIM(CodCtc)","ComPag1")
  =ordena("ALLTRIM(NumMes)+NumCp+ALLTRIM(CodCtc)","ComPag3","ESTADO#'51' and ESTADO#'99'")  
  =ordena("NumMeshc+Numhc","ComPag4")  
  =ordena("ALLTRIM(CodPrv)","ComPag5")
  =ordena("ALLTRIM(CodEmp)","ComPag6")
  =ordena("ALLTRIM(CodOtr)","ComPag7")
  =ordena("CODCTC+NUMMES+NUMCP","Compag8")
  =ordena("ALLTRIM(Codret)","ComPag9","tipdoc$'RESR'")
* =ordena("ALLTRIM(CODPRE)","compag10")
  USE

  ?'IteCp'
  USE IteCp EXCLUSIVE
  PACK
  =ordena("ALLTRIM(NumMes)+NumCp+ALLTRIM(CodCtc)","IteCp1")
  =ordena("CodPart+NumMes","IteCp2")
  =ordena("CodCtc+NUMMES+NUMCP","IteCp3")
  =ORDENA("PERIODO+NUMMES+CODCTC+CODPART","ITECP4")

  USE

  ?'Cheque'
  USE Cheque EXCLUSIVE
  PACK
  =ordena("NumMes+NumCp+CodCtc","Cheque1")
  =ordena("CodCtc+ALLTRIM(NumChq)","Cheque2")
  * para Abastecimiento
  =ordena("Periodo+NumMes+NumCp+CodCtc","Cheque3")
  =ordena("ALLTRIM(NumChq)","Cheque4")
  =ordena("ALLTRIM(Nomgir)","Cheque5")
  =ORDENA("CODCTC+NUMMES","CHEQUE6")

  USE

  ?'Cheque Pendiente'
  USE ChqPen EXCLUSIVE
  PACK
  =ordena("NumMes+Codctc","ChqPen1")
  =ordena("Codctc+ALLTRIM(Numchq)","ChqPen2")
  =ordena("Tipdoc","ChqPen3")

  USE

  ?'Cheque Certificado'
  USE ChqCert EXCLUSIVE
  PACK
  =ordena("NumMes+ALLTRIM(CodCtc)","Chqcert1")
  =ordena("NumMes+ALLTRIM(CodCtc)+ALLT(Numchq)","Chqcert2")

  USE
  
  ?'Hist¢rico Cheq. Pendiente'
  USE HChqPen EXCLUSIVE
  PACK
  =ordena("NumMes+Codctc","HChqPen1")
  =ordena("Codctc+ALLTRIM(Numchq)","HChqPen2")
  =ordena("Tipdoc","HChqPen3")

  USE

  ?'Reten'
  USE Reten EXCLUSIVE
  PACK
  =ordena("NumMes+NumCp+CodCtc","Reten1")
  =ordena("CodCtc+CodRet","Reten2")
  =ordena("Nummes+numcp+codctc+codret","Reten3","EMPTY(conpago)")
  =ordena("CodCtc+CodRet","Reten4","EMPTY(conpago)")
  =ordena("Mescppg+NumCppg+CodCtc","Reten5")
  USE

  ?'AstPat'
  USE Astpat EXCLUSIVE
  PACK
  =ordena("NumMes+NumRef+CodCtc","AstPat1")
  =ordena("NumMes+NumRef","AstPat2","TipDoc='H/C'")
  =ordena("NumMes+NumRef+CodCtc","AstPat3","TipDoc='C/P'")
  =ordena("ALLTRIM(NumMes)+NumRef+CodCtc","AstPat4")
  =ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat5","TipDoc='N/C'")
  =ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat6","TipDoc='N/A'")
  =ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat7","TipDoc='B/D'")
  =ordena("CodCta","AstPat8")
  =ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat9","TipDoc='NOC'")
  =ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat10")
  =ordena("PERIODO+ALLTRIM(NumMes)+NumRef+CodCtc","AstPat14","TipDoc='R/I'")
  =ordena("NumMes+ALLTRIM(NumRef)+CodCtc","AstPat15","TipDoc='REV'")  
  =ordena("NumMes+NumRef+CodCtc","AstPat18","TipDoc='H/A'")
  =ordena("NumMes+codctc+Numref+numDoc","AstPat19")
  =ordena("CodCtc+NumMes+NumRef","AstPat20","TipDoc='C/P'")
  USE

&&& USE Astpat97 EXCLUSIVE

  USE Astpre EXCLUSIVE
  PACK
  =ordena("Tipo+NumMes+NumRef+Cuenta","AstPre1", "TipDoc='H/C'")
  =ordena("Tipo+NumMes+NumRef+Cuenta","AstPre2", "TipDoc='C/P'")
  =ordena("Tipo+NumMes+NumRef+Cuenta","AstPre3", "TipDoc='H/M'")
  =ordena("Tipo+Cuenta","AstPre4")
  =ordena("NumMes+NumRef+TipDoc+Tipo","AstPre5")
  =ordena("Tipo+Periodo+NumMes+NumRef+cUENTA+cODCTC","AstPre6","TipDoc='R/I'")
  =ordena("NumMes+NumRef+CodCtc","AstPre7","TipDoc='C/P'")
  =ordena("Tipo+Cuenta","AstPre8","Estado#'99'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre9","TipDoc='N/A'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre10","TipDoc='N/C'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre11","TipDoc='B/D'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre12","TipDoc='NOC'")
  =ordena("NumMes+NumRef+CodCtc","AstPre13","TipDoc='N/A'")
  =ordena("NumMes+NumRef+CodCtc","AstPre14","TipDoc='N/C'")
  =ordena("NumMes+NumRef+CodCtc","AstPre15","TipDoc='B/D'")
  =ordena("NumMes+NumRef+CodCtc","AstPre16","TipDoc='NOC'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre18","TipDoc='H/A'")
  =ordena("NumMes+NumRef+CodCtc","AstPre19","TipDoc='H/A'")
  =ordena("Periodo+NumMes+NumRef+CodCtc","AstPre20","TipDoc='R/I'")
*  =ordena("NumMes+Numref+Tipdoc+Anureb","AstPre21")

  ?'HojAnu'
  USE HojAnu EXCLUSIVE
  PACK
  =ordena("ALLTRIM(NumMes)+Numha","hojanu1")
  =ordena("NumMes+Numha+codctc","hojanu2")
  =ordena("NumMesCP+NumCP+codctc","hojanu3")
  USE

  ?'IteHa'
  USE IteHa EXCLUSIVE
  PACK
  =ordena("ALLTRIM(nummes)+NUMHA","iteha1")
  USE

  ?'RecIng'
  USE RecIng EXCLUSIVE
  PACK
  =ordena("periodo+ALLTRIM(NumMes)+ALLTRIM(NumRi)","RecIng1")
  =ordena("Periodo+ALLTRIM(NumMes)+ALLTRIM(NumRi)","RecIng2","ESTADO#'51'")
  USE

  ?'IteRI'
  USE IteRi EXCLUSIVE
  PACK
  =ordena("PERIODO+ALLTRIM(NumMes)+ALLTRIM(NumRi)","IteRi1")
  =ordena("ALLTRIM(NumMes)+ALLTRIM(BOLDEP)","IteRi2")
  USE

*  ?'RecIng97'
*  USE RecIng97 EXCLUSIVE
*  PACK
*  =ordena("periodo+ALLTRIM(NumMes)+ALLTRIM(NumRi)","RecIng1")
*  =ordena("Periodo+ALLTRIM(NumMes)+ALLTRIM(NumRi)","RecIng2","ESTADO#'51'")
*  USE

*  ?'IteRI97'
*  USE IteRi97 EXCLUSIVE
*  PACK
*  =ordena("PERIODO+ALLTRIM(NumMes)+ALLTRIM(NumRi)","IteRi1")
*  =ordena("ALLTRIM(NumMes)+ALLTRIM(BOLDEP)","IteRi2")
*  USE

  ?'Factura'
  USE Factura EXCLUSIVE
  PACK
  =ordena("Serie+Numero","Factura1")
  =ordena("Perri+ALLT(Nummesri)+numri","Factura2")
*  =ordena("Serie+Numero+Conlic","Factura3")
  USE

  ?'ConLic'
  USE ConLic EXCLUSIVE
  PACK
  =ordena("ConLic","ConLic1","Tipo='C'")
  =ordena("ConLic","ConLic2","Tipo='O'")
  =ordena("Tipo+ConLic","ConLic3")
  USE

  ?'MemBas'
  USE Membas EXCLUSIVE
  PACK
  =ordena("Numero","Membas1")
  USE

  ?'CarFia'
  USE Carfia EXCLUSIVE
  PACK
  =ordena("numero","Carfia1")
  =ordena("Codban+NumDoc","Carfia2")
  USE
  
  ?'Cajas'
  USE Cajas EXCLUSIVE
  PACK
  =ordena("ALLTRIM(CodCtc)","Cajas1")
  =ordena("ALLTRIM(CodCtc)","Cajas2","Tipo='CC' AND Clase$'TC'")
  =ordena("ALLTRIM(CodCtc)","Cajas3","Tipo='RE'")
  =ordena("ALLTRIM(CodCtc)","Cajas4","Tipo='CC' AND Clase='A'")
  =ordena("ALLTRIM(CodCtc)","Cajas5","Tipo='CC' AND Sector='S'")
  =ordena("ALLTRIM(CodCtc)","Cajas6","Tipo='CC' AND Subsidio='S'")
  =ordena("ALLTRIM(CodCtc)","Cajas7","Clase='A'")
  =ordena("ALLTRIM(CodCtc)","Cajas8","Codfte='PRP'")
  =ordena("ALLTRIM(CodCtc)","Cajas9","Codfte='TRN' AND tipo='RE' AND ofigir='R'")
  =ordena("ALLTRIM(CodCtc)","Cajas10","Codfte='TRN' AND ofigir='S'")
  =ordena("ALLTRIM(CodCtc)","Cajas11","Codfte='TRN' AND Tipo='CC'")
  USE

  ?'SubCtas'
  USE SubCtas EXCLUSIVE
  PACK
  =ordena("ALLTRIM(Subcta)","SubctaS1")
  USE

  ?'ConBan'
  USE ConBan EXCLUSIVE
  PACK
  =ordena("percon+ALLTRIM(mescon)+codctc","conban1")
  USE
  
  ?'MovBco'
  USE MovBco EXCLUSIVE &&MOVIM. BCO. SEDE
  PACK
  =ordena("nummes+numero","MovBco1")
  =ordena("ALLTRIM(TipDoc)+ALLTRIM(Numero)+alltrim(CodCtc)","MovBco2")
  =ordena("ALLTRIM(TipDoc)+ALLTRIM(Numero)","MovBco3")
  =ordena("Fecha","MovBco4")
  =ordena("CodCtc+ALLTRIM(Numero)","MovBco5","ESTADO='40' AND Tipdoc='N/C'")    
  =ordena("CodCtc+ALLTRIM(Numero)","MovBco6","ESTADO='40' AND Tipdoc='N/A'")    
  =ordena("nummes+numero+codctc","MovBco7","ESTADO<>'51'  AND Tipdoc='B/D'")    
  =ordena("nummes+numero+codctc","MovBco8","ESTADO<>'51'  AND Tipdoc='N/C'")    
  =ordena("nummes+numero+codctc","MovBco9","ESTADO<>'51'  AND Tipdoc='N/A'")    
  =ordena("nummes+numero+codctc","MovBco10","ESTADO<>'51'  AND Tipdoc='NOC'")    
  =ordena("nummesC+numero","MovBco11")
  =ordena("PADL(ALLTRIM(STR(MONTH(Fecha))),2,'0')+ALLTRIM(TIPDOC)+STR(MONTO,18,2)+ALLTRIM(CODCTC)","MovBco12")
  =ordena("periodo+nummes+ALLTRIM(Numref)+numri+CODCTC","MovBco13")  
  =ordena("periodo+nummes+ALLTRIM(Numref)+codctc","MovBco14")  
  USE

  ?'SalBcos'
  USE Salbcos EXCLUSIVE
  PACK
  =ordena("CodCtc+periodo+nummes","salbcos1")
  USE

*  ?'SaldE6'
*  USE SaldE6 EXCLUSIVE
*  PACK
*  =ordena("Codpart+periodo+nummes","salde61")
*  USE

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
  
  ?'FonPag'
  USE FonPag EXCLUSIVE
  PACK
  =ordena("ALLTRIM(NumFp)+ALLTRIM(NumMes)","FonPag1")
  =ordena("ALLTRIM(NumFp)+ALLTRIM(NumMes)+ALLTRIM(codctc)","FonPag2")
  USE
  
  ?'IteFp'
  USE IteFp EXCLUSIVE
  PACK
  =ordena("ALLTRIM(NumFp)+ALLTRIM(NumMes)","IteFp1")
  =ordena("ALLTRIM(NumFp)+ALLTRIM(NumMes)+DTOC(fecdoc,1)","IteFp2")
  USE
  
  USE Reversio EXCLUSIVE
  PACK
  =ordena("ALLTRIM(NumMes)+ALLT(NumRev)","Rever1")
  =ordena("PerDoc+ALLT(mesdoc)+Numdoc+ALLT(NumRev)","Rever2","Tipdoc='R/I'")
  USE

*  ?'MaeReg'
*  USE Maereg EXCLUSIVE
*  PACK
*  =ordena("ALLTRIM(Nummes)+ALLTRIM(Numero)","Maereg1")
*  =ordena("ALLTRIM(NummescP)+ALLTRIM(NumCP)+ALLTRIM(Codctc)","Maereg2")
*  USE

*  ?'IteReg'
* USE IteReg EXCLUSIVE
*  PACK
*  =ordena("ALLTRIM(Nummes)+ALLTRIM(Numero)","Itereg1")
*  USE

  ?'CtaSec'
  USE CtaSec EXCLUSIVE
  PACK
  =ordena("CodCtc+Sector","CtaSec1")
  =ordena("CodCtc","CtaSec2")
  USE

  ?'Ingreso'
  USE Ingreso EXCLUSIVE
  PACK
  =ordena("coding+subing","ingreso1")
  USE

  ?'Oficheq'
  USE Oficheq EXCLUSIVE
  PACK
  =ordena("ALLT(nummes)+nroofi","Oficheq1")
  USE

  ?'Iteofich'
  USE Iteofich EXCLUSIVE
  PACK
  =ordena("ALLT(nummes)+nroofi","Iteofich1")
  =ordena("ALLT(nummes)+nroofi+ALLT(codctc)","Iteofich2","estado='1'")
  USE

*  ?'Contchq'
 * USE Contchq EXCLUSIVE
 * PACK
*  =ordena("ALLT(nummes)+numero","Contchq1")
*  =ordena("ALLT(Codctc)+STR(serie)","Contchq2","Estado='00'")
*  =ordena("ALLT(nummes)+numero","Contchq3","Estado='10'")
*  =ordena("ALLT(nummes)+numero","Contchq4","Estado='20'")
*  USE
  
ENDIF

IF cual=2		  &&-Archivo de Parametros

  ?'ParMae'
  USE ParMae EXCLUSIVE
  PACK
   =ordena("TIPO+ALLT(CODIGO)+CODIGOAUX","PARMAE1")
   =ordena("TIPO+DESCRI+CODIGO+CODIGOAUX","parmae2")
   =ordena("descri","parmae3")
  USE
ENDIF

RELEASE WINDOW Working
CLOSE DATA
SET TALK OFF

RETURN


FUNCTION ordena
*--------------
PARAMETER key,idx,condi
   IF PARAMETERS()<3
      INDEX ON &key TAG (idx)
   ELSE
      INDEX ON &key TAG (idx) FOR &condi
   ENDIF
RETURN ""


FUNCTION _num
*------------
PARAMETER _FF

   _ff = UPPER(_ff)

   DO CASE
   CASE _ff="TODOS"
      _ffun = 1
   CASE _ff="PARAME"
      _ffun = 2
   OTHERWISE
      _ffun = 0
   ENDCASE

RETURN _ffun