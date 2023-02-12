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
  =ordena("ALLTRIM(NUMMES)+ALLTRIM(CODCTC)+NUMCP","Compag10")
  USE

  ?'IteCp'
  USE IteCp EXCLUSIVE
  PACK
  =ordena("ALLTRIM(NumMes)+NumCp+ALLTRIM(CodCtc)","IteCp1")
  =ordena("CodPart+NumMes","IteCp2")
  =ordena("CodCtc+NUMMES+NUMCP","IteCp3")
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

  ?'Cheque'
  USE Cheque EXCLUSIVE
  PACK
  =ordena("NumMes+NumCp+CodCtc","Cheque1")
  =ordena("CodCtc+ALLTRIM(NumChq)","Cheque2")
  * para Abastecimiento
  =ordena("Periodo+NumMes+NumCp+CodCtc","Cheque3")
  =ordena("ALLTRIM(NumChq)","Cheque4")
  =ordena("ALLTRIM(Nomgir)","Cheque5")

  USE AstPre   EXCLUSIVE
  PACK
  =ordena("Tipo+NumMes+NumRef+Cuenta","AstPre1","Tipdoc='H/C'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre2","Tipdoc='C/P'")
  =ordena("Tipo+NumMes+NumRef+Cuenta","AstPre3","Tipdoc='H/M'")
  =ordena("Tipo+Cuenta","AstPre4")
  =ordena("NumMes+NumRef+TipDoc","AstPre5")
  =ordena("Tipo+periodo+NumMes+NumRef+Cuenta+CodCtc","AstPre6","Tipdoc='R/I'") 
  =ordena("NumMes+NumRef+CodCtc","AstPre7","Tipdoc='C/P'")
  =ordena("Tipo+Cuenta","AstPre8","ESTADO#'99'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre9","Tipdoc='N/A'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre10","Tipdoc='N/C'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre11","Tipdoc='B/D'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre12","Tipdoc='NOC'")
  =ordena("NumMes+NumRef+CodCtc","AstPre13","Tipdoc='N/A'")
  =ordena("NumMes+NumRef+CodCtc","AstPre14","Tipdoc='N/C'")
  =ordena("NumMes+NumRef+CodCtc","AstPre15","Tipdoc='B/D'")
  =ordena("NumMes+NumRef+CodCtc","AstPre16","Tipdoc='NOC'")
  =ordena("Tipo+NumMes+NumRef+Cuenta+CodCtc","AstPre18","Tipdoc='H/A'")
  =ordena("NumMes+NumRef+CodCtc","AstPre19","Tipdoc='H/A'")
  =ordena("Periodo+NumMes+NumRef+CodCtc","AstPre20","Tipdoc='R/I'")
  =ordena("NumMes+NumRef+TipDoc+Anureb","AstPre21")
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


FUNCTION ordena
*--------------
PARAMETER key,idx,condi
   IF PARAMETERS()<3
      INDEX ON &key TAG (idx)
   ELSE
      INDEX ON &key TAG (idx) FOR &condi
   ENDIF
RETURN ""

