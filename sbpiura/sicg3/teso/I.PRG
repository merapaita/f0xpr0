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
