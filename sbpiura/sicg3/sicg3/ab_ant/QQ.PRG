    USE IteOc EXCLUSIVE
    PACK
    =ordena("Periodo+NumOc+Codfte","IteOc1")
    =ordena("Periodo+NumOc+Codfte","IteOc2","Estado='00'")    
    =ordena("Periodo+Codfte+NumOc","IteOc3")    
    =ordena("alltrim(upper(descri))+dtoc(fecoc)","IteOc4")      
    =ordena1("alltrim(upper(descri))","IteOc5")              
    =ordena("ALLTRIM(CODCAL)+CODPART","IteOc6","TIPFUN='I'")
    =ordena("ALLTRIM(CODCAL)+CODANAL","IteOc7","TIPFUN='F'")
    =ordena("Periodo+NumOc+Codfte+Codanal","IteOc8")
    =ordena("CodArt","IteOc9")
    =ordena("PERIODO+NUMPEC+CODFTE+CODART+NUMORD","ITEOC10")
    USE
