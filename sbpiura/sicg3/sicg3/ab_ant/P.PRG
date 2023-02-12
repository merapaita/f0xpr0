    USE Itepec EXCLUSIVE
    PACK
    =ordena("Periodo+NumPec","Itepec1") &&Codfte+Codcad
    =ordena("CodArt+Periodo+NumPec","Itepec2","empty(NumSc)")
    =ordena("CodCad+CodArt","Itepec3","estado='20'")
    =ordena("CodCad+CodPrv","Itepec4","!empty(CodPrv)")
    =ordena("Periodo+NumPec+Codfte","Itepec5","estado#'50'")
    =ordena("Periodo+NumPec+Codfte+Codcad","Itepec6","estado='20'")
    =ordena("Periodo+NumOC+Newfte+Codart+numord","Itepec7")
    =ordena("ALLTRIM(UPPER(Descri))","Itepec8")
    =ordena("Periodo+Codcad+Codfte+Numpec","Itepec9")    
    =ordena("Codart","Itepec10")        
    =ordena("Periodo+NumSC+CodFte+CodArt+NumOrd+Numpec","Itepec11")    
    =ordena("Periodo+NumPEC+CodFte+CodArt","Itepec16")

    * Se usa en Tesorer¡a  ItePec12
    =ordena("Periodo+numoc+PADL(month(fecoc),2,'0')","ItePec12")
    =ordena("PERIODO+CODDEP+CODART","ItePec13")
    =ordena("PERIODO+NUMPEC+Codcad+CODFTE+CODART+NUMORD","ItePec14")
    =ordena("PERIODO+NUMOC+ORDEN","ItePec15","ORDEN='û'")
    USE
