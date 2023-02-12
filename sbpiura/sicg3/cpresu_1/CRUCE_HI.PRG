* Programa: Cruce_HI.prg
* Descripci¢n: Cruce del programa hojcon con el itehc
*              para verificaci¢n del codigo de calendario con la
*              estructura program tica.
                              
                              
USE hojcon IN 1 ORDER TAG hojcon1 ALIAS hoja
USE itehc  IN 2 ORDER TAG itehc1  ALIAS iteh

SET DEVICE TO PRINT
SET PRINT  ON
SET PRINT  TO FILE CRUC2_HI.LST

SELECT iteh
SET RELATION TO nummes + numhc INTO hoja
GO TOP
vnumerr=0
SCAN
        vcodcal = '96'+iteh.nummes+iteh.codfte+iteh.codprg+iteh.codsubpr+alltrim(iteh.codact)+iteh.codproy+iteh.codsupry
        WAIT WINDOW "Verificando ... "+alltrim(vcodcal) nowait
        SELECT hoja
        DO CASE
        CASE EOF() AND ITEH.NUMHC#'J'
           vnumerr=vnumerr+1
           ? str(vnumerr,3)+' H/C: '+iteh.nummes + '-'+iteh.numhc + " No tiene registro padre "
        CASE ALLTRIM(vcodcal)#ALLTRIM(hoja.codcal) AND iteh.estado<>'92'
           vnumerr=vnumerr+1
           ? str(vnumerr,3)+' H/C '+nummes + '-'+numhc + ' Verificar: '+ALLTRIM(vcodcal)+"' <> '"+ALLTRIM(hoja.codcal)+"'"+" --> "+iteh.codpart+" "+iteh.codanal
        ENDCASE
        SELECT iteh
ENDSCAN
CLOS DATA
SET PRINT OFF
SET PRINT TO
SET DEVICE TO SCREEN
WAIT WINDOW str(vnumerr,3)+" Errores " NOWAIT
RETURN
        
