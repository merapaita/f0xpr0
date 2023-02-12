SET DEVICE TO FILE PRUEBA.TXT
@PROW(),0 SAY CHR(18)
@PROW()+1,0 SAY ALLTRIM(CIA)
@PROW()+1,0 SAY 'LisHCP'
@PROW(),70 SAY DATE()
@PROW()+1,20 SAY 'PROCESO DE AFECTACION Y EJECUCION PRESUPUESTAL'
@PROW()+2,0 SAY 'N H/C      :'
@PROW(),15 SAY NUMHC+'.'+NUMMES 
@PROW(),60 SAY 'Estado :'
@PROW(),70 SAY  IIF(Estado='00','Pendiente',IIF(Estado='20','S/Cotizac',IIF(Estado='99','Anulado',IIF(Estado='50','Atendido',' -  '))))
@PROW()+1,0 SAY 'Fecha H/C   :'
@PROW(),15 SAY Fechc 
@PROW()+1,0 SAY 'Razon Social:'
@PROW(),15 SAY val_prv(codprv)
@PROW()+1,0 SAY 'Documento  :'
@PROW(),15 SAY tipdoc
@prow(),18 say numref
@PROW()+1,0 SAY 'Programa   :'
@PROW(),15 SAY substr(codcal,8,2)
@prow(),24 say val_para(substr(CodCal, 8,2),'CODPRG','D',26,40)
@PROW()+1,0 SAY 'SubPrograma:'
@PROW(),15 SAY substr(codcal,10,3)
@prow(),24 say Val_Cale(substr(codcal,10,3),'CODSUB'+substr(codcal,8,2)+'    ','D',22,40)
@prow()+1,0 say IIF(TIPFUN='I','Proyecto    :','Actividad   :')
@prow(),15 say substr(codcal,13,3)+'.'+substr(codcal,16,4)
@prow(),24 say OBSERVA
@prow()+1,0 say 'Fte. Finan. :'
@prow(),15 say substr(CodCal, 5,3)
@prow(),24 say val_para(substr(CodCal, 5,3),'CODFTE','D',26,50)
@prow()+2,30 say ' CODIFICACION PROGRAMATICA '
@PROW()+1,0 SAY 'Ú'+REPLICATE('Ä',78)+'¿'
@PROW()+1,0 SAY '³PLIEGO PROGRAMA  SUBPROGRAMA  ACTIVIDAD  PROYECTO   FUENTE FINANCIAMIENTO     ³'
@PROW()+1,0 SAY 'À'+REPLICATE('Ä',78)+'Ù'
@PROW()+1,2 SAY '001'
@PROW(),11 SAY substr(CodCal, 8,2) 
@PROW(),23 SAY substr(codcal,10,3)
@PROW(),35 SAY iif(TipFun='F',substr(codcal,13,2),' ')
@PROW(),46 SAY iif(TipFun='I',substr(codcal,13,3),' ')
@PROW(),55 SAY val_para(substr(CodCal, 5,3),'CODFTE','D',22,60)
@PROW()+1,0 SAY REPLICATE('_',80)
@PROW()+1,0 SAY '     COMPROMISO PRESUPUESTAL '
@prow(),43 say 'Ú'+replicate('Ä',35)+'¿'
@PROW()+1,0 SAY ' Cuentas      Debe         Haber'
@prow(),43 say '³Estadistica Diaria Objeto del Gasto³'
@prow()+1,0 say '----------------------------------'
@prow(),43 say 'À'+replicate('Ä',35)+'Ù'
@prow()+1,0 say  LEFT(HOJA.CTADEB,2)
@prow(),5 say HOJA.VALDEB
@prow(),43 say ' Partidad Analit              Parcial'
@prow()+1,6 say LEFT(HOJA.CTAHAB,2)
@prow(),20 say HOJA.VALHAB
@prow(),43 say '-------------------------------------'
@prow()+1,0 say 'Ú'+replicate('Ä',35)+'¿'
@prow()+1,0 say '³          OBSERVACIONES            ³'
@prow()+1,0 say 'À'+replicate('Ä',35)+'Ù'

SET DEVICE TO SCREEN
