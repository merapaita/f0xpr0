PROCEDURE Titulo
*---------------
pagina = pagina  + 1
vTitulo = ' ORDEN DE COMPRA '
@ 0,1   SAY CHR(18)+CHR(14)
@ 0,15 SAY VTITULO+CHR(18)

@ 1,2   SAY cia
@ 1,70  SAY 'P g.'
@ 1,74  SAY pagina   PICTURE '##,###'

@ 2,2   SAY SUBC
@ 2,72  SAY DATE()
@ 3,04  SAY '                  ÚÄÄÄÄÄÄÄÄÄ¿'
@ 4,04  SAY 'ORDEN DE COMPRA : ³ '+Orden.Numoc+'.'+Orden.Nummes+' ³'
@ 4,62  SAY 'Estado : '+IIF(Orden.Estado='00','Emitido  ',IIF(Orden.Estado='20','Afectado ',IIF(Orden.Estado='99','Anulado',IIF(Orden.Estado='50','Liquidado',' Atendido'))))

@ 5,04  SAY '                  ÀÄÄÄÄÄÄÄÄÄÙ'
@ 5,59  say 'Fecha O/C : '+DTOC(Orden.fecoC)

@ 6,66  say IIF(!EMPTY(Orden.NUMHC),'H/C :','  ')+IIF(!EMPTY(Orden.NUMHC),Orden.PERHC+"."+Orden.NUMHC,' ')

@ 7,02  SAY ' SE¥OR(ES) : '+CHR(27)+'G'+val_prv(Orden.Codprv)+CHR(27)+'H'
@ 8,02  SAY ' DIRECCION : '+Val_Fun('ProMae','CodPrv','Dirpro',Orden.Codprv)
@ 9,02  SAY ' -Le agradecemos enviar a nuestro Almac‚n : Av Chirichigno S/N Urb. San Eduardo'
@ 10,1 say chr(15)
@ 10,06 say IIF(SUBST(Orden.OBSERVA,1,4)='REF:','-'+CHR(15)+Orden.OBSERVA+CHR(18),' ')+CHR(18)
@ 11,02 say ' -Facturar a nombre de : REGION GRAU - R.U.C. N§ 15447745'+CHR(15)
@ 12,02 SAY IIF(ORDEN.TIPO='M',"REFERENCIA : "+MEMORAN,CHR(18)+VALCCC()+CHR(15))
@ 13,5 SAY 'ÚÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 14,5 SAY '³Pecosa ³  C¢digo   ³Unidad³                             Art¡culo                                    ³Cantidad³       PreUni³       Total  ³'
@ 15,5 SAY 'ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
linea = 16
RETURN

PROCEDURE LisOC3
*--------------
PARAMETER _desti
IF _desti=2
   SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINT
STORE 0 TO pagina, linea
Impri    = .F.
xColumna = SPACE(7)
GO TOP
vSuma = 0
SCAN
  IF pagina = 0 .OR. linea > 60
     DO Titulo
  ENDIF

  DO CASE
     CASE linea < 34
          @ LINEA,1   SAY CHR(15)
          @ LINEA,08  SAY iteoc.numpec
          @ LINEA,15  SAY iteoc.codart
          @ LINEA,27  SAY ITEoC.codUNI
          @ LINEA,35  SAY ALLTRIM(ITEoC.DEScri) + IIF(EMPTY(MLINE(Iteoc.observa,1)),' ',' - '+ALLTRIM(MLINE(Iteoc.observa,1)))
          @ LINEA,105 SAY ITEoc.CANreq PICTURE '99,999.999'
          @ LINEA,120 SAY ITEoc.PREUNI PICTURE '99,999.999'
          @ LINEA,133 SAY ITEOC.valtot PICTURE '999,999.99'
          vSuma = vSuma + ITEOC.valtot
          linea = linea + 1
      CASE linea >= 34
          @ linea,1 say Chr(18)
          @ linea,05 say repli('-',80)
          linea = linea + 1
		  @ linea,50 say 'S U B T O T A L   S/.'
		  @ Linea,71 say vSuma PICTURE '9,999,999.99'
          linea = linea + 2
          SELECT 2
          CC = RECNO()
          SELECT 1
          DO SUMARIO
          DO TITULO
          SELECT 2
          GO CC - 1
          SELECT 1
          @ linea,01 say chr(18)
          @ linea,52 say 'V I E N E N  S/.'
          @ linea,71 say vSuma PICTURE '9,999,999.99'
          linea = linea + 1
        OTHER    
  ENDCASE
  IF !EMPTY(ITEOC.OBSERVA)
  for xx = 2 to MEMLINES(ITEOC.OBSERVA)   
      @ linea,37 say MLINE(Iteoc.observa,xx)                       
      linea = linea + 1      
      if linea >= 38
          @ linea,1 say chr(18)
          @ linea,05 say repli('-',80)
          linea = linea + 1
		  @ linea,56 say '    V A N   S/.'
		  @ Linea,71 say vSuma PICTURE '9,999,999.99'
          linea = linea + 2
          SELECT 2
          CC = RECNO()
          SELECT 1
          DO SUMARIO
          DO TITULO
          SELECT 2
          GO CC 
          SELECT 1
          @ linea,01 say chr(18)
          @ linea,52 say 'V I E N E N  S/.'
          @ linea,71 say vSuma PICTURE '9,999,999.99'
          @ LINEA,79  SAY CHR(15)
          linea = linea + 1
       endif
  endfoR
  ENDI    
ENDSCAN

@ linea,1 say chr(18)
@ linea,05 say repli('Í',80)
linea = linea + 1
@ linea,56 say 'T O T A L   S/.'
@ Linea,71 say vSuma PICTURE '9,999,999.99'
linea = linea + 1
@ Linea,71 say 'ÍÍÍÍÍÍÍÍÍÍÍÍ'
linea = linea + 1
GO TOP
do coDific
do sumario
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN

procedure codific
*----------------
VALIAS = ALIAS()
VORDER = ORDER()
VRECNO = RECNO()
@ linea,04 say '         SON :'+ LETRAS1(ORDEN.VALTOT,'NUEVOS SOLES')
linea = linea + 2
@ linea,1 say chr(15)
IF !EMPTY(Orden.DESTINO) 
	@ linea,04 say '     Destino : ' + ALLTRIM(Orden.DESTINO)
	linea = linea + 1
ENDIF
@ linea,04 say '  Calendario : '+ NUMMES+'  '+val_para(orden.nummes,'FECMES','D',26,50)
linea = linea + 1
@ linea,04 say '  Cadena Fun.: '+ codcad + '   ' + ALLTRIM(val_codcad(m.Codcad,m.periodo,'D',26,60))
linea = linea + 1
@ linea,04 say 'Fte.Financia.: '+ codfte + '     ' + ALLTRIM(val_para(m.CodFte,'CODFTE','D',26,60))
linea = linea + 1
@ linea,04 say '     Funci¢n : '+ ALLTRIM(maepre.CODFUN) + '     ' + ALLTRIM(val_para(maepre.codfun,'CODFUN','D',26,60))
linea = linea + 1
@ linea,04 say '    Programa : '+ ALLTRIM(maepre.CODPRG) + '    ' +ALLTRIM(VAL_PARA1(maepre.CodPrg,'CODPRG'+maepre.codfun,'D',26,60))
linea = linea + 1
@ linea,04 say ' SubPrograma : '+ ALLTRIM(maepre.CODSPR) + '   ' +ALLTRIM(VAL_PARA1(maepre.CodSPr,'CODSPR'+maepre.codprg,'D',26,60))
linea = linea + 1
@ linea,04 say 'Activ./Proye.: '+ ALLTRIM(maepre.ACTPRY) + ' ' +ALLTRIM(val_para(maepre.actpry,'ACTPRY','D',26,60))
linea = linea + 1

SELEC ITEOC1
SET FILTER TO periodo+numoc = ORDEN.periodo+ORDEN.numoc
go top
SCAN
    @ LINEA,1   SAY CHR(15)
    @ LINEA,04  SAY '   Componente : ' + codcom
    @ LINEA,26  SAY 'Meta : '+ codmet
*   @ LINEA,40  SAY 'Partida : '+ codpart
*   @ LINEA,56  SAY val_para(right(codpart,2),'ESPGAS','D')
    @ LINEA,105 SAY valpart PICTURE '9,999,999.99'
    linea = linea + 1
	VRECNO1 = RECNO()
    DO CASE
      CASE linea >= 38
          SELECT 1
          DO SUMARIO
          DO TITULO
          @ linea,52 say 'V I E N E N '
          linea = linea + 2
    	  SELEC ITEOC1
    	  GO VRECNO1
  	ENDCASE  
ENDSCAN
SET FILTER TO
SELECT (VALIAS)
SET ORDER TO (VORDER)
GO VRECNO
return

PROCEDURE sumario
*----------------
@ 46,1   SAY CHR(18)

@ 47,07 SAY 'ÚÄ¿                                       ÚÄ¿'
@ 48,07 SAY '³1³                                       ³3³'
@ 49,07 SAY 'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-        ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-'
@ 50,07 say '     Director de Abastecimiento                   Jefe de Almacen '

@ 53,07 SAY 'ÚÄ¿                                                RECIBI CONFORME:  '
@ 54,07 SAY '³2³                                              ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ '
@ 55,07 SAY 'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-               ³ Fecha:   /  /   ³ '
@ 56,07 say '       Jefe de Adquisiciones                     ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ '

@ 57,56 SAY chr(15)+'Elaborado por: '+Vusua(user)
@ 59, 1 say chr(15)+'Nota: Esta Orden es Nula sin la firma mancomunada del Director de Abastecimientos y el Jefe de Adquisiciones .Nos reservamos el derecho de'
@ 60, 1 say chr(15)+'      devolver la mercader¡a que no est‚ de acuerdo con nuestras especificaciones.'+chr(18)+chr(12)
linea = 1
RETURN
