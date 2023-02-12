PUBLIC m.anyo, m.mes
m.anyo = STR(YEAR(DATE()), 4)
m.mes = PADL((MONTH(DATE())), 2,'0')
CLOSE DATABASES
USE IN 1 Compras ALIAS compras ORDER Compras01
USE IN 2 MaePrv ALIAS maeprv   ORDER MaePrv2
USE IN 3 Ventas ALIAS ventas   ORDER Ventas01
USE IN 4 Parmae ALIAS parma ORDER Parmae1
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
DEFINE WINDOW wlista FROM 3, 15 TO 20, 70 FLOAT TITLE    'Proceso de Creacion de Ficheros'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 03, 01 SAY  '  A¤o del Proceso :' GET m.anyo
@ 05, 01 SAY  '  Mes del proceso :' GET m.mes
@ 10, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1, 'Procesando Ficheros'
IF LASTKEY() <> 27 .AND. okcancel <> 2
     DO proceso
     DO espera WITH 2
ELSE
     DO standby WITH 'Proceso Cancelado.'
ENDIF
RETURN
*
PROCEDURE proceso
SELECT compras
SET FILTER TO anyo = m.anyo;
.AND. mes = m.mes
SET RELATION TO LEFT(numdocpr, 11) INTO;
maeprv ADDITIVE
SELECT ventas
SET FILTER TO anyo = m.anyo;
.AND. mes = m.mes
SET RELATION TO LEFT(numdoccl, 11) INTO;
maeprv ADDITIVE
marccom = '..\sunat\' + 'compras.xml'
marcven = '..\sunat\' + 'ventas.xml'
jarccom  = 'LE' + mruc + m.anyo + m.mes + '00080100001111.txt'
jarccom1 = 'LE' + mruc + m.anyo + m.mes + '00080200001011.txt'
jarcven  = 'LE' + mruc + m.anyo + m.mes + '00140100001111.txt'
farccom = FCREATE(marccom)
IF farccom < 0
    DO standby WITH 'No puedo abrir o crear el archivo de Copras'
ELSE
    linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
    = FPUTS(farccom, linea)
    linea = '<comven>'
    = FPUTS(farccom, linea)
ENDIF
farcven = FCREATE(marcven)
IF farcven < 0
    DO standby WITH 'No puedo abrir o crear el archivo de Copras'
ELSE
    linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
    = FPUTS(farcven, linea)
    linea = '<comven>'
    = FPUTS(farcven, linea)
ENDIF
lprocesa = .F.
IF (farccom < 0) .AND. (farcven < 0)
    DO standby WITH 'Error al crear archivos '
ELSE
    lprocesa = .T.
ENDIF
IF lprocesa
    SELECT compras
    SCAN
        linea = '<compras>'
        = FPUTS(farccom, linea)
        mcadena = ''
        mcadena = periodo + '|'						&&1
        mcadena = mcadena + ALLTRIM(correl) + '|'   &&2
        mcadena = mcadena + 'M' + mes + '|'   		&&3
        mcadena = mcadena + DTOC(feccom) + '|'      &&4
        mcadena = mcadena + IIF( .NOT. EMPTY(feccan), DTOC(feccan), '') + '|'   &&5
        mcadena = mcadena + ALLTRIM(tipcom) + '|'   &&6
        mcadena = mcadena + ALLTRIM(serie) + '|'    &&7
        mcadena = mcadena + '0|'  					&&8
        mcadena = mcadena + ALLTRIM(numcom) + '|'   &&9
        mcadena = mcadena + IIF( .NOT. EMPTY(ALLTRIM(numcomf)),ALLTRIM(numcomf),'') + '|'  &&10
        mcadena = mcadena + ALLTRIM(tipdocpr) + '|'   &&11
        mcadena = mcadena + ALLTRIM(numdocpr) + '|'   &&12
        mcadena = mcadena + ALLTRIM(maeprv.nomprv) + '|'  &&13
        DO CASE
            CASE tipadq = '1'
                mcadena = mcadena + IIF(desope = '1', ALLTRIM(STR(basimp, 10, 2)), '0.00') + '|'     &&14
                mcadena = mcadena + IIF(desope = '1', ALLTRIM(STR(igv, 10, 2)), '0.00') + '|'        &&15
                mcadena = mcadena + IIF(desope = '2', ALLTRIM(STR(basimp, 10, 2)), '0.00') + '|'     &&16
                mcadena = mcadena + IIF(desope = '2', ALLTRIM(STR(igv, 10, 2)), '0.00') + '|'        &&17
                mcadena = mcadena + IIF(desope = '3', ALLTRIM(STR(basimp, 10, 2)), '0.00') + '|'     &&18
                mcadena = mcadena + IIF(desope = '3', ALLTRIM(STR(igv, 10, 2)), '0.00') + '|'        &&19
                mcadena = mcadena + IIF(desope = '4', ALLTRIM(STR(basimp, 10, 2)), '0.00') + '|'     &&20
            CASE tipadq = '2'
                mcadena = mcadena + '0.00|0.00|0.00|0.00|0.00|0.00|'
                mcadena = mcadena + ALLTRIM(STR(basimp,10,2)) + '|'
            CASE tipadq = '3'
                mcadena = mcadena + '0.00|0.00|0.00|0.00|0.00|0.00|'
                mcadena = mcadena + ALLTRIM(STR(basimp, 10, 2)) + '|'
            CASE tipadq = '4'
                mcadena = mcadena + '0.00|0.00|0.00|0.00|0.00|0.00|'
                mcadena = mcadena + ALLTRIM(STR(basimp,10,2)) +'|'
        ENDCASE
        mcadena = mcadena + '0.00|'             &&21
        mcadena = mcadena + '0.00|'             &&22
        mcadena = mcadena + ALLTRIM(STR(monto, 10, 2)) + '|'      &&23
        mcadena = mcadena + 'PEN|'           &&24
        mcadena = mcadena + '1.000' + '|'    &&25
        
        mcadena = mcadena + IIF(tipcom = '07', DTOC(fecdocrf), '01/01/0001') + '|'     &&26
*       mcadena = mcadena + IIF(im = 'M', DTOC(feccom), '01/01/0001') + '|'     &&26
        mcadena = mcadena + IIF(tipcom = '07', tipdocrf, '00') + '|'                   &&27
*       mcadena = mcadena + IIF(im = 'M', tipcom, '00') + '|'                   &&27
        mcadena = mcadena + IIF(tipcom = '07', serdocrf, '-') + '|'                     &&28
*       mcadena = mcadena + IIF(im = 'M', serie, '-') + '|'                     &&28
        mcadena = mcadena + '|'                                                 &&29
        mcadena = mcadena + IIF(tipcom = '07', numdocrf, '-') + '|'                    &&30
*       mcadena = mcadena + IIF(im = 'M', numcom, '-') + '|'                    &&30

*        mcadena = mcadena + '-|'
        mcadena = mcadena + '01/01/0001' + '|'                    &&31
        mcadena = mcadena + '0' + '|'                             &&32
        mcadena = mcadena + ''  + '|'                             &&33
        mcadena = mcadena + '1' + '|'                             &&34      cLASIFICACION DE BIEN ADQUIRIDO
        mcadena = mcadena + '|'                             &&35      CONTRATOS Y PROYECTOS CON SOCIEDADES IRREGULARES
        mcadena = mcadena + '|'                             &&36      INCONSISTENCIA EN TIPO DE CAMBIO
        mcadena = mcadena + '|'                             &&37      INCONSISTENCIA CON PROVEEDORES NO HABIDOS
        mcadena = mcadena + '|'                             &&38      INCONSISTENCIA PROVEEDORES QUE RENUNCIARON AL igv
        mcadena = mcadena + '|'                             &&39      INCONSISTENCIA EN DNI QUE FUERON UTILIZADOS EN LIQ. DE COMPRA
        mcadena = mcadena + '1|'                            &&40      INDICADOR DE COMPROBANTES DE PAGO
        mcadena = mcadena + estado + '|'					&&41
        linea = '<campo>' + mcadena + '</campo>'
        = FPUTS(farccom, linea)
        linea = '</compras>'
        = FPUTS(farccom, linea)
    ENDSCAN
    linea = '</comven>'
    = FPUTS(farccom, linea)
    = FCLOSE(farccom)
    SELECT ventas
    SCAN
        linea = '<ventas>'
        = FPUTS(farcven, linea)
        mcadena = ''
        mcadena = periodo + '|'       &&1
        mcadena = mcadena + ALLTRIM(correl) + '|'    &&2
        mcadena = mcadena + 'M' + mes + '|'          &&3
        mcadena = mcadena + DTOC(fecven) + '|'       &&4
        mcadena = mcadena + IIF( .NOT. EMPTY(feccan), DTOC(feccan), '') + '|'  &&5
        mcadena = mcadena + ALLTRIM(tipcom) + '|'    &&6
        mcadena = mcadena + ALLTRIM(serie) + '|'     &&7
        mcadena = mcadena + ALLTRIM(numcom) + '|'    &&8
        mcadena = mcadena + IIF( .NOT. EMPTY(ALLTRIM(numcomf)), ALLTRIM(numcomf), '') + '|'  &&9
        mcadena = mcadena + ALLTRIM(tipdoccl) + '|'  &&10
        mcadena = mcadena + IIF(tipdoccl <> '0', ALLTRIM(numdoccl), '0') + '|'      &&11
        mcadena = mcadena + IIF(tipcom = '01' OR (tipcom = '03' and tipdoccl='1'), ALLTRIM(maeprv.nomprv), '-') + '|'   &&12
        mcadena = mcadena + '0.00|'                  &&13
        mcadena = mcadena + IIF(tipven = '1', ALLTRIM(STR(basimp, 10, 2)), '0.00') + '|'  &&14
        mcadena = mcadena + '0.00|'                  &&15
        mcadena = mcadena + ALLTRIM(STR(igv, 10, 2)) + '|'   &&16
        mcadena = mcadena + '0.00|'                  &&17
        mcadena = mcadena + IIF(tipven = '2', ALLTRIM(STR(basimp, 10, 2)), '0.00') + '|'  &&18
        mcadena = mcadena + IIF(tipven = '3', ALLTRIM(STR(basimp, 10, 2)), '0.00') + '|'  &&19
        mcadena = mcadena + '0.00' + '|'             &&20
        mcadena = mcadena + '0.00' + '|'             &&21
        mcadena = mcadena + '0.00' + '|'             &&22
        mcadena = mcadena + '0.00' + '|'             &&23
        mcadena = mcadena + ALLTRIM(STR(monto, 10, 2)) + '|'  &&24
        mcadena = mcadena + 'PEN' + '|'             &&25
        mcadena = mcadena + '1.000' + '|'           &&26
        mcadena = mcadena + IIF(tipcom = '07', DTOC(fecdocrf), '01/01/0001') + '|'    &&27
        mcadena = mcadena + IIF(tipcom = '07', tipdocrf, '00') + '|'         &&28
        mcadena = mcadena + IIF(tipcom = '07', serdocrf, '-') + '|'          &&29
        mcadena = mcadena + IIF(tipcom = '07', numdocrf, '-') + '|'          &&30
        mcadena = mcadena + '|'                &&31
        mcadena = mcadena + '|'                &&32   INCONSISTENCIA EN TIPO CAMBIO
        mcadena = mcadena + '1|'                &&33   Indicador de Comprobante de Pago
*        mcadena = mcadena + '0' + '|'
        mcadena = mcadena + estado + '|'
        linea = '<campo>' + mcadena + '</campo>'
        = FPUTS(farcven, linea)
        linea = '</ventas>'
        = FPUTS(farcven, linea)
    ENDSCAN
    linea = '</comven>'
    = FPUTS(farcven, linea)
    = FCLOSE(farcven)
ENDIF
cejecuta = ' creatxt.jar ' + m.anyo + ' '+ m.mes
RUN FOXSWAP java -jar &cEjecuta
RETURN
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
