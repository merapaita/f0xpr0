DEACTIVATE WINDOW ALL
SET DATE DMY
SET TALK OFF
SET ECHO OFF
SET SAFETY OFF
SET DELETED ON
SET ESCAPE OFF
SET CLOCK ON
CLOSE DATABASES
SET CURSOR ON
CLOSE INDEX
USE IN 1 Iteart ALIAS iteart  ;
    ORDER CODARTINI
USE IN 2 stock ALIAS stock ORDER  ;
    Stockx
SELECT iteart
GOTO TOP
SET RELATION TO codart INTO stock
SELECT iteart
GOTO TOP
IF  .NOT. EOF()
     DO reporte WITH 2, 'Total',  ;
        ' Lista de todos los Producto ',  ;
        1, .F., .T.
ENDIF
SET FILTER TO
RETURN
*
FUNCTION val_entrad
SET FILTER TO
toma = ALIAS()
canti = 0
SELECT stock
SET FILTER TO stock.codart = iteart.codart;
.AND. stock.tipomov = 'E'
GOTO TOP
DO WHILE  .NOT. EOF()
     canti = canti +  ;
             stock.cantidad
     SKIP
ENDDO
SET FILTER TO
SELECT iteart
RETURN canti
*
FUNCTION val_salida
canti1 = 0
SELECT stock
GOTO TOP
SET FILTER TO stock.codart = iteart.codart;
.AND. stock.tipomov = 'S'
SEEK iteart.codart
DO WHILE  .NOT. EOF()
     canti1 = canti1 +  ;
              stock.cantidad
     SKIP
ENDDO
SET FILTER TO
SELECT iteart
RETURN canti1
*
