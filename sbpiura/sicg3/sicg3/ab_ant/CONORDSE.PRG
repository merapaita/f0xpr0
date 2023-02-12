** ---------------------------------------------------
** CONORDC1.PRG
** Reporte Conciliaci¢n
** UDEP
** Autor : Ing. Luis Castillo Dur n
** ---------------------------------------------------
parameter vTipdoc
PUBLIC vperiodo,vnummes,vcodfte

* Abrir Archivos de trabajo
USE Parmae  IN 1 ORDER TAG Parmae1    ALIAS parma
USE OrdSer  IN 2 ORDER TAG OrdSer1    ALIAS OrdSer
USE OrdCom  IN 3 ORDER TAG OrdCom1    ALIAS Orden
USE IteOc   IN 4 ORDER TAG IteOc1     ALIAS IteOc
USE HojCon  IN 5 ORDER TAG Hojcon1    ALIAS Hoja
USE IteHc   IN 6 ORDER TAG Itehc2     ALIAS Itehc
USE Promae  IN 7 order tag Promae1    ALIAS Promae
USE Compag  IN 8 order tag Compag4    ALIAS Compag
USE Pteanu  IN 9 order tag Pteanu7    ALIAS Pteanu

DO Inicia
DO salida

RETURN


PROCEDURE Inicia
*--------------
vPeriodo = RIGHT(DTOC(DATE()),2)
vNumMes  = space(2)
vCodFte  = space(2)
vTipdoc   = '   '


 DEFINE WINDOW Lis FROM 9,18 TO 16,62 DOUBLE ;
 TITLE ' °° Listado Conciliaciones °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,2 say '    Periodo : '
 @  2,2 say '        Mes : '
 @  3,2 say '     Fuente : '
 @  4,2 say '  Documento : '
 
 @  1,17  GET vPeriodo   PICTURE '!!'   VALID !EMPTY(vPeriodo)
 @  2,17  GET vNumMes    PICTURE '!!'   VALID Val_Para(vNumMes  ,'FECMES',' ',17,10) AND !EMPTY(vNumMes)
 @  3,17  GET vCodFte    PICTURE '!!!'  VALID Val_Para(vCodFte  ,'CODFTE',' ',17,16) AND !EMPTY(vCodFte)
 @  4,17  GET vTipdoc    PICTURE '!!!'  VALID Val_Para(vTipDoc  ,'TIPDOC',' ',17,16) AND !EMPTY(vTipDoc)
 
 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF LASTKEY()=27
    RETURN
 ELSE
    vInd1 = SYS(3) + '.IDX'
    vInd2 = SYS(3) + '.IDX'
    IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
	   ACTIVATE WINDOW STANDBY
   	   @ 1,14 SAY 'Espere un Momento ....' color W*
       select hoja
       vInd = SYS(3) + '.IDX'
       index on periodo+nummes+Numhc to (vInd) for tipdoc=alltrim(vtipdoc) AND Periodo<=alltrim(vPeriodo) and Nummes <= alltrim(vNummes) and codfte=alltrim(vcodfte) and estado#'99' and numref#'B' and numref#'A' 
       set index to (vInd)
       
       do case 
          case ALLTRIM(vTipDoc)='O/C'
            select orden
     		set relation to periodo+Perhc+Numhc INTO HOJA
     		set relation to codprv              INTO promae  ADDI
            index on periodo+PerHc+Numhc  TO (vInd1) for Estado>= '20' and estado#'99' and periodo <= vPeriodo AND PerHc <= alltrim(vNummes) and (month(fecdesp)>=val(vNumMes) OR EMPTY(fecdesp)) and codfte=alltrim(vcodfte) and !EMPTY(hoja.NUMHC)
            set index to (vInd1)                                                            
          case ALLTRIM(vTipDoc)='O/S'       
       	    select ordser
			set relation to periodo+Perhc+Numhc INTO HOJA
     		set relation to codprv              INTO promae  ADDI
            index on periodo+PerHc+Numhc  TO (vInd2) for Estado>= '20' and estado#'99'  and periodo <= vPeriodo AND PerHc <= alltrim(vNummes) and (month(fecliq)>=val(vNumMes) OR EMPTY(FECLIQ)) and codfte=alltrim(vcodfte) and !EMPTY(hoja.NUMHC)
            set index to (vInd2)                                                        && and (month(fecliq)=val(vNumMes) OR EMPTY(FECLIQ))
          other
            return
       endcase   
       GO TOP
	   DEACTIVATE WINDOW STANDBY       
       do case 
          case ALLTRIM(vTipDoc)='O/C'
               DO REPORTE WITH 2,"conordC1",' Listado Conciliaciones '
          case ALLTRIM(vTipDoc)='O/S'
               DO REPORTE WITH 2,"conordS1",' Listado Conciliaciones '
       endcase                  
       CLOSE INDEX
       SELE HOJA
       CLOSE DATA
	   erase (vInd)       
	   erase (vInd1)       
	   erase (vInd2)       	   	   
    ENDIF
 ENDIF
RETURN

PROCEDURE busca_hc
*-----------------
private ad
ad=alias()
select compag
SET FILT TO ESTADO#'99'
seek hoja.numMes+hoja.numhc
IF FOUND() 
   vtotalcp = compag.import
ELSE
   IF ORDEN.ESTADO='50' AND !EMPTY(ORDEN.NUMREB)
  	   vtotalcp = iif(vtipdoc='O/C',orden.valtot-orden.anultot,ordser.valtot-ordser.anultot)
   ELSE
       vtotalcp = iif(vtipdoc='O/C',orden.valtot,ordser.valtot)
   ENDIF	   
ENDIF   
SET FILT TO
select (ad)
return vtotalcp


PROCEDURE Salida
*---------------
RELEASE WINDOW Lis
ACTIVATE SCREEN
CLOSE DATABASE
RETURN


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)

PROCEDURE busca_cp
*-----------------
private ad
ad=alias()
vnumcp = '       '
select compag
SET FILT TO ESTADO#'99'
seek hoja.numMes+hoja.numhc
IF FOUND() 
   vnumcp = compag.numcp+'.'+compag.nummes
ELSE
   vnumcp = hoja.numcp+'.'+hoja.nummescp
ENDIF   
SET FILT TO
select (ad)
return vnumcp


PROCEDURE  V_SOLSER
*------------------
PRIVATE ALI,VKEY
ALI = ALIAS()
VKEY = ORDSER.PERIODO+ORDSER.NUMSS
SELECT SOLSER
SEEK VKEY
IF FOUN()
   VRET = VAL_PARA(SOLSER.TIPSER,'TIPSER','D',22,60)
ELSE
   VRET = 'SIN S/S'
ENDIF   
SELE (ALI)
RETURN VRET


PROCEDURE VALANRB
*----------------
PRIVATE A1,A2
A2= ALIAS()
A1= 0
DO CASE
   CASE !EMPTY(ORDEN.NUMREB)
*        USE Pteanu  IN 9 order tag Pteanu3    ALIAS Pteanu
        SELECT PTEANU
        SEEK ORDEN.NUMREB
        IF FOUND()
           *IF PTEANU.MESPA=ALLTRIM(VNUMMES)
           IF PTEANU.NUMMESTR=ALLTRIM(VNUMMES)
              A1=ORDEN.ANULTOT   
           ENDIF
        ENDIF
   CASE !EMPTY(ORDEN.NUMANU)
*        USE Pteanu  IN 9 order tag Pteanu1    ALIAS Pteanu
        SELECT PTEANU
        *SEEK ORDEN.NUMANU
        SEEK ORDEN.PERHC+ORDEN.NUMHC
        IF FOUND()
           IF PTEANU.MESPA=ALLTRIM(VNUMMES)
           *IF PTEANU.NUMMESTR=ALLTRIM(VNUMMES)
              A1=ORDEN.ANULTOT   
           ENDIF
        ENDIF
   OTHER

ENDCASE  
*USE IN 9    
SELECT (A2)
RETURN A1


PROCEDURE VALANRBOS
*------------------
PARAMETER TIPO
PRIVATE A1,A2
A2= ALIAS()
A1= 0
DO CASE
   CASE TIPO='R' AND !EMPTY(ORDSER.NUMREB)
*        USE Pteanu  IN 9 order tag Pteanu4    ALIAS Pteanu
        SELECT PTEANU
        SEEK ORDSER.NUMREB
        IF FOUND()
           *IF PTEANU.MESPA=ALLTRIM(VNUMMES)
           IF PTEANU.NUMMESTR=ALLTRIM(VNUMMES)
              A1=ORDSER.ANULTOT   
           ENDIF
        ENDIF
   CASE !EMPTY(ORDSER.NUMANU)
*        USE Pteanu  IN 9 order tag Pteanu2    ALIAS Pteanu
        SELECT PTEANU
        SEEK ORDSER.NUMANU
        IF FOUND()
        BROW
           *IF PTEANU.MESPA=ALLTRIM(VNUMMES)
           IF PTEANU.NUMMESTR=ALLTRIM(VNUMMES)
              A1=ORDSER.ANULTOT   
           ENDIF
        ENDIF
   OTHER

ENDCASE  
USE IN 9    
SELECT (A2)
RETURN A1


PROCEDURE BUSC_CD
*----------------
PRIVATE AS,VKEY
AS=ALIAS()
IF ALLTRIM(Vcodfte)<>'  '
   VKEY = LEFT(ORDSER.CODPART,2)+RIGHT(ORDSER.CODPART,2)
ELSE
   cancel
ENDIF   
SELECT CLASE
SEEK VKEY
IF FOUND()
   VPAR = CLASE.CUENTA
ELSE
   VPAR = '    *    '
ENDIF
SELE (AS)
RETURN VPAR
