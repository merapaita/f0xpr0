** ---------------------------------------------------
** CONORD.PRG
** Reporte Conciliaci¢n
** UDEP
** Autor : Ing. Luis Castillo Dur n
** ---------------------------------------------------
parameter vTipdoc
* Abrir Archivos de trabajo
PUBLIC vperiodo,vnummes,vcodfte
USE Parmae  IN 1 ORDER TAG Parmae1    ALIAS parma
USE OrdSer  IN 2 ORDER TAG OrdSer1    ALIAS OrdSer
USE OrdCom  IN 3 ORDER TAG OrdCom1    ALIAS Orden
USE IteOc   IN 4 ORDER TAG IteOc1     ALIAS IteOc
USE HojCon  IN 5 ORDER TAG Hojcon1    ALIAS Hoja
USE IteHc   IN 6 ORDER TAG Itehc2     ALIAS Itehc
USE Promae  IN 7 order tag Promae1    ALIAS Promae
USE Compag  IN 8 order tag Compag4    ALIAS Compag
do case 
   case ALLTRIM(vTipDoc)='O/C'
        USE Pteanu  IN 9 order tag Pteanu1    ALIAS Pteanu
   case ALLTRIM(vTipDoc)='O/S'     
        USE Pteanu  IN 9 order tag Pteanu2    ALIAS Pteanu
endcase        
DO Inicia
DO salida

RETURN


PROCEDURE Inicia
*--------------

vPeriodo  = '98'
vNumMes   = '  '
vCodFte   = '  '
STORE date() to vFecFin
STORE CTOD('01-01-98') TO vFecini

 DEFINE WINDOW Lis FROM 9,18 TO 16,62 DOUBLE ;
 TITLE ' °° Listado Conciliaciones °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,2 say '    Periodo : '
 
 @  2,2 say '     Fechas : '
 
 @  3,2 say '     Fuente : '
 @  4,2 say '  Documento : '
 
 @  1,17  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)

*@  2,17  GET vNumMes    PICTURE '!!'  VALID Val_Para(vNumMes  ,'FECMES',' ',17,10) AND !EMPTY(vNumMes)

 @  2,17  GET vFecIni    PICTURE '@D'   
 @  2,27  GET vFecFin    PICTURE '@D'   VALID vFecFin>=vFecIni
 
 @  3,17  GET vCodFte    PICTURE '!!'   VALID Val_Para(vCodFte  ,'CODFTE',' ',17,16) AND !EMPTY(vCodFte)
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
       select hoja
       vInd = SYS(3) + '.IDX'
       index on periodo+nummes+Numhc to (vInd) for tipdoc=alltrim(vtipdoc) AND Periodo<=alltrim(vPeriodo) and Nummes <= alltrim(vNummes) and codfte=alltrim(vcodfte) and estado#'99' and numref#'B' and numref#'A'
       set index to (vInd)
       
       select pteanu
       vInd4 = SYS(3) + '.IDX'
       index on periodo+perhc+Numhc to (vInd4) for iif(ALLTRIM(vTipDoc)='O/C',tipdoc='O/C',tipdoc='O/S')  and month(pteanu.fecpa) >= val(vNummes) 
       set index to (vInd4)
       
       do case 
          case ALLTRIM(vTipDoc)='O/C'
            select orden
     		set relation to periodo+Perhc+Numhc INTO HOJA
      		set relation to periodo+Perhc+Numhc into pteanu addi   &&and fecdesp <= vFecfin
            index on periodo+PerHc+Numhc TO (vInd1) for Estado >= '20' and estado#'99' and ( fecdesp >= vFecIni OR empty(FecDesp) ) and periodo = vPeriodo and PerHc <= alltrim(vNummes)  and codfte=alltrim(vcodfte) and !EMPTY(hoja.NUMHC) 
            SET FILT TO iif(!EMPTY(PTEANU.NUMPA),ordEN.estado='21' AND vfecini >= pteanu.fecpa and vfecfin <= pteanu.fecpa AND PTEANU.PERIODO=VPERIODO,.t.)
            set index to (vInd1)                                                            && (month(fecdesp)=val(vNumMes) OR EMPTY(FECDESP)) and
          case ALLTRIM(vTipDoc)='O/S'       
       	    select ordser
			set relation to periodo+Perhc+Numhc INTO HOJA 
      		set relation to periodo+Perhc+Numhc into pteanu addi
            index on periodo+Perhc+Numhc  TO (vInd2) for Estado>= '20' and estado#'99' and (fecliq >= vFecIni OR EMPTY(FECLIQ)) AND periodo = vPeriodo AND PerHc <= alltrim(vNummes) and codfte=alltrim(vcodfte) and !EMPTY(hoja.NUMHC) 
            SET FILT TO iif(!EMPTY(PTEANU.NUMPA),ordser.estado='21' AND vfecini >= pteanu.fecpa and vfecfin <= pteanu.fecpa AND PTEANU.PERIODO=VPERIODO,.t.)
            set index to (vInd2)                                                        && and (month(fecliq)=val(vNumMes) OR EMPTY(FECLIQ))
          other
            return
       endcase   
       *SET FILT TO iif(ordser.estado='21' ,(vfecini >= pteanu.fecpa and vfecfin <= pteanu.fecpa),!EMPTY(PTEANU.NUMPA),.t.)
	   *SET FILTER TO iif(ordser.estado='21' AND (vfecini>=pteanu.fecpa  and vfecfin<=pteanu.fecpa AND YEAR(VFECFIN)=YEAR(PTEANU.FECPA) ) ,!empty(pteanu.numpa),.t.)
       GO TOP
       DO REPORTE WITH 2,"conordX",' Listado Conciliaciones '
       CLOSE INDEX
       SELE HOJA
       CLOSE DATA
	   erase (vInd)       
	   erase (vInd1)       
	   erase (vInd2)       	   	   
	   erase (vInd4)       	   	   	   
    ENDIF
 ENDIF
RETURN


PROCEDURE busca_hc
*-----------------
private ad
ad=alias()
IF HOJA.NUMMES < '02'
   vtotalcp = VALTOT
ELSE   
   	select compag
	SET FILT TO ESTADO#'99' &&AND !'M'$(COMPAG.NUMCP)
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
ENDIF	
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
SET FILTER TO ESTADO#'99' &&AND !'M'$(COMPAG.NUMCP)
seek hoja.numMes+hoja.numhc
IF FOUND() 
   vnumcp = compag.numcp+'.'+compag.nummes
ELSE
   vnumcp = hoja.numcp+'.'+hoja.nummescp
ENDIF   
*SET FILT TO
select (ad)
return vnumcp
