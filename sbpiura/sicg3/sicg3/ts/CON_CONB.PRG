* 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
* ? Con_ConB.PRG  22/08/96                             L:  228   ?	
* ? Consulta de Estado de Conciliaci줻 Bancaria                  ?
* ?         : Ing. Federico Montero Valdiviezo REGION GRAU       ?
* 突袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴槁

*-Abriendo archivos
USE Cajas   IN 1 ORDER Cajas1   ALIAS Caja
USE Parmae  IN 2 ORDER Parmae1  ALIAS Parma
STORE SPACE(02) TO vnummes
ACTIVATE SCREEN
DEFINE WINDOW VerSal FROM 1,0 TO 23,79 ;
COLOR SCHEME 10 TITLE "췋2? Busca Cuenta Corriente "
filaux = SYS(3) + '.DBF'
Vind   = SYS(3) + '.IDX'
SELE Caja
SET FILT TO Tipo$'CCRE' AND CLASE#'T' AND ALLT(DOCREF)#'95' 
ON KEY LABEL F2  DO Busca1
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE NOEDIT NOAPPEND FIELDS ;
Codctc :H='Cuenta.' :R,;
Descri :H='Descripci줻' :R :45,;
x1=IIF(EMPTY(mescie) AND EMPTY(percie),'SIN CONCILIAR',mFecha(VAL(mescie),VAL(percie))) :H='Ultima Conciliaci줻' :R :15 ;
WINDOW VerSal
ON KEY LABEL F2  
ON KEY LABEL F4  
ON KEY LABEL F10 
CLOSE DATA  
RETURN


PROCEDURE Busca1
*----------------

  DEFINE WINDOW VerSal FROM 10,20 TO 14,60 ;
  COLOR SCHEME 5 
  ACTIVATE WINDOW versal
  vreg = RECNO()
  vcodctc = SPACE(14)
  @ 1,1 SAY "    Cuenta : "  GET vcodctc
  READ
  SEEK ALLT(vcodctc)
  IF !FOUND()
	GO vreg	
  ENDIF
DEACTIVATE WINDOW versal
RETURN 

FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)