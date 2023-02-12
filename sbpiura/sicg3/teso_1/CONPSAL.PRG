*--------------------------------------------------------------------------
* conpsal.Prg
* consulta las salidas que emiten en cada dependencia
* Estado :
*   '00' Emitida     ** 	Este es el que se registra en el 
*   '20' Con Correlativo
*   '50' Con O/C
*   '70' Devuelta
*   '99' Anulada
* Autor: LCD
*--------------------------------------------------------------------------
*- Abriendo Archivos

USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE Pecosa   IN 5   order tag Pecosa1  ALIAS Pecosa
USE Itepec   IN 6   order tag ItePec1  ALIAS Itepec
USE Promae   IN 8   order tag Promae1  ALIAS Promae
USE calen    IN 9   order tag calen1   ALIAS calen
USE Iteart   IN 10  order tag iteart1  ALIAS iteart
USE Poliza   IN 11  order tag poliza2  ALIAS poliza
USE Itepol   IN 12  order tag itepol2  ALIAS itepol
USE Asiaut   IN 13  order tag asiaut1  ALIAS asig

*- Mensajes de aviso al usuario
Vmens01 = ' P¢lizas de Salida : REVISION '
Vmens02 = 'Registro de P¢liza de Salida'
Vmens04 = 'Dicho P¢liza no fue encontrado'
Vmens05 = 'No existe P¢liza anterior'
Vmens06 = 'No existe P¢liza siguiente'
Vmens07 = '¨ Desea Anular ‚sta P¢liza ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta P¢liza ha sido anulado'
Vmens10 = 'La P¢liza ya est  Atendido'
Vmens11 = 'La P¢liza ha sido devuelto'
Vmens12 = 'La P¢liza ya tiene O/C'

SELECT Poliza
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente                             Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 09,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 10,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: P¢liza    ®F9¯ Detalle : Item  ®F7¯ Verifica Precio' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,65 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_1
 CLEAr
 
 @  1, 2 SAY "          Periodo :"
 @  2, 2 SAY "          Emisi¢n :"
 @  3, 2 SAY "    N£mero P¢liza :"
 @  4, 2 SAY " F.Financiamiento :"
 @  5, 2 SAY "             Tipo :"
 @  6, 2 SAY "         Vigencia :"
 @  7, 2 SAY "         Analisis :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Poliza
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 ON KEY LABEL F9 DO vista_det
 SCATTER MEMVAR

 @  0,60 SAY vEstpol(m.Estado)
 @  1,22 SAY m.periodo 
 @  2,22 SAY m.FecEmi
 @  3,22 SAY m.NumPol

 @  4,22 SAY val_para(m.Codfte,'CODFTE','V',22,20,4)
 @  5,22 SAY val_para(m.Tipfun,'TIPFUN','V',22,15,4)
 
 @  6,22 SAY m.fecini 
 @  6,31 SAY m.fecfin
 @  7,22 SAY val_para(m.analisis,'ANALIS','V',22,15,3)
 DO VISTA_HIJO
 DO TOTAL
 RETURN

PROCEDURE VISTA_HIJO
*-------------------
hide popup all
SELECT ItePol
GO TOP
do case
   case ALLTRIM(m.analisis) = 'G'
	   BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo+m.NumPol TIMEOUT 0.001 ;
	   WINDOW Wind_2 ;
	   FIELDS;
	   DesPro : h='D' :1,;   
	   TipRef : h='REF' ,;
	   PerRef : h='Pr'  ,;
	   NumRef : h='N§Rf',;
	   fecref : h='FecRef',;
	   Codgen : h='GG' ,;
	   Descri : h='Definici¢n':36 ,;
	   ValTot : h='Total'
   case ALLTRIM(m.analisis) = 'C'   
	   BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo+m.NumPol TIMEOUT 0.001 ;
	   WINDOW Wind_2 ;
	   FIELDS;
	   DesPro : h='P':1,;   
	   TipRef : h='REF' ,;
	   PerRef : h='Pr'  ,;
	   NumRef : h='N§Rf',;
	   Fecref : h='FecRef',;
	   Codgen : h='GG' ,;
	   CODCLA : H='Clf' ,;
	   Descri : h='Definici¢n':33 ,;
	   ValTot : h='Total'
   case ALLTRIM(m.analisis) = 'A'   
	   BROWSE ;
	   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo+m.NumPol TIMEOUT 0.001 ;
	   WINDOW Wind_2 ;
	   FIELDS;
	   DesPro : h='P':1,;   
	   TipRef : h='REF' ,;
	   PerRef : h='Pr'  ,;
	   NumRef : h='N§Rf',;
	   Fecref : h='FecRef',;
	   Codgen : h='GG' ,;
	   CODCLA : H='Clf' ,;
	   coddet : H='Det' ,;	   
	   Descri : h='Definici¢n':29 ,;
	   ValTot : h='Total'
  ENDCASE   

  SELE Poliza
RETURN


PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
@ 0,0 SAY m.Valtot picture '9,999,999.99'
return


PROCEDURE VISTA_DET
*------------------
hide popup all
ON KEY LABEL F9 
SELECT ItePol
GO TOP
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH  KEY m.Periodo+m.NumPol ;
   WINDOW Wind_2 ;
   FIELDS;
   DesPro : h='D' :1,;   
   TipRef : h='REF' ,;
   PerRef : h='Pr'  ,;
   NumRef : h='N§Rf',;
   fecref : h='FecRef',;
   Codgen : h='GG' ,;
   Descri : h='Definici¢n':36 ,;
   ValTot : h='Total'
   SELE Poliza
 
   ON KEY LABEL F9 DO vista_det   
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE Poliza
private vtemp
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF LASTKEY()=27
    sele poliza
    DO VISTA
    RETURN
 ENDIF     
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumPol :H=' N§ ' ,;
 FecEmi :H='Fecha' ,;
 Est = IIF(Estado= '00','Pend',IIF(Estado = '51','Cont','    ')) :H='Estd' ,;
 Codfte :H='Fte ' ,;
 tipfun :H='T',;
 FecIni :H='Del :' ,;
 FecFin :H=' al :' ,;
 Fecemi :H='Emision',;
 Valtot :H='Total' :P='9,999,999.99'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SELE Poliza
*SET RELA TO
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
vtemp    = RECNO()
vPeriodo = RIGHT(DTOC(DATE()),2)
vNum_pol = '0000'
ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero P¢liza: ' GET vPeriodo PICTURE '!!'
 @ 1,27 SAY '-' GET vNum_Pol PICTURE '!!!!' &&LID vBusca()
READ
DEACTIVATE WINDOW standby

 IF EMPTY(vNum_pol) .or. LASTKEY()=27
    RETURN
 ELSE
    SEEK vPeriodo + ALLTRIM(vNum_pol) 
    IF !FOUND()
       DO standby WITH Vmens04
       GOTO Vtemp
    ELSE
       DO Vista
    ENDIF
 ENDIF
RETURN


PROCEDURE Anter
*--------------
SELE Poliza
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF !BOF()
    SKIP -1
 ENDIF
 IF BOF()
    GO TOP
    DO standby WITH Vmens05
 ELSE
    DO Vista
 ENDIF
 RETURN


PROCEDURE Proxi
*--------------
SELE Poliza
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH Vmens06
   GO BOTTOM
 ELSE
   DO Vista
 ENDIF
 RETURN


PROCEDURE vNumpol
*---------------
m.numpol=padl(alltrim(m.numpol),4,'0')
SEEK m.periodo+m.numpol
if !found()
   retur .t.
else
   do standby with 'La poliza ya se encuentra registrada..!'
   return .f.
endif   

PROCEDURE LISTA
*-------------
PRIVATE VTEMP
select POLIZA
vtemp =recno()
SET RELATION TO PERIODO+NUMPOL INTO ITEPOL
SET SKIP TO ITEPOL

if eof()
   do standby with vmens08
   return
else
   DO LISPOL WITH vTemp
endif
select POLIZA
SET RELATION TO
SET FILT TO
GO TOP
DO VISTA
RETURN

PROCEDURE LisPol
*---------------
PARAMETERS vReg
PRIVATE VORDE
vOrde = ORDER()
vrec  = RECNO()
DEFINE WINDOW LIS FROM 2,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Polizas' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vToPol,vToMes,vToFue,vTodep,vOrden,vtiplis
vNumpol = SPAC(4)
vFte    = SPAC(3)
vCodmes = SPAC(2)
vCoddep = SPAC(6)
vCodFte = SPAC(3)
vtiprep = 1
@ 01,01 SAY "Todas las Polizas : " GET vToPol  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToPol,3,22) 
@ 03,01 SAY "           Poliza : "
@ 03,22 GET vNumpol  PICTURE '!!!!' WHEN vToPol = 2  VALID vnumpol1()

@ 05,01 SAY "      Pecosa tipo : " get vTiprep FUNCTION '^ Todos;Compra;Stock'  WHEN vToPol = 2

@ 08,01 SAY "  Todos las Meses : " GET vTomes  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTomes,6,22)  WHEN vToPol = 1
@ 09,01 SAY "              Mes : "
@ 09,22 GET vCodmes  PICTURE '!!'  VALID VAL_PARA(vCodMes,'FECMES','C') WHEN vToPol = 1 AND vTomes = 2

@ 11,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTofue,9,22)  WHEN vToPol = 1
@ 12,01 SAY "           Fuente : "
@ 12,22 GET vCodFte  PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE','C')  WHEN vToPol = 1 and vTofue =2

@ 14,01 SAY "           Estado : " GET vTipLis  FUNCTION '^ Todos;Pendientes;Atendidos'  WHEN vToPol = 1

@ 18,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1 AND LASTKEY()#27
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON Periodo+Itepol.NumRef+itepol.Codgen TO (vInd) ;
      FOR IIF(vToPol=1,.T.,NumPol+CodFte = vNumpol+alltrim(vFte)) .AND. IIF(vTipLis=1,.T.,iif(vTipLis=2,Estado = '00',Estado = '50' )) and ;
                 iif(vToMes=1,.t.,month(fecemi)=val(vCodMes))  
   SET INDEX TO (VIND)

   SET FILTER TO TIPPOL='S' AND iif(vToFue=1,.t.,Codfte=alltrim(vCodfte)) AND iif(vtiprep=1,.t.,iif(vtiprep=2,itepol.tippec='O',itepol.tippec='S')) &&and iif(!empty(itepol.tippec),iif(vtiprep=1,.t.,iif(vtiprep=2,itepol.tippec='C',Itepol.tippec='S')),.t.)    &&AND itepol.valtot>0
   SELE ITEPOL                    
   GO TOP
   SELE POLIZA

   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipLis=1,' en General ',IIF(vTipLis=2,' Pendientes ',' Atendidos '))
   IF !EOF()
      if vTopol = 2 
      	 do case
      	    case alltrim(analisis)='G' 
				do reporte with 2,"polsal1",' Poliza de Salida',1,.F.,.T.
            case alltrim(analisis)='C' 
                do reporte with 2,"polsal_c",' Poliza de Salida',1,.F.,.T.
            case alltrim(analisis)='A'
                do reporte with 2,"polsal_a",' Poliza de Salida' ,1,.F.,.T.
         endcase      	          	    
         *do reporte with 2,"polsal1",' Poliza de Salida'
	  else
	     do reporte with 2,"Polsal",' Poliza de Salida ',1,.F.,.T.
	  endif   
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILT TO
   CLOSE INDEX
*  ERASE (VIND)
ENDIF
SELECT Poliza
SET ORDE TO (VORDE)
GO TOP
GO VREC
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_2
  RELEASE    WINDOW wind_3
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

PROCEDURE vNumpol1
*---------------
IF !EMPTY(vnumpol)
	vnumpol=padl(alltrim(vnumpol),4,'0')
	SEEK m.periodo+vnumpol
	if found()
	   retur .t.
	else
	   do standby with 'La poliza NO se encuentra registrada'
	   return .f.
	endif   
ELSE
	vnumpol=m.numpol
	RETURN .T.
ENDIF

function valpec
*-----------------
parameter vnumpec
private vfun
vfun = .t.
m.numpec=padl(alltrim(str(vnumpec,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif
return vfun

FUNCTION Val_ArtC  && Articulos
*------------------
PARAMETER xcod
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
_campo = VARREAD()
mAlias = ALIAS()
SELECT IteArt
GO TOP
_OldWnd = WOUTPUT()
v_Fun='*'

SEEK xcod
v_fun = IIF(FOUND(),Descri,"*")
*   SELECT itepol
*   IF RLOCK()
*      REPLACE descri  WITH Iteart.descri
*   ENDIF
SELECT (mAlias)

RETURN v_fun


PROCEDURE PROYACT
*----------------
DO CASE
   CASE EMPTY(substr(itepol.codcal,13,3))
        RETA = VAL_SUBP(substr(ITEPOL.codcal,10,3),'CODSUB'+substr(ITEPOL.codcal,8,2)+'    ','D',22,40)
   CASE EMPTY(substr(itepol.codcal,16,2))     
        RETA = VAL_PYAC(alltrim(substr(itepol.codcal,13,3)),itepol.Periodo+substr(itepol.CodCal, 8,5),'D',22,40,4) 
   CASE !EMPTY(substr(itepol.codcal,16,2))          
	    RETA = VAL_SUPY(alltrim(substr(itepol.codcal,16,2)),itepol.Periodo+substr(itepol.codcal, 8,8),'D',22,40,5) 
ENDCASE	    
retur (reta)

 FUNCTION BuscArt
 *----------------
 private vfun
 as=alias()
 sele produ
 set orde to 1
 seek 'B'+LEFT(ITEPEC.CODART,6)
 if !found()
    vFun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
 else
    vfun=UPPER(produ.descri)
 endif
 sele (as)
 return vfun

PROCEDURE UBICTA
*---------------
Private as
as = alias()
select asig
seek '08'+'1  '+itepec.tipfun+'B'+LEFT(itepec.codart,6)

*SELECT ITEPOL
if found()
   replace ITEPOL.DCuenta with asig.dcuenta, ITEPOL.HCuenta with asig.hcuenta
else   
   replace ITEPOL.DCuenta with '     *    ', ITEPOL.HCuenta with '     *    '
endif   
select (aS)
RETURN
