*------------------------------------------------------------------------
* Registra las Nota de Entrada de Almac‚n
* Estado :
*   '00' Emitida   Este es el que se registra en el Solicitud de Cotizaci¢n
*   '20' Afectada
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: Luis Castillo Dur n
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE Notalm   IN 1   order tag Notalm1  ALIAS Nota
USE Itealm   IN 2   order tag Itealm1  ALIAS Itealm
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE IteArt   IN 4   order tag Iteart3  ALIAS Iteart
USE Promae   IN 8   order tag Promae1  ALIAS Promae
USE ArtMae   IN 14  ORDER TAG ArtMae1  ALIAS Produ
USE KARDEX   IN 7   order tag KARDEX5  ALIAS KARDEX

PUBLIC vMes,vPart
*- Mensajes de aviso al usuario
Vmens01 = ' Nota de Entrada de Almac‚n : REVISION '
Vmens02 = ' Registro de Nota de Entrada de Almac‚n '
Vmens04 = 'Dicha Nota de Entrada de Almac‚n no fue encontrado'
Vmens05 = 'No existe Nota de Entrada de Almac‚n anterior'
Vmens06 = 'No existe Nota de Entrada de Almac‚n siguiente'
Vmens07 = '¨ Desea ANULAR ‚ste Nota de Entrada de Almac‚n ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Nota de Entrada de Almac‚n ha sido anulado'
Vmens10 = 'Este Nota de Entrada de Almac‚n ya fue atendido'
Vmens11 = 'Este Nota de Entrada de Almac‚n ha sido devuelto'

SELECT Nota
GO BOTTOM
ON KEY LABEL F7 DO LIQUIDA

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
HIDE POPUP ALL
DO Inicia                    && Define ventanas, men£s, t¡tulos
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
* vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula            Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 12,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 13,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: Nota de Entrada de Almac‚n        ®F9¯ Detalle : Item ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,68 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10
 
 DEFINE WINDOW Wind_4 FROM 20,68 TO 22,78 ;
 TITLE 'N§PECOSA:' COLOR SCHEME 10

 DEFINE WINDOW Wind_5 FROM 14,01 TO 16,79 ;
 TITLE ' Destino ' 

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD anula  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_1
CLEAR

 @  1, 2 SAY "          Periodo :"
 @  2, 2 SAY "       N£mero NEA :"
 @  2,40 SAY "            Fecha :"
 
 @  3, 2 SAY "      Dependencia :"
 @  4, 2 SAY "F. Financiamiento :"
 @  5, 2 SAY "      Procedencia :"
 @  6, 2 SAY "    Con destino a :"
 @  7, 2 SAY "      Liquidaci¢n :"
 @  8, 2 SAY "            Seg£n :"
 @  9, 2 SAY "    Observaciones :"
 @ 10,00 SAY PADC(' ° ®F7¯ Liquida ° ',79,' ') COLOR W+/B
RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Nota
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW WIND_1
 SCATTER MEMVAR
 @  0,60 SAY vEstna(m.Estado)
 @  1,22 SAY m.Periodo
 @  2,22 SAY m.NumAlm
 @  2,60 SAY m.FecAlm
 
 @  3,22 SAY val_para(m.CodDep,'CODDEP','D',22,50)
 @  4,22 SAY val_para(m.CodFte,'CODFTE','V',22,50)
 @  5,22 SAY val_para(m.Codprc,'CODPRC','D',22,50)

 @  6,22 SAY m.destino picture '@S50'
 @  7,22 SAY m.fecliq
 @  8,22 SAY m.Segun    picture '@S56'
 @  9,22 SAY m.Observa  picture '@S56'

 DO VISTA_HIJO
 RETURN


PROCEDURE VISTA_HIJO
*-------------------
hide popup all
ON KEY LABEL F9 DO VISTA_DET
SELECT IteALM
GO TOP

BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.NumALM TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   CanReq      : H= 'Cantidad' :P='9,999.99',;   
   CodArt      : H= 'C¢digo',;
   CodUni      : H= 'Uni'      :W=.F. :3,;   
   Descri      : H= 'Descripci¢n' :31 :W=.F. ,;
   PreUni      : H= 'PreUni' :P='99,999.999' :W=.F. ,;
   X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='99,999.99' :W=.F.

SELE Nota

RETURN


PROCEDURE VISTA_det
*------------------
hide popup all
ON KEY LABEL F9 DO observa
SELECT IteALM
GO TOP
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.NumALM ;
   WINDOW Wind_2 ;
   FIELDS;
   CanReq      : H= 'Cantidad' :P='9,999.99',;   
   CodArt      : H= 'C¢digo',;
   CodUni      : H= 'Uni'      :W=.F. :3,;   
   Descri      : H= 'Descripci¢n' :31 :W=.F. ,;
   PreUni      : H= 'PreUni' :P='99,999.999' :W=.F. ,;
   X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='99,999.99' :W=.F.

SELE Nota
do vista
ON KEY LABEL F9 
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT Nota
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO PERIODO+NUMalm INTO ITEalm
 SET SKIP TO ITEalm
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumALM :H=' N§ ' ,;
 Codfte :H='Fte' ,;
 FecALM  :H='Fecha',;
 itealm.descri :H='Articulo ' :36 ,;
 itealm.coduni :H='Unid' ,;
 itealm.Canreq :H='Cantid'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
 sele nota
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
 vNum_nea = '    '
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero NEA : ' GET vPeriodo PICTURE '!!'
 @ 1,26 SAY '-' GET vNum_NEA PICTURE '!!!!' VALID vBusca()
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_Nea) .or. LASTKEY()=27
    RETURN
 ELSE
	SEEK  vPeriodo + vNum_nea
 	IF !FOUND()
    	DO standby WITH Vmens04
     	GOTO Vtemp
	ELSE
    	DO Vista
	ENDIF
 ENDIF
RETURN


PROCEDURE vBusca
*---------------
vNum_nea=Padl(alltrim(vNum_nea),4,'0')
retur .t.


PROCEDURE Anter
*--------------
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


PROCEDURE Corri
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 DO CASE  
    CASE Estado = '99'
 		 * Anulada
         DO STANDBY WITH Vmens09
         RETURN
	CASE Estado = '40'
         * El Nota de Entrada de Almac‚n ya tiene ?
         DO STANDBY WITH 'La NEA ya est  atendida'
        RETURN
	CASE Estado = '5'
         * El Nota de Entrada de Almac‚n ya tiene ?
         DO STANDBY WITH 'La NEA ya est  liquidada/Contabilizada'
         RETURN
 ENDCASE
 *****
 SELECT Nota
 SCATTER MEMVAR
 ACTIVATE WINDOW Wind_1
 DO PANTALLA
 
 @  1,22 GET m.Periodo PICTURE '!!' 
 @  2,22 GET m.NumAlm
 @  2,60 GET m.FecAlm
 
 @  3,22 GET m.CodDep Picture "!!!!!!" valid val_para(m.CodDep,'CODDEP',' ',22,20)
 @  4,22 GET m.CodFte Picture "!!"     valid val_para(m.CodFte,'CODFTE',' ',22,20)
 @  5,22 GET m.CodPrc Picture "!!!"    valid val_para(m.Codprc,'CODPRC',' ',22,20)
 @  6,22 GET m.destino PICTURE "@S56"
 
 @  8,22 GET m.Segun   PICTURE "@S56"
 @  9,22 GET m.Observa PICTURE "@S56"
 
 READ VALID VAL_READ()
 IF LASTKEY() # 27
    DO WHILE .T.
       Ok =Trabaja_Hijo()
       IF LASTKEY() # 27 
          IF YESNO('¨ Conforme la modificaci¢n ?')
             EXIT
          ENDIF
       ELSE
          IF YESNO('¨ Cancela la modificaci¢n ?')
             EXIT
          ENDIF
       ENDIF
   ENDDO

   IF OK .AND. LASTKEY() # 27
      SELECT IteAlm
      SEEK m.Periodo + m.NumAlm
      vTotal = 0
      SCAN WHILE Periodo = m.Periodo and NumAlm = m.Numalm
           IF RLOCK()
              REPLACE VALTOT WITH Preuni*CanReq
           ENDIF
           UNLOCK
           vtotal = vtotal + valtot
      ENDSCAN
      m.nummes = padl(alltrim(str(month(m.fecalm),2)),2,'0')
      m.Valtot = vtotal
      SELECT Nota
      GATHER MEMVAR
   ELSE
      SELECT Nota
   ENDIF
   UNLOCK ALL
 ENDIF  
 SELECT Nota
 DO VISTA
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT Nota
 DO Pantalla
 SCATTER MEMVAR BLANK
 m.FecAlm = date()
 m.Periodo = STR(YEAR(DATE()) - 1900,2)
 
 @  1,22 GET m.Periodo PICTURE '!!' 
 @  2,22 GET m.NumAlm  VALID !EMPTY(m.NumAlm) AND VAL_ALM()
 @  2,60 GET m.FecAlm
 
 @  3,22 GET m.CodDep Picture "!!!!!!" valid val_para(m.CodDep,'CODDEP',' ',22,20)
 @  4,22 GET m.CodFte Picture "!!"     valid val_para(m.CodFte,'CODFTE',' ',22,20)
 @  5,22 GET m.CodPrc Picture "!"      valid val_para(m.Codprc,'CODPRC',' ',22,20)
 @  6,22 GET m.destino PICTURE "@S56"
 
 @  8,22 GET m.Segun   PICTURE "@S56"
 @  9,22 GET m.Observa PICTURE "@S56"
  
  READ VALID Val_Read()

 IF LASTKEY() # 27
    DO WHILE .T.
       Ok =Trabaja_Hijo()
       IF LASTKEY() # 27 AND Ok
          IF YESNO('¨ Confirme el ingreso ?')
             EXIT
          ENDIF
       ELSE
          DO STANDBY WITH ' Cancelado el Ingreso '
          ok = .F.
          EXIT
       ENDIF
    ENDDO
    IF Ok .AND. LASTKEY() # 27
          *- Aumento el correlativo
          SELECT ITEALM
          SEEK m.Periodo + m.NumAlm
          vTotal = 0
          SCAN WHILE Periodo = m.Periodo and NumALM = m.NumAlm 
               vtotal = vtotal + valtot
          ENDSCAN
          SELECT Nota
          m.Valtot = vtotal
          m.Estado = '00'
          m.nummes = padl(alltrim(str(month(m.fecalm),2)),2,'0')
          IF F_Appd()
             GATHER MEMVAR
          ENDIF
    ELSE
		DO STANDBY WITH 'Proceso cancelado'
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
ENDIF
SELECT Nota
GO BOTT
DO VISTA
RETURN


PROCEDURE Val_alm
*---------------
m.numalm=padl(alltrim(m.numalm),4,'0')
SEEK m.periodo+m.numalm
if !found()
   retur .t.
else
   do standby with 'La NEA se encuentra registrada'
   return .f.
endif   

PROCEDURE NUMERA
*--------------
SELE Nota
SEEK m.periodo+m.NumOc+alltrim(m.codfte)
IF FOUND()
    DO STANDBY WITH 'La O/C ya ha sido generada'
    RETURN .F.
ELSE
	RETURN .T.
ENDIF	


PROCEDURE Trabaja_Hijo
*---------------------
PRIVATE vfun
vFun=.t.
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO Agreg_item
ON KEY LABEL F8  DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE ITEALM
SEEK m.periodo+m.numAlm
IF !FOUND()
   DO Agreg_item
ENDIF
BROWSE NOREFRESH NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY m.periodo+m.numalm FIELD ;
   CodArt      : H= 'C¢digo' :V=VAL_ARTC(codArt,.F.):F :W=EMPTY(CodArt) ,;
   Descri      : H= 'Descripci¢n' :40 :W=.F.,;
   Coduni      : H= 'Uni' :W=.F. :3 ,;
   Canreq      : H= 'Cantd' :P='99,999.99' :v=canreq>0 :W=!EMPTY(CodArt) ,;
   PreUni      : H= 'PreUni' :P='99,999.999'  ,;
   X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='99,999.99' :W=.F.

 UNLOCK ALL

ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
if lastkey()=27
    vfun =.f.
endif   
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
SELECT nota
RETURN vfun


procedure liquida
*---------------
ON KEY LABEL F7
 SELECT Nota
 SCATTER MEMVAR
 ACTIVATE WINDOW Wind_1
 DO PANTALLA
 @  1,22 GET m.Periodo PICTURE '!!'  DISABLE
 @  2,22 GET m.NumAlm DISABLE
 @  2,60 GET m.FecAlm DISABLE
 
 @  3,22 GET m.CodDep Picture "!!!!!!" valid val_para(m.CodDep,'CODDEP',' ',22,20) DISABLE
 @  4,22 GET m.CodFte Picture "!!"     valid val_para(m.CodFte,'CODFTE',' ',22,20) DISABLE
 @  5,22 GET m.CodPrc Picture "!!!"    valid val_para(m.Codprc,'CODPRC',' ',22,20) DISABLE
 @  6,22 GET m.destino PICTURE "@S56" DISABLE
 @  7,22 GET m.Fecliq   
 @  8,22 GET m.Segun   PICTURE "@S56" DISABLE
 @  9,22 GET m.Observa PICTURE "@S56"
  
 READ VALID Val_Read()

 IF LASTKEY() # 27
    SELE ITEalm
    SEEK m.Periodo + m.Numalm
       SCAN WHILE PERIODO+NUMalm=m.Periodo + m.Numalm
            SELE KARDEX
            seek itealm.periodo+itealm.numALM+itealm.codfte+itealm.codart
            if found()
               	 IF RLOCK()
   		        	  REPLACE TIPDOC WITH 'NEA', NUMDOC WITH m.NumAlm ,FECDOC WITH m.Fecalm, CODART with ITEalm.CodArt,Tipord with '3';
                          FECENT WITH m.Fecliq , CODDEP WITH NOTA.Coddep ,ENTCAN with ITEalm.Canreq ,;
                         PREUNIE WITH ITEalm.PreUni, ENTIMP WITH ITEalm.PreUni*KARDEX.EntCan ,HORENT WITH TIME(),PERIODO WITH m.Periodo,codfte with m.codfte,ESTADO WITH '  '
            	 ENDIF 
            ELSE
	             IF F_APPD()
   		        	  REPLACE TIPDOC WITH 'NEA', NUMDOC WITH m.NumAlm ,FECDOC WITH m.Fecalm, CODART with ITEalm.CodArt,Tipord with '3';
                          FECENT WITH m.Fecliq , CODDEP WITH NOTA.Coddep ,ENTCAN with ITEalm.Canreq ,;
    	                  PREUNIE WITH ITEalm.PreUni, ENTIMP WITH ITEalm.PreUni*KARDEX.EntCan ,HORENT WITH TIME(),PERIODO WITH m.Periodo,codfte with m.codfte,ESTADO WITH '  '
        	     ENDIF
            UNLOCK
            ENDIF
            SELE ITealm
            IF RLOCK()
               REPLACE ESTADO WITH '50'
            ENDIF   
       ENDSCAN
       SELECT nota
       m.ESTADO = '50'
       gather memvar
ELSE
    DO STANDBY WITH 'Proceso cancelado'
ENDIF
SELECT Nota
GO BOTT
DO VISTA
ON KEY LABEL F7 DO LIQUIDA
RETURN


PROCEDURE Anula
*--------------
 SELE Nota
 PRIVATE vFun,vKey
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR ‚sta Nota de Entrada de Almac‚n ?')
 IF vElimina
    sele IteAlm
    SCAN WHILE Periodo+numalm = Nota.Periodo + Nota.Numalm
         IF RLOCK()
		  	REPLACE estado with '99'
         ENDIF
         unlock
    ENDSCAN
    SELE Nota
    IF  RLOCK()
        REPLACE ESTADO WITH '99' ,FECVER WITH DATE()
    ENDIF
    UNLOCK
    DO Vista
 ENDIF
 RETURN


PROCEDURE Agreg_Item
*-------------------
PRIVATE VFUN
VFUN=.F.
SELE ITEalm
IF F_appd()
  REPLACE Numalm WITH m.Numalm ,;
          Periodo WITH m.Periodo ,;
          CodFte with m.Codfte ,;
          ESTADO WITH '00'
  VFUN=.T.
ENDIF
UNLOCK 
RETURN VFUN

PROCEDURE Elimi_Item
*-------------------
SELECT Itealm
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif
unlock
return

PROCEDURE Lista
*--------------
SELECT Nota
SET RELATION TO PERIODO+NUMALM INTO IteAlm
vCli = NumAlm 
set skip to itealm
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
*  DO LisOrd
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON NumAlm TO (vInd) ;
      FOR NumAlm = vCli
   SET INDEX TO (VIND)
   DEACTIVATE WINDOW STANDBY
   IF !EOF()
      DO REPORTE WITH 2,"Notalm",' Nota de Almacen ',1
   endif   
endif
SELECT Nota
SET RELATION TO
SET ORDE TO 1
go vtemp
DO VISTA
RETURN

PROCEDURE lisOrd
*---------------
vOrde = ORDER()
DEFINE WINDOW LIS FROM 1,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Notaes de Compra' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vNota,vTipPro,vListado,vTofue
vCli = SPAC(4)
vAno = '95'
VFTE = '   '
vcodfte = '   '
@ 01,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ por Documento;Detallado;Resumido' 

@ 05,01 SAY "        Total NEA : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,7,22) when vlistado =1
@ 07,01 SAY "              NEA : "
@ 07,22 GET vCli    WHEN VTOCLI=2 and vlistado=1 PICTURE '!!!!' VALID VO() AND ValOrd()

@ 10,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON NumAlm TO (vInd) ;
      FOR IIF(vToCli=1,.T.,NumAlm = vCli) 
    SET INDEX TO (VIND)
   COUNT ALL TO vTotoc     
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,'Listado Nota Compra',IIF(vTipPro=2,'Listado Nota de Entrada de Almac‚n Pendientes',IIF(vTipPro=3,'Listado de Notaes de Compra Afectadas',IIF(vTipPro=4,'Listado Nota de Entrada de Almac‚n Anuladas','Listado Nota de Entrada de Almac‚n Liquidadas'))))
   SET MEMOWIDTH TO 43
   IF !EOF()
     SET SKIP TO IteAlm
     do case 
        case VLISTADO=1
	        DO REPORTE WITH 2,"LisOc1",' Notaes de Compra',1
        case VLISTADO=2
    	 	DO REPORTE WITH 2,"LisOrdc",' Notaes de Compra ',1,.F.,.T.
        case VLISTADO=3     	
    	 	DO REPORTE WITH 2,"LisOrdX",' Notaes de Compra ',1,.F.,.T.        
     ENDcase   
     SELECT Nota
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   CLOSE INDEX
   ERASE (VIND)
ENDIF
RETURN

PROCEDURE VO
*-----------
vCli=Padl(alltrim(vCli),4,'0')
RETURN .T.

PROCEDURE Valord
*--------------
SELECT Nota
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
SEEK vano+VCLI+ALLTRIM(vfte)
if !found()
    SET SKIP TO IteAlm
    GO TOP
	HIDE MENU mMenu
	ACTIVATE SCREEN
	vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DO Logos WITH Rotulo1,vTempo
	ON KEY LABEL F10 KEYBOARD CHR(23)
	BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	NumOc  :H=' N§ ' ,;
	FecOc  :H='Fecha' ,;
	ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
	IteAlm.descri :H='Articulo ' :36 ,;
	IteAlm.coduni :H='Unid' ,;
	IteAlm.Canreq :H='Cantid'
	vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	DO Logos WITH Rotulo1,vTempo
	IF LASTKEY()=27
	   SELE Nota
	   SET RELATION TO
	ENDIF
	SHOW MENU mMenu
	ON KEY LABEL F10
	SELE Nota
 endif
 vAno = Nota.Periodo	 
 VCli = Nota.NumOC
 vFte = Nota.Codfte
 RETURN 

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
  ON KEY LABEL F9 
  HIDE WINDOW WIND_1
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE WINDOW wind_0
  RELEASE WINDOW wind_1
  RELEASE WINDOW wind_3
  RELEASE WINDOW wind_4
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


function valprv
*--------------
private xx, vfun
vfun = .f.
m.codprv= iif( empty(m.codprv),m.codprv,padl(alltrim(m.codprv),4,'0'))
xx = val_prv( m.codprv,.t.,2,26)
if xx
   return .t.
endif
return vfun

function valOc
*-----------------
parameter vnumOc
private vfun
vfun = .t.
m.numOc =padl(alltrim(str(vnumOc,4)),4,'0')
if m.numOc  = '0000' or empty(m.numOc)
   vfun = .f.
endif
return vfun

FUNCTION Observa
*---------------
vAlias = ALIAS()
ON KEY LABEL F9
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle NEA ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERVa WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF

RELEASE WINDOW Observa
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci¢n '
ENDIF
ON KEY LABEL F9 DO OBSERVA
RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,18 TO 20,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle O/C ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERVa NOEDIT WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
RETURN .T.

procedure itep
*-------------
as=alias()
sele pecosa
seek IteAlm.periodo+IteAlm.numpec+IteAlm.codfte
*OK1 = val_para(PECOSA.CodDep,'CODDEP','D',22,60)
OK1 = PECOSA.DESTINO
sele (as)
retu (ok1)

procedure itepEC
*---------------
PRIVATE AZ
SELE IteAlm
SEEK m.Periodo + m.NumOc + ALLTRIM(m.CodFte)
aZ=alias()
select pecosa
seek IteAlm.periodo+IteAlm.numpec+IteAlm.codfte
m.destino = IIF(EMPTY(m.destino),PECOSA.DESTINO,m.destino)
sele (aZ)
ACTIVATE WINDOW wind_5
@ 0,0 SAY 'Destino: ' get m.destino picture '@S73'
READ
DEACTIVATE WINDOW wind_5
retuR

*--------------*-------------------
PROCEDURE ALAN
*-------------
RETU
AX=ALIAS()
SELE ALAN
SEEK ALLTRIM(M.CODFTE)+ALLTRIM(M.NUMOC)
IF FOUND()
   M.NUMMES = ALAN.NUMMES
ELSE
   M.NUMMES = '00'
ENDIF      
SELE (AX)
RETURN

FUNCTION Val_ArtC  && Articulos
*------------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo
_campo = VARREAD()
ORD=ORDER()
mAlias = ALIAS()
SELECT IteArt
GO TOP
_OldWnd = WOUTPUT()
v_Fun=.f.
IF !mEdita
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
ELSE
   IF EMPTY(xcod)
      SET ORDER TO 2
      ACTIVATE SCREEN
      ON KEY LABEL F10 KEYBOARD CHR(23)
      ON KEY LABEL F8 DO BorrDet
      ON KEY LABEL F5 DO Agr2Det
      ON KEY LABEL F2 DO FunBusDet
      DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
      BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
        CodArt   :H='C¢digo' :W=.F. ,;
        Descri   :H='Nombre':70  ,;
        CodUni   :H='Unidad':7   
      vORD = RECNO()  
      GO TOP
      SCAN WHILE EMPTY(DESCRI)
           IF RLOCK()
              DELETE NEXT 1
           ENDIF    
      ENDSCAN  
      GO TOP
      GO vord
      ON KEY LABEL F10
      ON KEY LABEL F8
      ON KEY LABEL F5
      ON KEY LABEL F2
      RELEASE WINDOW _BusArt
      SET ORDER TO 1
      
      IF Lastkey()=27
         V_FUN = .f.
      ELSE
         xcod = CodArt
         IF mMsg
            @ _x,_y SAY Descri
         ENDIF
         SELECT (mAlias)
         IF !_tipo
            REPLACE &_campo WITH  xcod
         ENDIF
         v_fun = .T.
      ENDIF
   ENDIF
ENDIf
IF v_Fun
   SELECT iteALM
   IF RLOCK()
      REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri
   ENDIF
endif
SELECT (mAlias)
SET ORDE TO (ORD)
ON KEY LABEL F5  DO Agreg_Item
ON KEY LABEL F8  DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
RETURN v_fun

