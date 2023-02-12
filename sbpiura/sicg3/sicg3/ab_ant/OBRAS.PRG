*--------------------------------------------------------------------------
* RegPre.Prg
* Registra Presupuestos
* Autor: Ing. Luis Castillo Dur n
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE Obra     IN 1   order tag Obra1    ALIAS Obra
USE Parmae   IN 2   order tag Parmae1  ALIAS Parma
USE Maepre   IN 3   order tag Maepre1  ALIAS Maepre
USE calen    IN 4   order tag calen3   ALIAS calen
USE itepar   in 5   order tag itepar1  ALIAS Itepar

*- Mensajes de aviso al usuario
Vmens01 = 'Registro de Obras'
Vmens02 = ' Obras : REVISION '
Vmens04 = 'Dicho Obras no fue encontrado'
Vmens05 = 'No existe Obra anterior'
Vmens06 = 'No existe Obra siguiente'
Vmens07 = '¨ Desea Anular ‚ste Obra ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Obra ha sido anulado'
Vmens10 = 'El Obra ya est  Atendido'
Vmens11 = 'El Obra ha sido devuelto'

SELECT maepre
SET FILT TO TIPFUN='I'
GO TOP

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente                                     Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 7,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM  8,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Obra          ®F9¯ Detalle : Item ' ;
 COLOR SCHEME 10

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
 CLEAR
 @  1, 2 SAY "          Periodo :"
 
 @  3, 2 SAY "         Programa :"
 @  4, 2 SAY "      SubPrograma :"
 
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT maepre
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 ON KEY LABEL F9 DO VISTA_DET
 SCATTER MEMVAR
 @  0,60 SAY IIF( m.Estado= '00','Emitido  ',IIF( m.Estado = '20','         ',IIF(m.Estado='99','Anulado  ',IIF(m.Estado='50','Atendido ',' ------- '))))

 @  1,22 SAY m.Periodo
 
 @  3,22 SAY VAL_PARA(m.CodPrg,'CODPRG','V',22,40)
 @  4,22 SAY VAL_SUBP(m.CodSubPr,'CODSUB'+m.CodPrg,'V',22,40)
 
 DO VISTA_HIJO 
 RETURN


PROCEDURE vista_hijo
*-------------------
SELECT obra

SEEK m.periodo+ALLTRIM(m.codprg)+ALLTRIM(m.CodSubPr)
BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+ALLTRIM(m.codprg)+ALLTRIM(m.CodSubPr) TIMEOUT 0.001 ;
   WINDOW wind_2 FIELDS;
       codproy     : H= 'Proy' ,;
	   Codsupry    : H= 'SubPy' ,;
	   Codobra     : H= 'Obra',;
	   Descri      : H= 'Detalle',;
	   Ubicac      : H= 'Ubicaci¢n',;
	   Observa     : H= 'Observaciones '
SELECT maepre
RETURN

PROCEDURE vista_DET
*-------------------
SELECT obra
ON KEY LABEL F9 
SEEK m.periodo+ALLTRIM(m.codprg)+ALLTRIM(m.CodSubPr)
BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+ALLTRIM(m.codprg)+ALLTRIM(m.CodSubPr) ;
   WINDOW wind_2 FIELDS;
       codproy     : H= 'Proy' ,;
	   Codsupry    : H= 'SubPy' ,;
	   Codobra     : H= 'Obra',;
	   Descri      : H= 'Detalle',;
	   Ubicac      : H= 'Ubicaci¢n',;
	   Observa     : H= 'Observaciones '
SELECT maepre
 ON KEY LABEL F9 DO VISTA_DET
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE obra
GO TOP
IF EOF()
    DO standby WITH Vmens08
    RETURN
ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 codprg   :H='Prg',;
 codsubpr :H='SPg',;
 codproy  :H='Pry',;
 codsupry :H='SPy',;
 descri   :H='Detalle'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 vkey = periodo+codprg+codsubpr
 SELE maepre
 seek vkey
 DO Vista
RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
SELECT maepre
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
vtemp    = RECNO()
vPeriodo = RIGHT(DTOC(DATE()),2)
vCodPrg  = '  '
vCodSubPr= '   '

 DEFINE WINDOW Lis FROM 11,12 TO 17,68 DOUBLE ;
 TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lis

 @  1,2 say ' Periodo : ' GET vPeriodo   PICTURE '!!'
 @  2,2 say 'Programa : ' GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',14,15)
 @  3,2 say 'SubProg. : ' GET vCodSubPr  PICTURE '!!!' VALID VAL_SUBP(vCodSubPr,'CODSUB'+VCodPrg,' ',14,15)

 READ

DEACTIVATE WINDOW lis

 IF EMPTY(vPeriodo) .or. LASTKEY()=27
    RETURN
 ELSE
    SEEK vPeriodo+ALLTRIM(vCodPrg)+ALLTRIM(vCodSubpr)
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


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT maepre
 av=RECNO()
 DO Pantalla
 SCATTER MEMVAR 
 @  1,22 GET m.Periodo   disable
 
 @  3,22 SAY VAL_PARA(m.CodPrg,'CODPRG','V',22,40)
 @  4,22 SAY VAL_SUBP(m.CodSubPr,'CODSUB'+m.CodPrg,'V',22,40)
 READ
 IF LASTKEY() # 27
    DO Trabaja_Hijo
 ENDIF
 UNLOCK ALL
 SELECT maepre
 GO TOP
 GO aV
 DO Vista
RETURN


PROCEDURE Trabaja_Hijo
*---------------------
PRIVATE VFUN
VFUN = .T.
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO Agreg_item
ON KEY LABEL F8  DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE obra
SEEK m.periodo+alltrim(m.codprg)+alltrim(m.codsubpr)
IF !FOUND()
   DO Agreg_item 
ENDIF
BROWSE NOREFRESH NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY m.periodo+alltrim(m.codprg)+alltrim(m.codsubpr) FIELD ;
       CodProy  : H='Pry'  :V=VAL_PYAC(Codproy,Periodo+codprg+Codsubpr,'vcodproy') :F  ,;
	   CodSupry : H='SPy'  :W=!EMPTY(Codproy) ,;
	   Codobra  : H= 'Obr' ,;
	   NumObra  : H= 'N§'  :p='!!',; 
	   Descri   : H= 'Detalle',;
	   Ubicac   : H= 'Ubicaci¢n',;
	   Observa  : H= 'Observaciones ' 
	   
 SEEK m.periodo+alltrim(m.codprg)+alltrim(m.codsubpr)
 SCAN WHILE m.periodo+alltrim(m.codprg)+alltrim(m.codsubpr) = Periodo+codprg+codsubpr
 	  IF EMPTY(codproy) OR EMPTY(codsupry)
         IF RLOCK()
            DELETE NEXT 1
         ENDIF   
         UNLOCK
      ENDIF   
 ENDSCAN
  
 ON KEY LABEL F5
 ON KEY LABEL F8
 ON KEY LABEL F10

 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT maepre
 RETURN vfun


PROCEDURE Elimi
*---------------
 SELECT Obra
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF !Estado # '  00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ELIMINAR ‚ste Obra ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
   dele next 1
   SKIP
   DO Vista
 ENDIF
 UNLOCK all
 RETURN

PROCEDURE ZAgreg_Item
*-------------------
PARAMETER VB
SELECT OBRA
IF VB=2
   if yesno('¨Conforme la Obra?')
      SELECT Parma
      SEEK 'CORREL' + 'CODOBR'
      REPLACE NumEnt WITH NumEnt + 1
      SELE OBRA
	  IF F_appd()
        REPLACE Periodo WITH m.Periodo ,;
                Codprg  WITH m.Codprg ,;
              CodSubpr  WITH m.Codsubpr
        RETURN .T.
      ENDIF
	  RETURN .F.    
   else
      retu .t.  
   endif   
else       
	IF F_appd()
        REPLACE Periodo WITH m.Periodo ,;
                Codprg  WITH m.Codprg ,;
              CodSubpr  WITH m.Codsubpr
        RETURN .T.
    ENDIF
	RETURN .F.    
endif    

PROCEDURE Agreg_Item
*-------------------
SELECT OBRA
SET ORDE TO 2
GO BOTT
VCOB = PADL(ALLTRIM(STR(VAL(CODOBRA)+1,3)),3,'0')

IF F_appd()
   REPLACE Periodo WITH m.Periodo ,;
           Codprg  WITH m.Codprg ,;
         CodSubpr  WITH m.Codsubpr ,;
           Codobra WITH vcob  
        SET ORDE TO 1   
        RETURN .T.
ENDIF
SET ORDE TO 1   
RETURN .F.    


PROCEDURE Elimi_Item
*-------------------
SELECT Obra
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif
return

PROCEDURE Corri_Item
*-----------------
REPLACE CodCal  WITH m.CodCal
RETURN .T.

PROCEDURE Lista
*--------------
select Obra
SCATTER MEMVAR
m.codfte='PRP'
GO TOP
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"Obras",' Listado Obras '
Endif

SELE Obra
GO TOP
do Vista
RETURN


PROCEDURE ValAP
*--------------
SELECT Obra
 vTem=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 *SET RELATION TO PERIODO+NUMOC INTO ITEOC
 *SET SKIP TO ITEOC
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 tipfun  :H='Tp' ,;
 codprg  :H='Prg',;
 codsubpr :H='SubPrg',;
 codact  :H='Act',;
 codproy :H='Proy',;
 descri :H='Obs'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 VCLI=CodPrg
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
 GOTO Vtemp
 RETURN .t.


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
  RELEASE    WINDOW wind_c1
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


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

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.

XX = Val_ArtDet(_Cod,.F.)
IF XX
  SELECT Obra
  REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri
  vFun = .T.
ENDIF
RETURN vFun

PROCEDURE Trimestre
*------------------
parameter vFecha

DO CASE
   CASE MONTH(vFecha) = 1  OR MONTH(vFecha) = 2   OR  MONTH(vFecha) = 3
        vTrim = '1'
   CASE MONTH(vFecha) = 4  OR MONTH(vFecha) = 5   OR  MONTH(vFecha) = 6
        vTrim = '2'
   CASE MONTH(vFecha) = 7  OR MONTH(vFecha) = 8   OR  MONTH(vFecha) = 9
        vTrim = '3'
   CASE MONTH(vFecha) = 10 OR MONTH(vFecha) = 11  OR  MONTH(vFecha) = 12
        vTrim = '4'
ENDCASE

return .t.

PROCEDURE  PROGRAMA
*-----------------
 as=alias()
 publi t01,t02,t03,t04,t05,t06,t07,t08,t09,t10
 sele parma
 set filt to Tipo='CODPRG'
 k=0
 scan
   k=k+1
   v=padl(k,2,'0')
   t&v=parma.Descri
 endscan
 sele (as)
 go top
return .t.
