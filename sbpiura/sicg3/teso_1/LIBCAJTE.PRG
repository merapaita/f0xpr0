* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ LibCajTe.PRG  15/05/96                              L: 281   ³	
* ³ Libro de Caja Tesorer¡a Sede                                 ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
* Abrir Archivos de trabajo
USE Parmae  IN 1  ORDER TAG Parmae1         ALIAS parma
USE AstPat  IN 2  ORDER TAG AstPat1         ALIAS AstPat
USE MovBco  IN 3  ORDER TAG MovBco11        ALIAS MovB
USE Cheque  IN 4  ORDER TAG Cheque1         ALIAS Cheque
STORE .T. TO vErdad
STORE 0 TO xano, xMes, vCon1
STORE SPACE(2) TO WMES
DO Inicia
DO salida
RETURN

PROCEDURE Inicia
*---------------
  vcol ="w+/w,n/w,W/W,w/W,,w+/b"
  IF Escolor
     DEFINE WINDOW LibCajTe FROM 11,52 TO 15,78  SHADOW COLOR &vcol
  ELSE
     DEFINE WINDOW LibCajTe FROM 11,52 TO 15,78  COLOR SCHEME c_popup
  ENDIF
  ACTIVATE WINDOW LibCajTe
  xbalance = 'Mensual'
  xdigitos = 2
*SELECT parma
*SEEK 'ANOMESACTUAL'
*vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
*xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
*xAno     = IIF(FOUND(), VAL(SUBSTR(STR(YEAR(vmesact),4),3,2)), VAL(SUBSTR(STR(YEAR(DATE()),4),3,2)))
 xMes     = MONTH(DATE())
 xAno     = VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
  @ 0, 0 SAY '    Libro Caja:' + xBalance
  @ 2, 0 SAY '     Mes y A¤o:' GET xMes     PICTURE '99' WHEN xBalance = 'Mensual';
             VALID xMes > 0 .AND. xMes <= 12
  @ 2,20 GET xAno                           PICTURE '99'
  READ
  IF XMES<10
     WMES="0"+ALLTRIM(STR(XMES))
  ELSE
     WMES=STR(XMES,2)   
  ENDIF   
  IF LASTKEY() # 27
  DEFINE WINDOW Xwait FROM 20,06 TO 22,78 COLOR SCHEME 05 
  ACTIVATE WINDOW Xwait
  filaux = SYS(3)+'.DBF'
  vind1=SYS(3)+'.IDX'
  SELECT 8
CREATE TABLE (filaux);
  (TipDoc    C(003),;
   Fecha     D(008),;
   periodo   C(002),;
   NumMes    C(002),;
   NumRef    C(004),;
   CodCta    C(010),;
   TipCta    C(001),;
   MtoDeb    N(13,2),;
   MtoHab    N(13,2),;
   CodCtc    C(014),;
   Numchq    C(014))
   USE IN 8
   USE (filaux) IN 5 EXCLUSIVE                  ALIAS Temp
  @0,10 SAY " Espere un Momento...Libro Caja en Proceso!" COLOR W+/BR*
  xruta=SET('PATH')
       DO CASE
        CASE xbalance = 'Mensual'
                SELECT AstPat
                DO Libro   &&proceso que genera los asientos de ingreso y salida de caja
                           &&101 al debe y haber y toma solo el monto liquido de la opera-
                           &&ci¢n en el caso de retenciones.
				USE Compag  IN 11  ORDER TAG Compag1        ALIAS Comp
				USE Cuentas IN 9  ORDER TAG Cuentas1        ALIAS Cuen
				USE RecIng  IN 10 ORDER TAG RecIng1         ALIAS RI
                SET MEMOWIDTH TO 65
				USE HojAnu  IN 12  ORDER TAG Hojanu1        ALIAS hojanu
				use Reversio IN 13 ORDER TAG Rever1         ALIAS rever
				SELECT TEMP
                SET RELATION TO Temp.CodCta                             INTO Cuen
                SET RELATION TO Temp.NumMes+Temp.NumRef+Temp.Codctc     INTO Comp   ADDITIVE
                SET RELATION TO Temp.Periodo+Temp.NumMes+Temp.NumRef    INTO RI     ADDITIVE
                SET RELATION TO Temp.Nummes+Temp.NumRef             INTO MovB   ADDITIVE
                SET RELATION TO Temp.NumMes+Temp.NumRef             INTO hojanu ADDITIVE
                SET RELATION TO Temp.NumMes+Temp.NumRef             INTO Rever  ADDITIVE
                INDEX ON dtoc(fecha)+Numref+tipdoc TO (vind1) FOR MONTH(Temp.Fecha) = xMes ;
                         .AND. VAL(RIGHT(ALLTRIM(STR(YEAR(Temp.Fecha))),2)) = xAno
				DEACTIVATE WINDOW Xwait
                DO REPORTE WITH 2, "LibCajTE", ' Libro Caja Mensual ',1,.F.,.T.
     ENDCASE
     SET INDEX TO
     CLOSE DATABASE
     ERASE (vind1)
     ERASE (Filaux)
  ENDIF
  DEACTIVATE WINDOW LibCajTe
  IF !escolor
     RESTORE SCREEN FROM principal
  ENDIF
  SHOW POPUP menu, pop_01
RETURN


FUNCTION Libro  &&FILTRA EN EL CASO DE RETENCIONES SOLO EL MONTO LIQUIDO
*-------------
vAli=ALIAS()
SELECT AstPat
vInd9 = SYS(3) + '.Idx'
VDBF9 = SYS(3) + '.DBF'
INDEX ON NumMes+NumRef TO (vInd9) FOR NUMMES = WMes ;
         .AND. VAL(RIGHT(ALLTRIM(STR(YEAR(Fecha))),2)) = xAno AND RET='S' AND TIPCTC='CC' AND SUBSTR(CODCTA,1,3)<>'101' AND CODCTC<>'631003044'  &&CAFAE
GO TOP
COPY TO (VDBF9)
USE IN 2
USE (VDBF9) IN 6  ALIAS RET
USE AstPat  IN 2  ORDER TAG AstPat1         ALIAS AstPat
SELE ASTPAT
SET RELATION TO NumMes+NumRef+CodCtc INTO Cheque
vInd8 = SYS(3) + '.Idx'
VDBF8 = SYS(3) + '.DBF'
INDEX ON NumMes+NumRef TO (vInd8) FOR NUMMES= WMes ;
         .AND. VAL(RIGHT(ALLTRIM(STR(YEAR(Fecha))),2)) = xAno .AND. RET<>'S' AND TIPCTC='CC'   AND SUBSTR(CODCTA,1,3)<>'101'  AND CODCTC<>'631003044'  &&CAFAE
GO TOP
vTipDoc = TipDoc
vFecha  = Fecha
vperiodo = periodo
vNumMes = NumMes
vNumRef = NumRef
vCodCta = CodCta
vTipCta = TipCta
vMtoDeb = MtoDeb
vMtoHab = MtoHab
vCodCtc = CodCtc
vCheque = Cheque.NumChq
SCAN
    vTipDoc = TipDoc
	vperiodo = periodo
    vFecha  = Fecha
    vNumMes = NumMes
    vNumRef = NumRef
    vCodCta = CodCta
    vTipCta = TipCta
    vMtoDeb = MtoDeb
    vMtoHab = MtoHab
    vCodCtc = CodCtc
    vCheque = Cheque.NumChq
    SCAN WHILE vTipDoc = TipDoc .AND. vFecha = Fecha .AND. vNumMes = NumMes .AND. vNumRef = NumRef
         vRec1 = RECNO()
         vTipDoc = TipDoc
	  	 vperiodo = periodo
         vFecha  = Fecha
         vNumMes = NumMes
         vNumRef = NumRef
         vCodCta = CodCta
         vTipCta = TipCta
         vMtoDeb = MtoDeb
         vMtoHab = MtoHab
         vCodCtc = CodCtc
         vCheque = Cheque.NumChq         
         vMto    = 0
         IF vTipCta = 'D' AND VTIPDOC='C/P'
            SELE RET
            SCAN WHILE vTipDoc = TipDoc .AND. vFecha = Fecha .AND. vNumMes = NumMes .AND. vNumRef = NumRef
	            VMTO=VMTO+MTOHAB
            ENDSCAN
         ENDIF
         SELECT Temp
         IF F_APPD()
               REPLACE TipDoc WITH vTipDoc        
               REPLACE Fecha  WITH vFecha         
               REPLACE periodo WITH vperiodo
               REPLACE NumMes WITH vNumMes        
               REPLACE NumRef WITH vNumRef        
               REPLACE CodCta WITH vCodCta        
               REPLACE TipCta WITH vTipCta        
               REPLACE MtoDeb WITH vMtoDeb - vMto 
               REPLACE MtoHab WITH vMtoHab        
               REPLACE CodCtc WITH vCodCtc
               REPLACE NumChq WITH vCheque
               @0,01 SAY STR(RECNO(),6)
        ENDIF
        SELECT AstPat
    ENDSCAN
    GO vRec1
    SELECT AstPat
ENDSCAN
SELECT (vAli)
RETURN


PROCEDURE Salida
*---------------
RELEASE    WINDOW LibCajTe
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN

FUNCTION Descri
*--------------
PUBLIC _x,vCue
vCue = LEFT(Temp.CodCta,2)+'00000000'
_x=Val_Fun('Cuen','CodCta','Descri',vCue)
RETURN _x


FUNCTION Descri1
*---------------
PUBLIC _y,vCue1
vCue1= LEFT(Temp.CodCta,3)+'0000000'
_y=Val_Fun('Cuen','CodCta','Descri',vCue1)
RETURN _y



FUNCTION Glosa
*---------------
PARAMETERS a
Private _z
_z=SPACE(65)
DO CASE
	CASE a=1
	   DO CASE
	      CASE Temp.Tipdoc='C/P'
	           _z = SUBST(Comp.GLOSA,25,65)
	      CASE Temp.Tipdoc='R/I'
			   _z = UPPER(ALLTRIM(MLINE(RI.Observ,3)))      
	      CASE Temp.Tipdoc='B/D' OR  Temp.Tipdoc='N/A' OR Temp.Tipdoc='N/C' OR Temp.Tipdoc='NOC'
			   _z = VAL_PARA(MovB.MOTIVO,'MOTIVO','D',22)      
	      CASE Temp.Tipdoc='H/A'
			   _z = LEFT(HOJANU.JUSTIF,65)      
	      CASE Temp.Tipdoc='REV'
			   _Z = LEFT(REVER.GLOSA,65)      
	 	ENDCASE		   
	CASE a=2	
	 DO CASE
	      CASE Temp.Tipdoc='C/P'
	           _z = SUBST(Comp.GLOSA,91,65)
	      CASE Temp.Tipdoc='R/I'
			   _z = UPPER(ALLTRIM(MLINE(RI.Observ,4)))      
	      CASE Temp.Tipdoc='N/C' OR  Temp.Tipdoc='N/A' OR Temp.Tipdoc='B/D' OR Temp.Tipdoc='NOC'
			   _z = LEFT(MOVB.NOMBRE,65)
	      CASE Temp.Tipdoc='H/A'
			   _z = SUBST(HOJANU.JUSTIF,66,65)      
	      CASE Temp.Tipdoc='REV'
			   _Z = SUBST(REVER.GLOSA,66,65)      
	ENDCASE
ENDCASE
RETURN _z

PROCEDURE Salida
*---------------
RELEASE    WINDOW LibCajTe
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)

PROCEDURE PENDIENTE
        IF xMes = MONTH(vmesact) .AND. xAno = VAL(SUBSTR(STR(YEAR(vmesact),4),3,2))
              ELSE
                vpath   = '..\'+ALLTRIM(STR(xmes,2)) + STR(xano,2)
                vdir    = '..\'+ALLTRIM(STR(xmes,2)) + STR(xano,2) + '\'
                vbackup = ALLTRIM(STR(xmes,2)) + STR(xano,2)+'.zip'
               IF !FILE(vdir + vbackup)
                   DO Standby WITH 'Archivo : de BackUp en Disco del mes ' ;
                   + ALLTRIM(STR(xmes)) + ' '+ALLTRIM(STR(xano,2)) + ' no existe'
                   RETURN
                ENDIF
                !CD &vpath >NUL
                !FOXSWAP PKUNZIP -o &vbackup Parmae.dbf AstPat.dbf Compag.dbf Cuentas.dbf >NUL
                !FOXSWAP PKUNZIP -o &vbackup Parmae.cdx AstPat.cdx Compag.cdx Cuentas.cdx >NUL
                !CD ..\FUENTES
                CLOSE DATA
                SET PATH TO &vpath
				DEACTIVATE WINDOW Xwait
                CLOSE DATA
                SET PATH TO &xruta
                vdbf = vdir + '*.dbf'
                vidx = vdir + '*.idx'
                vcdx = vdir + '*.cdx'
                !ERASE &vdbf >NUL
                !ERASE &vidx >NUL
                !ERASE &vcdx >NUL
             ENDIF
RETURN