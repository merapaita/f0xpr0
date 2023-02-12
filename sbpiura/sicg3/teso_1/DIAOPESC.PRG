* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ DiaOpesSc.PRG  15/05/96                             L: 187   ณ	
* ณ Diario de Operaciones Sede Central                           ณ
* ณ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
USE Compag   IN  1  ORDER TAG Compag1               ALIAS CompAG
USE cheque   IN  2  ORDER TAG cheque1      			ALIAS cheque
USE RecIng   IN  3  ORDER TAG RecIng1  			  	ALIAS RECING
USE MovBco   IN  4  ORDER TAG MovBco11			alias MOVBCO
USE Parmae   IN  5  ORDER TAG Parmae1               ALIAS Parma
USE HojAnu   IN  6  ORDER TAG Hojanu1               ALIAS hojanu
use Reversio IN  7  ORDER TAG Rever1                ALIAS rever
USE astpat   IN  8  ORDER TAG astpat1      			ALIAS astpat &&Egresos
USE cuentas  IN 14  ORDER TAG cuentas1     			ALIAS cuenta
PUBLIC VCTRL,VANO,VMES
VCTRL=.T.

DEFINE WINDOW lis FROM 6,10 TO 21,70 TITLE ' Listado de Operaciones de TESORERIA ' FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
XMES=MONTH(DATE())
STORE SPACE(2)  TO vMes
IF XMES<10
   VMES="0"+ALLTRIM(STR(XMES))
ELSE
   VMES=STR(XMES,2)   
ENDIF   

VAno= VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
@ 02,01 SAY "              Mes : " GET vMes    PICTURE '!!'  VALID val_para(vMes  ,'FECMES',' ',27,9) 
@ 02,25 GET vano  PICTURE '99'
@ 04,15 GET var1  PICTURE "@*C Boletas de Deposito "  DEFAULT 1
@ 05,15 GET var2  PICTURE "@*C Comprobantes de Pago"  DEFAULT 1
@ 06,15 GET var3  PICTURE "@*C Hojas de Anulaciขn  "  DEFAULT 1
@ 07,15 GET var4  PICTURE "@*C Notas de Abono      "  DEFAULT 1
@ 08,15 GET var5  PICTURE "@*C Notas de Cargo      "  DEFAULT 1
@ 09,15 GET var6  PICTURE "@*C Notas de Contab.OTES"  DEFAULT 1
@ 10,15 GET var7  PICTURE "@*C Recibos de Ingreso  "  DEFAULT 1
@ 11,15 GET var8  PICTURE "@*C Reversiones         "  DEFAULT 1
@ 13,15 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
DEACTIVATE WINDOW lis
IF okcancel = 1
   DO ESPERA WITH 1,'Espere un momento; Preparando Informaciขn ...!'
   XIND=SYS(3)+".IDX"
   SELECT AstPat
   SET ORDER TO ASTPAT7  &&Para Boletas de Depขsito
   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0")  AND TIPCTC='CC' AND CODCTC<>'631003044'  &&CAFAE
   DO ESPERA WITH 2
   GO TOP
   IF var1=1
	   do fval_exi
	   IF VCTRL
	   SET RELATION TO nummes+numREF  INTO movbco ADDI
	   DO REPORTE WITH 2,"LisOpeBD"," Listado de Operaciones de Boletas de Depขsito "
	   SET FILTER TO
	   SET RELATION OFF INTO movbco
	   ENDIF
   ENDIF



   IF VAR2=1
	   SET ORDER TO ASTPAT3  &&Para Comprobantes de Pago
	   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") AND TIPCTC<>'CC' AND CODCTC<>'631003044'  &&CAFAE
	   GO TOP
       do fval_exi
	   IF VCTRL
	   SET RELATION TO nummes+numref+codctc  INTO Compag ADDI
	   DO REPORTE WITH 2,"LisOpeCP"," Listado de Operaciones de Comprobantes de Pago "
	   SET FILTER TO
	   SET RELATION OFF INTO COMPAG
	   ENDIF
   ENDIF	   
 	   
   
   IF VAR3=1
	   SET ORDER TO ASTPAT18  &&Para Hojas de Anulaciขn
	   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0")  AND TIPCTC<>'CC' AND CODCTC<>'631003044'  &&CAFAE
	   GO TOP
       do fval_exi
	   IF VCTRL
	   SET RELATION TO nummes+numref  INTO hojanu ADDI
	   DO REPORTE WITH 2,"LisOpeha"," Listado de Operaciones de Hojas de anulaciขn "
	   SET FILTER TO
	   SET RELATION OFF INTO hojanu
	   ENDIF
   ENDIF   
   
   IF VAR4=1   
	   SET ORDER TO ASTPAT6  &&Para Notas de Abono
	   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") AND TIPCTC<>'CC' AND CODCTC<>'631003044'  &&CAFAE
	   GO TOP
       do fval_exi
	   IF VCTRL
	   SET RELATION TO nummes+numref  INTO MovBco ADDI
	   DO REPORTE WITH 2,"LisOpena"," Listado de Operaciones de Notas de Abono "
	   SET FILTER TO
	   SET RELATION OFF INTO movbco
	   ENDIF
   ENDIF   

   IF VAR5=1
	   SET ORDER TO ASTPAT5  &&Para Notas de Cargo
	   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") AND TIPCTC<>'CC' AND CODCTC<>'631003044'  &&CAFAE
	   GO TOP
       do fval_exi
	   IF VCTRL
	   SET RELATION TO nummes+numref  INTO MovBco ADDI
	   DO REPORTE WITH 2,"LisOpenc"," Listado de Operaciones de Notas de Cargo "
	   SET FILTER TO
	   SET RELATION OFF INTO movbco
	   ENDIF
   ENDIF
   
   IF VAR6=1
	   SET ORDER TO ASTPAT9  &&Para Notas de Contabilidad
	   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") AND TIPCTC<>'CC' AND CODCTC<>'631003044'  &&CAFAE
	   GO TOP
       do fval_exi
	   IF VCTRL
	   SET RELATION TO nummes+numref  INTO MovBco ADDI
	   DO REPORTE WITH 2,"LisOpeno"," Listado de Operaciones de Notas de Contabilidad"
	   SET FILTER TO
	   SET RELATION OFF INTO movbco
	   ENDIF
	ENDIF   
   
   
   IF VAR7=1
	   SET ORDER TO ASTPAT14  &&Para Recibos de Ingreso
	   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") AND TIPCTC<>'CC' AND CODCTC<>'631003044'  &&CAFAE
	   GO TOP
	   do fval_exi
	   IF VCTRL
	   SET RELATION TO PERIODO+nummes+numref  INTO recing ADDI
	   DO REPORTE WITH 2,"LisOperi"," Listado de Operaciones de Recibos de Ingreso "
	   SET FILTER TO
	   SET RELATION OFF INTO recing
	   ENDIF
   ENDIF   

   
   IF VAR8=1
       SET ORDER TO ASTPAT15  &&Para Reversiones
       SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0")  AND TIPCTC<>'CC' AND CODCTC<>'631003044'  &&CAFAE
       GO TOP
	   do fval_exi
	   IF VCTRL
	   SET RELATION TO nummes+numref  INTO rever ADDI
	   DO REPORTE WITH 2,"LisOpere"," Listado de Operaciones de Reversiones "
	   SET FILTER TO
	   SET RELATION OFF INTO rever
	   ENDIF
	ENDIF   
ENDIF
RELEASE    WINDOW LIS
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


PROCEDURE FVAL_EXI
IF EOF()
	SET FILT TO
	VCTRL=.F.
ELSE
    VCTRL=.T.	
ENDIF
RETURN 


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)

FUNCTION GLOSA
PUBLIC _q
SET MEMOWIDTH TO 60
_Q=UPPER(ALLTRIM(MLINE(RECING.Observ,3)))
RETURN _q