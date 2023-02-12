*-------------------------------
*- DIAOPERE.PRG
*- Diario de Operaciones Tesorer¡a Regional
*- AUTOR : Federico Montero Valdiviezo
*-------------------------------
USE Compag   IN  1  ORDER TAG Compag1               ALIAS CompAG
USE cheque   IN  2  ORDER TAG cheque1      			ALIAS cheque
USE RecIng   IN  3  ORDER TAG RecIng1  			  	ALIAS RECING
USE MovBco   IN  4  ORDER TAG MovBco11				alias MOVBCO
USE Parmae   IN  5  ORDER TAG Parmae1               ALIAS Parma
USE HojAnu   IN  6  ORDER TAG Hojanu1               ALIAS hojanu
use Reversio IN  7  ORDER TAG Rever1                ALIAS rever
USE astpat   IN  8  ORDER TAG astpat1      			ALIAS astpat &&Egresos
USE cuentas  IN 14  ORDER TAG cuentas1     			ALIAS cuenta
PUBLIC VCTRL,VANO,VMES
VCTRL=.T.
DEFINE WINDOW lis FROM 3,10 TO 10,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
XMES=MONTH(DATE())
STORE SPACE(2)  TO vMes,VPER
IF XMES<10
   VMES="0"+ALLTRIM(STR(XMES))
ELSE
   VMES=STR(XMES,2)   
ENDIF   

VAno= VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
@ 02,01 SAY "              Mes : " GET vMes    PICTURE '!!'  VALID val_para(vMes  ,'FECMES',' ',27,9) 
@ 02,25 GET vano    PICTURE '99'
@ 04,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
VPER=STR(VANO,2)
RELEASE WINDOW lis
IF okcancel = 1
   XIND=SYS(3)+".IDX"
   SELECT AstPat
   SET ORDER TO ASTPAT7  &&Para Boletas de Dep¢sito
   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0")  AND TIPCTC='RE'
   GO TOP
   do fval_exi
   IF VCTRL
   SET RELATION TO nummes+numREF  INTO movbco ADDI
   DO REPORTE WITH 2,"LisOpeBD"," Listado de Operaciones de Boletas de Dep¢sito "
   SET FILTER TO
   SET RELATION OFF INTO movbco
   ENDIF
   
   
   SET ORDER TO ASTPAT6  &&Para Notas de Abono
   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") AND TIPCTC='RE'
   GO TOP
   do fval_exi
   IF VCTRL
   SET RELATION TO nummes+numref  INTO MovBco ADDI
   DO REPORTE WITH 2,"LisOpena"," Listado de Operaciones de Notas de Abono "
   SET FILTER TO
   SET RELATION OFF INTO movbco
   ENDIF

   SET ORDER TO ASTPAT5  &&Para Notas de Cargo
   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") AND TIPCTC='RE'
   GO TOP
   do fval_exi
   IF VCTRL
   SET RELATION TO nummes+numref  INTO MovBco ADDI
   DO REPORTE WITH 2,"LisOpenc"," Listado de Operaciones de Notas de Cargo "
   SET FILTER TO
   SET RELATION OFF INTO movbco
   ENDIF
   
   SET ORDER TO ASTPAT9  &&Para Notas de Contabilidad
   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") AND TIPCTC='RE'
   GO TOP
   do fval_exi
   IF VCTRL
   SET RELATION TO nummes+numref  INTO MovBco ADDI
   DO REPORTE WITH 2,"LisOpeno"," Listado de Operaciones de Notas de Contabilidad"
   SET FILTER TO
   SET RELATION OFF INTO movbco
   ENDIF
   
   SET ORDER TO ASTPAT14  &&Para Recibos de Ingreso
   SET FILTER TO PERIODO=VPER AND NumMes =PADL(ALLTRIM(vMes),2,"0") AND TIPCTC='RE'
   GO TOP
   do fval_exi
   IF VCTRL
   SET RELATION TO PERIODO+nummes+numref  INTO recing ADDI
   DO REPORTE WITH 2,"LisOperi"," Listado de Operaciones de Recibos de Ingreso "
   SET FILTER TO
   SET RELATION OFF INTO recing
   ENDIF

   SET ORDER TO ASTPAT15  &&Para Reversiones
   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0")  AND TIPCTC='RE'
   GO TOP
   do fval_exi
   IF VCTRL
   SET RELATION TO nummes+numref  INTO rever ADDI
   DO REPORTE WITH 2,"LisOpere"," Listado de Operaciones de Reversiones "
   SET FILTER TO
   SET RELATION OFF INTO rever
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
