* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ Anticip.Prg                                                  ³	
* ³ Reporte de C/P por anticipos                                 ³
* ³ AUTOR   : Javier Fr¡as Gomez                                 ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

PUBLIC vfecini,vfecfin

*- Abriendo Archivos

CLOSE DATA
USE ComPag   IN 02  ORDER TAG ComPag1      ALIAS ComPag 
USE IteCp    IN 03  ORDER TAG IteCp1       ALIAS IteCp  
USE HojCon   IN 04  ORDER TAG HojCon1      ALIAS Hoja   
USE IteHc    IN 05  ORDER TAG IteHc1       ALIAS IteHc 
USE Cajas    IN 06  ORDER TAG Cajas2       ALIAS Caja
USE Cheque   IN 07  ORDER TAG Cheque1      ALIAS Cheque
USE Auxil    IN 08  ORDER TAG Auxil1       ALIAS Auxi
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
USE cuentas  IN 10  ORDER TAG cuentas1     ALIAS cuenta
USE Personal IN 11  ORDER TAG Personal2    ALIAS Personal
USE Promae   IN 12  order tag Promae1      ALIAS Promae

*- Inicia proceso
DO inicia                    && Define ventanas, men£s, t¡tulos
DO fin_opcion
RETURN


PROCEDURE inicia
*---------------
SELECT ComPag
VRECNO=RECNO()
SCATTER MEMVAR
vtemp = RECNO()
DEFINE WINDOW lis FROM 3,10 TO 21,70 FLOAT DOUBLE COLOR SCHEME 5 title ' ± Comprobantes de Pago por Anticipos ± '
ACTIVATE WINDOW lis
STORE 1  TO vtocta,vesta,Vsino,vlista
STORE SPACE(14) TO vCta,vCuenta
STORE SPACE(2)  TO vAno,vMes
STORE SPACE(4)  TO vCli
STORE SPACE(212) TO vobs
vcta=space(len(m.codctc))
Vano=m.nummes
Vcli=m.numcp
vcta1=space(3)

m.tipprv=' '
m.codprv=space(4)
m.codemp=space(5)
m.codotr=space(6)

STORE DATE() TO vfecini, vfecfin
@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ De una Cta.Cte,;Todas las Ctas.Ctes'
@ 05,01 SAY "   Cta. Corriente : "
@ 05,22 GET vcta   VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,08,07);
        WHEN vlista=1
@ 07,01 SAY "     Fecha Desde  : "
@ 07,22 GET vfecini  PICTURE '@D'  
@ 07,32 SAY 'Hasta: '
@ 07,40 GET vfecfin  PICTURE '@D'  VALID (vfecfin >= vfecini)  
@ 09,01 say "  Cuenta contable : " GET vcta1

@ 10,01 SAY "        Proveedor : " get m.TipPrv  PICTURE '@M P,E,O' 
@ 10,25 GET m.Codprv    PICTURE '!!!!'   VALID Val_prv1(m.Codprv,'20',' ',19)   WHEN m.TipPrv='P'
@ 10,25 GET m.CodEmp    PICTURE '!!!!!'  VALID Val_prv1(m.CodEmp,'30',' ',19)   WHEN m.TipPrv='E'
@ 10,25 GET m.Codotr    PICTURE '!!!!!!' VALID Val_prv1(m.Codotr,'09',' ',19)   when m.TipPrv='O'

@ 12,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW lis

IF okcancel = 1
   DO CASE
      CASE vLista=1   && De una cta.cte.
           xind=SYS(3)+".IDX"
           SELECT astpat
           DO Espera WITH 1,'Espere un momento; Procesando Informaci¢n ...'
           INDEX ON LEFT(CODCTA,3)+NUMMES+NUMREF+CODCTC TO (XIND) FOR ALLT(codctc)=ALLT(vcta) AND BETWEEN(fecHA,vfecini,vfecfin)	AND (LEFT(CODCTA,2)='38') AND TIPDOC='C/P' AND IIF(EMPTY(vcta1),.T.,LEFT(CODCTA,3)$vcta1)
           SET RELATION TO nummes+numref+codctc  INTO ComPag ADDI
           SET RELATION TO nummes+numref+codctc  INTO Cheque ADDI
           DO Espera WITH 2
           if !empty(m.codprv) or !empty(m.codemp) or !empty(m.codotr)
              set filt to compag.tipprv=m.tipprv and iif(m.tipprv='P',allt(m.codprv)=allt(compag.codprv),iif(m.tipprv='E',allt(m.codemp)=allt(compag.codemp),allt(m.codotr)=allt(compag.codotr)))
           endif
		   IF EOF()
		      DO STANDBY WITH "No hay registros para procesar"
		      SET INDEX TO
		      ERASE(xind)
    		  RETURN
    	   ENDIF  
           DO reporte WITH 2,"lisantic"," Resumen de Comprobantes de Pago de Anticipos "                                  	       
           SET INDEX TO
           ERASE(xind)
    CASE vlista=2   && De todas las ctas. ctes.
           xind=SYS(3)+".IDX"
           SELECT astpat
           DO Espera WITH 1,'Espere un momento; Procesando Informaci¢n ...'
           INDEX ON CODCTC+LEFT(CODCTA,3)+NUMMES+NUMREF TO (XIND) FOR BETWEEN(fecHA,vfecini,vfecfin) AND (LEFT(CODCTA,2)='38') AND TIPDOC='C/P' AND IIF(EMPTY(vcta1),.T.,LEFT(CODCTA,3)$vcta1)
           SET RELATION TO nummes+numref+codctc  INTO ComPag ADDI
           SET RELATION TO nummes+numref+codctc  INTO Cheque ADDI
           DO Espera WITH 2 
           if !empty(m.codprv) or !empty(m.codemp) or !empty(m.codotr)
              set filt to compag.tipprv=m.tipprv and iif(m.tipprv='P',allt(m.codprv)=allt(compag.codprv),iif(m.tipprv='E',allt(m.codemp)=allt(compag.codemp),allt(m.codotr)=allt(compag.codotr)))
           endif
		   IF EOF()
		      DO STANDBY WITH "No hay informaci¢n para procesar."
		      SET INDEX TO
		      ERASE(xind)
    		  RETURN
    	   ENDIF   
           DO reporte WITH 2,"lisantic"," Resumen de Comprobantes de Pago de Anticipos "                                  	       
           SET INDEX TO
           ERASE(xind)
    ENDCASE
	SELE ComPag
ENDIF
RETURN


PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RESTORE SCREEN FROM principal
RETURN


FUNCTION glosa
*--------------
vGlosa = compag.Glosa
vGlosa = Subs(compag.Glosa,25,200-25)
RETURN ALLTRIM(UPPER(vGlosa))


FUNCTION CUENTA1
*----------------
PARAMETER vcuen
vAli=SELECT()
vRet=''
SELE Cuenta
SEEK PADR(vcuen,10,'0')
vret = cuenta.descri
SELECT (vAli)
RETURN vret

FUNCTION VAL_PRO
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
private medita, mmsg, malias, v_fun, _oldwind,_campo

medita = (parameters()>=2)
mmsg   = (parameters()=4) .and.  _tipo

_campo = varread()

malias = alias()
select PERSONAL
_oldwnd = woutput()

If !medita
   SET ORDE TO 1
   seek xcod
   v_fun = iif(found(),Descri,"")
else
   if empty(xcod)
      set orde to 2
      on key label ENTER keyboard chr(23)
      define window _xx from 3,22 to 22,77
      browse window _xx title ' ®Enter¯  Selecciona   ' nolgrid noedit noappend nodelete nomenu fields;
         codigo   :h='C¢digo'     ,;
         descri   :h='Nombre'
       * dirpro   :h='Direccci¢n' :25
      on key label ENTER
      release window _xx
      set order to 2
      if !empty(_oldwnd)
         activate window &_oldwnd
      endif
      if lastkey()=27
         v_fun = .f.
      else
         xcod = codigo
         if mmsg
            @ _x,_y say descri
         endif
         select (malias)
         if !_tipo
            replace &_campo with  xcod
         endif
         v_fun = .t.
      endif
   else
      SET ORDE TO 1
      seek xcod
      if mmsg .and. found()
         @ _x,_y say descri
      endif
      v_fun = found()
   endif
endif
m.Codprv='    '
select (malias)
return v_fun

