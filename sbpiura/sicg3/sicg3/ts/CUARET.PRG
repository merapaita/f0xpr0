PARAMETERS newsistem
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ CuaRet.PRG  15/05/96                                L: 419   ³	
* ³ Cuadro de Retenciones                                        ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.     05/1997 ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
*-Abriendo archivos
USE parmae   IN 1  ORDER Parmae1  ALIAS parma 
USE Cajas    IN 2  ORDER Cajas1   ALIAS Caja
USE Reten    IN 3  ORDER Reten2   ALIAS Reten
USE Compag   IN 4  ORDER Compag1  ALIAS Compag
USE MaePre   IN 5  ORDER Maepre1  ALIAS Maepre

*---mensajes *------
vmens08 = 'No existe registros para procesar'
IF LASTKEY()=27
   CLOSE DATA
   RETURN
ENDIF
DO VerSal
close data
RETURN

PROCEDURE VerSal
*---------------
hide popup pop_01
sele reten
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp = RECNO()
 DEFINE WINDOW Lis FROM 5,5 TO 21,75 FLOAT DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW Lis
 STORE 1        TO vToCta,vTip
 STORE SPACE(14) TO vCta
 store space(6) to vrub

 STORE DATE()   TO vFecIni, vFecFin
 @ 00,01 SAY "   Todas las Ctas : " GET vToCta  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToCta,3,22)
 @ 02,01 SAY "   Cta. Corriente : "
 @ 02,22 GET vCta   WHEN vToCta=2 VALID Val_Fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,2,22)
 IF newsistem ='1'
 	@ 04,01 SAY "     Tipo Reporte : " GET vTip  FUNCTION '^ Gen‚rico;Detallado;Conciliaci¢n;Conciliaci¢n x Rubro;Retenciones x Rubro;Tributos Gen‚rico;Tributos Detallado;AFP Gen‚rico;AFP Detallado;Retenc.Pend.Gen‚rico;Retenc.Pend.Detallado;Fondo Garantia'
 ELSE
 	@ 04,01 SAY "     Tipo Reporte : " GET vTip  FUNCTION '^ Gen‚rico;Detallado;Conciliaci¢n;Conciliaci¢n x Rubro;Retenciones x Rubro;Tributos Gen‚rico;Tributos Detallado;AFP Gen‚rico;AFP Detallado;Retenc.Pend.Gen‚rico;Retenc.Pend.Detallado;Ret.x Meta Gen‚r.;Ret.x Meta Detall.' 
 ENDIF
 *@ 05,01 SAY "     Tipo Reporte : " GET vTip  FUNCTION '^ Gen‚rico;Detallado'
 @ 08,01 SAY "            Rubro : "
 @ 08,22 GET vrub PICTURE "!!!" VALID VAL_PARAD (vrub, 'CODRET', ' ', 22) WHEN (VTOCTA=1 OR vToCta=2) AND (VTIP=4 OR vtip=5) 
 @ 10,01 SAY " Fecha de Emisi¢n : "
 @ 10,22 GET vFecIni  PICTURE '@D' COLOR SCHEME 7
 @ 10,34 GET vFecFin  PICTURE '@D' VALID (vFecFin >= vFecIni) COLOR SCHEME 7
 @ 13,18 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
 READ CYCLE

 RELEASE WINDOW Lis
 IF OkCancel = 1
    DEFINE WINDOW Xwait FROM 12,06 TO 14,78 COLOR SCHEME 05 
	ACTIVATE WINDOW Xwait
	@0,10 SAY " Espere un Momento...Reporte en Proceso!" COLOR W+/BR*
   SELECT Reten
   wind1=sys(3)+".IDX"
   wind2=sys(3)+".DBF"
   wind3=sys(3)+".IDX"
   wind4=sys(3)+".DBF"
   wind5=sys(3)+".IDX"
   DO CASE
      CASE vTip = 1 &&Reten. Gen‚rico
 	   INDEX ON RETEN.CODCTC+RETEN.CODRET TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND LEFT(NUMCP,1)$'B0123456789'
 	   GO TOP
	     deactivate window xwait
	   IF EOF()
	      DO standby WITH Vmens08
	   ELSE   
         DO REPORTE WITH 2,"LisRet"," Listado Retenciones ",2,.F.,.T.
         SET INDEX TO
       ENDIF   
      CASE vTip = 2 &&Reten. Detallado
         sele reten
         INDEX ON RETEN.CODCTC+RETEN.CODRET+RETEN.NUMMES+RETEN.NUMCP TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND CODCTC<>'631017312' AND CODCTC<>'631017320' AND LEFT(NUMCP,1)$'B0123456789'
         SET RELA TO RETEN.NUMMES+RETEN.NUMCP+RETEN.CODCTC INTO COMPAG
		 SET SKIP TO COMPAG
	     deactivate window xwait
	     IF EOF()
	        DO standby WITH Vmens08
         ELSE   
	         DO REPORTE WITH 2,"LisRetd"," Listado Retenciones En Detalle ",2,.F.,.T.
    	     set INDEX TO
         endif
      CASE vTip = 3 &&Conciliaci¢n
*      return		
          SELE RETEN
          INDEX ON CODCTC+CODRET TO (WIND5) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) UNIQUE
          COPY TO (WIND4)
          SELE COMPAG
          INDEX ON COMPAG.CODCTC+COMPAG.CODRET+COMPAG.NUMMES+COMPAG.NUMCP TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(COMPAG.FecCP,vFecIni,vFecFin) AND COMPAG.TIPDOC="RE" AND ESTADO<>'99' AND LEFT(NUMCP,1)<>'M' AND CODCTC<>'631017312' AND CODCTC<>'631017320'
          COPY TO (WIND2)
          SELE COMPAG
          SET ORDER TO COMPAG1
          sele reten
          SET ORDER TO RETEN1
          INDEX ON RETEN.CODCTC+RETEN.CODRET+RETEN.NUMMES+RETEN.NUMCP TO (WIND3) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND CODCTC<>'631017312' AND CODCTC<>'631017320' AND LEFT(NUMCP,1)$'B0123456789'
          USE (WIND2) IN 6 ALIAS TEMPO
          USE (WIND4) IN 7 ALIAS RET
           deactivate window xwait
	     IF EOF()
	        DO standby WITH Vmens08
         ELSE   
	         DO REPPRG  WITH "LISRETEJ"," Arqueo de Retenciones "
    	     set INDEX TO
         endif
      CASE vTip = 4 &&Conciliaci¢n x Rubro
*      	 return
          SELE RETEN
          INDEX ON RETEN.CODCTC+RETEN.CODRET+RETEN.NUMMES+RETEN.NUMCP TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND RETEN.CODRET=VRUB AND LEFT(NUMCP,1)$'B0123456789'
          IF EOF()
             deactivate window xwait
             DO STANDBY with vmens08
          ELSE   
          SELE COMPAG
          INDEX ON COMPAG.CODCTC+COMPAG.CODRET+COMPAG.NUMMES+COMPAG.NUMCP TO (WIND3) FOR cODCtc=vCta and BETWEEN(COMPAG.FecCP,vFecIni,vFecFin) AND COMPAG.CODRET=SUBSTR(VRUB,1,4) AND ESTADO<>'99' AND LEFT(NUMCP,1)$'B0123456789'
          COPY TO (WIND2)
          SELE COMPAG
          SET ORDER TO COMPAG1
          USE (WIND2) IN 6 ALIAS TEMPO
	      deactivate window xwait
	      DO REPPRG  WITH "LISRETRU"," Arqueo de Retenciones "
	      ENDIF
          set INDEX TO
      CASE vTip = 5 &&Reten. x Rubro
          SELE RETEN
          INDEX ON RETEN.CODCTC+RETEN.CODRET+RETEN.NUMMES+RETEN.NUMCP TO (WIND1) FOR IIF(VTOCTA=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND ALLTRIM(VRUB)=ALLTRIM(RETEN.CODRET) AND ESTADO<>'99' AND LEFT(NUMCP,1)$'B0123456789'

*          AND CODCTC<>'631017312' AND CODCTC<>'631017320' AND LEFT(NUMCP,1)$'B0123456789'
          IF EOF()
		     deactivate window xwait
             DO STANDBY with vmens08
          ELSE   
          SET RELA TO RETEN.NUMMES+RETEN.NUMCP+RETEN.CODCTC INTO COMPAG
	      deactivate window xwait
          DO REPORTE WITH 2,"LiRetru"," Listado Retenciones por Rubro",2,.F.,.T.
	      ENDIF
          set INDEX TO
      CASE vTip = 6 &&Tributo Gen‚rico
 	   INDEX ON RETEN.CODCTC+RETEN.CODRET TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND CODCTC<>'631017312' AND CODCTC<>'631017320' AND LEFT(NUMCP,1)$'B0123456789' AND tributo='R'
 	   GO TOP
	     deactivate window xwait
	   IF EOF()
	      DO standby WITH Vmens08
	   ELSE   
         DO REPORTE WITH 2,"LisRet"," Listado Retenciones ",2,.F.,.T.
         SET INDEX TO
       ENDIF   
      CASE vTip = 7 &&Tributo Detallado
         sele reten
         INDEX ON RETEN.CODCTC+RETEN.CODRET+RETEN.NUMMES+RETEN.NUMCP TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND CODCTC<>'631017312' AND CODCTC<>'631017320'  AND LEFT(NUMCP,1)$'B0123456789' AND tributo='R'
         SET RELA TO RETEN.NUMMES+RETEN.NUMCP+RETEN.CODCTC INTO COMPAG
		 SET SKIP TO COMPAG
	     deactivate window xwait
	     IF EOF()
	        DO standby WITH Vmens08
         ELSE   
	         DO REPORTE WITH 2,"LisRetd"," Listado Retenciones Tributos En Detalle ",2,.F.,.T.
    	     set INDEX TO
         endif
      CASE vTip = 8 &&AFP Gen‚rico
 	   INDEX ON RETEN.CODCTC+RETEN.CODRET TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND CODCTC<>'631017312' AND CODCTC<>'631017320' AND LEFT(NUMCP,1)$'B0123456789' AND tributo='F'
 	   GO TOP
	     deactivate window xwait
	   IF EOF()
	      DO standby WITH Vmens08
	   ELSE   
         DO REPORTE WITH 2,"LisReta"," Listado de AFP G‚nerico ",2,.F.,.T.
         SET INDEX TO
       ENDIF   
      CASE vTip = 9 &&AFP Detallado
         sele reten
         INDEX ON RETEN.CODCTC+RETEN.CODRET+RETEN.NUMMES+RETEN.NUMCP TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND CODCTC<>'631017312' AND CODCTC<>'631017320'  AND LEFT(NUMCP,1)$'B0123456789' AND tributo='F'
         SET RELA TO RETEN.NUMMES+RETEN.NUMCP+RETEN.CODCTC INTO COMPAG
		 SET SKIP TO COMPAG
	     deactivate window xwait
	     IF EOF()
	        DO standby WITH Vmens08
         ELSE   
	         DO REPORTE WITH 2,"LiRetda"," Listado de AFP Detallado ",2,.F.,.T.
    	     set INDEX TO
         endif
       CASE vTip = 10 &&Reten. Pend. Gen‚rico
*         return
         SELECT reten
         SET FILTER TO EMPTY(conpago) AND Fecret>=vfecini AND Fecret<=vfecfin AND IIF(vtocta=1,.t.,ALLTRIM(Codctc)=ALLTRIM(vcta)) AND LEFT(NUMCP,1)$'B0123456789'
 	     deactivate window xwait
         DO REPORTE WITH 2,"RetPen"," Listado Retenciones Pendientes ",2,.F.,.T.
         SET FILTER TO
      CASE vTip = 11 &&Reten. Pend. Detallado
 *        return
         SELECT reten
         SET FILTER TO EMPTY(conpago) AND Fecret>=vfecini AND Fecret<=vfecfin AND IIF(vtocta=1,.t.,ALLTRIM(Codctc)=ALLTRIM(vcta)) AND LEFT(NUMCP,1)$'B0123456789'
         SET RELA TO RETEN.NUMMES+RETEN.NUMCP+RETEN.CODCTC INTO COMPAG
		 SET SKIP TO COMPAG
 	     deactivate window xwait
         *DO REPORTE WITH 2,"RetPend"," Listado Retenciones Pendientes "
         DO REPORTE WITH 2,"LisRetd"," Listado Retenciones Pendientes ",1,.F.,.T.
         SET RELATION TO
         SET FILTER TO
    CASE vTip = 12 &&Reten. x Meta Gen‚rico
    	IF newsistem ='1'
    		*
			sele reten
			INDEX ON RETEN.CODCTC+RETEN.CODRET+RETEN.NUMMES+RETEN.NUMCP TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND CODCTC<>'631017312' AND CODCTC<>'631017320'  AND LEFT(NUMCP,1)$'B0123456789' AND tributo='G'
			SET RELA TO RETEN.NUMMES+RETEN.NUMCP+RETEN.CODCTC INTO COMPAG
			SET SKIP TO COMPAG
			deactivate window xwait
			IF EOF()
		        DO standby WITH Vmens08
			ELSE   
				DO REPORTE WITH 2,"LiRetdF"," Listado de AFP Detallado ",2,.F.,.T.
				set INDEX TO
			endif
			*
		ELSE    		
			SELE RETEN
	  	   INDEX ON RETEN.CODCTC+RETEN.CODCAD+RETEN.CODRET TO (Wind1) ASCENDING FOR iif(vToCta=1,.t.,CodCtc=vCta)  AND BETWEEN(reten.fecret,vfecini,vfecfin) AND CODCTC<>'631017312' AND CODCTC<>'631017320' AND LEFT(NUMCP,1)$'B0123456789'
	 	   *INDEX ON RETEN.CODCTC+RETEN.CODRET TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND CODCTC<>'631017312' AND CODCTC<>'631017320' AND LEFT(NUMCP,1)$'0123456789'
	 	   GO TOP
		     deactivate window xwait
		   IF EOF()
		      DO standby WITH Vmens08
		   ELSE   
	         DO REPORTE WITH 2,"Lis"," Listado Retenciones x Meta G. ",2,.F.,.T.
	         SET INDEX TO
	       ENDIF   
		ENDIF
      CASE vTip = 13 &&Reten. x Meta Detallado
         sele reten
 		 INDEX ON RETEN.CODCTC+RETEN.CODCAD+RETEN.CODRET+RETEN.NUMMES+RETEN.NUMCP TO (Wind1) ASCENDING FOR iif(vToCta=1,.t.,CodCtc=vCta)  AND BETWEEN(reten.fecret,vfecini,vfecfin) AND CODCTC<>'631017312' AND CODCTC<>'631017320' AND LEFT(NUMCP,1)$'B0123456789'
         *INDEX ON RETEN.CODCTC+RETEN.CODRET+RETEN.NUMMES+RETEN.NUMCP TO (WIND1) FOR iif(vToCta=1,.t.,CodCtc=vCta) and BETWEEN(FecReT,vFecIni,vFecFin) AND CODCTC<>'631017312' AND CODCTC<>'631017320' AND LEFT(NUMCP,1)$'0123456789'
         SET RELA TO RETEN.NUMMES+RETEN.NUMCP+RETEN.CODCTC INTO COMPAG
	 	 SET SKIP TO COMPAG
	     deactivate window xwait
	     IF EOF()
	        DO standby WITH Vmens08
         ELSE   
	         DO REPORTE WITH 2,"LisReXM"," Listado Retenciones x Meta D.",2,.F.,.T.
    	     set INDEX TO
         endif
	ENDCASE	  
 ENDIF
 SELECT reten
 set filt to
 GO vtemp
 show popup pop_01
RETURN



PROCEDURE LISRETEJ
*----------------
PARAMETERS XCOP
PRIVATE FILA,VRET,R1,RT,RP,E1,ET,EP,VCTC
STORE 0 TO R1,RT,E1,ET,EP,RP
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
SELE RET
@1,1 SAY CHR(15)
fila=8
DO imp_header  
VRET=ALLTRIM(RET.CODCTC)+ALLTRIM(RET.CODRET)
VCTC=ALLTRIM(RET.CODCTC)
IF VTOCTA=1    
FILA=FILA+1
@FILA,33  say "CUENTA CORRIENTE : "+ALLTRIM(Val_Fun('Caja','RET.CodCtc',"ALLTRIM(Codctc)+' '+Descri",RET.CodCtc))
FILA=FILA+1
@FILA,2 SAY REPLICATE("Ä",162)
FILA=FILA+1
ENDIF
SCAN
VRET=ALLTRIM(RET.CODCTC)+ALLTRIM(RET.CODRET)
	IF ALLTRIM(RET.CODCTC)<>VCTC AND VTOCTA=1
	    FILA=FILA+1
	    @FILA,2 SAY REPLICATE("Ä",162)
	    FILA=FILA+1
		@FILA,22 SAY "TOTAL RETENIDO CTACTE=>"
		@FILA,64 SAY RP PICTURE "@Z 999,999,999.99"
		@FILA,102 SAY "TOTAL EJECUTADO CTACTE=>"
		@FILA,143 SAY EP PICTURE "@Z 999,999,999.99"
	    FILA=FILA+1
	    @FILA,2 SAY REPLICATE("Ä",162)
	    FILA=FILA+1
	    STORE 0 TO RP,EP
		FILA=8
		DO IMP_HEADER
		@FILA,33  say "CUENTA CORRIENTE : "+ALLTRIM(Val_Fun('Caja','RET.CodCtc',"ALLTRIModctc)+' '+Descri",RET.CodCtc))
	    FILA=FILA+1
	    @FILA,2 SAY REPLICATE("Ä",162)
	    FILA=FILA+1
    ENDIF
    @FILA,3 SAY RET.CODRET
    @FILA,12 SAY RET.NOMRET
    FILA=FILA+1
    @FILA,2 SAY REPLICATE("-",162)
    FILA=FILA+1
	SELE RETEN
	GO TOP
	LOCATE FOR ALLTRIM(RETEN.CODCTC)+ALLTRIM(RETEN.CODRET)=VRET   
	IF FOUND()
		SCAN WHILE ALLTRIM(CODCTC)+ALLTRIM(RETEN.CODRET)=VRET  &&Retenido
		    VKEY=RETEN.NUMMES+RETEN.NUMCP+RETEN.CODCTC
			@FILA,4 SAY reten.numcp
			SELE COMPAG
			SEEK VKEY
			@FILA,12 SAY SUBST(COMPAG.glosa,1,50)	
			SELE RETEN
			@FILA,64 SAY reten.valret PICTURE "@Z 999,999,999.99"
			FILA=FILA+1
			SELE COMPAG
			@FILA,12 SAY SUBST(COMPAG.glosa,51,50)	
			FILA=FILA+1
			@FILA,12 SAY SUBST(COMPAG.glosa,100,50)	
			SELE RETEN
			R1=R1+RETEN.VALRET
			RP=RP+RETEN.VALRET
			FILA=FILA+1
			IF FILA>=60
				FILA=8
				DO IMP_HEADER
			ENDIF   
		ENDSCAN
	ENDIF	
	RT=RT+R1
    FILA=FILA+1
	SELE TEMPO
	GO TOP
	LOCATE FOR ALLTRIM(TEMPO.CODCTC)+ALLTRIM(TEMPO.CODRET)=VRET
	IF FOUND()
		SCAN WHILE ALLTRIM(TEMPO.CODCTC)+ALLTRIM(TEMPO.CODRET)=VRET  &&Ejecutado
			@FILA,82 SAY TEMPO.numcp
			@FILA,92 SAY SUBST(TEMPO.glosa,1,50)	
			@FILA,143 SAY TEMPO.IMPORT PICTURE "@Z 999,999,999.99"
			FILA=FILA+1
			@FILA,92 SAY SUBST(TEMPO.glosa,51,50)	
			FILA=FILA+1
			@FILA,92 SAY SUBST(TEMPO.glosa,100,50)	
			E1=E1+TEMPO.IMPORT
			EP=EP+TEMPO.IMPORT
	 		FILA=FILA+1
			IF FILA>=60
				FILA=8
				DO IMP_HEADER
			ENDIF   
		ENDSCAN
	ENDIF
	SELE RET
	ET=ET+E1
    FILA=FILA+1
    @FILA,2 SAY REPLICATE("Ä",162)
    FILA=FILA+1
	@FILA,22 SAY "TOTAL RETENIDO =>"
	@FILA,64 SAY R1 PICTURE "@Z 999,999,999.99"
	@FILA,102 SAY "TOTAL EJECUTADO =>"
	@FILA,143 SAY E1 PICTURE "@Z 999,999,999.99"
    FILA=FILA+1
	STORE 0 TO E1,R1	
    @FILA,2 SAY REPLICATE("Ä",162)
    FILA=FILA+1
	IF FILA=60
		FILA=8
		DO IMP_HEADER
	ENDIF   
	IF VTOCTA=1
	    VCTC=ALLTRIM(RET.CODCTC)
    ENDIF
ENDSCAN     
IF FILA>=60
FILA=8
DO IMP_HEADER
ENDIF   
FILA=FILA+1
@FILA,2 SAY REPLICATE("Ä",162)
FILA=FILA+1
@FILA,22 SAY "TOTAL GENERAL RETENIDO =>"
@FILA,64 SAY RT PICTURE "@Z 999,999,999.99"
@FILA,102 SAY "TOTAL GENERAL EJECUTADO =>"
@FILA,143 SAY ET PICTURE "@Z 999,999,999.99"
FILA=FILA+1
@FILA,2 SAY REPLICATE("Ä",162)
SET DEVICE TO SCREEN 
RETURN



PROCEDURE LISRETRU
*----------------
PARAMETERS XCOP
PRIVATE FILA,VRET,R1,RT,RP,E1,ET,EP,VCTC
STORE 0 TO R1,RT,E1,ET,EP,RP
DO CASE
	CASE _DEST1=1
		SET DEVICE TO fILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
@1,1 SAY CHR(15)
fila=8
DO imp_headRUB
SELE RETEN
GO TOP
SCAN WHILE !EOF()
     VKEY=RETEN.NUMMES+RETEN.NUMCP+RETEN.CODCTC
     @FILA,4 SAY reten.numcp
	 SELE COMPAG
	 SEEK VKEY
	 @FILA,12 SAY SUBST(COMPAG.glosa,1,50)	
	 SELE RETEN
	 @FILA,64 SAY reten.valret PICTURE "@Z 999,999,999.99"
	 FILA=FILA+1
	 SELE COMPAG
	 @FILA,12 SAY SUBST(COMPAG.glosa,51,50)	
	 FILA=FILA+1
	 @FILA,12 SAY SUBST(COMPAG.glosa,100,50)	
	 SELE RETEN
	 R1=R1+RETEN.VALRET
     FILA=FILA+1
	 IF FILA>=60
		FILA=8
		DO IMP_HEADRUB
     ENDIF   
    ENDSCAN 
    FILA=FILA+1
	SELE TEMPO
	SCAN WHILE !EOF()
			@FILA,82 SAY TEMPO.numcp
			@FILA,92 SAY SUBST(TEMPO.glosa,1,50)	
			@FILA,143 SAY TEMPO.IMPORT PICTURE "@Z 999,999,999.99"
			FILA=FILA+1
			@FILA,92 SAY SUBST(TEMPO.glosa,51,50)	
			FILA=FILA+1
			@FILA,92 SAY SUBST(TEMPO.glosa,100,50)	
			E1=E1+TEMPO.IMPORT
			FILA=FILA+1
			IF FILA>=60
				FILA=8
				DO IMP_HEADRUB
			ENDIF   
	ENDSCAN
    FILA=FILA+1
    @FILA,2 SAY REPLICATE("Ä",162)
    FILA=FILA+1
	@FILA,22 SAY "TOTAL RETENIDO =>"
	@FILA,64 SAY R1 PICTURE "@Z 999,999,999.99"
	@FILA,102 SAY "TOTAL EJECUTADO =>"
	@FILA,143 SAY E1 PICTURE "@Z 999,999,999.99"
    FILA=FILA+1
    @FILA,2 SAY REPLICATE("Ä",162)
    FILA=FILA+1
	IF FILA=60
		FILA=8
		DO IMP_HEADRUB
	ENDIF   
SET DEVICE TO SCREEN 
RETURN




PROCEDURE imp_headRUB
*--------------------
@1,3   SAY ALLTRIM(CIA)
@1,145 say "PAG:"
@1,152 SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "CuaretRub"
@2,55  say 'CONCILIACION DE RETENCIONES: RUBRO '+ALLTRIM(VRUB)+' '+RETEN.NOMRET
*2,95 SAY VAL_PARA(alltrim(vrub),'CODRET',' ',' ',' ')
@2,145 SAY "FECHA:"
@2,152 SAY DATE()           
@3,55  say 'DEL '+DTOC(VFECINI)+'  AL  '+DTOC(VFECFIN)
@4,20  say "CUENTA CORRIENTE : "+ALLTRIM(VCTA)
@04,55 SAY val_fun('Caja','codctc','Descri',ALLTRIM(VCTA))
@5,2 SAY REPLICATE("Ä",162)
@6,3 SAY " C/P           DETALLE                                                  MONTO         C/P            DETALLE                                         MONTO"
@7,2 SAY REPLICATE("Ä",162) 
RETURN


PROCEDURE imp_headER
*--------------------
@1,3   SAY ALLTRIM(CIA)
@1,215 say "PAG:"
@1,220 SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "ConRet"
@2,70  say 'CONCILIACION DE RETENCIONES: RUBRO '+ALLTRIM(VRUB)+' '+RETEN.NOMRET
@2,215 SAY "FECHA:"
@2,222 SAY DATE()           
@3,70  say 'DEL '+ DTOC(VFECINI)+'  AL  '+DTOC(VFECFIN)
@4,60  say "CUENTA CORRIENTE : "+ALLTRIM(VCTA)
@04,97 SAY val_fun('Caja','codctc','Descri',ALLTRIM(VCTA))
@5,2 SAY REPLICATE("Ä",162)
@6,3 SAY " C/P           DETALLE                                                  MONTO         C/P            DETALLE                                         MONTO"
@7,2 SAY REPLICATE("Ä",162) 
RETURN