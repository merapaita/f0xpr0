* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ SegChq.PRG  15/02/96                               L:  115   ³	
* ³ Seguimiento de cheques                                       ³
* ³ Autor   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
* estados
*  00 Girado
*  99 Anulado
*  25 Rebajado con H/A
*  10 Entregado (40)
*-Abriendo archivos
USE Parmae   IN 1  ORDER Parmae1  ALIAS Parma
USE ComPag   IN 2  ORDER ComPag1  ALIAS ComPag
USE Cajas    IN 3  ORDER Cajas1   ALIAS Caja
USE Cheque   IN 4  ORDER Cheque1  ALIAS Cheque
USE hojanu   IN 5  ORDER hojanu2  ALIAS hojanu
vmens01='No existen registros a Procesar'

DEFINE WINDOW LisPrg FROM 2,0 TO 23,79 COLOR SCHEME 10 ;
   TITLE " Listados "
   HIDE POPUP POP_01
   store 1 to vlista
   STORE DATE() TO vfecini, vfecfin
   DEFINE WINDOW CC FROM 03,26 TO 20,78  FLOAT DOUBLE COLOR SCHEME 5
   ACTIVATE WINDOW CC
   @ 01,02 SAY "     Tipo Listado       : " GET vlista  FUNCTION '^ Girados;Entregados;Anulados'
   @ 06,02 SAY "     Cuenta Corriente N§: " GET vCtaCte PICTURE "@!" ;
   VALID Val_Fun('Caja','codctc',"codctc+' '+Descri",vCtaCte,1,8,10) ;
   DEFAULT SPACE(14)
   @ 10,02 SAY "       Fecha de Emisi¢n : "
   @ 10,29 GET vfecini  PICTURE '@D' COLOR SCHEME 7   
   @ 10,39 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7 
   @ 13,12 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
   READ CYCLE
   IF LASTKEY()=27
      RELEASE WINDOW CC
   RETURN
   ENDIF   
   RELEASE WINDOW CC
   IF okcancel = 1
     RELEASE WINDOW CC
     DEFINE WINDOW Xwait FROM 12,25 TO 14,78 COLOR SCHEME 05 
     ACTIVATE WINDOW Xwait
     @0,1 SAY "  Espere un Momento...Reporte en Proceso!" COLOR W+/BR*
	 DO CASE 
        CASE vlista=1
            SELE Cheque
            SET RELATION TO nummes+numcp+codctc INTO ComPag
       		SELE Cheque
       		VIND=SYS(3)+".IDX"
       		INDEX ON nummes+numcp+codctc to (vind) FOR codctc=vctaCTE AND BETWEEN(fecCHQ,vfecini,vfecfin) AND estado#"99" 		&& AND CODCTC<>'631003044'	&&Girado
*      		INDEX ON nummes+numcp+codctc to (vind) FOR codctc=vctaCTE AND BETWEEN(fecCHQ,vfecini,vfecfin) AND estado="00" AND CODCTC<>'631003044'	&&Girado
       		IF EOF()
       	        DEACTIVATE WINDOW xwait
       			DO STANDBY WITH vmens01
       			RETURN
       		ENDIF	
       	    DEACTIVATE WINDOW xwait
		    DO reporte WITH 2,"CHQASIG",' Cheques Girados '
			SET INDEX TO
			SET ORDER TO cheque1            
			SET RELA OFF INTO ComPag
*       CASE vlista=2
*            SELE CHEQUE
*            SET RELATION TO nummes+numcp+codctc INTO ComPag
*       		SELE CHEQUE
*       		VIND=SYS(3)+".IDX"
*       		INDEX ON nummes+numcp+codctc to (vind) FOR codctc=vctaCTE AND BETWEEN(fecautt,vfecini,vfecfin) AND estado="10"  AND CODCTC<>'631003044'&&Teso
*       		IF EOF()
*       	        DEACTIVATE WINDOW xwait
*       			DO STANDBY WITH vmens01
*       			RETURN
*       		ENDIF	
*       	    DEACTIVATE WINDOW xwait
*		    do reporte with 2,"CHQAUTT",' Cheques Autorizados por Tesorer¡a '
*			SET INDEX TO
*			SET ORDER TO cheque1            
*			SET RELA OFF INTO ComPag
       CASE vlista=2
            SELE CHEQUE
            SET RELATION TO nummes+numcp+codctc INTO ComPag
       		SELE CHEQUE
       		VIND=SYS(3)+".IDX"
       		INDEX ON nummes+numcp+codctc to (vind) FOR codctc=vctaCTE AND BETWEEN(fecent,vfecini,vfecfin) AND estado#"99" AND ConEnt = 'û'		&&  AND CODCTC<>'631003044'&&Entregado
*       		INDEX ON nummes+numcp+codctc to (vind) FOR codctc=vctaCTE AND BETWEEN(fecent,vfecini,vfecfin) AND estado="10" AND CODCTC<>'631003044'&&Entregado
			IF EOF()
				DEACTIVATE WINDOW xwait
				DO STANDBY WITH vmens01
				RETURN
       		ENDIF	
       	    DEACTIVATE WINDOW xwait
		    DO reporte with 2,"CHQENTR",' Cheques Autorizados por Administraci¢n '
			SET INDEX TO
			SET ORDER TO cheque1            
			SET RELA OFF INTO ComPag
       CASE vlista=3
            SELE CHEQUE
            SET RELATION TO nummes+numcp+codctc INTO ComPag
            SET RELATION TO nummesHA+NUMHA+codctc INTO hojanu
       		SELE CHEQUE
       		VIND=SYS(3)+".IDX"
       		INDEX ON nummes+numcp+codctc to (vind) FOR codctc=vctaCTE AND BETWEEN(fecanu,vfecini,vfecfin) AND estado="99"
       		IF EOF()
       	        DEACTIVATE WINDOW xwait
       			DO STANDBY WITH vmens01
       			RETURN
       		ENDIF	
       	    DEACTIVATE WINDOW xwait
		    DO reporte with 2,"CHQANUL",' Cheques Anulados '
			SET INDEX TO
			SET ORDER TO cheque1            
			SET RELA OFF INTO ComPag
			SET RELA OFF INTO hojanu
    ENDCASE        
   ENDIF   
RELEASE WINDOW LisPrg
RELEASE WINDOW xwait
close data
RETURN