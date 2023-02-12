*PROGRAMA REGMB
*PARTE INGRESA
* @  9,22 SAY IIF(m.hojcon='S',m.numhc+'.'+m.nummeshc,' ')
* IF !EMPTY(m.codcad) 
* 	DO vis_calen
* ENDIF
* @ 13,22 SAY m.numref
* @ 14,22 SAY m.Monto    PICTURE '999,999,999,999.99'
* @ 15,22 SAY IIF(m.transa='ABO',Val_para(m.Forma,'FORMAS','V',22),' ')
* @ 16,22 SAY m.Nombre SIZE 1,40
* @ 17,22 SAY m.Concept
* IF ALLTRIM(m.tipdoc)='N/C' AND (ALLTRIM(m.motivo)='15'  OR ALLTRIM(m.motivo)='14' )
*     @ 18,22 SAY m.codpart
*	 @ 18,28 SAY VAL_PART(SUBSTR(m.codpart,4,2),LEFT(m.codpart,2),'D',22) 
* ENDIF	 
* @ 19,22 SAY IIF(m.tipcla='S',m.parting,' ')
* @ 19,34 SAY IIF(m.tipcla='S',vdpart(m.parting),' ') 
* @ 20,22 SAY m.NummesC            PICTURE '99'
* @ 20,62 SAY m.FechaC             PICTURE '@E'

********
*SUSP
*    IF m.hojcon='S'
*	   OI=carhc()
 *     	 IF LASTKEY()=27 OR !OI
	*		 SELECT Movb
	*		 DO vista
	*		 RETURN
	*	  ENDIF
	*	 SELE movb 
	 *    @  9,28 SAY m.numhc+'.'+m.nummeshc
	*	 @ 10,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',22,40)
	*	 @ 11,22 SAY val_subp(SUBSTR(m.codcal,10,3),'CODSUB'+SUBSTR(m.codcal,8,2)+'    ','V',22,40)
	 *    @ 12,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.periodo+substr(m.CodCal, 8,5),'V',22,40,4)
*	ELSE     
*		 @ 10,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) 
*		 @ 11,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
*		 @ 12,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  
 *   ENDIF



****
*programa : REGCP
opcion LISTA
	  CASE vLista = 4
		    ACTIVATE WINDOW Xwait
		    @0,0 SAY " Procesando reporte...." COLOR W+/BR*
			USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
			SELE Astpat
			SELECT ComPag
			vrec1=RECNO()
			vord1=ORDER()
			IF vesta=1
			   INDEX ON NUMMES+NUMCP+CODCTC TO (XIND) FOR codctc=vcta AND BETWEEN(fecCP,vfecini,vfecfin) AND Prestamo='S' AND EMPTY(ConAAFF)
			ELSE   
			   INDEX ON NUMMES+NUMCP+CODCTC TO (XIND) FOR codctc=vcta AND BETWEEN(fecCP,vfecini,vfecfin)	AND Prestamo='S'
			ENDIF   
            SET RELATION TO nummes+numcp+codctc  INTO Astpat ADDI
   	        SET RELATION TO nummes+numcp+codctc  INTO Cheque ADDI
   	        DEACTIVATE WINDOW xwait
   	  	    IF EOF()
	           DO STANDBY WITH "Registros no existen"
			    SET INDEX TO
	     	    USE IN 9
	     	    SELE ComPag
    	 	    SET ORDER TO (vord1)
	     	    GO vrec1
	     	    RETURN
	     	 ENDIF   
	         Vtitulo = 'PRESTAMOS'
	         DO reporte WITH 2,"LisCpCon"," Resumen de Comprobantes de Pago de Prestamos por Cta.Cte. "
    	     USE IN 9
        	 SET INDEX TO
   	         SELE ComPag
   	         SET ORDER TO (vord1)
  	         GO vrec1
	  CASE vLista = 4
		    ACTIVATE WINDOW Xwait
		    @0,0 SAY " Procesando reporte...." COLOR W+/BR*
			USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
			SELECT astpat
			INDEX ON NUMMES+NUMREF+CODCTC TO (XIND) FOR codctc=vcta AND BETWEEN(fecHA,vfecini,vfecfin)	AND LEFT(CODCTA,3)='384' AND TIPDOC='C/P'
            SET RELATION TO nummes+numref+codctc  INTO ComPag ADDI
            SET RELATION TO nummes+numref+codctc  INTO Cheque ADDI
            DEACTIVATE WINDOW xwait
			      	IF EOF()
		              DO STANDBY WITH "Registros no existen"
		     	      SET INDEX TO
		     	      USE IN 9
		     	      SELE ComPag
		     	      GO BOTT
		     	      RETURN
		     	   ENDIF   
      	           VTITULO = 'ANTICIPOS'
                   DO reporte WITH 2,"LisCpCon"," Resumen de Comprobantes de Pago de Anticipos"                                  	       
                   SET INDEX TO
                   USE IN 9
