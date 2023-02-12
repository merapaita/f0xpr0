 CASE OPCION=2
	*------ SALDO PRESUPUESTAL -----------
	DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
		TITLE ' °°  Saldo Presupuestal °° ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1
	@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
    @  1,2 say '  Calendario : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)
	@  4,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID IIF(!EMPTY(vUniGes),val_para(vUniGes,'UNIGES',' ',18,30),.T.)
	@  5,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID IIF(!EMPTY(vUniEje),val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30),.T.)
	@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.) 
	@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)

	READ VALID val_read()

	DEACTIVATE WINDOW lis_1

	IF LASTKEY()= 27
		DO vista
		RETURN
	ENDIF
	SELE itepar
	IF EOF()
		DO standby WITH vmens08
	ELSE
	    ACTIVATE WINDOW Standby
		@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*

		SELE REPO
		vind = SYS(3) + '.IDX'
		
		INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (vind)
		SET INDEX TO (vind)
		
		IF vtotal = 1
			SELE itepar
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) ;
				AND	IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) and IIF(EMPTY(ALLT(meseje)),.t.,meseje<=allt(vCalend))
		ELSE
			SELE itepar
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) and IIF(EMPTY(ALLT(meseje)),.t.,meseje<=allt(vCalend))
		ENDIF	
		GO TOP
		SCAN
			SCATTER MEMVAR
			SELECT REPO
			SEEK LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
			vcod = 'FTE'+ALLTRIM(m.codfte)
			m.totcal = 0
			IF !FOUND()
			    m.&vcod = m.valparT
				APPEND BLANK
				GATHER MEMVAR
				m.&vcod=0
			ELSE
				IF RLOCK()
					REPLACE &vcod WITH &vcod + m.valpart+m.cresup+m.transf
				ENDIF
				UNLOCK
				&vcod = 0
			ENDIF
			SELECT ItePar
		ENDSCAN
		SELE REPO
		zind = SYS(3) + '.IDX'
		INDEX ON codcad+CODFTE+CODPART TO (zind)
		** recibos de ingresos
		SELECT Iteri
		SET FILTER TO NUMMES<=allt(vCalend) and tipo='P' AND ITERI.ESTADO#'99'
		GO TOP
		SCAN  
			sele maepre
			IF !EMPTY(ITERI.NUMRI)
				seek Vperiodo+'01001'+IIF(LEFT(ITERI.GLOSA,3)='N/A','0003',RECING.codcad)
			ELSE
				seek Vperiodo+'01001'+'0001'
			ENDIF
			VESTFUN=UNIGES+UNIEJE+CODFUN+CODPRG+CODSPR+ACTPRY+CODCOM+CODMET
			IF !EMPTY(ITERI.NUMRI)
				vkey = IIF(LEFT(ITERI.GLOSA,3)='N/A','0003',RECING.codcad)+IIF(LEFT(ITERI.GLOSA,3)='N/A','09',RECING.CODFTE)+ITEri.CODPART
				VCODCAD = IIF(LEFT(ITERI.GLOSA,3)='N/A','0003',RECING.codcad) 
				VCODFTE = IIF(LEFT(ITERI.GLOSA,3)='N/A','09',RECING.codFTE)
			ELSE
				vkey = '000113'+ITEri.CODPART
				VCODCAD = '0001'
				VCODFTE = '13'
			ENDIF
			vmes = 'MES_'+ALLTRIM(iteri.nummes)
			SELE REPO
			SEEK vkey
			IF FOUND()
				REPLACE totafe WITH IIF(!EMPTY(ITERI.NUMRI),totafe+iteri.impparc,totafe+(iteri.impparc*-1))
				REPLACE  &vmes WITH IIF(!EMPTY(ITERI.NUMRI),&vmes+iteri.impparc,&vmes+(iteri.impparc*-1))
			ELSE
				append blank
				REPLACE CODPART WITH ITEri.CODPART,;
						UNIGES  WITH MAEPRE.UNIGES,;
						UNIEJE  WITH MAEPRE.UNIEJE,;
						PERIODO WITH VPERIODO,;
						CODCAD  WITH VCODCAD,;
						CODFTE  WITH VCODFTE,;
						&vmes   WITH IIF(!EMPTY(ITERI.NUMRI),iteri.impparc,iteri.impparc*-1),;
						ESTFUN  WITH VESTFUN,;
						TOTAFE  WITH IIF(!EMPTY(ITERI.NUMRI),iteri.impparc,iteri.impparc*-1)
			ENDIF
			IF ITERI.NUMMES = ALLTRIM(vcalend)
				REPLACE totCAL WITH IIF(!EMPTY(ITERI.NUMRI),totCAL+iteri.impparc,totCAL+(iteri.impparc*-1))
			ENDIF	
			SELECT ITEri
		ENDSCAN
	    DEACTIVATE WINDOW Standby
		SELECT REPO
		GO TOP
		IF EOF()
			DO standby WITH 'No existe Registros para procesar'
		ELSE
			DO reporte WITH 2,"SalPrei1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
		ENDIF
		if yesno('Imprime Consolidado')
			xdbf = SYS(3) + '.DBF'
			xind = SYS(3) + '.IDX'
			COPY STRUC TO (xdbf)
			INDEX ON CODFTE+codpart TO (xind)
			use (xdbf) in 0 alias consol
			select repo
			go top
			SCAN
				vcodpart1 = CODFTE+codpart
				vcodpart2 = CODFTE+codpart
				VTOTAFE = 0	
				VTOTCAL = 0
				VPRESU = 0
				SCATTER MEMVAR
				do while vcodpart1=vcodpart2 AND !EOF()
					VPREsu  = VPREsu  + valpart
					VTOTCAL = VTOTCAL + TOTCAL	&& DEL MES
					VTOTAFE = VTOTAFE + TOTAFE	&& ACUMULADO
					SKIP
					vcodpart2 = CODFTE+codpart
				ENDDO
				m.valpart= vpresu
				m.totCAL = vtotCAL
				m.totafe = vtotafe
				SELE CONSOL
				APPEND BLANK
				GATHER MEMVAR
				select repo
				skip -1
			endscan	
			SELECT CONSOL
			REPLACE ALL CODCAD WITH '0001'
			GO TOP
			DO reporte WITH 2,"LisE5ci",' Consolidado de la Ejecucion ',1,.F.,.T.
			USE
			ERASE (XDBF)
		endif
	ENDIF
ENDCASE
