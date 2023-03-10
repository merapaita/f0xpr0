*--------------------------------------------------------------------------
* Planillas.Prg
* Registra los sueldos de los trabajadores
* Estado :
*   '00' Emitida   Este es el que se registra en la Solicitud de Cotización
* Autor:
*--------------------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
SET PROCEDURE TO SAI_LIB
USE Personal IN 1   order tag persona1  ALIAS Permae
USE Iteper   IN 2   order tag Iteper1   ALIAS Iteper
USE parmae   IN 3   order tag PARMAE1   alias parma
USE totapor  IN 4   order tag totapor1  alias totapor

PUBLIC okcancel,VMES,VTIPO,Vsuma01,Vsuma02,Vsumaing,VsumaDES,VTOCLI
   SELE PARMA
   SEEK 'FECSIS'
   SELECT ITEPER
   vmes_sue = LEFT(parma.descri,2)
   vperiodo = subs(parma.descri,4,4)
   SET FILTER TO mes_sue = vmes_sue AND PERIODO = VPERIODO

do Escoge

if okcancel=2
	CLOS DATA
	RETURN
ENDIF

SELECT iteper
SET RELATION TO lib_ele+mes_sue+periodo into totapor addi
COPY STRUC TO vTemp
COPY STRUC TO vTemp1
USE vTemp IN 0 
SELECT Permae
GO TOP
SCAN
	SCATTER MEMVAR 
	SELECT iteper
	SEEK m.lib_ele
	vgraba=.t.
	SCAN FOR lib_ele=m.lib_ele
		SCATTER MEMVAR
		IF M.porcent='S' and m.tip_adn='+'
			m.tot_adn = vporpos()
		ENDIF
		IF M.porcent='S' and m.tip_adn='-'
			m.tot_adn = vporneg()
		ENDIF
		SELE vTemp
		IF 	vTocli#4
		  IF permae.estado = '01'  or permae.estado = '05' or permae.estado = '06'  or permae.mes_vac=allt(vmes)
			if vgraba
				APPEND BLANK
				GATHER MEMVAR
				replace	num_pla with permae.num_pla, ESTADO WITH permae.estado
				replace tot_adn with 0
				IF mes_vac= allt(vmes)
					replace del_emp with 'V' 
				endif
				vgraba = .f.
			endif
		  ELSE
			APPEND BLANK
			GATHER MEMVAR
			replace	num_pla with permae.num_pla
		  ENDIF
		ELSE
		  IF permae.estado = '04' or permae.mes_vac=allt(vmes)
			if vgraba
				APPEND BLANK
				GATHER MEMVAR
				replace	num_pla with permae.num_pla, ESTADO WITH permae.estado
				replace tot_adn with 0
				IF mes_vac= allt(vmes)
					replace del_emp with 'V' 
				endif
				vgraba = .f.
			endif
		  ELSE
			APPEND BLANK
			GATHER MEMVAR
			replace	num_pla with permae.num_pla
		  ENDIF
		ENDIF
		SELECT iteper
	ENDSCAN
	SELECT Permae
ENDSCAN
select vtemp
index on num_pla+niv_per+tip_adn to xx
set relation to num_pla into permae addi
set relation to lib_ele into totapor addi
GO TOP
*SET STEP ON
DO REPPRG WITH 'LISPLA', ' Lista del Personal',1
USE vTemp1 IN 0
SELECT vtemp
index on tip_adn + cod_adn to xx
REPLA ALL LIB_ELE WITH '99999999'
GO TOP

SCAN
	SCATTER MEMVAR
	vcod_adn = cod_adn
	vsum_adn = 0
	DO while vcod_adn = cod_adn
		vsum_adn = vsum_adn + tot_adn
		skip
	ENDDO
	SKIP -1
	SELE vtemp1
	APPEND BLAN
	m.tot_adn = vsum_adn
	GATHER MEMVAR
	SELE vtemp	
ENDSCAN
selec VTEMP1
DO REPPRG WITH 'LISTOT', ' Lista del Personal',1
RETURN 

PROCEDURE Lispla
*---------------
PARAMETER _desti
IF _desti=2
	SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINT
STORE 0 TO pagina, linea
Impri    = .F.
SELECT VTEMP
GO TOP
store 0 to Vsumaing,Vsumades,Vsuma01,Vsuma02,LINEA
vcuAntos = 0
SCAN
	vcuAntos = vcuAntos + 1
	IF pagina = 0 .OR. linea > 59 OR vcuAntos = 1
		DO Titulo
		Do Cabecera
	 Else
		Do Cabecera
	ENDIF
	vlib_ele = lib_ele
	store 0 to Vunipos,vunineg
	IF estado # '01'  and estado # '05' and estado # '06' and del_emp#'V'
		DO WHILE vlib_ele = lib_ele
			voki = .T.
			voke = .T.
			FOR J= 1 TO 5
				columna = 2
				FOR I=1 TO 5
					IF tip_adn = '+' AND voki
						@ LINEA,02  SAY 'INGRESOS : '
						@ LINEA,02  SAY 'INGRESOS : '
						linea = linea + 1
						IF pagina = 0 .OR. linea > 59
							DO Titulo
							Do Cabecera
						ENDIF
						columna = 2
						voki = .F.
					ENDIF
					IF tip_adn = '-' AND voke
						IF i<=5
							linea =linea + 1
							IF pagina = 0 .OR. linea > 59
								DO Titulo
								Do Cabecera
							ENDIF
						endif
						@ LINEA,02  SAY 'DESCUENTOS: '
    	 				@ LINEA,02  SAY 'DESCUENTOS: '
	    	 			@ LINEA,122 SAY '============='
    		 			@ LINEA,122 SAY '============='
	     				linea = linea + 1
    	 				IF pagina = 0 .OR. linea > 59
     						DO Titulo
     						Do Cabecera
						ENDIF
						columna = 2
						I = 1
						voke = .F.
					ENDIF
					vdescri = Val_para(cod_adn,IIF(Tip_adn='+','AUMLEY','DESLEY'),'Z',10)
	          		@ LINEA,COLUMNA  SAY left(allt(vdescri),11)
	    	      	COLUMNA = COLUMNA + 12
    	    	  	@ LINEA,COLUMNA  SAY tot_adn PICTURE '9,999.99'
        	  		COLUMNA = COLUMNA + 13
	        	  	if tip_adn = '+'
    	      			vunipos = vunipos + tot_adn
	        	  	else
    	      			vunineg = vunineg + tot_adn
	    	      	endif
    	    	  	SKIP
        	  		IF vlib_ele # lib_ele
          				EXIT
		          	ENDIF
    		   	ENDFOR
        		LINEA = LINEA + 1
	        	IF vlib_ele # lib_ele
    	    		EXIT
	        	ENDIF
		  		IF pagina = 0 .OR. linea > 59
 					DO Titulo
  					Do Cabecera
				ENDIF
			ENDFOR
		ENDDO
		SKIP -1
		SELEC totapor  
		seek LIB_ELE + allt(vmes)
		IF FOUND()
	  		REPLACE tot_ing with vunipos
  			REPLACE tot_des with vuniNEG
		ENDIF
		SELE VTEMP
		@ linea,02  SAY 'INGRESO BRUTO : S/. ' 
		@ linea,02  SAY 'INGRESO BRUTO : S/. ' 
		@ linea,23  SAY vunipos PICTURE '99,999.99'
		@ linea,23  SAY vunipos PICTURE '99,999.99'
		@ linea,36  SAY 'DCTOS : S/. ' 
		@ linea,36  SAY 'DCTOS : S/. ' 
		@ linea,50  SAY vunineg PICTURE '99,999.99'
		@ linea,50  SAY vunineg PICTURE '99,999.99'
		@ linea,62  SAY 'INGRESO NETO : S/.' 
		@ linea,62  SAY 'INGRESO NETO : S/.' 
		@ linea,82  SAY vunipos - vunineg  PICTURE '99,999.99'
		@ linea,82  SAY vunipos - vunineg  PICTURE '99,999.99'
		LINEA = LINEA + 1
		Vsuma01  = Vsuma01  + ROUND(totapor.apo_01,2)
		Vsuma02  = Vsuma02  + ROUND(totapor.apo_02,2)
		Vsumaing = Vsumaing + TOTAPOR.TOT_ING
		Vsumades = Vsumades + TOTAPOR.TOT_des
	 ELSE
		@ linea,40  SAY val_para(permae.estado,'ESTPLA','D',22)
		@ linea,40  SAY val_para(permae.estado,'ESTPLA','D',22)
		IF del_emp = 'V'
			@ linea,68  SAY '- VACACIONES '
			@ linea,68  SAY '- VACACIONES '
		endif
		linea = LINEA +1
	ENDIF
	if vcuAntos = 4
		vcuAntos = 0
	endif
ENDSCAN
SET PRINTER TO
SET DEVICE TO SCREEN
RETURN

PROCEDURE LisTOT
*---------------
PARAMETER _desti
IF _desti=2
   SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINT
STORE 0 TO pagina, linea
Impri    = .F.
SELECT VTEMP1
GO TOP
SCAN
  IF pagina = 0 .OR. linea > 59
     DO TitRES
  endif
  vlib_ele = lib_ele
  DO WHILE vlib_ele = lib_ele
  	voki = .T.
  	voke = .T.
	FOR J= 1 TO 9
		columna = 2
 		FOR I=1 TO 5
 			IF tip_adn = '+' AND voki
  				@ LINEA,02  SAY 'INGRESOS : '
				@ LINEA,02  SAY 'INGRESOS : '
				@ LINEA,110 SAY 'C.E.M. 9%: '
				@ LINEA,110 SAY 'C.E.M. 9% : '
				@ LINEA,122 SAY Vsuma01 PICTURE '99,999.99'
				@ LINEA,122 SAY Vsuma01 PICTURE '99,999.99'
 	  			linea = linea + 1
  			    IF pagina = 0 .OR. linea > 59
					DO TitRES
				ENDIF
				columna = 2
				voki = .F.
 			ENDIF
	     	IF tip_adn = '-' AND voke
	     		IF i<=5
	     			linea =linea + 1
	     			IF pagina = 0 .OR. linea > 59
     					DO TitRES
					ENDIF
	     		endif	
    	 		@ LINEA,02  SAY 'DESCUENTOS: '
    	 		@ LINEA,02  SAY 'DESCUENTOS: '
				@ LINEA,110 SAY 'I.E.S. 5%: '
				@ LINEA,110 SAY 'I.E.S. 5% : '
				@ LINEA,122 SAY Vsuma02 PICTURE '99,999.99'
				@ LINEA,122 SAY Vsuma02 PICTURE '99,999.99'
     			linea = linea + 1
     			IF pagina = 0 .OR. linea > 59
     				DO TitRES
				ENDIF
				columna = 2
				I = 1
				voke = .F.
			ENDIF
			vdescri = Val_para(cod_adn,IIF(Tip_adn='+','AUMLEY','DESLEY'),'Z',10)
          	@ LINEA,COLUMNA  SAY left(allt(vdescri),11)
          	COLUMNA = COLUMNA + 12
          	@ LINEA,COLUMNA  SAY tot_adn PICTURE '9,999.99'
          	COLUMNA = COLUMNA + 13
          	SKIP
          	IF vlib_ele # lib_ele
          		EXIT
          	ENDIF
       	ENDFOR
        LINEA = LINEA + 1
        IF vlib_ele # lib_ele
        	EXIT
        ENDIF
  		IF pagina = 0 .OR. linea > 59
 			DO TitRES
		ENDIF
	ENDFOR
  ENDDO
  SKIP -1
  LINEA = LINEA + 1
  @ linea,02  SAY 'INGRESO BRUTO : S/. ' 
  @ linea,02  SAY 'INGRESO BRUTO : S/. ' 
  @ linea,23  SAY Vsumaing PICTURE '99,999.99'
  @ linea,23  SAY Vsumaing PICTURE '99,999.99'
  @ linea,36  SAY 'DCTOS : S/. ' 
  @ linea,36  SAY 'DCTOS : S/. ' 
  @ linea,50  SAY Vsumades PICTURE '99,999.99'
  @ linea,50  SAY Vsumades PICTURE '99,999.99'
  @ linea,62  SAY 'INGRESO NETO : S/.' 
  @ linea,62  SAY 'INGRESO NETO : S/.' 
  @ linea,62  SAY 'INGRESO NETO : S/.' 
  @ linea,82  SAY vsumaing-vsumades  PICTURE '99,999.99'
  @ linea,82  SAY vsumaing-vsumades  PICTURE '99,999.99'
  @ linea,82  SAY vsumaing-vsumades  PICTURE '99,999.99'
  @ linea,95  SAY 'TOTAL APORTACIONES : S/.' 
  @ linea,95  SAY 'TOTAL APORTACIONES : S/.' 
  @ linea,122 SAY vsuma01+vsuma02  PICTURE '99,999.99'
  @ linea,122 SAY vsuma01+vsuma02  PICTURE '99,999.99'
  LINEA = LINEA + 1
ENDSCAN
* REPORTE RESUMEN
SET PRINTER TO
SET DEVICE TO SCREEN
RETURN

PROCEDURE TitRES
*---------------
pagina = pagina  + 1
vTitulo = 'TOTAL PLANILLA UNICA DE REMUNERACIONES - '+vtipo
linea = 3
@ LINEA,1   SAY CHR(18)+CHR(14)
@ LINEA,05 SAY VTITULO+CHR(18)
linea = linea + 1
@ LINEA,50  say 'MES DE '+allt(VAL_PARA(allt(vmes),'FECMES','Z',10))+' DE '+STR(year(date()),4)
@ LINEA,50  say 'MES DE '+allt(VAL_PARA(allt(vmes),'FECMES','Z',10))+' DE '+STR(year(date()),4)
@ LINEA,115  SAY 'Pág.'
@ LINEA,120 SAY pagina   PICTURE '##,###'
linea = linea + 2
@ linea,2   SAY 'SECTOR  : ' + VAL_PARA('01','SECTOR','Z',80,80)
@ linea,2   SAY 'SECTOR  : ' + VAL_PARA('01','SECTOR','Z',80,80)
linea = linea + 1
@ linea,2   SAY 'PLIEGO  : ' + VAL_PARA('001','PLIEGO','Z',22)
@ linea,2   SAY 'PLIEGO  : ' + VAL_PARA('001','PLIEGO','Z',22)
linea = linea + 1
@ linea,2   SAY 'ENTIDAD : ' + cia
@ linea,2   SAY 'ENTIDAD : ' + cia
@ linea,115   SAY 'APORTACIONES'
@ linea,115   SAY 'APORTACIONES'
linea = linea + 1
@ linea,2  SAY REPLICATE('=',133)
@ linea,2  SAY REPLICATE('=',133)
linea = linea + 2
RETURN

PROCEDURE Titulo
*---------------
pagina = pagina  + 1
vTitulo = 'PLANILLA UNICA DE REMUNERACIONES - '+vtipo
@ 0,1   SAY CHR(18)+CHR(14)
@ 0,12 SAY VTITULO+CHR(18)
@ 1,50  say 'MES DE '+allt(VAL_PARA(allt(vmes),'FECMES','Z',10))+' DE '+STR(year(date()),4)
@ 1,50  say 'MES DE '+allt(VAL_PARA(allt(vmes),'FECMES','Z',10))+' DE '+STR(year(date()),4)
@ 1,115  SAY 'Pág.'
@ 1,120 SAY pagina   PICTURE '##,###'
linea = 3
@ linea,2   SAY 'SECTOR  : ' + VAL_PARA('01','SECTOR','Z',80,80)
@ linea,2   SAY 'SECTOR  : ' + VAL_PARA('01','SECTOR','Z',80,80)
linea = linea + 1
@ linea,2   SAY 'PLIEGO  : ' + VAL_PARA('001','PLIEGO','Z',22)
@ linea,2   SAY 'PLIEGO  : ' + VAL_PARA('001','PLIEGO','Z',22)
linea = linea + 1
@ linea,2   SAY 'ENTIDAD : ' + cia
@ linea,2   SAY 'ENTIDAD : ' + cia
linea = linea + 2
@ linea,2   SAY 'PLAZA'
@ linea,2   SAY 'PLAZA'
@ linea,10  SAY 'NOMBRES Y APELLIDOS'
@ linea,10  SAY 'NOMBRES Y APELLIDOS'
@ linea,50  SAY 'ESTADO'
@ linea,50  SAY 'ESTADO'
@ linea,80  SAY 'TIEMPO DE SERV.'
@ linea,80  SAY 'TIEMPO DE SERV.'
@ linea,110 SAY 'APORTACIONES'
@ linea,110 SAY 'APORTACIONES'
linea = linea + 1
@ linea,2   SAY 'CONDICION  ' 
@ linea,2   SAY 'CONDICION  ' 
@ linea,35  SAY 'NIVEL REMUNERATIVO  '
@ linea,35  SAY 'NIVEL REMUNERATIVO  '
@ linea,74  SAY 'REGIMEN DE PENSIONES  '
@ linea,74  SAY 'REGIMEN DE PENSIONES  '
@ linea,110 SAY 'CEM :  9%'
@ linea,110 SAY 'CEM :  9%'
linea = linea + 1
@ linea,2   SAY 'RECURSO' 
@ linea,2   SAY 'RECURSO' 
@ linea,35  SAY 'CARGO '
@ linea,35  SAY 'CARGO '
@ linea,110 SAY 'IES :  5%'
@ linea,110 SAY 'IES :  5%'
linea = linea + 1
RETURN

PROCEDURE Cabecera
*-----------------
@ linea,2  SAY REPLICATE('=',133)
@ linea,2  SAY REPLICATE('=',133)
linea = linea + 1
@ linea,2   SAY left(allt(permae.num_pla),2)
@ linea,2   SAY left(allt(permae.num_pla),2)
@ linea,2   SAY left(allt(permae.num_pla),2)
vnombre = allt(permae.ape_nom)
@ linea,08  SAY LEFT(vnombre,30)
@ linea,08  SAY LEFT(vnombre,30)
@ linea,08  SAY LEFT(vnombre,30)
@ linea,50  SAY val_para(permae.estado,'ESTPLA','D',22)
@ linea,80  SAY LEFT(permae.aĄo_ser,15)
@ linea,98  SAY permae.obs_per
@ linea,98  SAY permae.obs_per
linea = linea + 1
*SET STEP ON
@ linea,2   SAY val_para(permae.con_per,'CONPER','D',30)
@ linea,35  SAY val_para(permae.niv_per,'NIVPER','D',30)
@ linea,67  SAY val_para(permae.afp_per,'AFPPER','D',30)
IF estado # '01'  andr estado # '05'
	@ linea,110 SAY ROUND(TOTAPOR.apo_01,2) PICTURE '999'
	@ linea,110 SAY ROUND(TOTAPOR.apo_01,2) PICTURE '999'
ENDIF
linea = linea + 1
@ linea,2   SAY val_para(permae.rec_per,'RECPER','D',30)
@ linea,35  SAY val_para(permae.car_per,'CARPER','D',30)
IF estado # '01'  andr estado # '05'
	@ linea,110 SAY ROUND(TOTAPOR.apo_02,2) PICTURE '999'
	@ linea,110 SAY ROUND(TOTAPOR.apo_02,2) PICTURE '999'
ENDIF
linea = linea + 1
RETURN

PROCEDURE Escoge
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE 'Listado de Planillas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO vToCli,OKCANCEL
VCODPER = SPAC(8)
Vmes    = SPAC(2)
vdato   = space(20)
@ 02,01 SAY " Tipo de Planilla : " GET vTOCLI  FUNCTION '^ Nombrados;Donac. Transf.;Direc. Recaud.;Pensiones'
@ 06,01 SAY "   Mes a Reportar : "
@ 06,22 GET Vmes  Valid Val_para(vmes,'FECMES',' ',22)
@ 08,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
IF OKCANCEL = 1
   vInd = SYS(3) + '.IDX'
   VMES = PARMA.CODIGO
   IF allt(vmes)#vmes_sue
   	  do standby with 'MES A REPORTAR ES DIFERENTE AL DEL SISTEMA'
   	  CLOS DATA
   	  OKCANCEL = 2
   	  RETURN
   ENDIF
   DO CASE
		CASE vTocli=1
			SELECT Permae
			SET ORDER TO PERSONA1
			SET FILTER TO (CON_PER='1' OR CON_PER='2')
			vtipo = 'PERMANENTES'
		CASE vTocli=2
			SELECT Permae
			SET ORDER TO PERSONA1
			SET FILTER TO CON_PER='5' and rec_per='1'
			vtipo = 'CONTRATADOS - D.T.'
		CASE vTocli=3
			SELECT Permae
			SET ORDER TO PERSONA1
			SET FILTER TO CON_PER='5' and rec_per='2'
			vtipo = 'CONTRATADOS - D.R.'
		CASE vTocli=4
			SELECT Permae
			SET ORDER TO PERSONA5
			vtipo = 'PENSIONISTAS'
   ENDCASE
ENDIF
RETURN

FUNCTION vporpos
*---------------
vrecno = recno()
vaumento = 0
vaumentop = 0
vmonto = 0
SELE iteper
SEEK m.lib_ele
SCAN WHILE m.lib_ele=lib_ele
 	IF Tip_adn='+'
 	   IF porcent='N'
 	   	  IF cod_adn <> '26' AND cod_adn <> '28' AND cod_adn <> '11'
 	   	  	vaumento   = vaumento   + tot_adn
 	   	  ENDIF
 	   ELSE
   	   	  vaumentop   = vaumentoP   + tot_adn
 	   ENDIF
 	ENDIF 
ENDSCAN
vaumento   = (vaumento*vaumentop)/100
go vrecno
RETURN vaumento

FUNCTION vporneg
*---------------
vrecno = recno()
vmonto = 0
SELE iteper
SEEK m.lib_ele+allt(vmes)+m.niv_per+'+'+'06'
vmonto   = (totapor.tot_ing-iteper.tot_adn)
go vrecno
vdescu = ((vmonto*iteper.tot_adn)/100)
RETURN vdescu
