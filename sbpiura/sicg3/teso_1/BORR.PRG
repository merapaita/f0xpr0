&&&&Retenciones x Meta
*======================
CASE derm
	  STORE SPACE(5) TO cccomp,ccmet
      SELE Caja
      m.docref='C/P'
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO busCaja
  	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
	  SELECT ComPag
	  @ 1,22 SAY m.numcp 
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
      @ 3,02 SAY "Meta"
      @ 3,22 GET m.codcad PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',22,47) AND PGRET(1) 	  
      *VALID VAL_PARAD(m.codret,'CODRET',' ',22,40) AND Pagret('1')
      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)

*      IF newsistem='1'
*     	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  DISABLE
*      ELSE
*       	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',22,47) 	          DISABLE
*	  ENDIF

*	  READ VALID val_READ() 
      IF !EMPTY(m.CodCad) 
      	DO vis_calen
      ENDIF
	  m.estado = '00'
   	  m.tipdoc = 'RE'


&&&&Fin Retenciones x Meta
