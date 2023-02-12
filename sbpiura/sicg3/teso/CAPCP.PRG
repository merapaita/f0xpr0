close data

define window xwait from 21,35 to 23,75 color scheme 5 
activate window xwait
@0,0 say " Procesando Informaci¢n....." COLOR W+/BR*

use f:\datos\users\mpc\otro\ccmovgen   		in 1  alias ccmovgen
use h:\sis97\data\compag			   		in 2  alias compag
use h:\sis97\data\maepre  order tag maepre4	in 3  alias maepre

use f:\datos\users\mpc\otro\ccmovcta	order tag ccmovcta1 	in 4  alias ccmovcta
use f:\datos\users\mpc\otro\ccretenc	order tag ccretenc1 	in 5  alias ccretenc

use h:\sis97\data\itecp 			   		in 6  alias itecp
use h:\sis97\data\cheque			   		in 7  alias cheque
use h:\sis97\data\reten 			   		in 8  alias reten
use h:\sis97\data\astpat			   		in 9  alias astpat
use h:\sis97\data\astpre			   		in 10 alias astpre

sele compag
copy stru to cp
use cp in 2 alias cp

sele itecp
copy stru to ite
use ite in 6 alias ite

sele cheque
copy stru to chq
use chq in 7 alias chq

sele reten
copy stru to ret
use ret in 8 alias ret

sele astpat
copy stru to pat
use pat in 9 alias pat

sele astpre
copy stru to pre
use pre in 10 alias pre


sele ccmovgen
set filt to cctipodoc>='20'
go top

scan
	@0,30 say padl(ccnumdoc,4,'0') color w+/br*
	a=dtoc(ccfechacon)
	b=cctipodoc
	c=padl(ccnumdoc,4,'0')
	d=padl(ccnumadi,4,'0')

	sele cp
	append blank
	replace periodo  with '97'
	replace nummes   with padl(month(ccmovgen.ccfechacon),2,'0')
	replace numcp    with padl(ccmovgen.ccnumdoc,4,'0')
	replace feccp    with ccmovgen.ccfechacon
	replace fecref   with ccmovgen.ccfecdocr
	replace nummeshc with padl(ccmovgen.ccdocadi,2,'0')
	replace numhc    with padl(ccmovgen.ccdocref,4,'0')
	replace codfte   with iif(ccmovgen.fte_fto='220','09',right(ccmovgen.fte_fto,2))
	replace glosa    with ccmovgen.ccglosa
	replace observ   with ccmovgen.ccobserv	
	replace codctc   with ccmovgen.cctacte
	replace estado   with '10'
	replace tipdoc   with iif(!empty(ccmovgen.ccdocref),'HC','')
	replace docref   with iif(!empty(ccmovgen.ccdocref),'H/C','')
	replace numref   with iif(!empty(ccmovgen.ccdocref),numhc+'.'+nummeshc,'')
	replace nompre   with ccmovgen.ccnombre
	replace ctadeb   with iif(codfte='00','90301010101',iif(codfte='01','90301010102','90301010103')) for tipdoc='HC' next 1
	replace ctahab   with iif(codfte='00','90401010101',iif(codfte='01','90401010102','90401010103')) for tipdoc='HC' next 1
	replace usuario  with iif(ccmovgen.ccodific='MPC','L^A',iif(ccmovgen.ccodific='PPN','^^H',iif(ccmovgen.ccodific='FRV','DESQY','')))
	replace codcad   with cadena(alltrim(ccmovgen.ccproy))

	tbru=0
	sele ccmovcta
	seek a+b+c+d
	if found()
		scan while dtoc(ccmovcta.ccfechacon)+ccmovcta.cctipodoc+padl(ccmovcta.ccnumdoc,4,'0')+padl(ccmovcta.ccnumadi,4,'0')=a+b+c+d
			if !empty(cctipoasi)
				tbru=tbru+ccmonto
			endif
		endscan
		sele pat
		append blank
		replace periodo  with '97'
		replace tipdoc   with 'C/P'
		replace fecha    with cp.feccp
		replace nummes   with cp.nummes
		replace numref   with cp.numcp
		replace codcta   with ccmovgen.cuenta+'01010100'
		replace tipcta   with 'D'
		replace mtodeb   with tbru
		replace ret      with 'N'
		replace codctc   with cp.codctc
		replace fechc    with cp.fecref
		replace tipctc   with 'CC'
		sele ccmovcta
	endif
		
	liq=0
	sele ccmovcta
	seek a+b+c+d
	if found()
		scan while dtoc(ccmovcta.ccfechacon)+ccmovcta.cctipodoc+padl(ccmovcta.ccnumdoc,4,'0')+padl(ccmovcta.ccnumadi,4,'0')=a+b+c+d
			if !empty(cctipoasi)
				sele ite
				append blank
				replace periodo with '97'
				replace nummes  with cp.nummes
				replace numcp   with cp.numcp
				replace codctc  with cp.codctc
				do case
					case ccmovcta.cctipoasi='0' .or. ccmovcta.cctipoasi='S'
						replace codpart with '0000SU'
						replace cp.tipdoc  with 'SU'
					case ccmovcta.cctipoasi='R' 
						replace cp.tipdoc  with 'RE'
						replace codpart with '0000RE'
					otherwise
						replace codpart with partida(alltrim(ccmovcta.ccparti))
				endcase
				replace codmet  with '00001'
				replace codcom  with substr(ccmovgen.ccproy,28,1)+substr(ccmovgen.ccproy,30,4)
				replace impparc with ccmovcta.ccmonto
				replace codfte  with cp.codfte
				replace codcad  with cp.codcad
				replace tipdoc  with cp.tipdoc
				replace estado  with '00'
				sele ccmovcta
			else
				if left(ccmovcta.ccparti,3)='104' .or. left(ccmovcta.ccparti,3)='108'
					liq=ccmovcta.ccmonto
				endif
				sele pat
				append blank
				replace periodo  with '97'
				replace tipdoc   with 'C/P'
				replace fecha    with cp.feccp
				replace nummes   with cp.nummes
				replace numref   with cp.numcp
				replace codcta   with left(ccmovcta.ccparti,3)+'01010100'
				replace tipcta   with 'H'
				replace mtohab   with ccmovcta.ccmonto
				replace ret      with iif(left(ccmovcta.ccparti,3)='104' .or. left(ccmovcta.ccparti,3)='108','N','S')
				replace codctc   with cp.codctc
				replace fechc    with cp.fecref
				replace tipctc   with 'CC'
				sele ccmovcta
			endif
		endscan
		
		sele pat
		append blank
		replace periodo  with '97'
		replace tipdoc   with 'C/P'
		replace fecha    with cp.feccp
		replace nummes   with cp.nummes
		replace numref   with cp.numcp
		replace codcta   with '10101010100'
		replace tipcta   with 'D'
		replace mtodeb   with liq
		replace ret      with 'N'
		replace codctc   with cp.codctc
		replace fechc    with cp.fecref
		replace tipctc   with 'CC'

		append blank
		replace periodo  with '97'
		replace tipdoc   with 'C/P'
		replace fecha    with cp.feccp
		replace nummes   with cp.nummes
		replace numref   with cp.numcp
		replace codcta   with '10101010100'
		replace tipcta   with 'H'
		replace mtohab   with liq
		replace ret      with 'N'
		replace codctc   with cp.codctc
		replace fechc    with cp.fecref
		replace tipctc   with 'CC'
		
	endif
	
	if cp.tipdoc='HC' .or. cp.tipdoc='RE'
		sele pre
		append blank
		replace periodo  with '97'
		replace nummes   with cp.nummes
		replace numref   with cp.numcp
		replace tipdoc   with 'C/P'
		replace fecref   with cp.feccp
		replace codcad   with cp.codcad
		replace tipo     with 'D'
		replace ctadeb   with cp.ctadeb
		replace cuenta   with cp.ctadeb
		replace valdeb   with liq
		replace codctc   with cp.codctc
		
		append blank
		replace periodo  with '97'
		replace nummes   with cp.nummes
		replace numref   with cp.numcp
		replace tipdoc   with 'C/P'
		replace fecref   with cp.feccp
		replace codcad   with cp.codcad
		replace tipo     with 'H'
		replace ctahab   with cp.ctahab
		replace cuenta   with cp.ctahab
		replace valhab   with liq
		replace codctc   with cp.codctc
	endif
	
	sele ccmovgen
endscan

deactivate window xwait


function cadena
*--------------
parameters vcad
if len(vcad)<33
	return ''
else
	newcad=left(vcad,2)+substr(vcad,4,3)+substr(vcad,8,2)+substr(vcad,11,3)+substr(vcad,15,4)+substr(vcad,20,1)+substr(vcad,22,5)+substr(vcad,28,1)+substr(vcad,30,4)
	sele maepre
	seek '97'+newcad
	if found()
		ccad= maepre.codcad
	else
		ccad= 'x'
	endif
	sele cp
	return ccad
endif


function partida
*---------------
parameters vpar
if len(vpar)<9
	return vpar
else
	newpar=left(vpar,1)+substr(vpar,3,1)+substr(vpar,5,2)+substr(vpar,8,2)
	return newpar
endif
	