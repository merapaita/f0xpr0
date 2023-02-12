* ROCI.PRG
USE ComPag   IN  3  ORDER TAG ComPag1      ALIAS ComPag 
*
USE Reten    IN 26  ORDER tag Reten1       ALIAS Reten          
*
SELE COMPAG
SCAN
	VNUMSIAF=compag.numsiaf
	vnummes = compag.nummes
	vnumcp  = compag.numcp
	vcodctc = compag.codctc
	sele reten
	seek alltrim(vnummes)+vnumcp+alltrim(vcodctc)
	IF !FOUND()
		do while nummes=vnummes and numcp=vnumcp and codctc=vcodctc and not eof()
			repla ctadeb with vnumsiaf
			skip
		enddo
		skip -1
	Else
		repl tipfun with 'N'
	endif
	sele compag
endscan	
clos data
return
		
	