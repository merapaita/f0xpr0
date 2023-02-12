* (See also 'FPTALL' below this procedure)
*
* FixFPT
*
* Written rather hastily by:               Bill "Hank" Castello
*                                          Budget Systems
*                                          3805 Brookfield
*                                          Arlington, TX 76017
*
* 
SET TALK OFF
SET ECHO OFF
SET ESCAPE OFF
SET SHADOW OFF
SET CONFIRM ON
ON KEY LABEL F1 DO fixhelp
CLEAR
DEFINE WINDOW msgwin    FROM 11,18 TO 13,62 DOUBLE FLOAT SHADOW COLOR SCHEME 10 ;
	TITLE ' FPTFIX '
DO fptbeg
CLEAR
dimension sasc(4), sval(4)
store space(8) to filename
store "" to string, header
STORE 0 TO size, fblocksize, fptshould, fptpointer
mblocksize = SET('BLOCKSIZE')

mblocksize = IIF(mblocksize < 33, 64, mblocksize)

CLOSE DATA

retmsg = 'No .fpt pointer problem encountered'

@ 10,30 SAY 'Filename' GET filename PICTURE "@!" ;
		MESSAGE "Press F1 for HELP"
READ

filename = TRIM(filename)

IF EMPTY(filename) 
	activate screen
	clear
	?
	? 'Cannot fix .fpt without filename'
	? 'Please try again'
	wait window
	RETURN
ENDIF

SET COLOR TO
@ 12, 30 SAY   '    CHOOSE ONE'
@ 14,30 PROMPT 'Attempt to FIX .fpt     ' 
@ 15,30 PROMPT 'Disengage .fpt from .dbf' 
MENU TO mchoice

DO CASE
CASE mchoice = 1
	
filename = IIF('.' $ filename, UPPER(LEFT(filename, AT('.', filename) -1)) ;
		+ '.FPT', UPPER(filename) + '.FPT')

	IF ! FILE(filename)
		activate screen
		clear
		?
		? filename + ' not found'
		?
		? 'Use SET DEFAULT or place FPTFIX in same directory with .fpt'
		wait window
		RETURN
	ENDIF
	DO getbksize

	CLEAR
	@ 08,30 SAY 'File shows: ' + STR(fblocksize)
	@ 09,30 SAY 'Env  shows: ' + STR(mblocksize)
	@ 12,30 SAY 'Blocksize' GET mblocksize PICTURE "99,999,999" ;
		RANGE 33, 32000000 ;
		MESSAGE "Default is 64"
	READ
	

	fptshould = INT((size/mblocksize) + 1)


	IF fptshould <> fptpointer
		IF fixorno()
			DO figstring
			DO fixwrite
			retmsg = '.fpt pointer problem encountered, & fixed'
		ENDIF
	ENDIF

CASE mchoice = 2

filename = IIF('.' $ filename, UPPER(LEFT(filename, AT('.', filename) -1)) ;
		+ '.DBF', UPPER(filename) + '.DBF')

	handle = fopen(filename,2)
	= fseek(handle, 0)
	if handle > 0
		= fwrite(handle, chr(3))
	endif
	= fclose(handle)

	IF FERROR() <> 0
		retmsg = 'Error #' + str(ferror()) + ' resolve & try again'
	ELSE
		retmsg = 'Your .dbf and .fpt are seperated'
	ENDIF

ENDCASE




CLOSE ALL
@ 24,00 CLEAR TO 24,79
ACTIVATE WINDOW msgwin
@ 00,01 SAY retmsg
ACTIVATE SCREEN
WAIT WINDOW
RELEASE WINDOW msgwin
DO fptend
ON KEY LABEL F1
RETURN


PROCEDURE getbksize
* Read file
handle = fopen(filename,2)
header = fread(handle, 8)
size = FSEEK(handle, 0, 2)
= fclose(handle)

blockstring = LEFT(header, 4)

* Blocksize per .fpt file -

fblocksize = asc(substr(header, 7, 1)) * 256 + ;
			asc(substr(header, 8, 1))
			
*  If blocksize less than 33, then multiply by 512
fblocksize = iif(fblocksize < 33, fblocksize * 512, fblocksize)			

* Evaluate bytes for next block pointer
for i = 1 to 4
	sval(i) = asc(substr(blockstring,i,1))
next

* Block pointer per .fpt file -
 
fptpointer = CEILING((sval(1) * 256^3) + (sval(2) * 256^2) + (sval(3) * 256) + sval(4))

RETURN


PROCEDURE figstring
hexcube = 256^3
hexsqua = 256^2
hexadec = 256

sval(1) = int(fptshould/ 256^3)
bal = fptshould - sval(1) * hexcube

sval(2) = int(bal / 256^2)
bal = bal - sval(2) * hexsqua

sval(3) = int(bal / 256)
bal = bal - sval(3) * hexadec

sval(4) = int(bal)

for i = 1 to 4
	string = string + chr(sval(i))
endfor
string = string + chr(0) + chr(0)
hi = int(mblocksize/256)
lo = mblocksize - (hi * 256)
string = string + chr(hi) + chr(lo)
RETURN	
	
PROCEDURE fixwrite
handle = fopen(filename,2)
= fseek(handle, 0)
= fwrite(handle, string)
= fclose(handle)
IF FERROR() <> 0
	?
	? 'File error ' + STR(FERROR())
	retmsg = '.fpt pointer problem encountered & not fixed'
ENDIF
RETURN



* From info file FPTBEG
* D:\CARPRO\FPTEND.PRG
* SET TALK OFF
PROCEDURE fptend
CLEAR
SET COLOR TO BG+/B
@ 6,15,20,66 BOX "лллллллл"
SET COLOR TO W+/B
@ 7,16 CLEAR TO 19,65
@ 8,37 SAY "FPTFIX"
@10,19 SAY "I hope FPTFIX has helped!  "

@13,19 SAY "Modify the source as needed.  If you have"
@14,18 SAY "an improved version, let us know:"
@16,34 SAY "Budget Systems"
@17,34 SAY "3805 Brookfield"
@18,32 SAY "Arlington, TX 76017"
@ 21, 10 SAY " "
WAIT WINDOW
RETURN


FUNCTION fixorno
PARAMETERS none
CLEAR
ACTIVATE WINDOW msgwin
mans = "Y"
@ 00,01 SAY "Repair " + filename + " (Y/N)" GET mans PICTURE "@M Y,N"
READ
DEACTIVATE WINDOW msgwin
mret = IIF(mans = "Y", .t., .f.)
retmsg = '     FPTFIX aborted'
RETURN mret


* From info file FPTBEG
* D:\CARPRO\FPTBEG.PRG  ** c o p y w r i t e   b u d g e t    s y s t e m s
* SET TALK OFF                                 a r l i n g t o n, t e x a s
*                                              ( 8 1 7 )    5 7 2 - 4 0 1 5
PROCEDURE fptbeg
SET COLOR TO BG+/B
@ 6,15,20,66 BOX "лллллллл"
SET COLOR TO W+/B
@ 7,16 CLEAR TO 19,65
@ 8,37 SAY "FPTFIX"
@10,19 SAY "Backup your .fpt file first.  FPTFIX should"
@11,18 SAY "cure 95% of your .fpt problems."
@13,19 SAY "  "
@14,18 SAY " "
@16,34 SAY "Budget Systems"
@17,34 SAY "3805 Brookfield"
@18,32 SAY "Arlington, TX 76017"

WAIT  WINDOW
RETURN


* From info file <unnamed>
* D:\CARPRO\ORDER.PRG
* SET TALK OFF
PROCEDURE orderit
SET PRINTER ON
? " "
? "FPTFIX will be shipped the same day this order is received."
? " "
? "You will receive 100% FoxPro source code, in a .PRG and its"
? "equivalent .fxp (compiled) file."
? " "
? " "
? "My name:______________________________________________"
? " "
? "Address:______________________________________________"
? " "
? "City:"+SPACE(3)+"_____________________ State: ____ Zip: _______"
? " "
? " "
? "FPTFIX source is $20.  Include $5 extra for laser printout on a"
? "single sheet printed both sides & ready for 3-ring binder."
? " "
? "Please note that the source code is copywrited.  While you will have"
? "the rights to use it within your programs, you may not distribute "
? "FPTFIX source by itself."
? " "
? " Please check one:"
? "				[] 5.25 in. disk (360k)     [] 3.5 in. disk (720k)"
? " "
? " "
? "Enclose check or M.O. (cash at YOUR risk) -"
? " "
? "Mail to:"+SPACE(3)+"Budget Systems"
? "3805 Brookfield"
? "Arlington, TX 76017"
? " "
SET PRINTER OFF
EJECT
RETURN

PROCEDURE fixhelp
ON KEY LABEL F1
SAVE SCREEN TO fptscr
CLEAR
TEXT



	FPTFIX must either be in the same directory as your .fpt, or the
 .fpt must be in your (SET) default directory.
	Blocksize is critical to proper operation of FPTFIX.  
 FPTFIX uses SET('BLOCKSIZE') for default.  If your environment has
 not changed since running the application concerned, this should
 suffice.
	If you are in doubt and cannot do a ? SET('BLOCKSIZE') or a
 DISPLAY STATUS from the application's environment - then copy a GOOD 
 .fpt from the same application and use it for testing. If FPTFIX says
 that it does not appear to need repair, then you have the correct 
 blocksize.
	If you order the source code, and latest version, you will be able
 to incorporate FPTFIX as a procedure within your application. Have it 
 check ALL .fpt's so you don't burden your users with the problem of 
 determining WHICH file is messed up.
		
	FPTFIX is freeware.  Feel free to pass along copies to your 
 friends.  Send no money if you don't need the SOURCE CODE.
	We also have applications for used car dealers, small retail 
 stores, rental stores, repair shops, jewelry stores, etc.
ENDTEXT
WAIT WINDOW

clear
text

 	P.S.
 	
 	If your memo file is hopelessly corrupted, or maybe just unimportant,
 use the second program on this disk, "DBFFREE" to disengage your .dbf
 from the .fpt.
 
 	This allows use of the .dbf without the memo field, which may then be
 recreated with modi struc or merely left memoless!
 
ENDTEXT
WAIT WINDOW
RESTORE SCREEN FROM fptscr
ON KEY LABEL F1 DO fixhelp
RETURN

*
*  PROCEDURE: FPTALL
* Use with : FPTFIX
*
* Hope this "bonus" prg is helpful
* 
* 
*
*  
*  
*            
*			
*
PROCEDURE FPTALL
set talk on

* Create your own "fptfiles.txt" file or, get 'em all like this:
***********************************
!DIR | SORT > FPTFILES.TXT
***********************************

* Now that we have a text file of .fpt files, lets get to work on 'em...

handle = FOPEN("fptfiles.txt")


* some 'suspenders', just in case
if ferror() !=0
	? 'error opening'
	= FCLOSE("fptfiles.txt")
	return
endif

* (and a belt too!) - replace RETURN with your error routine
on error return

do while !feof(handle)

	mfile = fgets(handle)
	if 'fpt' $ mfile  									&& .fpt's only
		mfile = left(mfile, at(' ', mfile) -1) + '.fpt' && reconstruct
		do fptfix with (mfile)   						&& do it!
	endif
	
enddo

return

* EOF FPTFIXALL
