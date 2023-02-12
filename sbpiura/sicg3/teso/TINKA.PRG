
close databases
set talk oFF
set dele on
CLEA 
SET PROCEDURE TO TES_LIB
USE TINKA    IN 1 ORDER TINKA1 ALIAS TINKA
USE LISTINKA IN 2 ORDER LISTINKA1 ALIAS LIS
SELE LIS
DELE ALL
SELE TINKA
DEFINE WINDOW cc FROM 19,01 TO 23,78  ;
TITLE 'SIMULACION DE NUMEROS ALEATORIOS DE LA TINKA->  ®Esc¯ Terminar **' ;
COLOR SCHEME 15
ACTIVATE WINDOW cc
DECLARE A[120]
declare z[3]
STORE 0 TO X,FILA,I,columna
columna=15
FILA=2  
s=0
I=1
j=1
f=1
g=1
R=1
H=1
z[1]='²'
z[2]='°'
z[3]=''
DO WHILE .t.
   X=str(rand()*100,2)
   IF VAL(X)<=36 AND VAL(X)>=1
      SEEK X
      IF !FOUND()      
	      A[I]=X    
	      APPEND BLANK
	      REPLACE A WITH A[I]
	      sele LIS
	      append blank
	      replace lista with g, a with A[I]
	      sele tinka
	      I=I+1
	      H=H+1
	  ENDIF    
	  IF H>=7
	     g=g+1
	     H=1
	     SELE TINKA
	     dele all
	  ENDIF   
   ENDIF
   IF I>=121
      EXIT
   ENDIF 
   j=j+1
   if f<=2
	  @ f,j say z[r]
   else 
	  f=1	  
	  r=r+1
	  if r=4
	     r=1
	  endif   
   endif
   if j>70
      j=1
      f=f+1
   endif
ENDDO   
DEFINE WINDOW xx FROM 01,01 TO 23,78  DOUBLE ;
TITLE 'LISTA A ESCOGER->  ®F5¯ Terminar **' ;
COLOR SCHEME 5
ACTIVATE WINDOW xx
fila=1
columna=15
SELE LIS
DO WHILE FILA<=20
   SEEK FILA
   @ fila,2 say fila picture '99'
   @ fila,5 say '-->' 
   scan  while lista=fila
         @fila,columna SAY  LIS.A picture '99'
         columna=columna+10
   endscan   
   fila=fila+1
   Columna=15   
ENDDO   
wait ' '
DO REPORTE WITH 2,"TINKA","LISTADO DE NUMEROS "
RELE WIND CC
RELE WIND XX
close databases
RETURN


FUNCTION AB
*--------------
SKIP
RETURN A

