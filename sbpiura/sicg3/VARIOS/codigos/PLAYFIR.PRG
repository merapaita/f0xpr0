*<?php
*//Tabla del alfabeto...
*$T=array();
*$Tp=array();
*function buscar($s,$c=true)
*{ global $Tp;
*for($i=0; $i<5; $i++)for($j=0;$j<5; $j++) if($Tp[$i][$j]==$s)
*return $c?$i:$j;
*return false;
*}
*function setcar($s)
*{ global $T;
*if(trim($s)!=''){ for($i=0; $i<count($T); $i++)if($T[$i]==$s){
*$T[$i]=''; return $s; } return ''; }
*for($i=0; $i<count($T); $i++)if($T[$i]!=''){ $s=$T[$i]; $T[$i]='';
*return $s; }
*}
*function matrizTp($K)
*{ global $Tp,$T;
*$Tp=array();
*$T=array(
*'A','B','C','D','E',/**/
*'F','G','H','I','J',/**/
*'K','L','M','N','O',/**/
*'P','Q','R','S','T',/**/
*'U','V','X','Y','Z');
*$K=str_replace('W','V',strtoupper($K)); $n=0;
*for($i=0; $i<5; $i++)
*{ for($j=0;$j<5; $j++)
*{ do{ $Tp[$i][$j]=setcar($K[$n++]);
*}while($Tp[$i][$j]=='');
*}
*}}
*function playfair($M,$K)
*{ global $Tp;
*matrizTp($K);
*$M=str_replace('W','V',strtoupper($M));
*for($i=0; $i<strlen($M); $i+=2)
*{ $C.=$Tp[buscar($M[$i],true)][buscar($M[$i+1],false)].
*$Tp[buscar($M[$i+1],true)][buscar($M[$i],false)];
*}
*return $C;
*}
*/* Ejemplo..*/
*$K='comunidad'; //Clave
*$M='lenguaje';
*$C=playfair($M,$K);
*echo '<code>Algoritmo de Playfair<hr>';
*echo 'Clave: '.$K.'<hr>';
*echo 'Cifrado: '.$C.'<hr>';
*for($i=0; $i<5; $i++)
*{ echo '<br>';
*for($j=0;$j<5; $j++) echo $Tp[$i][$j].' ';
*} echo '<hr>';
*$M=playfair($C,$K);
*echo 'Decifrado: '.$M.'</code><hr>';
*?>
*************************************


* Traducido a Fox 
* Marco 

*Tabla del alfabeto...
PUBLIC T,Tp

DIMENSION T(5,5)
DIMENSION Tp(5,5)

** Ejemplo..*
K='comunidad'; //Clave
M='lenguaje';
C=playfair(M,K);
? 'Algoritmo de Playfair'
? 'Clave: '+ K
? 'Cifrado: ' + C

FOR i= 1 TO 5
	?
	FOR j 1 TO 5
		? Tp(i,j)+' '
	ENDFOR
ENDFOR
?
M=playfair(C,K);
? 'Decifrado: ' + M

FUNCTION buscar($s,$c=true)
FOR i= 1 to 5
	FOR j = 1 TO 5
		IF Tp(i,j)==s
			RETURN IIF(C,I,J)
		ENDIF
	ENDFOR
ENDFOR
RETURN .F.


FUNCTION setcar($s)
IF TRIM(S)#''
	FOR i = 1 TO CONTADOR DE T
		IF T(i)==s
			T(i)=''
			RETURN s;
		ENDIF
	ENDFOR
	RETURN ''
ENDIF

FOR i = 1 TO CONTADOR DE T
	IF T(i)#''
		s = t(i)
		t(i) = ''
		RETURN s
	ENDIF
ENDFOR

FUNCTION matrizTp($K)
	{ global $Tp,$T;
	$Tp=array();
	$T=array(
	'A','B','C','D','E',/**/
	'F','G','H','I','J',/**/
	'K','L','M','N','O',/**/
	'P','Q','R','S','T',/**/
	'U','V','X','Y','Z');
	$K=str_replace('W','V',strtoupper($K)); $n=0;
	FOR i = 1 TO 5
		FOR j = 1 TO 5
			{ do
				{ Tp(i,j)=setcar($K[$n++]);
				} while($Tp[$i][$j]=='');
			}
	ENDFOR
	}


function playfair
PARAMETERS M,K
matrizTp(K);
M=strTRAN(UPPER(M),'W','V');

	for I = 1 TO len(M) STEP 2
		
		{ $C.=$Tp[buscar($M[$i],true)][buscar($M[$i+1],false)].
		
		
	$Tp[buscar($M[$i+1],true)][buscar($M[$i],false)];
	}
	return $C;
	


K='comunidad'; //Clave
M='lenguaje';
C=playfair(M,K);
? 'Algoritmo de Playfair'
? 'Clave: '+ K
? 'Cifrado: ' + C

FOR i= 1 TO 5
	?
	FOR j 1 TO 5
		? Tp(i,j)+' '
	ENDFOR
ENDFOR
?
M=playfair(C,K);
? 'Decifrado: ' + M

