** PlayFir..*

set step on

K='comunidad'		&&Clave
M='lenguaje'
C=playfair(M,K)

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

function playfair
PARAMETERS M,K
DIMENSION T(5,5)

PUBLIC Tp
=matrizTp(K)
	*$M=str_replace('W','V',strtoupper($M));
	*for($i=0; $i<strlen($M); $i+=2)
	*{ $C.=$Tp[buscar($M[$i],true)][buscar($M[$i+1],false)].
		*$Tp[buscar($M[$i+1],true)][buscar($M[$i],false)];
	*}
	*return $C;

function matrizTp
PARAMETERS K
		*$Tp=array()
T(1,1) = "A"
T(1,2) = "B"
T(1,3) = "C"
T(1,4) = "C"
T(1,5) = "E"
T(2,1) = "F"
T(2,2) = "G"
T(2,3) = "H"
T(2,4) = "I"
T(2,5) = "J"
T(3,1) = "K"
T(3,2) = "L"
T(3,3) = "M"
T(3,4) = "N"
T(3,5) = "O"
T(4,1) = "P"
T(4,2) = "Q"
T(4,3) = "R"
T(4,4) = "S"
T(4,5) = "T"
T(5,1) = "U"
T(5,2) = "V"
T(5,3) = "X"
T(5,4) = "Y"
T(5,5) = "Z"

K=strTRAN(UPPER(K),'W','V')
n=1
FOR I = 1 TO 5
	FOR J = 1 TO 5
		***
		=setcar(SUBSTR(K,n,1))
		***
		*{ do{ $Tp[$i][$j]=setcar(SUBSTR(K,n,1))
		n=n+1
			*}while($Tp[$i][$j]=='')
	ENDFOR
ENDFOR
RETURN

function setcar
PARAMETERS s
	*{ global $T
		if ALLTRIM(s)#''
			FOR I = 1 TO 5			&&CONTADOR DE 5 count($T)
				if T(i)==s
					T(i) = ''
					return s
				ENDIF
				return ''
			ENDFOR
		ENDIF
		for i=1 TO 5				&&CONTADOR DE 5 count($T)
			if T(i)#''
				s   = T(i)
				T(i)= ''
				return s
			 ENDIF
		ENDFOR


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
