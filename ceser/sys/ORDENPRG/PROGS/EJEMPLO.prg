*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
DEFINE MENU ejemplo COLOR SCHEME  ;
       7
DEFINE PAD opcion1 OF ejemplo  ;
       PROMPT '\<Ingreso'
DEFINE PAD opcion2 OF ejemplo  ;
       PROMPT '\<Modifica'
DEFINE PAD opcion3 OF ejemplo  ;
       PROMPT '\<Eliminar'
DEFINE PAD opcion4 OF ejemplo  ;
       PROMPT '\<Salir'
ON SELECTION PAD opcion1 OF ejemplo do;
ingresos
ON SELECTION PAD opcion2 OF ejemplo do;
modifica
ON SELECTION PAD opcion3 OF ejemplo do;
elimina
ON SELECTION PAD opcion4 OF ejemplo do;
salir
ACTIVATE MENU ejemplo
DEACTIVATE MENU ejemplo
*
*** 
*** ReFox - retrace your steps ... 
***
