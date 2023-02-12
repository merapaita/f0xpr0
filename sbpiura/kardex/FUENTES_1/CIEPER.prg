ok = ve_passw('BENEF')
IF  .NOT. ok
     RETURN
ENDIF
= poperror( ;
  'Aseg£rese que NADIE m s use el sistema para poder realizar el proceso de Cierre de Mes sin ning£n problema.', ;
  .F.)
USE IN 1 KardexV ALIAS kardex  ;
    ORDER Kardexv1
IF se_cierra()
     DO espera WITH 1,  ;
        'Cierre del mes en proceso ...'
     SELECT kardex
     REPLACE estado WITH '20',  ;
             feccierre WITH  ;
             m.fecsis FOR estado <>  ;
             '99'
     DO espera WITH 2
ENDIF
DO fin_opcion
RETURN
*
FUNCTION se_cierra
IF yesno( ;
   'Esta Ud. Seguro de Cerrar el Periodo' ;
   )
     RETURN .T.
ELSE
     RETURN .F.
ENDIF
RETURN .F.
*
PROCEDURE fin_opcion
CLOSE DATABASES
RETURN
*
