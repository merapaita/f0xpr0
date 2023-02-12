*:*****************************************************************************
*:
*: Procedure file: \CEMENTER\NEWFAL02.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 14/12/2006 at 14:27:46
*:
*:         Set by: FALLEC01.PRG                      
*:
*:           Uses: ULTIFALL.DBF           Alias: UL
*:               : FALLECID.DBF           Alias: FA
*:               : CUARTEL.DBF            Alias: CU
*:               : TIP_CUAR.DBF           Alias: TI
*:               : MAESTRO.DBF            Alias: MA
*:               : CU.DBF             
*:               : FA.DBF             
*:
*:      CDX files: FALLECID.CDX
*:               : CUARTEL.CDX
*:               : TIP_CUAR.CDX
*:               : MAESTRO.CDX
*:
*:      Documented 20:34:24                                FoxDoc version 3.00a
*:*****************************************************************************
* newfal02.PRG  Ingresa información general de Fallecidos ingresados al Cementerio 2

set exclu off
clear
close all
clear all

SET REPROCESS TO -1	

define popup cdcampo prompt field nom_cuarte scroll margin mark chr(16)
define popup cdtipo prompt field nom_tipo scroll margin mark chr(16)

* contador para cementerio 1
use ultifal2 in 0 share alia ul

use fallecid in 0 share orde cod_fall alia fa
use cuartel  in 0 share orde cementerio alia cu
use tip_cuar in 0 share orde tipo alia ti
use maestro  in 0 share orde cementerio alia ma
****use usuario  in 0 share orde usuario alia uuu
sele ma
loca for cod_cement = 2
cod = cod_cement
nom = nom_cement
res = responsabl
define window falle01 from 02, 3 to 40, 80 float double color rgb(192,192,192,192,192,192) title 'DATOS GENERALES CEMENTERIO '+trim(nom_cement) fill file blanco.bmp

store space(30) to app,apm,nombre,solic
store space(20) to appad,ampad,npad,apmad,ammad,nmad
store space(10) to compr
store date() to fecf
store ctod('01/01/1700') to fecn
store 0 to colu,ult, paso, error,costo,ocfu,sinletra,cti
store ' ' to fi,comentar
valida = .t.
do while .t.
   clear
   activ window falle01
   do while .t.
      clear
      @0,2 say 'Cementerio:  ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @0,17 get cod color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '#'
      @0,21 get nom color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      clear gets
      
      * AQUI VAMOS A PONER PARA QUE PUEDAN ELEGIR UN ÚNICO PABELLÓN
      if paso = 0
         sele ti
         @2,2 get listac2 popup cdtipo size 12,30 default nom_tipo
         @ 15,10 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 1, 10, 1
         read cycle
         if aceptarcancelar = 2 or lastkey() = 27
            close data
            release window falle01
            return
         endif
         cti = cod_tipo
         nct = nom_tipo
         @2,2 clear to 17,40
      endif
      sele cu
      selec * from cu where cod_tipo = cti and cod_cementer = cod order by cu.nom_cuarte into cursor ccc
      sele ccc
      store '' to nom_cuarte
      store 0 to codcu,totcolumnas,totfilas,totalnicho
      store .F. to lleno_sn
     @2,2 get listacampo popup cdcampo size 12,20 default nom_cuarte
    	 if paso = 0
        	@ 15,10 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 1, 10, 1
	        read
    	    if aceptarcancelar = 2 or lastkey() = 27
        	   close data
	           release window falle01
    	       return
	        endif
	        @15,2 clear to 17,40
    	 endif
	     nomcuartel = nom_cuarte
	     codcu = cod_cuarte
	     totcolumnas = columnas
	     totfilas = filas
	     totalnicho = tot_nichos
	     lleno_sn = lleno
      if at('MAUS',nct)>0 and paso = 0
         if lleno = .t.
            wait window 'EL MAUSOLEO ESTÁ LLENO, NO PUEDEN INGRESAR OTRO FALLECIDO...'
            loop
         endif
         sele fa
         count to ocupado for cod_cuartel = codcu AND cod = cod_cement
         vacante = totalnicho - ocupado
         if vacante <= 0
            wait window 'EL MAUSOLEO ESTÁ LLENO, NO PUEDEN INGRESAR OTRO FALLECIDO...'
            sele cu
            loca for cod_cuarte = codcu
            repla lleno with .t.
            loop
         endif
      endif
      
      @0,55 get ocfu function '*RNH Ocupación Futura' size 1, 5, 1
      if cti = 1
         @2,25 say 'Letra : ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
         @2,35 get fi color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@A!'
         @2,40 say 'Nro. : ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
         @2,49 get colu color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
         @2,55 get sinletra function '*RNH Nicho sin Letra' size 1, 5, 1
      endif
      x = 4
      @x,25 say 'Ap.Paterno : ' get app color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!'
      @x+2,25 say 'Ap.Materno : ' get apm color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!'
      @x+4,25 say 'Nombres:   : ' get nombre color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!'
      @x+6,25 get sex function '*RNH Masculino;Femenino' size 1, 10, 1 default 'Masculino'
      @x+6,50 get adupar function '*RNH Adulto;Párvulo' size 1, 10, 1 default 'Adulto'
      @x+8,25  say 'Fecha de Nacimiento : ' get fecn color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+10,25 say 'Fecha Fallecimiento : ' get fecf color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      x=x+11
      @x,2 say 'DATOS COMPLEMENTARIOS' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+2,2 say 'Apell.Paterno Padre    Apell.Materno Padre      Nombres del Padre   ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+3,2 get appad color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @x+3,25 get ampad color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @x+3,50 get npad  color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      x=x+3
      @x+2,2 say 'Apell.Paterno Madre    Apell.Materno Madre      Nombres de la Madre ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+3,2 get apmad color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @x+3,25 get ammad color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @x+3,50 get nmad  color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      x=x+3
      @x+2,2 say 'Persona Ilustre : ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+2,25 get pei function '*RNH Si;No' size 1, 5, 1 default 'No'
      @x+3,2 say 'Cambió Cementerio: ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+3,25 get ccem function '*RNH Si;No' size 1, 5, 1 default 'No'
      @x+3,40 say 'Cambió Nicho: ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+3,55 get cnic function '*RNH Si;No' size 1, 5, 1 default 'No'
      @x+5,2 say 'COMENTARIOS' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+5,15 edit comentar size 3,60
      @x+9,2 say 'Solicitante de Nicho : ' get solic color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!'
      @x+10,2 say 'Costo del nicho: ' get costo color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###,###.##'
      @x+10,45 say 'Comprobante: ' get compr color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!'
      x=x+10
      if paso = 0 or paso = 1
         @x+2,25 get mielecc function '*H' picture '\<Cancelar;\<Grabar' size 2,17,1 default 'Cancelar'
         read cycle
         do escapar
      else
         if paso = 2
            @x+2,5 get mielecc function '*H' picture '\<Salir;\<Volver a Grabar' size 2,17,1 default 'Salir'
            do escapar
            read cycle
         endif
      endif
      
      error = 0
      
      if mielecc='Cancelar' or mielecc = 'Salir' or lastkey() = 27
         clear
         release window falle01
         close data
         clear
         return
      else
         if mielecc='Grabar' or mielecc = 'Volver a Grabar'
            **** AQUI PONER VALIDACIONES DE LA INFORMACIÓN QUE SE ESTÁ INGRESANDO
            sele fa
            sele fa.* from fa where trim(ape_patern) == trim(app) and trim(ape_matern)==trim(apm);
               and trim(nombres) == trim(nombre) and cod_cement = cod  and ocup_futur <> .t. into cursor qq
            sele qq
            homon = .f.
            cod_homo = 0
            if !eof()
               wait window 'El nombre ingresado ya existe...'
               snh = ' '
               wait window 'Se trata de un Homónimo  S = Sí / N = No ' to snh
               if upper(snh)<> 'S'
                  error = error + 1
               else
                  wait window '¿Desea grabar el Homónimo  S = Sí / N = No ? ' to dgh
                  if upper(dgh) <> 'S'
                     wait window 'Ha elegido no grabar la información...'
                     error = error + 1
                  else
                     homon = .t.
                     cod_homo = cod_fallec
                  endif
               endif
            endif
            if (len(alltrim(app))=0 or len(alltrim(apm))=0 or len(alltrim(nombre))=0)
                  wait window 'Nombres o apellidos están en blanco, eso no es permitido... Revisar'
                  error = error + 1
            endif
            if cti = 1
            	IF SINLETRA=0
	               longitud=0
    	           letras='ABCDEFGHIJKLMNÑOPQRSTUVWXYZ'
        	       longitud = substr(letras,1,totfilas)
            	   q = 0
	               q = at(fi,longitud)
    	           valida = .f.
        	       if q>0
            	      valida = .t.
	               endif
    	           if valida=.f.
        			wait window 'LA LETRA DEL NICHO NO CORRESPONDE... CORREGIR.'
    	            error = error + 1
	               endif
	            ELSE
	            	FI=''
				ENDIF
               if colu > totcolumnas or colu = 0
                  wait window 'EL NRO. DEL NICHO NO CORRESPONDE... CORREGIR.'
                  error = error + 1
               endif
               sele fa
               SET EXACT ON
               set orde to x_cua_fc
               ARMO = allt(str(cod))+allt(str(codcu))+fi+alltr(str(colu))
               seek ARMO
               if !eof()
                  wait window 'EL NICHO YA HA SIDO OCUPADO POR:'+trim(ape_patern)+' '+trim(ape_matern)+', '+trim(nombres)
                  error = error + 1
               endif
               SET EXACT OFF
               set orde to cod_fall
            endif
            if error > 0
               wait window 'Revise y vuelva a intentar grabar o cancele la operación.'
               paso = 1
               loop
            endif
            if adupar='Adulto'
            	adpa = 'A'
            else
            	adpa = 'P'
            endif
            if sex='Masculino'
               sss='M'
            else
               sss='F'
            endif
            if pei='Si'
               pil = .t.
            else
               pil = .f.
            endif
            cce = .f.
            if ccem = 'Si'
               cce = .t.
            endif
            cnni = .f.
            if cnic = 'Si'
               cnni = .t.
            endif
            ocfutura = .f.
            if ocfu = 1
               ocfutura = .t.
            endif
			SELE ul
			? RLOCK('1','ul')
		    GO TOP	
			ult = ultimo+1
		    repla ultimo with ult
			flush
			unlock
            sele fa
            append blank
            
            repla cod_cement with cod, cod_cuarte with codcu,cod_fallec with ult,fila with fi,columna with colu,ape_patern with app,ape_matern with apm,nombres with nombre,fec_fallec with fecf,fec_nacim with fecn
            repla ap_pat_pad with appad,ap_mat_pad with ampad,nom_padre with npad,ap_pat_mad with apmad,ap_mat_mad with ammad
            repla nom_madre with nmad,sexo with sss, pers_ilust with pil,camb_nicho with cnni,camb_cemen with cce,solicitant with solic,comp_pago with compr,valor_nich with costo
            repla antec_fami with comentar, homonimo with homon, cod_homoni with cod_homo,ocup_futur with ocfutura, adul_parv with adpa
            repla DIGITADOR WITH SUBSTR(SYS(0),1,10), fec_digit with date(),hor_digit with time()
            
            paso = 2
         endif
      endif
   enddo
   loop
enddo
do escapar
clear
close all
return

*!*****************************************************************************
*!
*!       Function: ESCAPAR
*!
*!      Called by: NEWCUA01.PRG                      
*!               : NEWFAL01.PRG                      
*!               : NEWCUA02.PRG                      
*!               : NEWFAL02.PRG                      
*!
*!          Calls: LASTKEY()          (function in ?)
*!
*!*****************************************************************************
function escapar
if lastkey() = 27
   release window falle01
   close data
   clear
   return
endif
*: EOF: NEWFAL01.PRG
