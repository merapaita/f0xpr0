*:*****************************************************************************
*:
*: Procedure file: y:\CEMENTER\CONOCF02.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 08/12/2006 at 18:49:28
*:
*:         Set by: FALLEC02.PRG                      
*:
*:          Calls: CHR()              (function in ?)
*:               : RGB()              (function in ?)
*:               : TRIM()             (function in ?)
*:               : LASTKEY()          (function in ?)
*:               : ALLT()             (function in ?)
*:               : STR()              (function in ?)
*:               : EOF()              (function in ?)
*:               : RECNO()            (function in ?)
*:               : SPACE()            (function in ?)
*:               : DATE()             (function in ?)
*:               : ALLTRIM()          (function in ?)
*:               : UPPER()            (function in ?)
*:               : DTOC()             (function in ?)
*:               : LEN()              (function in ?)
*:               : SUBSTR()           (function in ?)
*:               : AT()               (function in ?)
*:
*:           Uses: FALLECID.DBF           Alias: FA
*:               : MAESTRO.DBF            Alias: MA
*:               : CUARTEL.DBF            Alias: CU
*:               : TIP_CUAR.DBF           Alias: TI
*:               : CU.DBF             
*:
*:      CDX files: FALLECID.CDX
*:               : MAESTRO.CDX
*:               : CUARTEL.CDX
*:               : TIP_CUAR.CDX
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* conocf02.PRG  Para Consultar/Modificar/Eliminar Nichos Ocupación Futura del Cementerio 2

set exclu off
clear
close all
clear all

define popup cdcampo prompt field nom_cuarte scroll margin mark chr(16)
define popup cdtipo prompt field nom_tipo scroll margin mark chr(16)

use fallecid in 0 share orde x_nombre alia fa
use maestro  in 0 share orde cementerio alia ma
use cuartel  in 0 share orde cementerio alia cu
use tip_cuar in 0 share orde tipo alia ti

sele ma
loca for cod_cement = 2   && código cementerio
cod = cod_cement
nomce = nom_cement
cementerio = nomce

res = responsabl
define window ingr01 from 02, 3 to 40,80 float double color rgb(192,192,192,192,192,192) title 'CONSULTA / MODIFICACIÓN COMPRA OCUPACIÓN FUTURA CEMENTERIO '+trim(cementerio) fill file blanco.bmp
select cu.* from cu where cu.cod_cement = cod order by cu.nom_cuarte into cursor ccc

activ window ingr01
fi =' '
co = 0
do while .t.
   clear
   sele ti
   @2,2 get listac2 popup cdtipo size 12,20 default nom_tipo
   @ 15,10 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 1, 10, 1
   read cycle
   if aceptarcancelar = 2 or lastkey() = 27
      close data
      release window ingr01
      return
   endif
   nti = nom_tipo
   cdt = cod_tipo
   sele cu
   selec * from cuartel where cod_tipo = cdt and cod_cementer = cod into cursor uc
   sele uc
   @2,25 get listacampo popup cdcampo size 12,20 default nom_cuarte
   @15,10 clear to 19,40
   @ 15,10 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 1, 10, 1
   read cycle
   if aceptarcancelar = 2 or lastkey() = 27
      close data
      release window ingr01
      return
   endif
   @15,10 clear to 19,40
   nomcuartel = nom_cuarte
   codcuartel = cod_cuarte
   @15,10 clear to 17,40
   @15,2 say 'Letra del Nicho a Buscar : ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @15,35 get fi color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!(A'
   @17,2 say 'Nro. del Nicho a Buscar : ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @17,35 get co color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
   read
   sele fa
   set orde to x_cua_fc
   aaa= allt(str(cod))+allt(str(codcuartel))+fi+allt(str(co))
   seek aaa
   if eof()
      wait window 'EL NICHO BUSCADO NO EXISTE EN '+trim(nti)+', '+nomcuartel
      loop
   else
      if ocup_futur
         registro = recno()
         exit
      else
         wait window 'EL Nicho buscado no tiene categoría de OCUPACIÓN FUTURA'
         loop
      endif
   endif
enddo
store space(30) to ap,am,no
fec = date()
store 0 to cn,cna,cnn, ult, paso, error
do while .t.
   clear
   x=4
   sele fa
   goto registro
   codcu = cod_cuarte
   codfa = cod_fallec
   fi    = fila
   co    = columna
   fiant = fi
   coluant = co
   app   = ape_patern
   apm   = ape_matern
   nomb  = nombres
   fecf  = fec_fallec
   fecn  = fec_nacim
   appad = ap_pat_pad
   ampad = ap_mat_pad
   npad = nom_padre
   apmad = ap_pat_mad
   ammad = ap_mat_mad
   nmad = nom_madre
   se    = sexo
   if se='M'
      sexx = 'Masculino'
   else
      sexx = 'Femenino'
   endif
   pil   = pers_ilust
   if pil=.t.
      pei='Si'
   else
      pei='No'
   endif
   comentar = allt(antec_fami)
   cnic    = camb_nicho
   cnic1 = 'No'
   if cnic = .t.
      cnic1 = 'Si'
   endif
   ccem	= camb_cemen
   ccem1 = 'No'
   if ccem = .t.
      ccem1 = 'Si'
   endif
   solic = solicitant
   compr = comp_pago
   costo = valor_nich
   sele cu
	SET ORDE TO CODCU
	SET EXAC ON
   seek alltrim(str(cod))+alltrim(str(codcu))
    SET ORDE TO CEMENTERIO
    SET EXAC OFF
   if !eof()
      nomcuartel = nom_cuarte
      tipcu = cod_tipo
   else
      wait window 'ERROR, EL FALLECIDO NO POSEE NOMBRE DE CUARTEL... Revisar...'
      *		LOOP
   endif
   clear typeahead
   sele ti
   loca for cod_tipo = tipcu
   nomti = nom_tipo
   do while .t.
      clear
      @0,2 say 'Cementerio:  ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @0,17 get cod color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '#'
      @0,21 get nomce color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @1,2 get nomti color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @1,15 get nomcuartel color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      
      if tipcu=1
         @2,25 say 'Letra : ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
         @2,35 get fi color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(A'
         @2,40 say 'Nro. : ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
         @2,49 get co color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
      endif
      x = 4
      @x,25 say 'Ap.Paterno : ' get app color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @x+2,25 say 'Ap.Materno : ' get apm color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @x+4,25 say 'Nombres:   : ' get nomb color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
      @x+6,25 get sexx function '*RNH Masculino;Femenino' size 1, 10, 1      &&DEFAULT &sexx
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
      @x+3,25 get ccem1 function '*RNH Si;No' size 1, 5, 1 default 'No'
      @x+3,40 say 'Cambió Nicho: ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+3,55 get cnic1 function '*RNH Si;No' size 1, 5, 1 default 'No'
      @x+5,2 say 'COMENTARIOS' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+5,15 edit comentar size 3,60
      @x+9,2 say 'Solicitante de Nicho : ' get solic color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!'
      @x+10,2 say 'Costo del nicho: ' get costo color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###,###.##'
      @x+10,45 say 'Comprobante: ' get compr color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!'
      x=x+10
      _curobj = 1
      clear gets
      @x+2,5 get mielecc function '*H' picture '\<Salir;\<Modificar;\<Eliminar' size 2,17,1 default 'Salir'
      read cycle
      if mielecc='Eliminar'
         wait window 'Seguro que desea eliminarlo S/N' to sn
         if upper(sn) = 'S'
            * procedemos a eliminar lògicamente el registro
            sele fa
            goto registro
            if !eof()
               delete
               wait window 'El Registro ha sido ELIMINADO'
               release window ingr01
               close data
               clear
               return
            else
               wait window 'ERROR AL BORRAR EL REGISTRO... Comunicarse con el Centro de Cómputo'
            endif
         endif
      endif
      error = 0
      select ti
      tc = cod_tipo
      if mielecc = 'Salir' &&or lastkey() = 27
         clear
         release window ingr01
         close data
         clear
         return
      endif
      if mielecc = 'Modificar'
         do while .t.
            set escape off
            clear
            @0,2 say 'Cementerio:  ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
            @0,17 get cod color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '#'
            @0,21 get nomce color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
            @1,2 get nomti color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
            @1,15 get nomcuartel color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
            select ccc
            loca for nom_cuarte=nomcuartel
            @2,2 get listacampo popup cdcampo size 12,20 default nom_cuarte
            if tipcu=1
               @2,25 say 'Letra : ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
               @2,35 get fi color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(A'
               @2,40 say 'Nro. : ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
               @2,49 get co color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
            endif
            totcolumnas = columnas
            totfilas = filas
            clear gets
            x = 4
            @x,25 say 'Ap.Paterno : ' get app color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
            @x+2,25 say 'Ap.Materno : ' get apm color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
            @x+4,25 say 'Nombres:   : ' get nomb color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@(!'
            @x+6,25 get sexx function '*RNH Masculino;Femenino' size 1, 10, 1      &&DEFAULT &sexx
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
            x = x + 3
            @x+2,2 say 'Persona Ilustre : ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
            @x+2,25 get pei function '*RNH Si;No' size 1, 5, 1 default 'No'
            @x+3,2 say 'Cambió Cementerio: ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
            @x+3,25 get ccem1 function '*RNH Si;No' size 1, 5, 1 default 'No'
            @x+3,40 say 'Cambió Nicho: ' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
            @x+3,55 get cnic1 function '*RNH Si;No' size 1, 5, 1 default 'No'
            @x+5,2 say 'COMENTARIOS' color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
            comentar = comentar+'. CON FECHA '+dtoc(fecf)+' SE ADQUIRIÓ NICHO COMO OCUPACIÓN FUTURA.'
            @x+5,15 edit comentar size 3,60
            @x+9,2 say 'Solicitante de Nicho : ' get solic color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!'
            @x+10,2 say 'Costo del nicho: ' get costo color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###,###.##'
            @x+10,45 say 'Comprobante: ' get compr color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!'
            x=x+10
            @x+2,5 get mielecc function '*H' picture '\<Salir;\<Actualizar' size 2,17,1 default 'Salir'
            read cycle
            codcu = cod_cuartel
            error = 0
            if mielecc = 'Salir'
               clear
               set escape on
               release window ingr01
               close data
               clear
               return
            else
               if mielecc='Actualizar'
                  **** AQUI PONER VALIDACIONES DE LA INFORMACIÓN QUE SE ESTÁ INGRESANDO
                  sele fa
                  *				  	sele count(nombres) as n from fa where trim(ape_patern) == trim(app) and trim(ape_matern)==trim(apm);
                  *						and trim(nombres) == trim(nomb) into cursor qq
                  *				  	sele qq
                  *				  	if n>0
                  *				  	  wait window 'El nombre ingresado ya existe. Revise sus datos...'
                  *  				  	  error = error + 1
                  *				  	endif
                  if len(alltrim(app))=0 or len(alltrim(apm))=0 or len(alltrim(nomb))=0
                     wait window 'Nombres o apellidos están en blanco, eso no es permitido... Revisar'
                     error = error + 1
                  endif
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
                  if co > totcolumnas or co = 0
                     wait window 'EL NRO. DE COLUMNA NO CORRESPONDE... CORREGIR.'
                     error = error + 1
                  endif
                  *				  	if fi <> fiant or co <> coluant
                  *					  	sele fa
                  *					  	set orde to x_fila_col
                  *					  	seek allt(str(cod))+fi+allt(str(co))
                  *					  	if !eof()
                  *					  		wait window 'EL NICHO YA HA SIDO OCUPADO POR:'+trim(ape_patern)+' '+trim(ape_matern)+', '+trim(nombres)
                  *					  		error = error + 1
                  *					  	endif
                  *						set orde to cod_fall
                  *				  	endif
                  if error > 0
                     loop
                  endif
                  if sexx='Masculino'
                     sss='M'
                  else
                     sss='F'
                  endif
                  if pei='Si'
                     pil = .t.
                  else
                     pil = .f.
                  endif
                  cnni = .f.
                  if cnic1 ='Si'
                     cnni = .t.
                  endif
                  cce = .f.
                  if ccem1 = 'Si'
                     cce = .t.
                  endif
                  
                  sele fa
                  goto registro
                  repla cod_cuarte with codcu,fila with fi,columna with co,ape_patern with app,ape_matern with apm,nombres with nomb,fec_fallec with fecf,fec_nacim with fecn
                  repla ap_pat_pad with appad,ap_mat_pad with ampad,nom_padre with npad,ap_pat_mad with apmad,ap_mat_mad with ammad
                  repla nom_madre with nmad,sexo with sss, pers_ilust with pil, camb_nicho with cnni,camb_cemen with cce,solicitant with solic,comp_pago with compr,valor_nich with costo
                  repla antec_fami with comentar,ocup_futur with .f.
                  wait window 'INFORMACIÓN FUE ACTUALIZADA...'
                  tipcu = tc
                  close all
                  release window ingr01
                  close data
                  clear
                  return
               endif
            endif
         enddo
      endif
   enddo
   loop
enddo
clear
close all
return
*: EOF: CONOCF02.PRG
