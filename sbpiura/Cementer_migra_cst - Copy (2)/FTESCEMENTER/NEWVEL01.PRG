*:*****************************************************************************
*:
*: Procedure file: y:\CEMENTER\NEWVEL01.PRG
*:         System: SIS_CEM
*:         Author: IBAN CHERRES FIESTAS
*:      Copyright (c) 14/12/2006, V.1.0
*:  Last modified: 14/12/2006 at 14:53:44
*:
*:  Procs & Fncts: VEVACIO()
*:
*:         Set by: VELACI01.PRG                      
*:
*:
*:           Uses: FALLECID.DBF           Alias: FA
*:               : MASTERVE.DBF           Alias: MV
*:               : VELACION.DBF           Alias: VE
*:               : MAESTRO.DBF            Alias: MA
*:               : CUARTEL.DBF            Alias: CU
*:               : TIP_CUAR.DBF           Alias: TI
*:
*:      CDX files: FALLECID.CDX
*:               : VELACION.CDX
*:               : MAESTRO.CDX
*:               : CUARTEL.CDX
*:               : TIP_CUAR.CDX
*:
*:      Documented 20:34:25                                FoxDoc version 3.00a
*:*****************************************************************************
* newvel01.PRG  Para Obtener Recibo por foco para velaciones Cementerio 1

set exclu off
clear
close all
clear all


define popup cdcampo prompt field nom_cuarte scroll margin mark chr(16)
define popup cdtipo prompt field nom_tipo scroll margin mark chr(16)

use fallecid in 0 share orde x_nombre alia fa
use masterve in 0 share alia mv
use velacion in 0 share orde falle_anyo alia ve
use maestro  in 0 share orde cementerio alia ma
use cuartel  in 0 share orde Codcu alia cu
*use cuartel  in 0 share orde cementerio alia cu
use tip_cuar in 0 share orde tipo alia ti

use paboleta in 0 share alia pabo


sele ma
loca for cod_cement = 1   && código cementerio
cod = cod_cement
nomce = nom_cement
cementerio = nomce
res = responsabl

define window ingr01 from 02, 3 to 40,80 float double color rgb(192,192,192,192,192,192) title 'IMPRESIÓN BOLETA DE ALQUILER FOCO VELACIONES' fill file blanco.bmp
activ window ingr01

sele mv
anyovelacion = allt(str(year(date())))
loca for anyo=anyovelacion and cod_cement = cod
if !eof()
   if abierto =.f.
      wait window 'El alquiler de focos está cerrado para este año...'
      close data
      release window ingr01
      return
   endif
   NichoCosto = valor_Nich
   Mausocosto = valor_Maus
   numregmv = recno()
else
   wait window 'ERROR EN EL AÑO DE LA VELACIÓN ... AVISAR A INFORMÁTICA...'
   close data
   release window ingr01
   return
endif

store 0 to cn,cna,cnn, ult, paso, error
nom_nicho =1
do while .t.
   clear
   @ 5,10 get nom_nicho function '*V \!Por \<Nombre;Por Nicho; \?Can\<celar' default 1 size 2, 17, 1
   read cycle
   clear
   if nom_nicho = 3 or lastkey() = 27
      close data
      release window ingr01
      return
   endif
   fi =' '
   co = 0
   do while nom_nicho = 2
      clear
      sele ti
      @2,2 get listac2 popup cdtipo size 12,20 default nom_tipo
      @ 15,10 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 1, 10, 1
      read cycle
      if aceptarcancelar = 2 or lastkey() = 27
         exit
      endif
      nti = nom_tipo
      cdt = cod_tipo
      sele cu
      selec * from cuartel where cod_tipo = cdt and cod_cementer = cod into cursor uc
      sele uc
      @2,25 get listacampo popup cdcampo size 12,20 default nom_cuarte
      @15,10 clear to 17,40
      @ 15,10 get aceptarcancelar function '*V \!Aceptar;\?Cancelar' default 1 size 1, 10, 1
      read cycle
      if aceptarcancelar = 2 or lastkey() = 27
         exit
      endif
      @15,10 clear to 17,40
      nomcuartel = nom_cuarte
      codcuartel = cod_cuarte
      @15,2 say 'Letra del Nicho a Buscar : '	color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @15,35 get fi								color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '@!(A'
      @17,2 say 'Nro. del Nicho a Buscar : '	color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @17,35 get co								color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192) pict '###'
      read
      sele fa
      set filter to cod_cement = cod
      set orde to x_cua_fc
      aaa= allt(str(cod))+allt(str(codcuartel))+fi+allt(str(co))
      seek aaa
      if eof()
         wait window 'EL NICHO BUSCADO NO EXISTE EN '+trim(nti)+', '+nomcuartel
         loop
      else
         if ocup_futur
            wait window 'El nicho tiene categoría de OCUPACIÓN FUTURA...'
            loop
         endif
         registro = recno()
         exit
      endif
   enddo
   if lastkey() = 27
      loop
   endif
   ************ busqueda por nombre
   store space (30) to ap,am,no
   do while nom_nicho = 1
      sele fa
      set relation to allt(str(cod_cement))+allt(str(cod_cuarte)) into cu
      clear
      activ window ingr01
      x=4
      @x,2   say "Datos a Buscar :    "	color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+2,2 say 'Apellido Paterno    '	color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+2,35 say 'Apellido Materno   '	color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+3,2 get ap pict '@(!'
      @x+3,35 get am pict '@(!'
      @x+5,2 say 'Nombres '				color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+6,2 get no pict '@(!'
      read
      if lastkey() = 27 or (len(alltrim(ap))=0 and len(alltrim(am))=0 and len(alltrim(no))=0)
         release window ingr01
         close data
         clear
         return
      endif
      sele fa
      set order to tag x_nombre of fallecid.cdx
      buscanom = allt(str(cod))+allt(ap)+allt(am)+allt(no)
      seek buscanom
      if eof()
         wait window "NO EXISTE EL NOMBRE BUSCADO..."
         loop
      endif
      
      define window encontrado from 8,1 to 23,85 title 'FALLECIDOS ENCONTRADOS... ELEGIR EL QUE DESEA CONSULTAR' close float grow shadow zoom
      browse fields cod_fallec :2:h='C',ape_patern :20:h='APELLIDO PATERNO',ape_matern :20:h='APELLIDO MATERNO',nombres :20:h='NOMBRES',;
         fila :h='Fila',columna :h='N° ', cu.nom_cuarte :25:h='Pabellón' nomodify noappend nodelete window encontrado
      registro = recno()
      release window encontrado
      exit
   enddo
   ************ hasta aquí búsqueda por nombre
   
   ***************** impresión de boleta de foco o abandono de la impresión
   
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
   comentar    = antec_fami
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
   sele cu
   seek alltrim(str(cod))+alltrim(str(codcu))
   if !eof()
      nomcuartel = nom_cuarte
      tipcu = cod_tipo
   else
      wait window 'ERROR, EL FALLECIDO NO POSEE NOMBRE DE CUARTEL... Revisar...'
      *		LOOP
   endif
   
   IF tipcu = 3 OR tipcu = 4
     nomcuartel = ""
     nomMausol = nom_cuarte
   	 cCosto = Mausocosto
     tVta   = 'MAUSOLEO'
     cCol   = "rgb(255,0,0,255,255,255)"
   ELSE
     nomcuartel = nom_cuarte
     nomMausol = ""
   	 cCosto = Nichocosto
     tVta   = 'NICHO'
     cCol   = "rgb(0,0,255,255,255,255)"
   ENDIF

   sele ti
   loca for cod_tipo = tipcu
   nomti = nom_tipo
   clear
   @0,2 say 'Cementerio:  '			color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   c_1 = allt(str(cod))+' - '+allt(nomce)
   @0,17 get c_1  pict '@(!'		color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   c_2 = allt(nomti)+ ' - ' + allt(nomcuartel)
   @2,2 get c_2 pict '@(!'			color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   if tipcu=1
      @2,35 say 'Letra : '			color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @2,45 get fi  pict '@(A'		color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @2,50 say 'Nro. : '			color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @2,59 get co	 pict '###'		color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   endif

   @3,59 say "S/. " + STR(cCosto,5,2)	color &cCol		FONT" font roman", 20

   x = 4
   n_1 = allt(nomb)+' '+allt(app)+' '+allt(apm)
   @x,2 say 'Fallecido(a) : ' get n_1 pict '@(!'						color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+2,5 get sexx function '*RNH Masculino;Femenino' size 1, 10, 1      &&DEFAULT &sexx
   @x+4,5 say 'Fecha Fallecimiento : ' get fecf							color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   x=x+6
   @x,2 say 'DATOS COMPLEMENTARIOS'										color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+2,2 say 'Apell.Paterno Padre    Apell.Materno Padre      Nombres del Padre   '		color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+3,2 get appad pict '@(!'											color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+3,25 get ampad pict '@(!'											color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+3,50 get npad  pict '@(!'											color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   x=x+3
   @x+2,2 say 'Apell.Paterno Madre    Apell.Materno Madre      Nombres de la Madre '		color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+3,2 get apmad pict '@(!'											color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+3,25 get ammad pict '@(!'											color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+3,50 get nmad  pict '@(!'											color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   x=x+3
   @x+2,2 say 'Persona Ilustre : '										color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+2,25 get pei function '*RNH Si;No' size 1, 5, 1 default 'No'
   @x+4,2 say 'Cambió Cementerio: '										color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+4,25 get ccem1 function '*RNH Si;No' size 1, 5, 1 default 'No'
   @x+4,40 say 'Cambió Nicho: '											color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+4,55 get cnic1 function '*RNH Si;No' size 1, 5, 1 default 'No'
   @x+6,2 say 'COMENTARIOS'												color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
   @x+6,15 edit comentar size 3,60
   x=x+10
   clear gets
   if ccem1 <> 'Si'
      @x+2,5 get mielecc function '*H' picture '\<Salir;\<Imprimir Boleta' size 2,17,1 default 'Salir'
      read cycle
   else
      @x+2,5 say 'ESTE FALLECIDO CAMBIÓ DE CEMENTERIO...' font 'ROMAN', 18 style 'B'		color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      @x+5,5 say 'NO PUEDE IMPRIMIRSE BOLETA' font 'ROMAN', 18 style 'B'					color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
      x=x+6
      @x+2,5 get mielecc function '*H' picture '\<Salir' size 2,17,1 default 'Salir'
      read cycle
   endif
   
   if mielecc = 'Salir' &&or lastkey() = 27
      loop
   endif
   
   sele ve
   bv = allt(str(cod))+allt(str(codfa))+anyovelacion
   seek bv
*   if !eof()
*      wait window 'NICHO YA POSEE FOCO PARA ESTE AÑO... ¿Contratará otro foco? S = Si ;  N = No   ' to sn1
*      if upper(sn1)<>'S'
*         loop
*      endif
*   endif
   
   store space(30) to contr,direc

   do while .t.
*      wait window 'PONGA LA IMPRESORA EN CABECERA DE LA BOLETA Y PRESIONE <I> PARA IMPRIMIR...' to im
*      if upper(im)<>'I'
*         loop
*      else
      	 
         define window ingr02 from 02, 3 to 20,60 float color rgb(192,192,192,192,192,192) title 'DATOS BOLETA VELACIONES' fill file blanco.bmp
         activ window ingr02
         @1,1 say 'Venta de foco en ' + tvta  color 			rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
         @3,1 say 'Nombre contratante :' get contr pict '@(!' valid vevacio()	color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
         @5,1 say 'Dirección          :' get direc pict '@(!'					color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
         read
         
         **********				 		aqui tomo el número del recibo, lo actualizo
         ********** 					OJO DEBO Activar comandos cuando es para RED
         
         IF lastkey()=27
         	 deactivate windows ingr02
         	 release widnows ingr02
	         exit
		 ENDIF
		 
         sele mv
         goto numregmv
         *			lock
         nr = '0000000'+allt(str(num_inicia))
         nr = right(nr,7)
         nuevonumin = num_inicia+1
         repla num_inicia with nuevonumin
         *			unlock
         **********
         
         release window ingr02
         
         *			Set printer on 		&& 1ra opción de impresión
         
         set device to printer			&& prompt			&& 2da opción de impresión
         
         store printstatus() to lista
         if not lista
            wait '¡Asegúrese de que la impresora está conectada y encendida!' window
         endif
         
         *			**** adaptar impresión a modelo de boleta.
         *			?
         *			?
         *			? '       '+DTOC(date())+'          '+dtoc(date())
         *			?
         *			? '                                 '+nomce
         *			? '       ' + contr+'               '+n_1
         *			? '       ' + direc+'               '+c_2
         *			?
         *			? '       ' + n_1+'                 '+fi+' / '+allt(str(co))
         *			? '       S/. '
         *			?? fococosto
         *			?
         *			?
         *			?
         *			?
         
         
*         @1,32 say nr				color rgb(0,0,0,255,255,255)
 *        @1,71 say nr				color rgb(0,0,0,255,255,255)
  *       @2,32 say dtoc(date())		color rgb(0,0,0,255,255,255)
   *      @2,71 say dtoc(date())		color rgb(0,0,0,255,255,255)	&& color rgb(255,255,0,0,0,128)
    *     @3,32 say nomce			color rgb(0,0,0,255,255,255)
     *    @5,32 say contr			color rgb(0,0,0,255,255,255)
      *   @5,71 say n_1				color rgb(0,0,0,255,255,255)
       *  @6,32 say direc			color rgb(0,0,0,255,255,255)
*         @6,71 say nomce			color rgb(0,0,0,255,255,255)
 *        @8,32 say n_1				color rgb(0,0,0,255,255,255)
  *       @8,71 say c_2				color rgb(0,0,0,255,255,255)
   *      @9,71 say fi+' / '+allt(str(co))		color rgb(0,0,0,255,255,255)
    *     @10,15 say 'S/.'			color rgb(0,0,0,255,255,255)
     *    @10,20 say fococosto pict "###.00"		color rgb(0,0,0,255,255,255)


sele pabo

*		repor form bolet01 to print noeject

sele mv

*         @2,15 say '                '+ dtoc(date())+'                '+dtoc(date()) color rgb(255,255,0,0,0,128)
*         @4,15 say 'CEMENTERIO      '+nomce
*         @5,15 say 'Nom. Contratante'+contr+'                Difunto'+n_1
*         @6,15 say 'Dir. Contratante'+direc+'             Cementerio'+nomce
*         @8,15 say 'Nom. Difunto    '+n_1  +'                Cuartel'+c_2
*         @9,15 say '                        '+'                Nicho'+fi+' / '+allt(str(co))
*         @10,15 say 'S/.'
*         @10,25 say fococosto pict "###.00"
*			set printer off
*         eject

****         SET PRINTER TO

         
         sele ve
         appen blank
         repla cod_cement with cod, anyo_velac with anyovelacion, cod_fallec with codfa,fec_recibo with date()
         repla num_recibo with nr, nom_adquir with contr, dir_adquir with direc,Costo WITH cCosto
         flush
         sele fa
	     set device to screen
   		 set console on 
         exit
*       endif
   enddo
   ***************** hasta aquí termina la impresión de la boleta
   loop
enddo

*!*****************************************************************************
*!
*!       Function: VEVACIO
*!
*!      Called by: NEWVEL01.PRG                      
*!               : NEWVEL02.PRG                      
*!
*!          Calls: LEN()              (function in ?)
*!               : ALLT()             (function in ?)
*!
*!*****************************************************************************
function vevacio
if len(allt(contr))=0
   wait window 'No puede ir en blanco el nombre del contratante...'
   return .f.
endif
return .t.
*: EOF: NEWVEL01.PRG
