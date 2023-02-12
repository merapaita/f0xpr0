PRIVATE rutimp, rutact, vdbf
= poperror( ;
  'La importaci¢n del kardex es una opcion que solo se hace al inicio del' +  ;
  ' periodo cualquier manipulaci¢n errada puede traer consecuencias desastrozas' +  ;
  CHR(13) +  ;
  'Aseg£rese que NADIE use el sistema para empezar el proceso.', ;
  .F.)
rutimp = '..\DATA\' + STR(YEAR(m.fecsis) - 1,4)
rutact = '..\DATA\' + STR(YEAR(m.fecsis), 4)
vdbf = SYS(3) + '.Dbf'
rutimp = GETDIR(rutimp,' SELECCIONE RUTA A IMPORTAR ')
IF  .NOT. EMPTY(rutimp)
     DO espera WITH 1,'Importando informaci¢n. Espere un mom... '
     !COPY &RutImp.parmae.*  &RutAct >NULL
     !COPY &RutImp.ArtMae.*  &RutAct >NULL
     !COPY &RutImp.Promae.*  &RutAct >NULL
     !COPY &RutImp.IteArt.*  &RutAct >NULL
     !COPY &RutImp.Cuentas.* &RutAct >NULL
     DO espera WITH 1,'Procesando informaci¢n. Espere un mom... '
     USE IN 1 InvIni
     USE IN 2 IteII
     USE IN 6 IteArt ALIAS iteart ORDER IteArt1
     SELECT iteii
     COPY TO (vdbf) STRUCTURE
     calias = rutimp + 'StkAlmv'
     USE IN 3 (calias) ORDER STKALMV3
     USE IN 4 (vdbf) ALIAS invact
     SELECT stkalmv
     GOTO TOP
     IF  .NOT. EOF()
          m.item = '0000'
          SCAN
               SCATTER MEMVAR
               m.precom = 0
               m.periodo = RIGHT(STR(YEAR(m.fecsis),4),2)
               m.numii = '0001'
               m.estado = '00'
               m.item = PADL(ALLTRIM(STR(VAL(m.item) + 1)), 4,'0')
               m.cantidad = stkalmv.salcant
               m.totcan = stkalmv.salfrac
               m.valcom = m.totcan * m.valunifr
               m.tuser = 'I'
               m.user = vuser_id
               m.huser = TIME()
               m.duser = DATE()
               = val_pre(m.codart)
               SELECT invact
               APPEND BLANK
               GATHER MEMVAR
          ENDSCAN
          SELECT iteii
          APPEND FROM (vdbf)
          SELECT invini
          m.docref = 'MEM'
          m.fecii = m.fecsis
          APPEND BLANK
          GATHER MEMVAR
     ENDIF
     USE IN 3
     USE IN 4
     ERASE (vdbf)
     DO espera WITH 1,'Generando Inventario. Espere un mom... '
     USE IN 3 Parmae ALIAS parma ORDER Parmae1
     USE IN 4 KardexV ALIAS kardex ORDER Kardexv1
     USE IN 5 StkAlmV ALIAS stkalmv ORDER StkAlmv1
     USE IN 6 IteArt ALIAS iteart ORDER IteArt1
     SELECT iteii
     GOTO TOP
     IF  .NOT. EOF()
          SCAN
               SCATTER MEMVAR
               ak = actkarpr('INV',numii,m.fecii)
          ENDSCAN
          corrinv = 1
     ELSE
          corrinv = 0
     ENDIF
     DO espera WITH 1,'Actualizando Correlativos. Espere un mom... '
     SELECT parma
     SEEK 'CORRELINVINI'
     IF FOUND()
          REPLACE nument WITH corrinv
     ENDIF
     SEEK 'CORRELFORDP '
     IF FOUND()
          REPLACE nument WITH 0
     ENDIF
     USE IN 1
     USE IN 2
     USE IN 3
     USE IN 4
     USE IN 5
     USE IN 6
     DO espera WITH 2
ELSE
     DO standby WITH 'Proceso cancelado'
     RETURN .T.
ENDIF
RETURN
*
PROCEDURE val_pre
PARAMETER cart
PRIVATE cali, cord
cali = ALIAS()
SELECT iteart
cord = ORDER()
SET ORDER TO IteArt1
IF SEEK('B' + cart)
     m.valunifr = m.valuni / iteart.fraccion
     m.valcom = m.valunifr * m.totcan
     m.precom = (m.preuni /  iteart.fraccion) * m.totcan
ELSE
     DO standby WITH 'Error en catalogo de Articulos'
ENDIF
SET ORDER TO (cord)
SELECT (cali)
RETURN
*
