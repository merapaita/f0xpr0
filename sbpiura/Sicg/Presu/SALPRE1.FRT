     !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              LEFT(ESTFUN,5)+REPO.CodCad                                      &LEFT(ESTFUN,5)+REPO.CodCad+REPO.CodFte                          chr(15)                        _PAGENO                        "9999"                         ALLTRIM(cia)                   date()                         "P g.:"                        "MODIFICACIONES"               "SALDO"                        "PRESUPUESTAL"                 "+/-"                          "(EN NUEVOS SOLES)"            VALPART                        "@( 999,999,999.99"            REPO.TOTAFE                    "@Z( 999,999,999.99"           J                              "APROBADO POR:"                "TELEFONO:"                    "ELABORADO POR:"               "T O T A L   G E N E R A L"                                     VALPART-TOTCAL                 "@Z( 999,999,999.99"           J                              LIIF(EMPTY(vCODFTE),'Todas Las Fuentes',VAL_PARA(allt(vCodfte),'CODFTE','D'))                     "@I"                           "COMPROMISO"                   "ACUMULADO"                    "PRESUPUESTO "                 "AUTORIZADO"                   REPO.TOTCAL                    "@Z( 999,999,999.99"           J                              "|"                            "CALENDARIO"                   
"APROBADO"                     "|"                            "|"                            "(En Nuevo Soles)"             "SALDO PRESUPUESTAL AL MES DE"                                  .VAL_PARA(VCALEND,'FECMES','D')+' DE '+VPERIODO                  "@T"                           "(2)"                          "(1)"                          "(3)"                          "(1) - (2)"                    REPO.CODCAD                    "Codigo"                       "Cadena"                       "Func."                        *"GASTO CORRIENTE Y DE CAPITAL POR CADENAS"                      "Y ASIGNACIONES ESPECIFICAS"                                    REPO.CODFTE                    "Val_Para(REPO.CodFte,'CODFTE','D')                              2val_parti(SUBSTR(codpart,4,5),LEFT(codpart,3),'D')              REPO.CODPART                   VALPART-TOTCAL                 "@( 999,999,999.99"            J                              TOTAFE                         "@( 999,999,999.99"            J                              TOTCAL                         "@Z( 999,999,999.99"           J                              VALPART                        "@( 999,999,999.99"            "------------------------------------------------------------------------------------------------------------------------------"                                   "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            sumpre2(codcad+codfte,'2')                                      "@Z( 999,999,999.99"           sumpre2(codcad,'1')            "@Z( 999,999,999.99"           "sumcal1(LEFT(ESTFUN,5)+codcad,'1')                              "@Z( 999,999,999.99"           )sumcal1(LEFT(ESTFUN,5)+codcad+codfte,'2')                       "@Z( 999,999,999.99"           "sumafe1(LEFT(ESTFUN,5)+codcad,'1')                              "@Z( 999,999,999.99"           )sumafe1(LEFT(ESTFUN,5)+codcad+codfte,'2')                       "@Z( 999,999,999.99"           "sumsal1(LEFT(ESTFUN,5)+codcad,'1')                              "@Z( 999,999,999.99"           )sumsal1(LEFT(ESTFUN,5)+codcad+codfte,'2')                       "@Z( 999,999,999.99"           "------------------------------------------------------------------------------------------------------------------------------"                                   "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "================"             "================"             "================"             "================"             "|"                            "=============================================================================================================================="                                   "|"                            "|"                            "|"                            "----------------"             "----------------"             "----------------"             "----------------"             "=============================================================================================================================="                                   "------------------------------------------------------------------------------------------------------------------------------"                                   ¸SUBST(ESTFUN,6,2)+'.'+SUBST(ESTFUN,8,3)+'.'+SUBST(ESTFUN,11,4)+'.'+SUBST(ESTFUN,15,1)+'.'+SUBST(ESTFUN,16,5)+'.'+SUBST(ESTFUN,21,1)+'.'+SUBST(ESTFUN,22,4)+'  META: '+SUBST(ESTFUN,26,5)            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "UNIDAD EJECUTORA :"           "UNIDAD GESTORA :"             !VAL_PARA('01','UNIGES','V',22,60)                               $VAL_PARA('01001','UNIEJE','V',22,60)                            
"PLIEGO :"                     
"SECTOR :"                     !Val_Para('034','CODSEC','D',2,60)                               !Val_Para('340','CODPLI','D',2,60)                               "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "=============================================================================================================================="                                   "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            	"Salpre1"                      LEFT(ESTFUN,5)+REPO.CodCad                                      &LEFT(ESTFUN,5)+REPO.CodCad+REPO.CodFte                          chr(15)                        _PAGENO                        "9999"                         ALLTRIM(cia)                   date()                         "P g.:"                        "MODIFICACIONES"               "SALDO"                        "PRESUPUESTAL"                 "+/-"                          "(EN NUEVOS SOLES)"            VALPART                        "@( 999,999,999.99"            REPO.TOTAFE                    J                              "@Z( 999,999,999.99"           "APROBADO POR:"                "TELEFONO:"                    "ELABORADO POR:"               "T O T A L   G E N E R A L"                                     VALPART-TOTCAL                 J                              "@Z( 999,999,999.99"           LIIF(EMPTY(vCODFTE),'Todas Las Fuentes',VAL_PARA(allt(vCodfte),'CODFTE','D'))                     "@I"                           "COMPROMISO"                   "ACUMULADO"                    "PRESUPUESTO "                 "AUTORIZADO"                   REPO.TOTCAL                    J                              "@Z( 999,999,999.99"           "|"                            "CALENDARIO"                   
"APROBADO"                     "|"                            "|"                            "(En Nuevo Soles)"             "SALDO PRESUPUESTAL AL MES DE"                                  .VAL_PARA(VCALEND,'FECMES','D')+' DE '+VPERIODO                  "@T"                           "(2)"                          "(1)"                          "(3)"                          "(1) - (2)"                    REPO.CODCAD                    "Codigo"                       "Cadena"                       "Func."                        *"GASTO CORRIENTE Y DE CAPITAL POR CADENAS"                      "Y ASIGNACIONES ESPECIFICAS"                                    REPO.CODFTE                    "Val_Para(REPO.CodFte,'CODFTE','D')                              2val_parti(SUBSTR(codpart,4,5),LEFT(codpart,3),'D')              REPO.CODPART                   VALPART-TOTCAL                 J                              "@( 999,999,999.99"            TOTAFE                         J                              "@( 999,999,999.99"            TOTCAL                         J                              "@Z( 999,999,999.99"           VALPART                        "@( 999,999,999.99"            "------------------------------------------------------------------------------------------------------------------------------"                                   "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            sumpre2(codcad+codfte,'2')                                      "@Z( 999,999,999.99"           sumpre2(codcad,'1')            "@Z( 999,999,999.99"           "sumcal1(LEFT(ESTFUN,5)+codcad,'1')                              "@Z( 999,999,999.99"           )sumcal1(LEFT(ESTFUN,5)+codcad+codfte,'2')                       "@Z( 999,999,999.99"           "sumafe1(LEFT(ESTFUN,5)+codcad,'1')                              "@Z( 999,999,999.99"           )sumafe1(LEFT(ESTFUN,5)+codcad+codfte,'2')                       "@Z( 999,999,999.99"           "sumsal1(LEFT(ESTFUN,5)+codcad,'1')                              "@Z( 999,999,999.99"           )sumsal1(LEFT(ESTFUN,5)+codcad+codfte,'2')                       "@Z( 999,999,999.99"           "------------------------------------------------------------------------------------------------------------------------------"                                   "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "================"             "================"             "================"             "================"             "|"                            "=============================================================================================================================="                                   "|"                            "|"                            "|"                            "----------------"             "----------------"             "----------------"             "----------------"             "=============================================================================================================================="                                   "------------------------------------------------------------------------------------------------------------------------------"                                   ¸SUBST(ESTFUN,6,2)+'.'+SUBST(ESTFUN,8,3)+'.'+SUBST(ESTFUN,11,4)+'.'+SUBST(ESTFUN,15,1)+'.'+SUBST(ESTFUN,16,5)+'.'+SUBST(ESTFUN,21,1)+'.'+SUBST(ESTFUN,22,4)+'  META: '+SUBST(ESTFUN,26,5)            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "UNIDAD EJECUTORA :"           "UNIDAD GESTORA :"             !VAL_PARA('01','UNIGES','V',22,60)                               $VAL_PARA('01001','UNIEJE','V',22,60)                            
"PLIEGO :"                     
"SECTOR :"                     !Val_Para('034','CODSEC','D',2,60)                               !Val_Para('340','CODPLI','D',2,60)                               "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "=============================================================================================================================="                                   "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            "|"                            	"Salpre1"