   ?   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              periodo+numPEC                 chr(18)                        _PAGENO                        "9999"                         ALLTRIM(Cia)                   date()                         "P?g.:"                        yIIF(Estado='00','Emitido',IIF(Estado='20','S/Cotizac',IIF(Estado='99','Anulado',IIF(Estado='50','Liquidado','Con O/C'))))         fecpec                         NUMPEC+'.'+PERIODO             ITEpec.CODART                  ITEpec.DEScri                  ITEpec.codUNI                  ITEpec.CANreq                  "C?digo"                       ,ALLTRIM(val_para(CODFTE,'CODFTE','D',26,50))                    
"Art?culo"                     "Cantid"                       "Unid."                         "PEDIDO - COMPROBANTE DE SALIDA"                                "PECOSA N? :"                  "Solicito entregar a: "        pecosa.atte                    #val_para(coddep,'CODDEP','D',22,60)                             	"Lispec1"                      "Dependencia"                  ":"                            "1"                            "S O L I C I T A D O"          PADC(ALLTRIM(STR(NUM,2)),2,'0')                                 J                              "N?"                           "Emisi?n :"                    
"Estado :"                     "FUENTE FINANCIAMIENTO :"      iif(itepec.tipcdr='N','*','?')                                  CHR(14)+'G'                    CHR(27)+'H'                    DESTINO                        chr(15)                        #IIF(EMPTY(DESTINO),' ','Destino :')                             chr(15)                        chr(15)                        chr(18)                        chr(18)                        "------------------------"                                      "------------------------"                                      "------------------------"                                      "------------------------"                                      "4"                            "Solicitante"                  "Jefe de Abastecimiento"       "Jefe de Almacen"              "Recib? Conforme"              
"Despacho"                     "2"                            "3"                            	":  /  /"                      chr(15)                        chr(18)                        chr(18)                        "CN"                           NUM                            NUM                            1                              periodo+numPEC                 chr(18)                        _PAGENO                        "9999"                         ALLTRIM(Cia)                   date()                         "P?g.:"                        yIIF(Estado='00','Emitido',IIF(Estado='20','S/Cotizac',IIF(Estado='99','Anulado',IIF(Estado='50','Liquidado','Con O/C'))))         fecpec                         NUMPEC+'.'+PERIODO             ITEpec.CODART                  ITEpec.DEScri                  ITEpec.codUNI                  ITEpec.CANreq                  "C?digo"                       ,ALLTRIM(val_para(CODFTE,'CODFTE','D',26,50))                    
"Art?culo"                     "Cantid"                       "Unid."                         "PEDIDO - COMPROBANTE DE SALIDA"                                "PECOSA N? :"                  "Solicito entregar a: "        pecosa.atte                    #val_para(coddep,'CODDEP','D',22,60)                             	"Lispec1"                      "Dependencia"                  ":"                            "1"                            "S O L I C I T A D O"          PADC(ALLTRIM(STR(NUM,2)),2,'0')                                 J                              "N?"                           "Emisi?n :"                    
"Estado :"                     "FUENTE FINANCIAMIENTO :"      iif(itepec.tipcdr='N','*','?')                                  CHR(14)+'G'                    CHR(27)+'H'                    DESTINO                        chr(15)                        #IIF(EMPTY(DESTINO),' ','Destino :')                             chr(15)                        chr(15)                        chr(18)                        chr(18)                        "------------------------"                                      "------------------------"                                      "------------------------"                                      "------------------------"                                      "4"                            "Solicitante"                  "Jefe de Abastecimiento"       "Jefe de Almacen"              "Recib? Conforme"              
"Despacho"                     "2"                            "3"                            	":  /  /"                      chr(15)                        chr(18)                        chr(18)                        "CN"                           NUM                            NUM                            1