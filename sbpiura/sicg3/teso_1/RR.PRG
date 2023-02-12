sele numfp,fecdoc,tipdoc,parmae.descri as docum,numdoc,ITEFP.descri as desDES,import,totigv,codpart;
 from itefp, parmae;
 where parmae.tipo='DOCCAJ' AND itefp.tipdoc = alltrim(parmae.codigo);
 ORDER BY NUMFP,FECDOC

*