CLEA
*- PROGRAMA PARA IMPRIMIR LOGOTIPO
*- DE LA EMPRESA.
*---------------------------------
=INKEY(1,'HM')
DECLARE LOGOT[14]
  LOGOT[01]='北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北'
  LOGOT[02]='北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北'
  LOGOT[03]='北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北'
  LOGOT[04]='北辈膊膊膊辈膊膊脖北膊脖北膊膊脖北北北辈膊膊膊辈脖膊北辈脖膊膊脖北辈膊北北北'
  LOGOT[05]='北辈舶鞍膊氨安舶鞍辈膊膊北膊鞍膊北北北辈舶鞍膊安舶膊氨辈舶膊鞍膊北膊膊脖北北'
  LOGOT[06]='北辈舶北卑氨辈舶北膊鞍安脖膊氨膊氨北北辈舶北膊安舶膊氨辈舶膊氨膊安舶鞍膊北北'
  LOGOT[07]='北辈舶北北北辈舶北膊膊膊舶膊膊舶氨北北辈膊膊膊安舶膊氨辈舶膊膊舶安膊膊膊氨北'
  LOGOT[08]='北辈舶北膊北辈舶北膊鞍安舶膊氨膊氨北北辈舶鞍鞍安舶膊氨辈舶膊鞍膊安舶鞍膊氨北'
  LOGOT[09]='北辈膊膊膊氨辈舶北膊氨辈舶膊氨膊氨北北辈舶北北辈舶膊膊膊舶膊氨膊安舶北膊氨北'
  LOGOT[10]='北北鞍鞍鞍氨北鞍北卑氨北鞍卑氨卑氨北北北鞍北北北鞍卑鞍鞍鞍卑氨卑氨鞍北卑氨北'
  LOGOT[11]='北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北'
  LOGOT[12]='北北北北北北Consejo Transitorio de Administraci Regional Piura北北北北北北'
  LOGOT[13]='北北北北北北北北北北北北盌irecci de Contabilidad北北北北北北北北北北北北北'
  LOGOT[14]='北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北'

  FOR J=1 TO 38  
     @ 06,40-J SAY PARTE1(LOGOT[01],J)
     @ 07,40-J SAY PARTE1(LOGOT[02],J)
     @ 08,40-J SAY PARTE1(LOGOT[03],J)
     @ 09,40-J SAY PARTE1(LOGOT[04],J)
     @ 10,40-J SAY PARTE1(LOGOT[05],J)
     @ 11,40-J SAY PARTE1(LOGOT[06],J)
     @ 12,40-J SAY PARTE1(LOGOT[07],J)
     @ 13,40-J SAY PARTE1(LOGOT[08],J)
     @ 14,40-J SAY PARTE1(LOGOT[09],J)
     @ 15,40-J SAY PARTE1(LOGOT[10],J)
     @ 16,40-J SAY PARTE1(LOGOT[11],J)
     @ 17,40-J SAY PARTE1(LOGOT[12],J)
     @ 18,40-J SAY PARTE1(LOGOT[13],J)
     @ 19,40-J SAY PARTE1(LOGOT[14],J)
     =INKEY(0.01,'HM')
  ENDFOR
  
FUNCTION PARTE1
*--------------
PARAMETERS CADENA,NUMERO
RETURN SUBS(CADENA,1,NUMERO)+SUBS(CADENA,77-NUMERO,NUMERO)

