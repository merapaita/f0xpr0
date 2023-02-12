*SELECT *;
*FROM HojCon B,OrdSer D;
*WHERE (B.TipDoc = 'O/S' AND B.Periodo + B.NumRef = D.Periodo+D.NumOS)

*SELECT *;
*FROM HojCon B,Ordcom C;
*WHERE (B.TipDoc = 'O/C' AND B.Periodo + B.NumRef = C.Periodo+C.NumOC)

SELECT *;
FROM ComPag A,HojCon B;
WHERE (A.NumMesHC+A.numHC = B.NumMes + B.NumHC AND A.TipDoc='HC') OR A.TipDoc#'HC'

*WHERE (A.NumMesHC+A.numHC = B.NumMes + B.NumHC) OR (B.TipDoc = 'O/C' AND B.Periodo + B.NumRef = C.Periodo+C.NumOC) OR (B.TipDoc = 'O/S' AND B.Periodo + B.NumRef = D.Periodo+D.NumOS)