CLOSE DATA
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE calen    IN 2   order tag calen3   ALIAS calen
USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE IteArt   IN 6   order tag IteArt1  ALIAS Iteart
USE maepre   IN 10  order tag maepre1  ALIAS maepre
USE itepar   in 11  order tag itepar1  ALIAS ITEPAR          
USE pROMAE   IN 12  ORDER TAG PROMAE1  ALIAS PROMAE
USE OrdCom   IN 13  ORDER TAG Ordcom1  ALIAS Orden
USE HojCon   IN 14  ORDER TAG HojCon1  ALIAS Hoja

USE GARATE IN 15 ALIAS QQ
GO TOP
SET PROC TO SAI_LIB
SELE QQ
LIST OFF FIELD NUMOC,PERHC,NUMHC,NUMCP,FECOC,VAL_PRV(CODPRV),VALTOT,ANULTOT,FECDESP TO GARA