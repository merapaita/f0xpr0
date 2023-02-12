*------------------------------------------
* resumen de ingresos y gastos de loteria 
*------------------------------------------
SET DELETED ON
SET EXCLUSIVE OFF
SET TALK ON
SET CENTURY on
SET DEFAULT TO h:\sicgdata\data2016

USE cheque
SET FILTER TO BETWEEN(VAL(nummes),4,6) AND codctc='631-140440'
COPY TO gastos_loteria.xls TYPE XL5

USE movbco
SET FILTER TO BETWEEN(VAL(nummes),4,6) AND codctc='631-140440'
COPY TO ingresos_loteria.xls TYPE XL5
