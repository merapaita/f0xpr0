WORKER=.T.
SET PROCEDURE TO Sai_lib
DO Fox_ambi
SET PATH TO F:\BENE2000\DATA
xAno = '2000'
CIA='SOCIEDAD DE BENEFICENCIA DE PIURA'

do reshc WITH '08','0019','531130'
do reshc WITH '08','0019','531139'
do reshc WITH '12','0019','531130'
do reshc WITH '12','0019','531139'

SET PATH TO F:\BENE2001\DATA
xAno = '2001'

do reshc WITH '09','0010','531130'
do reshc WITH '09','0010','531139'
do reshc WITH '10','0010','531130'
do reshc WITH '10','0010','531139'
