USE CAJAS IN 1 ORDER TAG CAJAS1 ALIAS CAJA
USE C:\PLAN98\PLAN98 IN 2 ORDER TAG PLAN1 alias plan

SELECT CAJA
GO TOP
SCAN
      vcodctc = allt(caja.codctc)
      select plan
      seek '10'
      scan while left(plan.cuenta,2)='10'
           if vcodctc$plan.descri
              replace caja.newctah with plan.cuenta
              EXIT
           endif
      endscan
      select caja
ENDSCAN
      