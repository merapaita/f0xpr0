*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
USE SHARED GC_PRO00
GOTO TOP
a = 1
DO WHILE  .NOT. EOF()
     @ 10, 20 TO 20, 60 DOUBLE
     @ 15,25 SAY "Registro No.
     @ 15, 40 SAY a
     DO CASE
          CASE pro_coremo < 0.33 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'A   '
          CASE pro_coremo < 0.58 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'B   '
          CASE pro_coremo < 0.92 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'C   '
          CASE pro_coremo < 1.27 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'D   '
          CASE pro_coremo < 1.66 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'E   '
          CASE pro_coremo < 2.20 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'F   '
          CASE pro_coremo < 3.08 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'G   '
          CASE pro_coremo < 3.72 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'H   '
          CASE pro_coremo < 4.82 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'J   '
          CASE pro_coremo < 6.74 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'K   '
          CASE pro_coremo < 8.44 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'L   '
          CASE pro_coremo < 10.75 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'M   '
          CASE pro_coremo < 12.74 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'N   '
          CASE pro_coremo < 15.92 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'P   '
          CASE pro_coremo < 20.97 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'Q   '
          CASE pro_coremo < 24.35 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'R   '
          CASE pro_coremo < 33.75 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'T   '
          CASE pro_coremo < 39.83 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'U   '
          CASE pro_coremo < 44.06 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'W   '
          CASE pro_coremo < 49.11 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'X   '
          CASE pro_coremo < 53.91 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'Y   '
          CASE pro_coremo < 62.63 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YA  '
          CASE pro_coremo < 70.32 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YB  '
          CASE pro_coremo < 77.86 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YC  '
          CASE pro_coremo < 84.84 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YD  '
          CASE pro_coremo < 95.63 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YE  '
          CASE pro_coremo <  ;
               106.72 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YF  '
          CASE pro_coremo <  ;
               114.15 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YG  '
          CASE pro_coremo <  ;
               121.43 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YH  '
          CASE pro_coremo <  ;
               129.96 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YJ  '
          CASE pro_coremo <  ;
               139.01 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YK  '
          CASE pro_coremo <  ;
               149.02 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YL  '
          CASE pro_coremo <  ;
               158.17 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YM  '
          CASE pro_coremo <  ;
               167.99 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YN  '
          CASE pro_coremo <  ;
               179.57 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YP  '
          CASE pro_coremo <  ;
               193.71 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YQ  '
          CASE pro_coremo >=  ;
               193.71 
               REPLACE pro_clacom  ;
                       WITH  ;
                       'YQ  '
     ENDCASE
     SKIP
     a = a + 1
ENDDO
*
*** 
*** ReFox - retrace your steps ... 
***
