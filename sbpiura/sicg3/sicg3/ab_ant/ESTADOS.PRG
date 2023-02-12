USE PROMAE EXCL
INDEX ON CODPRV TO PP
go top    
SCAN    
          DO CASE 
 	    	 CASE GIRO='JURIDICA'
 	    	      DO CASE
 	    	          CASE !EMPTY(TEST) AND !EMPTY(RL_LE) AND !EMPTY(REGUNF) AND !EMPTY(NUMRUC) 
 	    	                replace Estado with 'VG'
 	    	          CASE  EMPTY(TEST) AND  EMPTY(RL_LE) AND  EMPTY(REGUNF) AND  EMPTY(NUMRUC) 
 	    	                replace Estado with 'PD'
 	    	          OTHER       
 	    	          		replace Estado with 'RG'
 	    	      ENDCASE
    	     CASE GIRO='NATURAL' 
    	           DO CASE 
    	              CASE !EMPTY(RL_LE) AND !EMPTY(REGUNF) AND !EMPTY(NUMRUC) 
 	    	                replace Estado with 'VG'    	              
       	              CASE  EMPTY(RL_LE) AND  EMPTY(REGUNF) AND  EMPTY(NUMRUC) 
 	    	                replace Estado with 'PD'       	              
 	    	          OTHER      
 	    	          		replace Estado with 'RG' 	    	          
       	           ENDCASE   
	         CASE EMPTY(GIRO)
                  replace Estado with 'PD'      
    	  ENDCASE         
endscan
