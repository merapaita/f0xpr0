USE PROMAE IN 1 ORDER TAG PROMAE1
codant=" "
SCAN
    wait window 'Procesando...'+allt(promae.nompro) nowait
    if codant=promae.codprv
       ? promae.codprv+' '+promae.nompro
    endif
    codant=promae.Codprv
ENDSCAN
CLOS DATA
RETURN
    
    