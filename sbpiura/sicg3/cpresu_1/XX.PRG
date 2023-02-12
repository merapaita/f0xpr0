go bott
scatter memvar
m.numhc = "0000"
m.nummes = "07"
for i = 1 to 74
	m.numhc = alltrim(str(val(m.numhc) + 1))
	append blank
	gather memvar
endfor
