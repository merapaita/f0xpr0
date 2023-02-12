SET TEXTMERGE ON
SET TEXTMERGE TO catasi.txt

USE CatAsi IN 1 ORDER TAG CatAsi5
GO TOP

\USE `sicg2009`;

SCAN
	cTippre  = TipPre
	cGeneric = Generic
	cSgn1    = IIF(!EMPTY(SgN1), ALLTRIM(STR(val(SgN1),2)),'')
	cSgn2    = IIF(!EMPTY(sgn2), ALLTRIM(STR(val(SgN2),2)),'')
	cEspn1   = IIF(!EMPTY(EspN1),ALLTRIM(STR(val(Espn1),2)),'')
	cEspn2   = IIF(!EMPTY(EspN2),ALLTRIM(STR(val(EspN2),2)),'')
	cEspn3   = IIF(!EMPTY(EspN3),ALLTRIM(STR(val(EspN3),2)),'')
	cEspn4   = IIF(!EMPTY(EspN4),ALLTRIM(STR(val(Espn4),2)),'')
	cDetalle = Detalle
	cDescri  = ALLTRIM(Descri)
	cCodPart  = ALLTRIM(cTipPre +IIF(!empty(cGeneric),'.','')+ cGeneric +IIF(!empty(cSGN1),'.','')+ cSGN1 +IIF(!empty(cSGN2),'.','')+ cSGN2 +IIF(!empty(cEspN1),'.','')+ cEspN1 +IIF(!empty(cEspN2),'.','')+ cEspN2 +IIF(!empty(cEspN3),'.','')+ cEspN3 +IIF(!empty(cEspN4),'.','')+ cEspn4)
	
	\INSERT INTO `CatAsi`(`tippre`,`generic`,`sgn1`,`sgn2`,`espn1`,`espn2`,`espn3`,`espn4`,`detalle`,`codpart`,`descri`) VALUES ('<<cTipPre>>','<<cGeneric>>','<<cSGN1>>','<<cSGN2>>','<<cEspN1>>','<<cEspN2>>','<<cEspN3>>','<<cEspn4>>','<<cDetalle>>','<<cCodPart>>','<<cDescri>>');

ENDSCAN

SET TEXTMERGE TO
SET TEXTMERGE OFF

*\N/C 26 <<PADL(xNumCP,5,'0')>> <<xMes>>  cadena <<xCad>> NO DEBE TENER PRESUPUESTALES
*  insert  into `actpry`(`codfun`,`codprg`,`codspr`,`actpry`,`descri`) values ('20','001','0001','000002','fun 20 prg 1 spr 1 ap2b'),('20','001','0001','000003','fun 20 prg 1 spr 1 ap 3'),('20','001','0001','000004','fun 20 prg 1 spr 1 ap 4'),('20','001','0001','000005','fun 20 prg 1 spr 1 ap 5'),('20','001','0001','000006','fun 20 prg 1 spr 1 ap 6'),('20','001','0001','000007','fun 20 prg 1 spr 1 ap 7'),('20','001','0001','000008','fun 20 prg 1 spr 1 ap 8');
