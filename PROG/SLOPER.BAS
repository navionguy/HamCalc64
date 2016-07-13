1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'SLOPER - Sloper Antennas - 07 JAN 97 rev. 02 OCT 97
20 IF EX$=""THEN EX$="EXIT"
30 '
40 COLOR 7,0,1
50 CLS:KEY OFF    'start
60 PI=3.141592
70 U$="###.###"
80 COLOR 15,2
90 PRINT " SLOPER ANTENNA DIMENSIONS";TAB(57);;"by George Murphy VE3ERP ";
100 COLOR 1,0:PRINT STRING$(80,223);
110 COLOR 7,0
120 '
130 '.....diagram
140 LOCATE 3
150 PRINT "  Ú¯  Ì\"
160 PRINT "  ³   º \®ÄÄ upper support"
170 PRINT "  ³   º  \"
180 PRINT "  ³   º   0®ÄÄ insulator"
190 PRINT "  ³   º    \"
200 PRINT "  ³   º     \"
210 PRINT "  ³   º      \"
220 PRINT "  ³   º       \"
230 PRINT "  ³   º        \"
240 PRINT "  A   º         \®ÄÄ antenna wire"
250 PRINT "  ³   º®- metal  \"
260 PRINT "  ³   º   mast or \"
270 PRINT "  ³   º   tower    \"
280 PRINT "  ³   º  (grounded) \"
290 PRINT "  ³   º              \";'®ÄÄ HIGH VOLTAGES MAY APPEAR AT THIS POINT !;";
300 PRINT "  ³   º    slope      0®ÄÄ insulator"
310 PRINT "  ³   º    angle ÄÄ¯é  \®ÄÄ lower support "
320 PRINT "  À¯ Ä×ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ\ÄÄÄ ground"
330 PRINT "      ³®ÄÄÄÄÄÄ B ÄÄÄÄÄÄ¯³"
340 '
350 GOSUB 2040
360 GOSUB 1920:COLOR 0,7
370 PRINT " Press 1 to continue or 0 to EXIT....."
380 COLOR 7,0
390 Z$=INKEY$:IF Z$=""THEN 390
400 IF Z$="0"THEN CLS:RUN EX$
410 IF Z$="1"THEN 430
420 GOTO 390
430 GOSUB 1920:COLOR 0,7
440 PRINT " Do you want dimensions in (m)etres or (f)eet?  (m/f) "
450 COLOR 7,0
460 Z$=INKEY$:IF Z$=""THEN 460
470 IF Z$="m"THEN D$="metres":K=3.048000E-01:GOTO 500
480 IF Z$="f"THEN D$="feet":K=1:GOTO 500
490 GOTO 460
500 GOSUB 1920:COLOR 0,7
510 LOCATE 5,45:PRINT " All dimensions are in ";D$;;" "
520 LOCATE 23:COLOR 0,7
530 INPUT " ENTER: Frequency of operation in MHz ";F$
540 F=VAL(F$):WL=2.997925E+02/F/3.048000E-01/4   '1/4 wavelength in free space (feet)
550 COLOR 7,0:GOSUB 1920:LOCATE 23:COLOR 0,7
560 PRINT " Do you want a (f)ull sloper or a (h)alf sloper?  (f/h) "
570 COLOR 7,0
580 Z$=INKEY$:IF Z$=""THEN 580
590 IF Z$="h"THEN S$="HALF":COLOR 7,0:GOTO 620
600 IF Z$="f"THEN S$="FULL":COLOR 7,0:GOTO 650
610 GOTO 580
620 LOCATE  7,12:PRINT "Í ®ÄÄ feedpoint"
630 LOCATE 12,18:PRINT "®ÄÄ antenna wire";:GOTO 690
640 '
650 LOCATE  6,16:PRINT "insulator"
660 LOCATE 9,15:PRINT "®ÄÄ antenna wire"
670 LOCATE 12,17:PRINT "Í ®ÄÄ feedpoint  "
680 LOCATE 15,21:PRINT "®ÄÄ antenna wire"
690 IF S$="FULL"THEN 720
700 LOCATE 17,23:PRINT "®ÄÄ";
710 COLOR 0,7:PRINT " HIGH VOLTAGES MAY APPEAR AT THIS POINT !"
720 COLOR 7,0
730 LOCATE 3,45:PRINT S$;;"-SLOPER ANTENNA for ";F$;;" MHz"
740 LOCATE 4,45:PRINT STRING$(34,205)
750 GOSUB 1090
760 '
770 LOCATE 12,4:COLOR 30:PRINT "*"
780 GOSUB 1920:LOCATE 23
790 COLOR 0,7:PRINT " ENTER: Height to top attachment point in ";D$;;": A= ";
800 INPUT A
810 LOCATE 9,45:PRINT " A =";USING U$;A;:PRINT " "
820 LOCATE 12,4:COLOR 7,0:PRINT " "
830 GOSUB 1920
840 GOSUB 2150
850 '
860 COLOR 15,0:LOCATE 21
870 PRINT " Keep upper support as short as practicable "
880 LOCATE 4,27:COLOR 30:PRINT "*"
890 LOCATE 23
900 COLOR 0,7:PRINT " ENTER: Length of upper support (";D$;;")";
910 INPUT UG
920 LOCATE 4,27:PRINT USING U$;UG;:PRINT " long"
930 COLOR 7,0
940 '
950 LOW=(A-UG-EL)*1.009999:IF LOW<=0 THEN LOW=0
960 GOSUB 1920
970 LOCATE 22:COLOR 15,0
980 PRINT " Minimum length of lower support is";
990 PRINT USING "###.##";LOW:PRINT " ";D$
1000 LOCATE 19,42:COLOR 30:PRINT "*"
1010 LOCATE 23
1020 COLOR 0,7:PRINT " ENTER: Length of lower support (";D$;;")";
1030 INPUT LG
1040 LOCATE 19,42:PRINT " C= ";USING U$;LG;:PRINT " long"
1050 COLOR 7,0
1060 IF LG<LOW THEN BEEP:GOTO 960
1070 GOTO 1380
1080 '
1090 '.....wire element lengths
1100 GOSUB 1920:LOCATE 23
1110 IF S$="FULL"THEN 1140
1120 IF S$="HALF"THEN 1270
1130 '
1140 '.....full sloper
1150 COLOR 15,0:LOCATE 22
1160 PRINT " At ";F$;;" MHz the length of a quarter-wave in free space is";
1170 PRINT USING "####.##";WL*K;:PRINT " ";D$
1180 COLOR 0,7:LOCATE 23
1190 PRINT " ENTER: Length of each antenna wire element in ";D$;;" ";:INPUT WIRE
1200 EL=2*WIRE  'element length
1210 COLOR 0,7
1220 LOCATE 10,19:PRINT USING U$;WIRE;:PRINT " long"
1230 LOCATE 16,25:PRINT USING U$;WIRE;:PRINT " long"
1240 COLOR 7,0
1250 RETURN
1260 '
1270 '.....half sloper
1280 COLOR 15,0:LOCATE 22
1290 PRINT " At ";F$;;" MHz the length of a quarter-wave in free space is";
1300 PRINT USING "####.##";WL*K;:PRINT " ";D$
1310 COLOR 0,7:LOCATE 23
1320 PRINT " ENTER: Length of antenna wire element in ";D$;;" ";:INPUT WIRE
1330 EL=WIRE  'element length
1340 LOCATE 13,22:PRINT USING U$;WIRE;:PRINT " long"
1350 COLOR 7,0
1360 RETURN
1370 '
1380 HYP=EL+UG+LG        'hypotenuse
1390 B=SQR(HYP^2-A^2)
1400 TH=ATN(A/B)*180/PI    'angle in degrees
1410 GOSUB 1430:GOTO 1650
1420 '
1430 '.....display dimensions
1440 LOCATE  7,45:PRINT " Length of slope =";
1450 COLOR 0,7:PRINT USING U$;HYP;:PRINT " "
1460 COLOR 0,7
1470 LOCATE  9,45:PRINT " A =";USING U$;A;:PRINT " "
1480 LOCATE 11,45:PRINT " B =";USING U$;B;:PRINT " "
1490 LOCATE 13,45:PRINT " é =";USING "###.#";TH;:PRINT "ø "
1500 COLOR 7,0:GOSUB 1920:COLOR 15
1510 PRINT" Press number in < > to change a value, accept as-is, or start over:"
1520 LOCATE CSRLIN-1,18:COLOR 30:PRINT "< >"
1530 COLOR 0,7
1540 PRINT "Û <1> dim.A Û <2> dim.B Û <3> dim.C ";
1550 PRINT "Û <4> angle é Û <5> Accept Û <6> Re-start ";
1560 COLOR 7,0
1570 Z$=INKEY$:IF Z$=""THEN 1570
1580 IF Z$="1"THEN 1660
1590 IF Z$="2"THEN 1740
1600 IF Z$="3"THEN 1810
1610 IF Z$="4"THEN 1840
1620 IF Z$="5"THEN 1960
1630 IF Z$="6"THEN 40
1640 GOTO 1570
1650 '
1660 '.....change A
1670 GOSUB 1920:LOCATE 22
1680 COLOR 15,0:PRINT " Dim. A must be less than ";USING "###.## ";HYP;:PRINT D$
1690 COLOR 0,7:PRINT " ENTER: New dimension A in ";D$;:INPUT A
1700 IF A>=HYP THEN BEEP:COLOR 7,0:GOSUB 1920:GOTO 1690
1710 B=SQR(HYP^2-A^2)
1720 TH=ATN(A/B)*180/PI
1730 GOTO 1460
1740 '.....change B
1750 GOSUB 1920:COLOR 0,7
1760 PRINT " ENTER: New dimension B in ";D$;:INPUT B
1770 IF B>=HYP THEN BEEP:GOSUB 1920:GOTO 1760
1780 A=SQR(HYP^2-B^2)
1790 TH=ATN(A/B)*180/PI
1800 GOTO 1460
1810 '.....change C
1820 GOSUB 1920:COLOR 7,0
1830 GOTO 960
1840 '.....change (é)
1850 GOSUB 1920:COLOR 0,7
1860 INPUT " ENTER: New angle (é) in degrees";TH
1870 IF TH>=90 THEN BEEP:GOSUB 1920:GOTO 1860
1880 TR=TH*PI/180    'angle in radians
1890 A=SIN(TR)*HYP:B=COS(TR)*HYP
1900 GOTO 1460
1910 '
1920 '.....erase lines 22-24
1930 VIEW PRINT 22 TO 24:CLS:VIEW PRINT:LOCATE 23
1940 RETURN
1950 '
1960 '.....end
1970 GOSUB 1920:LOCATE 22
1980 PRINT " Dimensions shown are exact for a right-angle triangle. Allow";
1990 PRINT " extra length in"
2000 PRINT " supports and antenna wires to suit actual site conditions and for";
2010 PRINT " pruning.";
2020 GOSUB 2220:GOTO 40
2030 '
2040 '.....preface
2050 X=8:Y=43
2060 COLOR 15,2:LOCATE X
2070 LOCATE ,Y:PRINT " See the ARRL HANDBOOK or the ARRL   "
2080 LOCATE ,Y:PRINT " ANTENNA BOOK to determine lengths   "
2090 LOCATE ,Y:PRINT " of antenna wire elements and other  "
2100 LOCATE ,Y:PRINT " essential design criteria. This     "
2110 LOCATE ,Y:PRINT " program is intended ONLY to compute "
2120 LOCATE ,Y:PRINT " the geometry of the installation.   "
2130 COLOR 7,0:RETURN
2140 '
2150 '.....erase preface
2160 FOR Z=X TO 13
2170 LOCATE Z,Y:PRINT STRING$(80-Y,32);
2180 NEXT Z
2190 RETURN
2200 END
2210 '
2220 'PRT
2230 KEY OFF:GOSUB 2300:LOCATE 25,5:COLOR 0,2
2240 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
2250 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
2260 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 2260 ELSE GOSUB 2300
2270 IF Z$="3"THEN RETURN
2280 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
2290 IF Z$="2"THEN LPRINT CHR$(12) ELSE 2230
2300 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
