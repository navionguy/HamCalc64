1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'CATENARY - 20 MAR 2009
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 '.....title page
70 CLS
80 COLOR 15,2
90 PRINT " CATENARY CURVE";TAB(57);"by George Murphy VE3ERP ";
100 COLOR 1,0:PRINT STRING$(80,223);
110 LOCATE CSRLIN-1,24
120 COLOR 15,2:PRINT " Algorithm by R.J.Dehoney, IEEE "
130 COLOR 7,0
140 '.....diagram
150 LOCATE 3,9:PRINT "H1 þA";
160 LOCATE 4,12:PRINT "³®";:PRINT STRING$(53,196);:PRINT ;"¯³"
170 LOCATE 4,38:PRINT " D "
180 LOCATE 5,12:PRINT "³®";STRING$(20,196);;"¯³®";STRING$(30,196);;"¯³"
190 LOCATE 5,21:PRINT " D2";:LOCATE 5,50:PRINT ;" D1"
200 LOCATE 5,35:PRINT "³";:LOCATE 6,68:PRINT ;"þB H2"
210 LOCATE 7,31:PRINT " H3 þC (lowest point)"
220 LOCATE 9,10:PRINT STRING$(62,196)
230 LOCATE 9,33:PRINT " datum level "
240 '.....preamble
250 LN=CSRLIN
260 LOCATE ,7
270 PRINT " This program calculates the catenary curve formed by a wire strung"
280 LOCATE ,7
290 PRINT " between points A and B, which may or may not be at the same height"
300 LOCATE ,7
310 PRINT " above ground datum. For given heights H1 & H2 it calculates H3,"
320 LOCATE ,7
330 PRINT " horizontal distance D betwwen points A & B and distances E & F for"
340 LOCATE ,7
350 PRINT " low point C."
360 LOCATE ,11
370 PRINT "(ref. The ARRL Antenna Book, 18th edition, pp. 20-1-20.3)"
380 PRINT
390 LOCATE ,22:COLOR 0,7
400 PRINT " Press <1> to continue or <0> to EXIT ";:COLOR 7,0
410 Z$=INKEY$:IF Z$=""THEN 410
420 IF Z$="0"THEN CLS:RUN GO$
430 IF Z$="1"THEN 450
440 GOTO 410 'GOTO 410
450 GOSUB 1170
460 PRINT " Press <3> for copper clad steel wire or (4> for hard drawn copper"
470 Z$=INKEY$:IF Z$=""THEN 470
480 IF Z$="3"THEN W=1:W$="Copper Clad Steel":GOTO 510
490 IF Z$="4"THEN W=2:W$="Hard Drawn Copper":GOTO 510
500 GOTO 470
510 GOSUB 1170: GOTO 1310
520 PRINT
530 INPUT " ENTER: Length of antenna wire (in feet):";L:K=L:Q=0
540 GOSUB 1170
550 PRINT
560 PRINT " Press letter in < > for AWG wire size:"
570 PRINT " <a> #4";
580 PRINT " <b> #6";
590 PRINT " <c> #8"
600 PRINT " <d> #10"
610 PRINT " <e> #12 "
620 PRINT " <f> #14"
630 PRINT " <g> #16";
640 PRINT " <h> #18";
650 PRINT " <i> #20";
660 Z$=INKEY$:IF Z$=""THEN 660
670 IF Z$="a"AND W=1 THEN GA=4:T=495: P=1.158000E-01:GOTO 860
680 IF Z$="a"AND W=2 THEN GA=4:T=214: P=7.949999E-02:GOTO 860
690 IF Z$="b"AND W=1 THEN GA=6:T=310: P=7.289999E-02:GOTO 860
700 IF Z$="b"AND W=2 THEN GA=6:T=130: P=7.949999E-02:GOTO 860
710 IF Z$="c"AND W=1 THEN GA=8:T=195: P=4.549999E-02:GOTO 860
720 IF Z$="c"AND W=2 THEN GA=8:T=84 : P=3.139999E-02:GOTO 860
730 IF Z$="d"AND W=1 THEN GA=10:T=120:P=2.879999E-02:GOTO 860
740 IF Z$="d"AND W=2 THEN GA=10:T=52 :P=3.139999E-02:GOTO 860
750 IF Z$="e"AND W=1 THEN GA=12:T=75 :P=1.810000E-02:GOTO 860
760 IF Z$="e"AND W=2 THEN GA=12:T=32 :P=1.240000E-02:GOTO 860
770 IF Z$="f"AND W=1 THEN GA=14:T=50 :P=1.140000E-02:GOTO 860
780 IF Z$="f"AND W=2 THEN G=A14:T=20 :P=1.240000E-02:GOTO 860
790 IF Z$="g"AND W=1 THEN GA=16:T=31 :P=7.100000E-03:GOTO 860
800 IF Z$="g"AND W=2 THEN GA=16:T=13 :P=4.900000E-03:GOTO 860
810 IF Z$="h"AND W=1 THEN GA=18:T=19 :P=4.499999E-03:GOTO 860
820 IF Z$="h"AND W=2 THEN GA=18:T=8  :P=4.900000E-03:GOTO 860
830 IF Z$="i"AND W=1 THEN GA=20:T=12 :P=2.799999E-03:GOTO 860
840 IF Z$="i"AND W=2 THEN GA=20:T=5  :P=3.100000E-03:GOTO 860
850 GOTO 660 'GOTO 660
860 GOSUB 1170
870 INPUT " ENTER: Height of high point A (feet)";H1
880 GOSUB 1170
890 INPUT " ENTER: Height of low point B (feet)";H2
900 IF H2>H1 THEN SWAP H1,H2
910 A=H1-H2
920 IF A<L THEN 940 ELSE  PRINT :PRINT ;" WIRE TOO SHORT ! ....press any key...."
930 Z$=INKEY$:IF Z$="" THEN 930 ELSE 60
940 IF T>P*(A+L)/2 THEN 970
950 PRINT " WIRE CANNOT SUPPORT ITS OWN WEIGHT ....press any key...."
960 Z$=INKEY$:IF Z$="" THEN 960 ELSE 60
970 GOSUB 1170:GOSUB 1310
980 '.....display  results
990 COLOR 0,7:PRINT " ";W$;" wire: ";:COLOR 7,0
1000 PRINT USING " Wire length............ ###.## ft. = ###.## m.";L,L*3.048000E-01
1010 PRINT USING " Wire size.............. ### AWG";GA
1020 PRINT USING " Weight per foot........ ###.## lb. = ###.## Kg";P,P*4.535999E-01
1030 WT=L*P
1040 PRINT USING " Total weight of wire... ###.## lb. = ###.## Kg";WT,WT*4.535999E-01
1050 PRINT USING " Tension................ ###.## lb. = ###.## Kg";T,T*4.535999E-01
1060 PRINT USING " Height H1 (point A).... ###.## ft. = ###.## m.";H1,H1*3.048000E-01
1070 PRINT USING " Height H2 (point B).... ###.## ft. = ###.## m.";H2,H2*3.048000E-01
1080 IF Q THEN 1120
1090 H3=H2-H
1100 PRINT USING " Height H3 (point C).... ###.## ft. = ###.## m.";H3,H3*3.048000E-01
1110 PRINT USING " Amount of sag.......... ###.## ft. = ###.## m.";H,H*3.048000E-01
1120 PRINT USING " Dimension D............ ###.## ft. = ###.## m.";D,D*3.048000E-01
1130 IF Q THEN PRINT:PRINT ;" Lowest point of wire is at its low end!";:GOTO 1160
1140 PRINT USING " Dimension D1........... ###.## ft. = ###.## m.";D1,D1*3.048000E-01
1150 PRINT USING " Dimension D2........... ###.## ft. = ###.## m.";D2,D2*3.048000E-01
1160 GOSUB 1190:GOSUB 1860:GOSUB 1190:GOTO 60
1170 '.....clear to bottom of screen
1180 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN:RETURN
1190 '.....PRT
1200 KEY OFF:GOSUB 1280:LOCATE 25,5:COLOR 0,2
1210 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1220 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1230 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1230 ELSE GOSUB 1280
1240 IF H<= 0 THEN PRINT " Lowest point in wire is at its low end.";:GOTO 1160
1250 IF Z$="3"THEN RETURN
1260 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1270 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1200
1280 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
1290 PRINT USING " Weight per foot............ ###.### lb. (###.##Kg)";P,P*4.535999E-01
1300 PRINT USING " Wire length................ ###.## ft. (###.##m.)";L,L*3.048000E-01
1310 '-----------------CAT4e.BAS, rjd, 3/10/09-----------------------
1320 '(a)-corrected cable weight. (b)-added calculation of D1.(c)-added high end
1330 'tension calculation. (d)-added routine to find D for given tension and
1340 'moved print statements. (e)-added routine to handle no-sag situation.
1350 'If we suspend a flexible cable of length L, weight L*P, between two points
1360 'with a height difference of A and tension T, then the separation will be D
1370 IF A=>L THEN PRINT "Cable length too short."; :GOTO 510
1380 IF T<=P*(A+L)/2 THEN 1390 ELSE 1410
1390 PRINT "Cable cannot support its own weight....press any key...."
1400 Z$=INKEY$:IF Z$=""THEN 1400 ELSE 60
1410 IF A>0 THEN 1440 ELSE H=T/P-SQR(T^2/P^2-L^2/4) :F=T
1420 D=(L^2/4-H^2)/H*LOG((L/2+H)/(L/2-H)) :D1=D/2 :D2=D/2 :GOTO 1850
1430 '
1440 'If the end heights are unequal, we can find the tension in the cable by
1450 'examining the section between the lower height and the maximum sag point.
1460 'If T=>P*(L^2+A^2)/2/A, there will be no sag, so we go to the routine later
1470 'in the program. For T<P*(L^2+A^2)/2/A, there is a sag in the cable. First,
1480 'we find L1, the length of cable between the lower end and the lowest sag
1490 'sag point. We start with a value of H that is just under Hmax = (L-A)/2.
1500 IF T=>P*(L^2+A^2)/2/A THEN 1620 'branch to the "no sag" routine.
1510 H0=(L-A)/2.049999 :H=H0
1520 L1=-(H*L-SQR(H*(A+H)*(L^2-A^2)))/A
1530 FX1=P*(L1^2-H^2)/2/H :FY1=P*F1 :FX2=FX1 :FY2=P*(L-L1) :F=SQR(FX2^2+FY2^2)
1540 IF F<T THEN H=H*9.900000E-01 :GOTO 1520
1550 '--------------routine to find distances between supports--------
1560 NUM1=(L^2-A^2)*(A+2*H)-2*L*SQR(H*(A+H)*(L^2-A^2)) :DENOM1=A^2
1570 NUM2=A^2 :DENOM2=L*(A+2*H)-2*SQR(H*(A+H)*(L^2-A^2))
1580 X=NUM2/DENOM2 :IF X>1 THEN PRINT "X=";X :STOP
1590 D=NUM1/DENOM1/2*LOG((1+X)/(1-X))
1600 D1=(L1^2-H^2)/2/H*LOG((L1+H)/(L1-H)) :D2=D-D1
1610 RETURN
1620 '----------------no sag routine---------------
1630 'This routine is for the case when the cable does not sag below the lower
1640 'support point.
1650 HMAX=T/P-A :H=HMAX :L2=L
1660 L1A=SQR(2/P*(A+H)*T-(A+H)^2)-L2 :L1B=SQR((H+A)*(L1A^2-H^2)/H+(A+H)^2)-L2
1670 IF L1A>L1B THEN H=H*9.900000E-01 :GOTO 1660
1680 L1=L1A :D1=(L1^2-H^2)/2/H*LOG((L1+H)/(L1-H))
1690 L=2*L1+L2 :X=A^2/(L*(A+2*H)-2*SQR(H*(A+H)*(L^2-A^2)))
1700 Y=(L^2-A^2)*(A+2*H) :Z=2*L*SQR(H*(A+H)*(L^2-A^2))
1710 D=(Y-Z)/A^2*LOG((1+X)/(1-X))/2 'from equation 11
1720 D=D-2*D1:D2=D-D1:L=K:Q=1:GOTO  980
1730 '---------------------------Theory---------------------
1740 'For end points at the same height, the tension is W*(L^2/4+H^2)/2/H, where
1750 'L is the length of cable and H is the sag. The distance between end points
1760 'is (L^2/4-H^2)/H*LOG((L/2+H)/(L/2-H). For end points at different heights
1770 'we must resort to trickery. We first assume a sag H, then calculate L1,the
1780 'the tension components Fx1 and Fy1. Fx1=P*(L1^2-H^2)/2/H, Fy1=P*L1. The
1790 'tension is the same at the point in the cable at the same height on the
1800 'other side of the sag, so at the upper point, the tension is greater by
1810 'the added weight of cable. So, at the upper point Fx2=Fx1, Fy2=P*(L-L1).
1820 'We now compare F=sqr(Fx2^2+Fy2^2) with our given T and, if F>T, we reduce
1830 'our trial H and recalculate. We start the calculation with H=Hmax which is
1840 '(L-A)/2.
1850 RETURN
1860 '.....text
1870 LOCATE 6
1880 TB=7     'tab for text
1890 CLS:PRINT :PRINT
1900 PRINT TAB(TB);
1910 PRINT "  The sag in a horizontal wire is the distance between the centre"
1920 PRINT TAB(TB);
1930 PRINT "of the wire and an imaginary line drawn between its ends. The"
1940 PRINT TAB(TB);
1950 PRINT "minimum amount of sag should be such that the tension in the wire"
1960 PRINT TAB(TB);
1970 PRINT "due to its own weight does not exceed 10% of the breaking point of"
1980 PRINT TAB(TB);
1990 PRINT "of the wire. This will allow for additional normal stresses due to"
2000 PRINT TAB(TB);
2010 PRINT "wind and ice loading and lightweight feedlines without stretching"
2020 PRINT TAB(TB);
2030 PRINT "the wire."
2040 PRINT TAB(TB);
2050 PRINT "  Decreasing the recommended sag by pulling the wire tighter"
2060 PRINT TAB(TB);
2070 PRINT "increases the tension, causing the wire to stretch and change its"
2080 PRINT TAB(TB);
2090 PRINT "electrical properties at RF frequencies as well as lowering its"
2100 PRINT TAB(TB);
2110 PRINT "breaking point. This can start a continuing cycle of further"
2120 PRINT TAB(TB);
2130 PRINT "sagging of the weakened wire due to its own weight and retightening"
2140 PRINT TAB(TB);
2150 PRINT "to reduce the sag until the recurring over-stressing of the"
2160 PRINT TAB(TB);
2170 PRINT "increasingly weakened wire causes it to break under normal loads."
2180 PRINT TAB(TB);
2190 PRINT "  Contrary to a popular misconception, reducing the wire size does"
2200 PRINT TAB(TB);
2210 PRINT "not reduce the recommended amount of sag. While the overall weight"
2220 PRINT TAB(TB);
2230 PRINT "will be reduced the lower breaking point of the smaller wire will"
2240 PRINT TAB(TB);
2250 PRINT "result in the recommended sag remaining the same."
2260 PRINT TAB(TB);
2270 PRINT "  Do not use aluminum or soft copper wire for antennas. To meet the"
2280 PRINT TAB(TB);
2290 PRINT "design criteria expressed above, excessively large and unsightly"
2300 PRINT TAB(TB);
2310 PRINT "amounts of sag would be required."
2320 RETURN
