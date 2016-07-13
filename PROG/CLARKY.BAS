1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'CLARKY - 06 FEB 2010
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 PI=ATN(1)*4
70 '.....title page
80 COLOR 15,2
90 PRINT " CLARK Y AIRFOIL";TAB(57);"by George Murphy VE3ERP ";
100 COLOR 7,0
110 '.....start
120 '.....diagram
130 COLOR 0,7
140 PRINT "                                                   "
150 PRINT "   AÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿B             "
160 PRINT "                                    ³              "
170 PRINT "                                   EÃÄÄÄÄÄÄÄÄÄÄÄF   F=leading edge  "
180 PRINT "   CÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´D          ³   C=trailing edge "
190 PRINT "    ³                               ³           ³  "
200 PRINT "    ³                               ÅG          ³  "
210 PRINT "    ³®ÄÄÄÄÄÄÄÄÄÄÄÄÄchordÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¯³  "
220 PRINT "                                                   "
230 PRINT "    Radius R=GB=GC                                 "
240 PRINT "    Curve CB is an arc with radius R and centre G  "
250 PRINT "    Curve BF is a ¬ elipse with axes BE and EF     "
260 PRINT "    Curve DF is a ¬ elipse with axes DE and EF     "
270 COLOR 7,0
280 PRINT :LN=CSRLIN
290 PRINT " This program designs templates for Clark Y Airfoils."
300 PRINT
310 PRINT " Press <1> to RUN or <0> to EXIT";
320 Z$=INKEY$:IF Z$=""THEN 320
330 IF Z$="0"THEN RUN GO$
340 IF Z$="1"THEN 360
350 GOTO 320
360 VIEW PRINT LN TO 24:CLS:VIEW PRINT
370 '......data input
380 LOCATE LN:INPUT " ENTER length of chord         ";L
390 LOCATE 9,19:PRINT " CHORD = ";L
400 CD=L*6.999999E-01:EF=L*3.000000E-01:BD=L*1.169999E-01:DE=BD/3:BE=BD-DE
410 BC=SQR(CD^2+BD^2):BH=BC/2
420 LD=9.488900 'angle-decimal
430 LR=LD*PI/180'angle-radians
440 J=SIN(LR)
450 GB=BH/J
460 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
470 PRINT USING " Flat bottom CD......... ####.###";CD
480 PRINT USING " Thickness BD........... ####.###";BD
490 PRINT USING " Axis EF................ ####.###";EF
500 PRINT USING " Axis BE................ ####.###";BE
510 PRINT USING " Axis DE................ ####.###";DE
520 PRINT USING " R,GB,GC................ ####.###";GB
530 PRINT USING " Angle BGC.............. ####.###ø";LD*2
540 PRINT "  For a detail drawing e-mail ve3erp@rac.ca and ask for CLARKY.jpg"
550 GOSUB 560:CLS:GOTO 70
560 '.....end
570 KEY OFF:GOSUB 640:LOCATE 25,5:COLOR 0,2
580 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
590 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
600 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 600 ELSE GOSUB 640
610 IF Z$="3"THEN RETURN
620 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
630 IF Z$="2"THEN LPRINT CHR$(12) ELSE 570
640 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
