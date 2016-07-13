1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'COILQUIK - 13 FEB 2006
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 UL$=STRING$(80,205)
70 GOTO 110
80 '
90 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
100 RETURN
110 '....start
120 PRINT " COIL Q Quick Estimator";TAB(57);"by George Murphy VE3ERP ";
130 PRINT UL$;
140 PRINT " This program can be used to estimate the umloaded Q of an unshi";
150 PRINT "elded solenoid."
160 PRINT "   Ref:  Reference Data for Radio Engineers, 6th edition, page 6";
170 PRINT "-4, Fig.3.     "
180 PRINT UL$;
190 LN=CSRLIN
200 PRINT " To run program........................press 1";
210 PRINT " To run `Coil Q Calculator'program.....press 2";
220 PRINT " To EXIT...............................press 0";
230 Z$=INKEY$:IF Z$=""THEN 230
240 IF Z$="0"THEN CLS:RUN EX$
250 IF Z$="1"THEN GOSUB 90:GOTO 290
260 IF Z$="2"THEN CLS:RUN "coilq"
270 GOTO 230
280 '
290 LN=CSRLIN
300 INPUT " ENTER: Mean diameter of coil (inches)......";D
310 INPUT " ENTER: Length of coil (inches).............";L
320 INPUT " ENTER: Frequency (MHz).....................";F
330 GOSUB 90
340 LN=CSRLIN
350 PRINT USING " Mean diameter of coil = ###.### in.";D
360 PRINT USING " Length of coil        = ###.### in.";L
370 R=L/D
380 PRINT USING " Length/Diameter ratio = ###.### : 1";R
390 PRINT USING " Frequency             = ###.### MHz";F
400 LN=CSRLIN
410 PRINT UL$;
420 IF R>2.000000E-01 THEN A1=4.690000E+01:A2=70
430 IF R>4.000000E-01 THEN A1=70:  A2=7.769999E+01
440 IF R>5.000000E-01 THEN A1=7.769999E+01:A2=8.459999E+01
450 IF R>6.000000E-01 THEN A1=8.459999E+01:A2=90
460 IF R>6.999999E-01 THEN A1=90:  A2=9.459999E+01
470 IF R>8.000000E-01 THEN A1=9.459999E+01:A2=1.015000E+02
480 IF R>1 THEN A1=1.015000E+02:A2=1.215000E+02
490 IF R>2 THEN A1=1.215000E+02:A2=130
500 IF R>3 THEN A1=130:A2=137
510 IF R>5 THEN A1=137:A2=140
520 Q1=A1*D*F^5.000000E-01
530 Q2=A2*D*F^5.000000E-01
540 GOSUB 90
550 PRINT
560 PRINT " Q could be within the range of";INT(Q1);"to";INT(Q2);
570 PRINT " (depending on conductor size and spacing. Maximum Q occurs when s";
580 PRINT "pacing is "
590 PRINT " about twice the conductor diameter.)"
600 PRINT
610 PRINT " For a more accurate calculation of Q run the `Coil Q Calculator' ";
620 PRINT "program"
630 PRINT UL$;
640 PRINT " .....Press any key....."
650 Z$=INKEY$:IF Z$=""THEN 650
660 CLS:GOTO 110
