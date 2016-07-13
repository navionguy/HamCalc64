1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'MOXON - Moxon Rectangle - 30 JAN 2000 rev. 24 FEB 2000
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 COLOR 7,0,1
50 '
60 '.....title page
70 CLS
80 COLOR 15,2
90 PRINT " MOXON RECTANGLE ANTENNA";TAB(57);"by George Murphy VE3ERP ";
100 COLOR 1,0:PRINT STRING$(80,223);
110 LOCATE CSRLIN-1,26:COLOR 15,2
120 PRINT " Algorithm by L.B.Cebik W4RNL "
130 COLOR 7,0
140 '
150 '.....start
160 GOSUB 820 'diagram
170 PRINT
180 LN=CSRLIN:GOSUB 980  'preface
190 COLOR 0,7:LOCATE 25,24
200 PRINT " Press 1 to continue or 0 to EXIT ";:COLOR 7,0
210 Z$=INKEY$:IF Z$=""THEN 210
220 IF Z$="0"THEN CLS:RUN EX$
230 IF Z$="1"THEN 260
240 GOTO 210
250 '
260 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
270 INPUT " ENTER: Frequency in MHz";F
280 PRINT "        Do you want to enter conductor diameter in:"
290 PRINT "        (a)AWG, (i)nches, (m)illimetres or (w)avelengths?  (a/i/m/w)"
300 Z$=INKEY$:IF Z$=""THEN 300
310 IF Z$="a"THEN U=0:M$="AWG":D$=" AWG........":GOTO 360
320 IF Z$="i"THEN U=1:M$="in.":D$=" inches.....":GOTO 360
330 IF Z$="m"THEN U=2:M$="mm.":D$=" millimetres":GOTO 360
340 IF Z$="w"THEN U=3:D$=" wavelengths":GOTO 360
350 GOTO 300
360 PRINT " ENTER: Conductor diameter in";D$;;".........";:INPUT W:WD=W
370 IF W<=0 THEN LOCATE CSRLIN-1:GOTO 360
380 IF U=0 THEN WD=4.600000E-01/1.122831^(WD+3):WLI=1.180270E+04/F:DW=WD/WLI
390 IF U=1 THEN WLI=1.180270E+04/F:DW=WD/WLI
400 IF U=2 THEN WLI=2.997925E+05/F:DW=WD/WLI
410 IF U=3 THEN DW=WD
420 LN=CSRLIN-4:VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
430 D1=4.342944E-01*LOG(DW)
440 '
450 IF D1<=-2 AND D1>=-6 THEN 520
460 IF D1>-2 THEN D1$="greater than 1E-2"
470 IF D1<-6 THEN D1$="less than 1E-6"
480 LOCATE LN-1:BEEP:COLOR 14,4
490 PRINT " Conductor diameter ";D1$;;" wavelengths: RESULTS UNCERTAIN ! "
500 COLOR 7,0
510 '
520 AA=-8.571428570999999E-04:AB=-9.571428570999999E-03:AC=3.398571428999999E-01
530 A=(AA*(D1^2))+(AB*D1)+AC
540 BA=-2.142857142999999E-03:BB=-2.035714285999999E-02:BC=8.285714285999999E-03
550 B=(BA*(D1^2))+(BB*D1)+BC
560 CA=1.809523380999999E-03:CB=1.780952381000000E-02:CC=5.164285713999999E-02
570 C=(CA*(D1^2))+(CB*D1)+CC
580 DA=1.000000E-03:DB=7.178571429000000E-02
590 D=(DA*D1)+DB
600 E=(B+C)+D
610 WL=2.997919E+02/F
620 WF=9.835592E+02/F
630 '
640 PRINT USING " Operating freq. = ###.### MHz. º";F;
650 PRINT USING " Wavelength (wL)= ###.##m. = ###.## ft.";WL,WL/3.048000E-01
660 PRINT " Conductor dia.  =";W;M$;TAB(33);;"=  ";DW;;"wavelengths"
670 U$=" = ##.##### wL = ####.### in. = ###.### ft. = ###.###m."
680 COLOR 0,7
690 PRINT USING " A";U$;A,A*WF*12,A*WF,A*WF*3.048000E-01
700 COLOR 7,0
710 PRINT USING " B";U$;B,B*WF*12,B*WF,B*WF*3.048000E-01
720 PRINT USING " C";U$;C,C*WF*12,C*WF,C*WF*3.048000E-01
730 PRINT USING " D";U$;D,D*WF*12,D*WF,D*WF*3.048000E-01
740 COLOR 0,7
750 PRINT USING " E";U$;E,E*WF*12,E*WF,E*WF*3.048000E-01
760 COLOR 7,0
770 X=SQR((A*WF)^2+(E*WF)^2):Y=X/2
780 PRINT TAB(17);USING " Diagonal x-x   = ###.### ft. = ###.###m.";X,X*3.048000E-01
790 PRINT TAB(17);USING " Turning radius = ###.### ft. = ###.###m.";Y,Y*3.048000E-01
800 GOTO 1140  'end
810 '
820 '.....diagram
830 T=21:LOCATE 4
840 COLOR 0,7
850 LOCATE ,T:PRINT "   MOXON Horizontal Rectangle Antenna   "
860 LOCATE ,T:PRINT "    ³®ÄÄÄÄÄÄÄÄÄÄ A ÄÄÄÄÄÄÄÄÄÄ¯³         "
870 LOCATE ,T:PRINT "   xÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ ®ÄÂÄ¿   "
880 LOCATE ,T:PRINT "    ³        reflector        ³   D ³   "
890 LOCATE ,T:PRINT "    Á                         Á ®Ä´ ³   "
900 LOCATE ,T:PRINT "    |®ÄÄ insulated spacers ÄÄ¯|   C E   "
910 LOCATE ,T:PRINT "    Â                         Â ®Ä´ ³   "
920 LOCATE ,T:PRINT "    ³      driven element     ³   B ³   "
930 LOCATE ,T:PRINT "    ÀÄÄÄÄÄÄÄÄÄÄÄ¿ ÚÄÄÄÄÄÄÄÄÄÄÄÙx ®ÁÄÙ   "
940 LOCATE ,T:PRINT "           50ê feedline                 "
950 COLOR 7,0
960 RETURN
970 '
980 '.....preface
990 PRINT " All equations correlated to NEC antenna modeling software for wir";
1000 PRINT "e diameters"
1010 PRINT " from 1E-6 to 1E-2 wavelengths."
1020 PRINT
1030 PRINT " References:"
1040 PRINT "  ";CHR$(34);;"HF Antennas for All Locations";CHR$(34);;", (London, ";
1050 PRINT "RSGB, 1982)."
1060 PRINT "  ";CHR$(34);;"VK2ABQ Squares & Moxon Rectangles";CHR$(34);;", Sprin";
1070 PRINT "g 1995 Communications Quarterly."
1080 PRINT "  ";CHR$(34);;"An Aluminum Moxon Rectangle for 10 Meters";CHR$(34);
1090 PRINT ", ARRL Compendium, Vol. 6."
1100 PRINT "  ";CHR$(34);;"Compact 2-Element 2 Meter Beam";CHR$(34);
1110 PRINT ", QST, Jan.2000."
1120 RETURN
1130 '
1140 '.....end
1150 GOSUB 1170:VIEW PRINT LN-1 TO 24:CLS:VIEW PRINT:LOCATE LN:GOTO 180
1160 '
1170 'PRT
1180 KEY OFF:GOSUB 1250:LOCATE 25,5:COLOR 0,2
1190 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1200 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1210 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1210 ELSE GOSUB 1250
1220 IF Z$="3"THEN RETURN
1230 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1240 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1180
1250 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
