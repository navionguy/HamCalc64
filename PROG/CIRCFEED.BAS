1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'CIRCFEED - Circular Waveguide Dish Feeds - 18 OCT 96 rev. 21 MAY 2000
20 'Ref. The ARRL Antenna Book, 17th Edition, page 18-14
30 IF EX$=""THEN EX$="EXIT"
40 CLS:KEY OFF
50 COLOR 7,0,0
60 UL$=STRING$(80,205)
70 U$="###,###"
80 V$="###.##"
90 '
100 COLOR 15,2
110 PRINT " CIRCULAR WAVEGUIDE DISH FEEDS";TAB(57);;"by George Murphy VE3ERP ";
120 COLOR 1,0:PRINT STRING$(80,223);
130 COLOR 7,0
140 '
150 '.....start
160 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
170 GOSUB 620    'preface
180 PRINT UL$;
190 COLOR 0,7:LOCATE ,23
200 PRINT " Press 1 to continue or 0 to EXIT..."
210 COLOR 7,0
220 Z$=INKEY$:IF Z$=""THEN 220
230 IF Z$="0"THEN CLS:RUN EX$
240 IF Z$="1"THEN 260
250 GOTO 220
260 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
270 '
280 '.....input data
290 INPUT " ENTER: Operating Frequency in MHz";FO
300 WO=300/FO   'operating wavelength
310 LOCATE CSRLIN-1
320 PRINT " Operating Frequency =";USING U$;FO;
330 PRINT " MHz (";USING "###.##";WO*100;:PRINT ;" cm.)"
340 PRINT UL$;
350 PRINT TAB(2);"ÚÄInside DiameterÄ¿";TAB(27);"Cutoff";TAB(44);"Cutoff";
360 PRINT TAB(65);;"Waveguide"
370 PRINT TAB(3);"ž     cm.   inches";TAB(27);"TE11 MODE";TAB(44);"TM01 MODE";
380 PRINT TAB(65);;"Length"
390 PRINT UL$;
400 FOR LA=65 TO 77  '% of wavelength
410 D=LA/100*WO*10^3/2.539999E+01    'I.D. of waveguide in inches
420 TE=6.917259E+03/D      'Fc(TE11)
430 TM=9.034848E+03/D      'Fc(TM01)
440 GW=1.180284E+04/SQR(FO^2-TE^2) 'guide wavelength
450 IF LA=70 THEN COLOR 0,7 ELSE COLOR 7,0
460 PRINT USING " ##%";LA;
470 PRINT " =";USING V$;D*2.539999;
480 PRINT " =";USING V$;D;
490 PRINT TAB(25);USING U$;TE;:PRINT ;" MHz";
500 PRINT TAB(42);USING U$;TM;:PRINT ;" MHz";
510 PRINT TAB(60)USING V$;GW*2.539999;:PRINT ;" cm =";
520 PRINT USING V$;GW;:PRINT CHR$(34);;" "
530 IF LA=70 THEN LOCATE CSRLIN-1:COLOR 0,7:PRINT "*";:COLOR 7,0
540 NEXT LA
550 PRINT
560 PRINT " ž = diameter expressed in percentage of 1 wavelength."
570 COLOR 0,7:PRINT "* provides nearly optimum illumination ";:COLOR 7,0
580 GOSUB 740
590 GOTO 150
600 END
610 '
620 '.....preface
630 T=7
640 PRINT TAB(T);
650 PRINT "This program computes diameters and lengths of circular waveguide"
660 PRINT TAB(T);
670 PRINT "feeds for parabolic dish antennas in the frequency range of"
680 PRINT TAB(T);
690 PRINT "902 to 10,000 MHz, from equations presented in The ARRL ANTENNA"
700 PRINT TAB(T);
710 PRINT "BOOK, 17th edition, page 18-14."
720 RETURN
730 '
740 'PRT
830 KEY OFF:GOSUB 900:LOCATE 25,5:COLOR 0,2
840 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
850 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
860 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 860 ELSE GOSUB 900
870 IF Z$="3"THEN RETURN
880 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
890 IF Z$="2"THEN LPRINT CHR$(12) ELSE 830
900 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
