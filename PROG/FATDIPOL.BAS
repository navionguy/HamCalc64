1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'FATDIPOL - Fat Dipole - 17 NOV 95 rev.25 AUG 07
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF:COLOR 7,0,1
40 UL$=STRING$(80,205)
50 U1$="####.###"
60 U2$="####.## "
70 '
80 '.....start
90 CLS
100 COLOR 15,2
110 PRINT " FAT DIPOLE (Broadbanded)";
120 PRINT TAB(57);;"by George Murphy VE3ERP ";
130 COLOR 1,0:PRINT STRING$(80,223);:COLOR 7,0
140 '
150 '.....standard antenna notes
160 'OPEN "I",1,"\hamcalc\docfiles\antenna.doc
170 'IF EOF(1)THEN 190
180 'INPUT #1,Z$:PRINT TAB(3);Z$:GOTO 170
190 'CLOSE
200 'PRINT UL$;
210 '
220 'preface
230 T=8
240 PRINT TAB(T);
250 PRINT "Making a dipole conductor thicker than normal with respect to"
260 PRINT TAB(T);
270 PRINT "wavelength will increase the bandwidth and modify the working"
280 PRINT TAB(T);
290 PRINT "impedance of the antenna. The trick is to make a dipole 'fat' in"
300 PRINT TAB(T);
310 PRINT "such a way that it may easily be constructed from cheap materials,"
320 PRINT TAB(T);
330 PRINT "be highly efficient and at the same time arrange things so that it"
340 PRINT TAB(T);
350 PRINT "will match the transmission line from the lower band edge to the"
360 PRINT TAB(T);
370 PRINT "upper band edge."
380 PRINT UL$;
390 PRINT "  PRESS number in < > to:"
400 PRINT UL$;
410 PRINT "   < 1 >  RUN program"
420 PRINT "   < 0 >  EXIT"
430 Z$=INKEY$:
440 IF Z$="1"THEN 480
450 IF Z$="0"THEN CLS:RUN EX$
460 GOTO 430
470 '
480 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
490 PRINT TAB(T);
500 PRINT "For a detailed discussion of the theory and construction of this"
510 PRINT TAB(T);
520 PRINT "type of antenna see the article 'FAT DIPOLES', by Robert C. Wilson,"
530 PRINT TAB(T);
540 PRINT "in The ARRL Antenna Compendium, Vol. 2, pages 106-107."
550 PRINT UL$;
560 '.....diagram
570 R=7:T=14
580 COLOR 0,7
590 LOCATE R+ 0,T:PRINT "                                                     "
600 LOCATE R+ 1,T:PRINT " ³®ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ L ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¯³   "
610 LOCATE R+ 2,T:PRINT " ³                    ³® A ¯³                    ³   "
620 LOCATE R+ 3,T:PRINT " ìÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂì---ìÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄì®¿ "
630 LOCATE R+ 4,T:PRINT "                      ÃÄÂìÂÄ´                      A "
640 LOCATE R+ 5,T:PRINT " ìÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁì³-³ìÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄì®Ù "
650 LOCATE R+ 6,T:PRINT "                       ÚÁÄÁ¿                         "
660 LOCATE R+ 7,T:PRINT "                       ³1:1³ Balun                   "
670 LOCATE R+ 8,T:PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿    ÀÄÒÄÙ                         "
680 LOCATE R+ 9,T:PRINT " All wire #14 AWG ³      º 50 ê Coax                 "
690 LOCATE R+10,T:PRINT "     (í1.6mm)     ³                                  "
700 COLOR 7,0
710 PRINT UL$;
720 '
730 '.....calculate
740 INPUT " ENTER: Band of Operation Centre Frequency (MHz) ";F
750 LOCATE CSRLIN-1:PRINT STRING$(80,32);:LOCATE CSRLIN-1
760 PRINT TAB(T);;"Centre frequency of operation....";USING U1$;F;:PRINT " MHz"
770 L1=4.425000E+02/F:L2=L1*3.048000E-01        'length
780 A1=1.125000E+01/F*12:A2=A1*2.539999E-02     'wire spacing in inches
790 H1=1.125000E+02/F:IF H1<8 THEN H1=8  'height above ground
800 H2=H1*3.048000E-01
810 B=F/7.519999                'bandwidth
820 PRINT TAB(T);;"Recommended minimum height.......";USING U2$;H1;
830 PRINT "feet   =";USING U1$;H2;:PRINT " metres"
840 PRINT TAB(T);;"Length.........................L=";USING U2$;L1;
850 PRINT "feet   =";USING U1$;L2;:PRINT " metres"
860 PRINT TAB(T);;"Wire spacing...................A=";USING U2$;A1;
870 PRINT "inches =";USING U1$;A2;:PRINT " metres"
880 PRINT TAB(T);;"Bandwidth........................";USING U1$;B;:PRINT " MHz"
890 GOSUB 920
900 GOTO 80
910 '
920 'PRT
930 KEY OFF:GOSUB 1000:LOCATE 25,5:COLOR 0,2
940 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
950 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
960 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 960 ELSE GOSUB 1000
970 IF Z$="3"THEN RETURN
980 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
990 IF Z$="2"THEN LPRINT CHR$(12) ELSE 930
1000 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
