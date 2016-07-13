1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'FISHBONE - Fishbone Antenna - 13 JUL 97 rev.
20 CLS:KEY OFF
30 IF EX$=""THEN  EX$="EXIT"
40 COLOR 7,0,1
50 UL$=STRING$(80,205)
60 U$="#####.## "
70 VL=2.997924E+06/10^4  'velocity of light (megametres/sec.)- ISO 1992 standard
80 '.....AWG calculator
90 DIM GA(40)
100 K=(4.600000E-01/4.999999E-03)^(1/39)    'increment multiplier
110 FOR Z=1 TO 40
120 N=Z+3
130 GA(Z)=4.600000E-01/K^N
140 NEXT Z
150 '
160 COLOR 15,2
170 PRINT " FISHBONE ANTENNA";TAB(57);;"by George Murphy VE3ERP ";
180 COLOR 1,0:PRINT STRING$(80,223);
190 COLOR 7,0
200 GOSUB 870   'diagram
210 PRINT UL$;
220 '
230 GOSUB 1050   'prologue
240 COLOR 0,7:LOCATE 25,24
250 PRINT " Press 1 to continue or 0 to EXIT ";:COLOR 7,0
260 Z$=INKEY$:IF Z$=""THEN 260
270 IF Z$="0"THEN CLS:RUN EX$
280 IF Z$="1"THEN VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3:GOTO 310
290 GOTO 260
300 '
310 '.....start
320 INPUT  " ENTER: Frequency of operation (MHz)";F
330 WL=VL/F   'wavelength in free space
340 PRINT "   1 wavelength @";F;;"MHz =";WL;;"metres in free space."
350 PRINT UL$;
360 COLOR 0,7
370 PRINT " Indicate wire size: "
380 COLOR 7,0
390 PRINT
400 PRINT " For wire diameter in millimetres:"
410 PRINT "   Enter a decimal value between 1 and 7."
420 PRINT
430 PRINT " For wire diameter in inches:"
440 PRINT "   Enter a decimal value less than 1."
450 PRINT
460 PRINT " For AWG wire sizes:"
470 PRINT "   Enter an AWG size greater than 7."
480 PRINT
490 COLOR 0,7
500 INPUT " ENTER: One of the above values ";Z
510 COLOR 7,0
520 IF Z>=1 AND Z<=7 THEN W=Z/2.539999E+01:GOTO 560
530 IF Z<1 THEN W=Z:GOTO 560
540 IF Z>7 AND Z<=40 THEN W=GA(Z):AWG=Z:GOTO 560
550 LOCATE CSRLIN-1:PRINT STRING$(80,32);:LOCATE CSRLIN-1:GOTO 500
560 LOCATE CSRLIN-1:PRINT STRING$(80,32);:LOCATE CSRLIN-1
570 PRINT " Diameter of selected wire:"
580 PRINT USING "####.#";W*2.539999E+01;:PRINT " mm. =";
590 PRINT USING "##.###";W;:PRINT " in.";
600 IF AWG THEN PRINT " ( AWG#";AWG;;")"; ELSE PRINT ;""
610 PRINT
620 COLOR 0,7:PRINT " Press any key to continue ";:COLOR 7,0
630 IF INKEY$=""THEN 630
640 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:GOSUB 870  'diagram
650 LS=10^(600/276)/2*W*2.539999E+01   'xmission line spacing in mm.
660 OL=WL*5      'overall length in metres
670 EL=4.499999E-01*WL    'element length in metres
680 ES=WL/4      'element spacing
690 PRINT UL$;
700 PRINT " Frequency of operation............";USING "#####.###";F;
710 PRINT " MHz =";USING U$;WL;:PRINT " metres"
720 PRINT " Number of elements................";USING "#####";21
730 PRINT " Wire Diameter.....................";USING U$;W*2.539999E+01;
740 PRINT " mm. =";USING U$;W;:PRINT " in.";
750 IF AWG THEN PRINT " ( AWG#";AWG;;")"; ELSE PRINT ;""
760 PRINT " Feedline Spacing...............LS=";USING U$;LS;
770 PRINT " mm. =";USING U$;LS/2.539999E+01;:PRINT " in.";
780 PRINT " Element Spacing................ES=";USING U$;ES;
790 PRINT " m.  =";USING U$;ES/3.048000E-01;:PRINT " ft."
800 PRINT " Length of 1/2 Element..........EL=";USING U$;EL;
810 PRINT " m.  =";USING U$;EL/3.048000E-01;:PRINT " ft."
820 PRINT " Overall Length.................OL=";USING U$;OL;
830 PRINT " m.  =";USING U$;OL/3.048000E-01;:PRINT " ft."
840 GOTO 1260   'end
850 END
860 '
870 '.....diagram
880 LOCATE 3:T=23
890 COLOR 0,7
900 LOCATE ,T:PRINT "                600 ê               "
910 LOCATE ,T:PRINT "              Ú\/\/\/¿              "
920 LOCATE ,T:PRINT "              ÀÄÄ¿ÚÄÄÙ              "
930 LOCATE ,T:PRINT "  ÚÄ¯ ùùìÄÄÄÄÄÄÄÄ´ÃÄÄÄÄÄÄÄÄìùù      "
940 LOCATE ,T:PRINT "  ³   ùùìÄÄÄELÄÄÄ´ÃÄÄÄELÄÄÄìùù®Ä¿ES "
950 LOCATE ,T:PRINT "  ³   ùùìÄÄÄÄÄÄÄÄ´ÃÄÄÄÄÄÄÄÄìùù®ÄÙ   "
960 LOCATE ,T:PRINT " OL              ÷÷                 "
970 LOCATE ,T:PRINT "  ³   ùùìÄÄÄÄÄÄÄÄ´ÃÄÄÄÄÄÄÄÄìùù      "
980 LOCATE ,T:PRINT "  ³   ùùìÄÄÄÄÄÄÄÄ´ÃÄÄÄÄÄÄÄÄìùù      "
990 LOCATE ,T:PRINT "  ÀÄ¯ ùùìÄÄÄÄÄÄÄÄ´ÃÄÄÄÄÄÄÄÄìùù      "
1000 LOCATE ,T:PRINT "              ÄÄ¯³³®ÄÄLS            "
1010 LOCATE ,T:PRINT "      600 ê open-wire feedline      "
1020 COLOR 7,0
1030 RETURN
1040 '
1050 '.....prologue
1060 T=7
1070 PRINT TAB(T);
1080 PRINT "The fishbone antenna is well suited for use at HF. This version,"
1090 PRINT TAB(T);
1100 PRINT "the English HAD antenna, obtains low-angle response primarily as a"
1110 PRINT TAB(T);
1120 PRINT "function of its height, and therefore is generally installed 18 to"
1130 PRINT TAB(T);
1140 PRINT"36 metres (60 to 120 feet) above ground. It functions very well as a"
1150 PRINT TAB(T);
1160 PRINT "transmitting antenna because of its excellent gain and broadband"
1170 PRINT TAB(T);
1180 PRINT "nature. Terminate the 600ê line with a non-inductive resistor of"
1190 PRINT TAB(T);
1200 PRINT "sufficient wattage to suit the transmitted power."
1210 PRINT
1220 PRINT TAB(T+2);
1230 PRINT "(Ref. The ARRL ANTENNA BOOK, 17th Edition, pp. 13-17 to 13-19).";
1240 RETURN
1250 '
1260 '.....end
1270 GOSUB 1300
1280 VIEW PRINT 16 TO 24:CLS:VIEW PRINT:LOCATE 16:GOTO 230
1290 '
1300 'PRT
1310 KEY OFF:GOSUB 1380:LOCATE 25,5:COLOR 0,2
1320 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1330 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1340 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1340 ELSE GOSUB 1380
1350 IF Z$="3"THEN RETURN
1360 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1370 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1310
1380 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
