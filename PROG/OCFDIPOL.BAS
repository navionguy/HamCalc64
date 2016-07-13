1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,FRQ,ZP,ZS,PROG$
10 'OCFDIPOL - Off-Centre-Fed Dipole - 26 DEC 2001
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 
50 COLOR 7,0,1
60 UL$=STRING$(80,205)
70 '
80 '.....title page
90 CLS
100 COLOR 15,2
110 PRINT " OFF-CENTRE FED DIPOLE";TAB(57);"by George Murphy VE3ERP ";
120 COLOR 1,0:PRINT STRING$(80,223);
130 COLOR 7,0
140 '
150 '.....diagram
160 T=22:COLOR 0,7:LOCATE 2
170 LOCATE ,T:PRINT "           TRI-BAND DIPOLE           "
180 LOCATE ,T:PRINT " Ё╝дддддддддддддд A дддддддддддддд╞Ё "
190 LOCATE ,T:PRINT " Ё╝ддд B ддд╞Ё╝дддддддд C дддддддд╞Ё "
200 LOCATE ,T:PRINT " ЛддддддддддбЛбддддддддддддддддддддЛ "
210 LOCATE ,T:PRINT "           зада©balun                "
220 LOCATE ,T:PRINT "           юдрды                     "
230 LOCATE ,T:PRINT "             ╨                       "
240 LOCATE ,T:PRINT "             ╨<д 50/75 Й coax cable, "
250 LOCATE ,T:PRINT "             ╨   any length          "
260 COLOR 7,0
270 PRINT UL$;
280 '
290 '.....preface
300 PRINT " An off-centre fed dipole may be used at its fundamental, 2nd an";
310 PRINT "d 4th harmonic "
320 PRINT " frequencies, e.g.,Ё 1.8    Ё 3.58    Ё 7.0    Ё"
330 PRINT "                   Ё 3.6    Ё 7.16    Ё14.0    Ё etc."
340 PRINT "                   Ё 7.2 MHzЁ14.32 MHzЁ28.0 MHzЁ"
350 PRINT " The feed point impedance is about 200 ohms."
360 PRINT
370 PRINT "   (ref. Art Blick VE3AHU, The Canadian Amateur, June 2001, p.16)"
380 PRINT "   (ref. ARRL Antenna Book, 18th edition, page 7-21)"
390 PRINT UL$;
400 LN=12
410 COLOR 0,7:LOCATE 25,23
420 PRINT " Press 1 to continue or 0 to EXIT ";:COLOR 7,0
430 Z$=INKEY$:IF Z$=""THEN 430
440 IF Z$="0"THEN CLS:CHAIN EX$
450 IF Z$="1"THEN 490
460 GOTO 430
470 '
480 '.....start
490 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN+1
500 INPUT " ENTER: Fundamental frequency (MHz)";F
510 FRQ=F
520 L=4.672000E+02/F
530 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
540 PRINT USING "                        з ###.### MHz - fundamental";F
550 PRINT USING " Operating frequencies де ###.### MHz - 2nd harmonic";F*2
560 PRINT USING "                        ю ###.### MHz - 4th harmonic";F*4
570 A=L:B=L/3:C=L/3*2
580 PRINT USING "                       A= ###.## m. = ###.## ft.";A/3.048000,A
590 PRINT USING "                       B= ###.## m. = ###.## ft.";B/3.048000,B
600 PRINT USING "                       C= ###.## m. = ###.## ft.";C/3.048000,C
610 PRINT UL$;
620 '
630 '.....feedline options
640 PRINT " Press number in < > to:"
650 PRINT "   <1> Design a balun for this antenna"
660 PRINT "   <2> Design a series-section matching transformer for this antenna"
670 PRINT
680 PRINT "   <0> Quit"
690 Z$=INKEY$:IF Z$=""THEN 690
700 IF Z$="0"THEN 1000
710 IF Z$="1"THEN 750
720 IF Z$="2"THEN 830
730 GOTO 690
740 '
750 '.....balun
760 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
770 COLOR 0,7:INPUT " ENTER: Impedance of coaxial feedline (ohms)";ZS:COLOR 7,0
780 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
790 PRINT " (If you do not know the feedpoint impedance then assume 200 ohms)"
800 COLOR 0,7:INPUT " ENTER: Antenna feedpoint impedance   (ohms)";ZP:COLOR 7,0
810 CLS:PROG$="balun":CHAIN"\hamcalc\prog\broadfer"
820 '
830 '.....series-sect.
840 T=22:COLOR 0,7:LOCATE 5
850 LOCATE ,T:PRINT " ЛдддддддддддрдддддддддддддддддддддЛ "
860 LOCATE ,T:PRINT "             ╨<д coaxial cable       "
870 LOCATE ,T:PRINT "            зп©                      "
880 LOCATE ,T:PRINT "            Ё Ё<д matching section   "
890 LOCATE ,T:PRINT "            юры                      "
900 LOCATE ,T:PRINT "             ╨<д coaxial cable       "
910 COLOR 7,0
920 VIEW PRINT LN TO 24:CLS:VIEW PRINT
930 LOCATE LN
940 COLOR 0,7:PRINT " See new diagram above ";:COLOR 7,0
950 PRINT " (If you do not know the feedpoint impedance then assume 200 ohms)"
960 COLOR 0,7:INPUT " ENTER: Antenna feedpoint impedance   (ohms)";ZP:COLOR 7,0
970 CLS:CHAIN"serisect"
980 END
990 '
1000 '.....end
1010 VIEW PRINT 19 TO 24:CLS:VIEW PRINT
1020 GOSUB 1040:GOTO 80
1030 '
1040 '.....PRT
1050 KEY OFF:GOSUB 1120:LOCATE 25,5:COLOR 0,2
1060 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1070 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1080 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1080 ELSE GOSUB 1120
1090 IF Z$="3"THEN RETURN
1100 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1110 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1050
1120 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
