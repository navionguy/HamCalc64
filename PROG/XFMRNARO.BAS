1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,L
10 'XFMRNARO - Transformer, Narrow Band - 18 FEB 96 rev.13 APR 97
20 IF EX$=""THEN EX$="EXIT"
30 
40 CLS:KEY OFF
50 COLOR 7,0,1
60 PI=3.141592
70 U$="#####.###"
80 X$=STRING$(80,32)
90 GOTO 160
100 '
110 '.....format display
120 LOCATE CSRLIN-1:PRINT SPC(7);
130 LOCATE CSRLIN,47:PRINT STRING$(7,".");USING U$;ZZ;
140 RETURN
150 '
160 '.....start
170 CLS
180 COLOR 15,2
190 PRINT " TRANSFORMER, Narrowband";
200 PRINT TAB(57);;"by George Murphy VE3ERP ";
210 COLOR 1,0:PRINT STRING$(80,223);:COLOR 7,0
220 GOSUB 950      'diagram
230 PRINT
240 GOSUB 1080      'text
250 '
260 LOCATE 25,22:COLOR 0,7
270 PRINT " Press 1 to continue or 0 to EXIT.....";
280 COLOR 7,0
290 Z$=INKEY$:IF Z$=""THEN 290
300 IF Z$="0"THEN CLS:RUN EX$
310 IF Z$="1"THEN GOSUB 340:GOTO 370
320 GOTO 290
330 '
340 VIEW PRINT 11 TO 24:CLS:VIEW PRINT:LOCATE 11
350 RETURN
360 '
370 '.....input data
380 '
390 INPUT " ENTER: LO-Z impedance............................(ohms)";LO
400 ZZ=LO:GOSUB 110:PRINT " ohms"
410 '
420 INPUT " ENTER: HI-Z impedance............................(ohms)";HI
430 ZZ=HI:GOSUB 110:PRINT " ohms"
440 '
450 INPUT " ENTER: Top edge of desired bandwidth..............(MHz)";F2
460 ZZ=F2:GOSUB 110:PRINT " Mhz"
470 '
480 INPUT " ENTER: Bottom edge of desired bandwidth...........(MHz)";F1
490 ZZ=F1:GOSUB 110:PRINT " Mhz"
500 '
510 CF=SQR(F1*F2)
520 PRINT "        Centre Frequency of desired bandwidth........";USING U$;CF;
530 PRINT " MHz"
540 '
550 BW=F2-F1
560 PRINT "        Bandwidth....................................";USING U$;BW;
570 PRINT " MHz"
580 '
590 QL=CF/(F2-F1)
600 QX=SQR(HI/LO-1)
610 IF QL>QX THEN 680
620 COLOR 0,7
630 PRINT "        Bandwidth is too wide...press any key to specify another..."
640 COLOR 7,0
650 IF INKEY$=""THEN 650
660 GOSUB 340:GOTO 370
670 '
680 CT=QL/(2*PI*CF*(HI/2))*10^6
690 CC=SQR(HI/LO)-1
700 X=1
710 C1=X:C2=X*CC
720 Y=C1*C2/(C1+C2)
730 C1=CT/Y
740 C2=C1*CC
750 XL=1/(2*PI*CF*(CT*10^-6))
760 L1=XL/(2*PI*CF):L=L1  '(for chaining to other programs)
770 '
780 PRINT "        Capacitor C1.................................";USING U$;C1;
790 PRINT " pF"
800 PRINT "        Capacitor C2.................................";USING U$;C2;
810 PRINT " pF"
820 PRINT "        Inductive Reactance, Inductor L1.............";USING U$;XL;
830 PRINT " ohms"
840 PRINT "        Inductor L1..................................";USING U$;L1;
850 PRINT " æH"
860 GOSUB 1320
870 LOCATE 23,9:COLOR 0,7
880 PRINT " Want to design a toroid inductor for L1?   (y/n) ";:COLOR 7,0
890 LOCATE 25,1:PRINT STRING$(78,32);
900 Z$=INKEY$:IF Z$=""THEN 900
910 IF Z$="n"THEN 160
920 IF Z$="y"THEN CLS:CHAIN"toroid"
930 GOTO 900
940 '
950 '.....diagram
960 COLOR 0,7
970 T=25
980 LOCATE ,T:PRINT "                                "
990 LOCATE ,T:PRINT "         ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿    "
1000 LOCATE ,T:PRINT "        ÍØÍ C1  ³  L1      ³    "
1010 LOCATE ,T:PRINT "   ÚÄÄÄÄÄ´      ÀÄïïïïÄ¿  HI-Z  "
1020 LOCATE ,T:PRINT "  LO-Z  ÍØÍ C2         ³   ³    "
1030 LOCATE ,T:PRINT "   ÀÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÙ    "
1040 LOCATE ,T:PRINT "                                "
1050 COLOR 7,0
1060 RETURN
1070 '
1080 '.....preface
1090 T=7
1100 PRINT TAB(7);
1110 PRINT "A narrow-band transformer can be built with a single winding by"
1120 PRINT TAB(7);
1130 PRINT "using a capacitive divider to match a low-impedance source to a"
1140 PRINT TAB(7);
1150 PRINT "high-impedance load, or vice versa. This kind of circuit is used"
1160 PRINT TAB(7);
1170 PRINT "frequently for matching one transistor stage to another. It has a"
1180 PRINT TAB(7);
1190 PRINT "band pass characteristic and is, therefore, useful in minimizing"
1200 PRINT TAB(7);
1210 PRINT "harmonic energy."
1220 PRINT
1230 PRINT TAB(7);
1240 PRINT "To attain precise resonance a small trimmer capacitor may be placed"
1250 PRINT TAB(7);
1260 PRINT "in parallel with C1, or L1 may be a variable (slug tuned) inductor."
1270 RETURN
1280 '
1290 '.....end
1300 GOSUB 1320:GOTO 160
1310 '
1320 'PRT
1330 KEY OFF:GOSUB 1400:LOCATE 25,5:COLOR 0,2
1340 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1350 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1360 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1360 ELSE GOSUB 1400
1370 IF Z$="3"THEN RETURN
1380 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1390 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1330
1400 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
