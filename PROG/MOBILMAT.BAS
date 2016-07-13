1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON FQ
10 'MOBILMAT - Mobile Antenna Matching - 08 APR 96 rev.15 JAN 2006
20 IF EX$=""THEN EX$="EXIT"
30 
40 CLS:KEY OFF
50 COLOR 7,0,0
60 PI=3.141592
70 UL$=STRING$(80,205)
80 U$="#####.###"
90 '
100 PRINT " MOBILE ANTENNA MATCHING"
110 PRINT
120 GOTO 180
130 '.....format input line
140 LOCATE LN-1:PRINT SPC(7);
150 LOCATE LN-1,47:PRINT STRING$(7,".");USING U$;ZZ;
160 RETURN
170 '
180 '.....start
190 GOSUB 560
200 PRINT UL$;
210 '
220 GOSUB 710
230 LOCATE 22,24:COLOR 0,7
240 PRINT " Press 1 to continue or 0 to EXIT "
250 COLOR 7,0
260 Z$=INKEY$:IF Z$=""THEN 260
270 IF Z$="0"THEN CLS:RUN "mowhip"
280 IF Z$="1"THEN 310
290 GOTO 260
300 '
310 '.....data input
320 VIEW PRINT 14 TO 24:CLS:VIEW PRINT:LOCATE 14
330 IF FQ THEN GOSUB 380:GOTO 420
340 INPUT " ENTER: Frequency of operation..................(MHz)";Z
350 LN=CSRLIN:ZZ=Z:GOSUB 130:PRINT " MHz"
360 F=Z*10^3
370 GOTO 420
380 PRINT "        Frequency of operation..................(MHz)";Z
390 LN=CSRLIN:ZZ=FQ:GOSUB 130:PRINT " MHz"
400 F=FQ*10^3
410 '
420 INPUT " ENTER: Antenna Feedpoint Resistance...........(ohms)";RA
430 LN=CSRLIN:ZZ=RA:GOSUB 130:PRINT " ohms"
440 '
450 INPUT " ENTER: Characteristic Impedance of Feedline...(ohms)";ZO
460 LN=CSRLIN:ZZ=ZO:GOSUB 130:PRINT " ohms"
470 '
480 C1=SQR(RA*(ZO-RA))/(2*PI*F*RA*ZO)*10^9
490 L1=SQR(RA*(ZO-RA))/(2*PI*F)*10^3
500 PRINT "        Capacitance C1...............................";USING U$;C1;
510 PRINT " pF"
520 PRINT "        Inductance L1................................";USING U$;L1;
530 PRINT " æH"
540 GOTO 860
550 '
560 '.....diagram
570 T=27:COLOR 0,7
580 LOCATE ,T:PRINT "                            "
590 LOCATE ,T:PRINT "                         ³  "
600 LOCATE ,T:PRINT "        resonant whip ÄÄ¯³  "
610 LOCATE ,T:PRINT "                         ³  "
620 LOCATE ,T:PRINT "              ÚÄÄÄÂÄïïïïÄÙ  "
630 LOCATE ,T:PRINT "  coaxial ÍÍÍÑ¼   ³  L1     "
640 LOCATE ,T:PRINT "  feedline   ÃÄ´ÃÄÙ         "
650 LOCATE ,T:PRINT "             ³ C1           "
660 LOCATE ,T:PRINT "   vehicle \\Á\\ body       "
670 LOCATE ,T:PRINT "                            "
680 COLOR 7,0
690 RETURN
700 '
710 '.....preface
720 T=8
730 PRINT TAB(T);
740 PRINT "Any resonant mobile antenna that has a feed-point impedance less"
750 PRINT TAB(T);
760 PRINT "than the characteristic impedance of the transmission line can be"
770 PRINT TAB(T);
780 PRINT "matched to the line by means of a simple L-network as shown above."
790 PRINT
800 PRINT TAB(T);
810 PRINT "(The 1997 ARRL HANDBOOK for RADIO AMATEURS, pages 20.46 & 20.47)"
820 PRINT TAB(T);
830 PRINT "(The ARRL ANNTENNA BOOK, 18th edition, page 16-18)"
840 RETURN
850 '
860 '.....end
870 GOSUB 910
880 VIEW PRINT 14 TO 24:CLS:VIEW PRINT:LOCATE 14:GOTO 220
890 END
900 '
910 'PRT
920 KEY OFF:GOSUB 990:LOCATE 25,5:COLOR 0,2
930 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
940 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
950 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 950 ELSE GOSUB 990
960 IF Z$="3"THEN RETURN
970 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
980 IF Z$="2"THEN LPRINT CHR$(12) ELSE 920
990 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
