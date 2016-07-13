1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'VARAYFED - Vertical Antenna Array Feed - 27 JUN 1998
20 IF EX$=""THEN EX$="EXIT"
30 CLS:KEY OFF
40 COLOR 7,0,1
50 PI=4*ATN(1)  '3.141593
60 UL$=STRING$(80,205)
70 '
80 '.....title page
90 CLS
100 COLOR 15,2
110 PRINT " VERTICAL ANTENNA ARRAY FEED METHOD";
120 PRINT TAB(57);;"by George Murphy VE3ERP ";
130 COLOR 1,0:PRINT STRING$(80,223);
140 COLOR 7,0
150 '
160 '.....diagram"
170 COLOR 0,7:T=25
180 LOCATE 3
190 LOCATE ,T:PRINT "           PLAN VIEW           "
200 LOCATE ,T:PRINT "  ³®ÄÄÄÄÄÄÄÄÄÄ A ÄÄÄÄÄÄÄÄÄÄ¯³  "
210 LOCATE ,T:PRINT "  ³®ÄÄÄÄÄÄÄÄÄ 90ø ÄÄÄÄÄÄÄÄÄ¯³  "
220 LOCATE ,T:PRINT "  ³®ÄÄÄÄÄ ¬ wavelength ÄÄÄÄ¯³  "
230 LOCATE ,T:PRINT "                               "
240 LOCATE ,T:PRINT " [+]®Ä Vertical Antennas Ä¯[+] ÄÄÄ> maximum radiation >"
250 LOCATE ,T:PRINT "  º®¿                     Ú¯º  "
260 LOCATE ,T:PRINT "  º ³ 84ø          161ø   ³ º  "
270 LOCATE ,T:PRINT "  º ÀÄ B ÄÄ¯³®ÄÄÄÄÄ C ÄÄÄÄÙ º  "
280 LOCATE ,T:PRINT "  ÈÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼  "
290 LOCATE ,T:PRINT "            º®Ä to Transmitter "
300 LOCATE ,T:PRINT "    Transmatch not required    "
310 LOCATE ,T:PRINT "        (ÍÍ = 52ê coax)        "
320 COLOR 7,0
330 PRINT UL$;
340 PRINT TAB(16);"(Ref. ARRL ANTENNA BOOK, 17th Edition, page 4-12)"
350 PRINT TAB(7);
360 PRINT "(Ref. Al Christman KB8I,`Feeding Phased Arrays: An Alternate Method'";
370 PRINT TAB(22);
380 PRINT "HAM RADIO, May 1985, pp 58-59, 61-64)"
390 PRINT
400 COLOR 0,7:LOCATE ,24:PRINT " Press 1 to continue or 0 to EXIT ";:COLOR 7,0
410 Z$=INKEY$:IF Z$=""THEN 410
420 IF Z$="0"THEN RUN EX$
430 IF Z$="1"THEN 460
440 GOTO 410
450 '
460 VIEW PRINT 17 TO 24:CLS:VIEW PRINT:LOCATE 17
470 INPUT " ENTER: Frequency in MHz ..............................";F
480 INPUT " ENTER: Velocity Factor of your 52ê coax (decimal).....";VF
490 A=300/F/4
500 B=984*VF*84/(F*360)*3.048000E-01
510 C=984*VF*161/(F*360)*3.048000E-01
520 VIEW PRINT 17 TO 24:CLS:VIEW PRINT:LOCATE 17
530 T=21
540 PRINT TAB(T);USING "Frequency       Ÿ=###.### MHz";F
550 PRINT TAB(T);USING "Antenna Spacing A=###.## m.=####.## ft.=";A,A/3.048000E-01;
560 PRINT " 90ø= ¬ wavelength"
570 PRINT TAB(T);USING "Length of coax  B=###.## m.=####.## ft.";B,B/3.048000E-01
580 PRINT TAB(T);USING "Length of coax  C=###.## m.=####.## ft.";C,C/3.048000E-01
590 '
600 '.....end
610 GOSUB 630:GOTO 80
620 '
630 '.....PRT
640 KEY OFF:GOSUB 710:LOCATE 25,5:COLOR 0,2
650 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
660 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
670 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 670 ELSE GOSUB 710
680 IF Z$="3"THEN RETURN
690 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
700 IF Z$="2"THEN LPRINT CHR$(12) ELSE 640
710 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
