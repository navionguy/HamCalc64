1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$
10 'DIPOL160 - Short dipole for 160/80/40 - 09 JUN 01
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 
50 COLOR 7,0,1
60 PI=4*ATN(1)  '3.141593
70 UL$=STRING$(80,205)
80 '
90 '.....title page
100 CLS
110 COLOR 15,2
120 PRINT " SHORT DIPOLE for 160/80/40 metres";TAB(57);"by George Murphy VE3ERP ";
130 COLOR 1,0:PRINT STRING$(80,223);
140 COLOR 15,2:LOCATE CSRLIN-1,26:PRINT " Designed by Al Buxton W8NX "
150 COLOR 7,0
160 PRINT TAB(29);"(QST, July 1992, p.35)"
170 COLOR 0,7:Q=11
180 LOCATE,Q:PRINT "                                                           "
190 LOCATE,Q:PRINT " Ё╝ддддддддддддддддддддддддд A ддддддддддддддддддддддддд╞Ё "
200 LOCATE,Q:PRINT " Ё╝дDд╞Ё╝ддCдд╞Ё╝дддддBддддд╞Ё╝дддддBддддд╞Ё╝ддCдд╞Ё╝дDд╞Ё "
210 LOCATE,Q:PRINT " Ё     Ё       Ё           здад©balun      Ё       Ё     Ё "
220 LOCATE,Q:PRINT " ЛддддОООдддддОООдддддддддд╢1:1цддддддддддОООдддддОООддддЛ "
230 LOCATE,Q:PRINT "      80m     40m          юдрды          40m     80m      "
240 LOCATE,Q:PRINT "  SuperTrap SuperTrap     50-75 Й     SuperTrap SuperTrap  "
250 LOCATE,Q:PRINT "                       coaxial cable                       "
260 COLOR 7,0
270 PRINT
280 PRINT TAB(19);"Dimensions for 1.865, 3.825, and 7.225 MHz"
290 PRINT TAB(19);STRING$(42,205)
300 PRINT TAB(27);USING "A = ###.## m. = ###.## ft.";4.034999E+01,4.034999E+01*3.048000
310 PRINT TAB(27);USING "B = ###.## m. = ###.## ft.";1.056000E+01,1.056000E+01*3.048000
320 PRINT TAB(27);USING "C = ###.## m. = ###.## ft."; 3.609999, 3.609999*3.048000
330 PRINT TAB(27);USING "A = ###.## m. = ###.## ft."; 6.000000 , 6.000000 *3.048000
340 PRINT UL$;
350 LN=CSRLIN
360 COLOR 0,7:LOCATE LN,23
370 PRINT " Press 1 to continue or 0 to EXIT "
380 COLOR 7,0
390 Z$=INKEY$:IF Z$=""THEN 390
400 IF Z$="1"THEN 430
410 IF Z$="0"THEN CLS:CHAIN EX$
420 GOTO 390
430 VIEW PRINT LN TO 24:CLS:VIEW PRINT:GOSUB 510:LOCATE LN,18
440 COLOR 0,7:PRINT " Press 2 for SuperTrap design, or 3 to quit ";:COLOR 7,0
450 Z$=INKEY$:IF Z$=""THEN 450
460 IF Z$="2"THEN CLS:CHAIN"buxtrap"
470 IF Z$="3"THEN 490
480 GOTO 450
490 VIEW PRINT LN TO 24:CLS:VIEW PRINT:GOTO 360
500 '
510 '.....PRT
520 KEY OFF:GOSUB 590:LOCATE 25,5:COLOR 0,2
530 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
540 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
550 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 550 ELSE GOSUB 590
560 IF Z$="3"THEN RETURN
570 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
580 IF Z$="2"THEN LPRINT CHR$(12) ELSE 520
590 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
