1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'HALFLOOP - Half-Loop Antenna - 17 JUN 2001 rev. 24 JAN 06
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 COLOR 7,0,1
50 UL$=STRING$(80,205)
60 '
70 '.....title page
80 CLS
90 COLOR 15,2
100 PRINT " HALF-LOOP ANTENNA";TAB(57);"by George Murphy VE3ERP ";
110 COLOR 1,0:PRINT STRING$(80,223);
120 COLOR 7,0
130 '
140 '....start
150 COLOR 0,7:I=22
160 LOCATE ,I:PRINT "    Half-Loop Low Profile Antenna    "
170 LOCATE ,I:PRINT "  ÚÄÄÄÄÄÄÄÄ wire L long ÄÄÄÄÄÄÄÄÄÄ¿  "
180 LOCATE ,I:PRINT "  ³   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³  "
190 LOCATE ,I:PRINT "  ³   ³                       ³   ³  "
200 LOCATE ,I:PRINT "  ³   ³                       ³   ³  "
210 LOCATE ,I:PRINT "  ³   ³                       H   ³  "
220 LOCATE ,I:PRINT "  À¯ÄÄÙ®ÄÄ¿ 50ê feedline      ³   ³  "
230 LOCATE ,I:PRINT "    ÄÄ¿  12 in.               ³   ³  "
240 LOCATE ,I:PRINT " -----Å®ÄÄÙ-----earth---------Å ®ÄÙ  "
250 LOCATE ,I:PRINT "      Á ®ÄÄÄ ground rods ÄÄÄ¯ Á      "
260 COLOR 7,0
270 PRINT UL$;
280 LN1=CSRLIN:GOSUB 480  'preface
290 PRINT UL$;
300 LN2=CSRLIN:LOCATE LN2,23
310 COLOR 0,7:PRINT " Press 1 to continue or 0 to EXIT ";:COLOR 7,0
320 Z$=INKEY$:IF Z$=""THEN 320
330 IF Z$="0"THEN CLS:CHAIN EX$
340 IF Z$="1"THEN 360
350 GOTO 320
360 VIEW PRINT LN1 TO 24:CLS:VIEW PRINT:LOCATE LN1
370 '
380 '.....data input
390 INPUT " ENTER: Frequency of operation (MHz)";F
400 VIEW PRINT LN1 TO 24:CLS:VIEW PRINT:LOCATE LN1
410 L=530/F
420 PRINT USING " Frequency of operation..Ÿ= ####.### MHz";F
430 PRINT USING " Total wire required.....L= ####.### m.= ###.## ft.";L*3.048000E-01,L;
440 IF L>32 THEN PRINT "";ELSE PRINT USING " = ###.### in.";L*12
450 PRINT       " Height..................H= minimum practicable"
460 GOTO 650
470 END
480 '.....preface
490 PRINT " The half-loop antenna is a vertically polarized, almost omnidire";
500 PRINT "ctional       "
510 PRINT " radiator that offers a reasonable match to 50-ohm transmission l";
520 PRINT "ine and has   "
530 PRINT " maximum radiation at low angles. As its name implies, the half-l";
540 PRINT "oop is one    "
550 PRINT " half of a full-wave loop. The ground plane below the antenna pro";
560 PRINT "vides a return"
570 PRINT " path for the loop. The antenna is fed at the base of either leg ";
580 PRINT "with 50-ohm   "
590 PRINT " line at a point as close to the ground as practicable. A set of ";
600 PRINT "radials is    "
610 PRINT " recommended at each ground rod."
620 PRINT TAB(17);"(Ref: ARRL Antenna Compendium, vol.2, page 88)"
630 RETURN
640 '
650 '.....end
660 GOSUB 680:GOTO 70
670 '
680 '.....PRT
690 KEY OFF:GOSUB 760:LOCATE 25,5:COLOR 0,2
700 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
710 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
720 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 720 ELSE GOSUB 760
730 IF Z$="3"THEN RETURN
740 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
750 IF Z$="2"THEN LPRINT CHR$(12) ELSE 690
760 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
