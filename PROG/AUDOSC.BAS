1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'AUDOSC - Audio Oscillator - 06 JUN 2007
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 '
70 '.....title page
80 CLS
90 COLOR 15,2,0
100 PRINT " LM324 AUDIO OSCILLATOR";TAB(57);"by George Murphy VE3ERP ";
110 COLOR 1,0:PRINT STRING$(80,223);
120 COLOR 15,2:LOCATE CSRLIN-1,25:PRINT " Algorithm by R.J.Dehoney IEEE "
130 COLOR 7,0
140 GOSUB 610:LN=CSRLIN+1
150 PRINT
160 PRINT " This program analyzes an oscillator that uses a grounded resistor"
170 PRINT " to control its frequency. The frequency is independent of the"
180 PRINT " +/- DC supplies and inversely proportional to Rx. The period is"
190 PRINT " directly proportional to Rx. The period T=1/F=4*Rx*C1*R1/R2.
200 PRINT
210 PRINT " For a tone at a given frequency look for an Rx at that frequency,"
220 PRINT " then find the frequency with an Rx of the nearest standard value."
230 PRINT: COLOR 0,7
240 LOCATE ,20:PRINT " .....Press 1 to continue or 0 to exit..... ":COLOR 7,0
250 Z$=INKEY$:IF Z$=""THEN 250
260 IF Z$="1"THEN 290
270 IF Z$="0"THEN chain GO$
280 GOTO 250
290 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
300 '.....start
310 COLOR 7,0,0
320 R1=1.000000E+05 :R2=1.000000E+05 : C1=1.000000E-07     ' (C1 in farads)
330 PRINT  " Want to find (f)requency or R(x)?  (f/x)
340 Z$=INKEY$
350 IF Z$="f"THEN 380
360 IF Z$="x"THEN 410
370 GOTO 340
380 '.....find frequency
390 INPUT " ENTER: Rx in kê";Z:RX=Z*1000
400 F=R2/4/C1/RX/R1:GOTO 440
410 '.....find Rx
420 INPUT " ENTER: Frequency in Hz";F
430 RX=R2/(4*C1*F*R1):GOTO 440
440 '....DISPLAY
450 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
460 DT=5.000000E-01/F:DTM=2*DT*1000
470 PRINT USING " Frequency....... ###,###.# Hz";F
480 PRINT USING " Period.......... ###,###.# msec";DTM
490 PRINT USING " Rx.............. ###,###.# kê";RX/1000
500 PRINT " Would you like to hear a";CINT(F);"Hz tone?   (y/n) ":COLOR 7,0
510 Z$=INKEY$:IF Z$=""THEN 510
520 IF Z$="n"THEN 600
530 IF Z$="y"AND F<37 THEN PRINT:GOTO 560
540 IF Z$="y"THEN LOCATE CSRLIN-1:PRINT STRING$(79,32):SOUND F,40:GOTO 600
550 GOTO 510
560 PRINT TAB(10);"Sorry - GWBASIC cannot generate tones lower than 3 Hz!"
570 PRINT TAB(29);".....press any key....."
580 Z$=INKEY$:IF Z$=""THEN 580
590 LOCATE CSRLIN-1:PRINT STRING$(79,32)
600 GOTO 770
610 '.....diagram
620 COLOR 0,7:T=14
630 LOCATE ,T:PRINT "                                                     "
640 LOCATE ,T:PRINT " ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿  U1--LM 324        "
650 LOCATE ,T:PRINT " ³      ÚÄÄÄÄC1ÄÄÄÄ¿     V+     ³  R1--100K ohm      "
660 LOCATE ,T:PRINT " ³      ³ ÚÄÄÄÄÄ¿  ³      ³     ³  R2--100K ohm      "
670 LOCATE ,T:PRINT " ³ ÚÄR1ÄÁÄ´-    ³  ³   ÚÄÄÁÄÄ¿  ³  C1--0.1 æF        "
680 LOCATE ,T:PRINT " ÀÄ´      ³ U1a ÃÄÄÁÄÄÄ´+    ³  ³                    "
690 LOCATE ,T:PRINT "   ÀÄR2ÄÂÄ´+    ³      ³ U1b ÃÄÄÁÄÄ> Square Wave out "
700 LOCATE ,T:PRINT "        ³ ÀÄÄÄÄÄÙ    ÚÄ´-    ³                       "
710 LOCATE ,T:PRINT "        Rx           ³ ÀÄÄÂÄÄÙ                       "
720 LOCATE ,T:PRINT "        ³            ³    ³                          "
730 LOCATE ,T:PRINT "        Ï            Ï   V-                          "
740 COLOR 7,0
750 RETURN
760 '.....end
770 GOSUB 780:GOTO 70
780 '.....PRT
790 KEY OFF:GOSUB 860:LOCATE 25,5:COLOR 0,2
800 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
810 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
820 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 820 ELSE GOSUB 860
830 IF Z$="3"THEN RETURN
840 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
850 IF Z$="2"THEN LPRINT CHR$(12) ELSE 790
860 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
