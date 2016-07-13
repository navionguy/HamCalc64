1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'CONCUR - Constant Current Op Amp - 10 OCT 06
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 UL$=STRING$(80,205)
70 '
80 '.....title page
90 CLS
100 COLOR 15,2,1
110 PRINT " CONSTANT CURRENT OP AMP CIRCUIT";TAB(61);"by R.J.Dehoney IEEE ";
120 COLOR 1,0:PRINT STRING$(80,223);
130 COLOR 15,2:LOCATE CSRLIN-1,17
140 PRINT " Edited for HAMCALC by George Murphy VE3ERP "
150 COLOR 7,0
160 '.....start
170 GOSUB 180:GOSUB 300
180 PRINT UL$;
190 LOCATE CSRLIN-1,26:COLOR 0,7:PRINT " Constant Current Op Amp ";:COLOR 7,0
200 PRINT "                   зддддR1дддбдддR3дддддд©";
210 PRINT "                   о         Ё  зддддд©  Ё";
220 PRINT "                             юдд╢- op Ё  Ё         Io";
230 PRINT "                                Ё amp цддаддR5дбдддддддд©";
240 PRINT "                   зддддR2дддбдд╢+    Ё        Ё        RL";
250 PRINT "                  Vdc        Ё  юддддды        Ё        Ё";
260 PRINT "                   о         юддддR4ддддддддддды        о";
270 PRINT UL$;
280 RETURN
290 '.....preamble
300 PRINT " Knowing the required Io, select an op amp. Decide on the control";
310 PRINT " voltage range Vdc. Choose R4, bearing in mind that the op amp bias";
320 PRINT " current flowing through R4 will create an offset voltage. A value of";
330 PRINT " around 100k will probably be OK with most low current op amps. The";
340 PRINT " program assumes that R1=M*R4 and R2=R1, then calculates the required";
350 PRINT " values for R3 and R5 vs M. Choose a value of M giving a low value of";
360 PRINT " R5, since Io*R5 subtracts from the available output voltage. If you";
370 PRINT " know the actual value of the resistors, the program will calculate";
380 PRINT " Io for various RL. A schematic for a typical circuit is included."
390 PRINT UL$;
400 PRINT " Press <1> to run program or <0> to EXIT."
410 Z$=INKEY$:IF Z$=""THEN 410
420 IF Z$="0"THEN RUN EX$
430 IF Z$="1"THEN CLS:COLOR 7,0,0:GOSUB 180:GOTO 450
440 GOTO 410
450 '.....data entry
460 INPUT " ENTER: desired maximum output current Io (ma)";IO
470 INPUT " ENTER: maximum control voltage Vdc (volts)";VDC
480 INPUT " ENTER: R4 in kilohms ";R4
490 LOCATE 10,34:PRINT " R4=";R4;"K "
500 PRINT " M     R1 and R2 K   R3 K      R5 ohms"
510 READ M :IF M=0 THEN 560
520 R5=1/(M*IO/VDC-1/R4) :R3=R4+R5
530 PRINT M;TAB(8);USING "######.#   ";R4*M;R3;R5*1000
540 GOTO 510
550 DATA .1,.2,.5,1,2,5,10,15,20,0
560 GOSUB 1040:CLS:GOSUB 180
570 PRINT "Do you want to see Io vs RL for actual resistor values? (y/n) "
580 A$=INKEY$:IF A$=""THEN 580
590 IF A$="y"THEN 680
600 IF A$="n"THEN 620
610 GOTO 590:STOP
620 PRINT "Do you want to see a typical circuit? (y/n) "
630 A$=INKEY$:IF A$=""THEN 630
640 IF A$="y"THEN CLS:GOSUB 180:GOTO 910
650 IF A$="n"THEN 680
660 GOTO 590
670 '.....data entry
680 CLS
690 PRINT "           Circuit Performance"
700 INPUT " ENTER: R1 (kilohms)";R1
710 INPUT " ENTER: R2 (kilohms)";R2
720 INPUT " ENTER: R3 (kilohms)";R3
730 INPUT " ENTER: R4 (kilohms)";R4
740 INPUT " ENTER: R5 (ohms)   ";R5:R5=R5/1000
750 INPUT " ENTER: Enter control voltage (volts)";VDC
760 K1=R1/(R1+R3) :K2=R2/(R2+R4) :K4=R4/(R2+R4) :K5=R5/(R2+R4)
770 CLS
780 PRINT "Control voltage =";VDC;;"volts"
790 PRINT "RL ohms     Io ma   Vo volts"
800 FOR N=1 TO 20 :RL=N/10
810 IOX=VDC*(K4/K1+K5)/(R5+RL*(1+K5-K2/K1))
820 PRINT RL*1000;TAB(10) USING "##.##      ";IOX;IOX*RL
830 NEXT N :PRINT :RESTORE
840 GOSUB 1040:CLS:GOSUB 180:GOTO 850
850 PRINT "Do you want to change values (y/n) ? "
860 A$=INKEY$:IF A$=""THEN 860
870 IF A$="n"THEN CLS:GOSUB 180:GOTO 910
880 IF A$="y"THEN CLS:GOSUB 180:GOTO 680
890 GOTO 870
900 '.....typical circuit
910 PRINT "Typical Practical Circuit "
920 PRINT "     +12 v            зддR1ддбддддR3ддддд©  +15 v on pin 8";
930 PRINT "     здад©            о      Ё  зддддд©  Ё  -15 v on pin 4";
940 PRINT "     Ё   Ё    зддддд©        юд6╢+    Ё  Ё";
950 PRINT "     ЁPotцддд3╢+    Ё           Ё 353 ц7дадR5дбдддддд©";
960 PRINT "     Ё   Ё    Ё 353 ц1бддR2ддбд5╢-    Ё       Ё   Io Ё";
970 PRINT "     юдбды  з2╢-    Ё Ё      Ё  юддддды       Ё      RL";
980 PRINT "     -12 v  Ё юддддды Ё      юдддддддR4ддддддды      Ё";
990 PRINT "            юддддддддды      LF353 dual op amp       о";
1000 PRINT UL$;
1010 '.....end
1020 GOSUB 1040:CLS:GOSUB 180:RESTORE:GOTO 400
1030 END
1040 '.....PRT
1050 KEY OFF:GOSUB 1120:LOCATE 25,5:COLOR 0,2
1060 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1070 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1080 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1080 ELSE GOSUB 1120
1090 IF Z$="3"THEN RETURN
1100 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1110 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1050
1120 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
