1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'PULSEGEN - Op Amp Pulse Generator - 03 OCT 2005
20 IF EX$=""THEN EX$="EXIT"
30 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
40 
50 '
60 '....start
70 CLS:KEY OFF:COLOR 15,2,1
80 PRINT " PULSE GENERATOR";TAB(63);"by Robert Dehoney ";
90 COLOR 1,0:PRINT STRING$(80,223);
100 COLOR 7,0,0
110 PRINT " This program calculates resistor values for the Schmidt trigger";
120 PRINT " circuit shown below. It uses an op amp that can swing rail to rail.";
130 PRINT STRING$(80,205);
140 PRINT "                            Vdc";
150 PRINT "                            ÚÄÄÁÄÄ¿";
160 PRINT "                  VinÄÄÄÄÄÄÄ´-    ³";
170 PRINT "                            ³OPAMPÃÄÄÄÂÄÄÄOUT";
180 PRINT "                        ÚÄÄÄ´+    ³   ³";
190 PRINT "                        ³   ÀÄÄÂÄÄÙ   R3";
200 PRINT "                        ³      Ï      ³";
210 PRINT "              VdcÄÄÄR1ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄ´";
220 PRINT "                                      R2";
230 PRINT "                                      ³";
240 PRINT "                                      Ï";
250 PRINT ";
260 LN=CSRLIN
270 PRINT " To run program.....press 1"
280 PRINT " To EXIT............press 0 "
290 Z$=INKEY$
300 IF Z$="1"THEN 330
310 IF Z$="0"THEN CLS:RUN GO$
320 GOTO 290
330 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
340 INPUT " ENTER: Op Amp supply voltage, Vdc. ",V
350 INPUT " ENTER: Upper switching threshold voltage ",VT1
360 INPUT " ENTER: Lower switching threshold voltage ",VT2 :IF VT2=>VT1 THEN 360
370 INPUT " ENTER: any value for R1 ",R1
380 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
390 A=(V-VT2)/(V-VT1)*VT1/VT2
400 B=VT1/(V-VT1)+1
410 R3=B*R1/(A-1)
420 R2=VT1/(V-VT1)*1/(1/R1+1/R3)
430 PRINT " Vdc=";V;"   Threshold voltages=";VT2;"&";VT1
440 PRINT " R1=";R1;" R2=";R2;"  R3=";R3  ;"  R2+R3=";R2+R3
450 LN=CSRLIN
460 PRINT " Want to use a lower value for R3?   (y/n)"
470 Z$=INKEY$:IF Z$="y" THEN 510
480 IF Z$="n" THEN LOCATE CSRLIN-1:PRINT STRING$(79,32):GOSUB 750:GOTO 60
490 GOTO 470
500 '
510 VIEW PRINT 6 TO 24 :CLS:VIEW PRINT:LOCATE 6
520 PRINT " To avoid high ohms for R3, use the circuit below."; :PRINT
530 PRINT "                              Vdc";
540 PRINT "                            ÚÄÄÁÄÄ¿";
550 PRINT "                  VinÄÄÄÄÄÄÄ´-    ³";
560 PRINT "                            ³OPAMPÃÄÄÄÂÄÄÄOUT";
570 PRINT "                        ÚÄÄÄ´+    ³   ³";
580 PRINT "                        ³   ÀÄÄÂÄÄÙ   Rc";
590 PRINT "                        ³      Ï      ³";
600 PRINT "              VdcÄÄÄR1ÄÄÁÄÄÄÄÄRaÄÄÄÄÄÄ´";
610 PRINT "                                      Rb";
620 PRINT "                                      ³";
630 PRINT "                                      Ï";
640 LN=CSRLIN
650 LOCATE LN:PRINT "ENTER largest value <";R3;;" you can live with for Rc";
660 INPUT RC
670 IF RC<R3 THEN 690
680 LOCATE LN:PRINT STRING$(79,32):GOTO 650
690 RX=RC*(R2+R3)/(R3-RC) :RS=R2+R3+RX :RA=R2*R3/RS :RB=R2*RX/RS
700 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
710 PRINT " Vdc=";V;"   Threshold voltages=";VT2;"&";VT1
720 PRINT :PRINT " R1=";R1;" Ra=";RA;"  Rb=";RB;"  Rc=";RC; "  Rb+Rc=";RB+RC
730 GOSUB 750:GOTO 70
740 '
750 '.....PRT
760 KEY OFF:GOSUB 830:LOCATE 25,5:COLOR 0,2
770 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
780 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
790 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 790 ELSE GOSUB 830
800 IF Z$="3"THEN RETURN
810 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
820 IF Z$="2"THEN LPRINT CHR$(12) ELSE 760
830 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
