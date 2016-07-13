1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'PRACOSC - rjd's version of Code Oscillator - 14 JAN 2011
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 '.....title page
70 COLOR 15,2
80 PRINT " CODE PRACTICE OSCILLATOR";TAB(57);"by George Murphy VE3ERP ";
90 COLOR 7,0
100 PRINT
110 PRINT " ÚÄÄS1ÄÄÂÄÄÂÄK1ÄÄR1ÄÄÄÄÂÄR2Ä¿";
120 PRINT " ³      ³  ³  ÚÄÄUÄÄ¿  ³    ³";
130 PRINT " ³      C1 ÃÄÄ´4   7ÃÄÄÙ    ³";
140 PRINT " ³      ³  ÀÄÄ´8   2ÃÄÄÂÄÄÄÄ´";
150 PRINT " ³+     ÃÄC4ÄÄ´5   6ÃÄÄÙ    C3";
160 PRINT " ³12vdc ÃÄÄÄÄÄ´1   3ÃÄR3Ä¿  ³";
170 PRINT " ³-     ³     ÀÄÄÄÄÄÙ    ³  Ï";
180 PRINT " ³      ³                R4®ÄÄÄÄ* audio";
190 PRINT " ³      ³                ³  ÚÄÄÄ* out";
200 PRINT " Ï      Ï                Ï  Ï";
210 PRINT " ...the code dashes you are hearing are the tones this produces...";
220 PRINT " S1............ SPST switch";
230 PRINT " K1............ Telegraph key";
240 PRINT " U............. 555 CMOS timer";
250 PRINT " C1,C3,........ .01 æF capacitor";
260 PRINT " C4............ 0.1 æF capacitor";
270 PRINT " R1............ 6.8Kê resistor";
280 PRINT " R2............ 75Kê resistor";
290 PRINT " R3............ 22 Kê resistor";
300 PRINT " R4............ 10 Kê audio taper potentiometer";
310  HZ=1.442999/9.999999E-03/(6.800000+2*75)*1000
320 FOR A=1 TO 12
330 SOUND HZ,3:SOUND 0,3:NEXT A
340 PRINT USING " Audio tone = ###.# Hz";HZ
350 GOSUB 360:RUN EX$
360 '.....PRT
370 KEY OFF:GOSUB 440:LOCATE 25,5:COLOR 0,2
380 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
390 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
400 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 400 ELSE GOSUB 440
410 IF Z$="3"THEN RETURN
420 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
430 IF Z$="2"THEN LPRINT CHR$(12) ELSE 370
440 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
