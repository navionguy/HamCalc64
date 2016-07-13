1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'TRIMR - 11 DEC 2007 rev. 07 JAN 2008
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 '.....title page
70 CLS
80 COLOR 15,2,1
90 PRINT " CAPACITORS - TRIMMER";TAB(57);"by George Murphy VE3ERP ";
100 COLOR 1,0:PRINT STRING$(80,223);
110 COLOR 7,0,0
120 '
130 PRINT " This program calculates values of a fixed capacitor C1 with its ";
140 PRINT "variable"
150 PRINT " trimmer capacitor C2."
160 LN=CSRLIN
170 PRINT
180 PRINT " .....Press 1 to continue or 0 to exit.....";
190 Z$=INKEY$:IF Z$=""THEN 190
200 IF Z$="1" THEN VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN+1:GOTO 230
210 IF Z$="0" THEN RUN EX$
220 GOTO 190
230 '.....start
240 C1=0:C2=0
250 INPUT " ENTER: Required capacitance in pF";X
260 INPUT " ENTER: Capacitance in pF of any existing variable capacitor";C2
270 IF X>C2 THEN X$="parallel.":C1=X+C2
280 IF X<C2 THEN X$="series.":C1=1/(1/X+1/C2)
290 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN+1
300 COLOR 0,7
310 LOCATE ,2:PRINT " Connect C1 and C2 in ";X$
320 COLOR 7,0
330 PRINT
340 PRINT USING " Required Capacitance....... ####.### pF";X
350 PRINT
360 PRINT USING " Fixed C1................... ####.## pF";C1;
370 PRINT "  (see note below)"
380 PRINT USING " Variable C2................ ####.## pF";C2
390 PRINT
400 PRINT " If there is no standard capacitor close to";C1;"pF run HamCalc's"
410 PRINT " `Capacitors - Precision' program";
420 '.....end
430 GOSUB 440:GOTO 60
440 '.....PRT
450 KEY OFF:GOSUB 520:LOCATE 25,5:COLOR 0,2
460 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
470 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
480 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 480 ELSE GOSUB 520
490 IF Z$="3"THEN RETURN
500 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
510 IF Z$="2"THEN LPRINT CHR$(12) ELSE 450
520 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
