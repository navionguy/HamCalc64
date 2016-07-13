1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'REMSIG - 18 DEC 2010
20 'REMSIG - 17 DEC 2010
30 CLS:KEY OFF
40 IF EX$=""THEN EX$="EXIT"
50 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
60 
70 '.....title page
80 COLOR 15,2
90 PRINT " REMOTE SIGNAL LAMP";TAB(57);"by George Murphy VE3ERP ";
100 COLOR 7,0
110 '.....start
120 PRINT ";
130 PRINT "                             K1nc*ÄÄÄÄÄÄ¿";
140 PRINT "     Ú*ÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*K1cp  S1 (LP on/off)";
150 PRINT "     ³      ³T1ÚÄÄ>ÃÄ* to *¿ K1no*ÄÄÄ>ÃÄ´";
160 PRINT "     ³      >³³<  D1   PB. ³         D3 ³";
170 PRINT "   110vAC   >³³<12vAC      ÀÄ´<Ä¿       LP";
180 PRINT "     ³      >³³<             D2 ³       ³";
190 PRINT "     ³      ³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄK1ÄÙ       ³";
200 PRINT "     À*ÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ";
210 PRINT
220 PRINT " T1......... 110/12 volt 300 ma. transformer";
230 PRINT " S1..........SPST switch";
240 PRINT " D1,D2,,,,,,,1N4001 rectifier diode";
250 PRINT " D3..........1N5402 3A. rectifier diode";
260 PRINT " K1..........12 vDC SPDT relay";
270 PRINT " PB..........Remote pushbutton";
280 PRINT " LP..........110 v. signal lanp in out-of-sight location";
290 PRINT
300 PRINT " This device signals anyone at a remote location where the signal lamp is"
310 PRINT " located. When the pushtton is pushed, if tha lamp is off it will turn on."
320 PRINT " If on, it will go dim."
325 GOSUB 330:GOTO 70
330 '.....PRT
340 KEY OFF:GOSUB 410:LOCATE 25,5:COLOR 0,2
350 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
360 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
370 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 370 ELSE GOSUB 410
380 IF Z$="3"THEN CLS:RUN EX$
390 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
400 IF Z$="2"THEN LPRINT CHR$(12) ELSE 340
410 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
