1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'FINANCE -Financial Calculators - 30 DEC 2000 rev.
20 CLS:KEY OFF
30 PROG$="finance"
40 IF EX$=""THEN EX$="EXIT"
50 
60 COLOR 7,0,1
70 DIM A$(7),B$(7),C$(7)
80 UL$=STRING$(80,205)
90 FOR N=1 TO 7:READ A$(N),B$(N),C$(N):NEXT N
100 '
110 '.....start
120 CLS
130 COLOR 15,2
140 PRINT " FINANCIAL CALCULATORS";TAB(57);;"by George Murphy VE3ERP ";
150 COLOR 1,0:PRINT STRING$(80,223);
160 COLOR 7,0
170 GOSUB 420 'preface
180 PRINT UL$;
190 PRINT " Press letter in ( ) to select:";
200 PRINT UL$;
210 '
220 FOR N=1 TO 7
230 PRINT TAB(10);"(";A$(N);;")  ";C$(N)
240 IF N=7 THEN PRINT
250 NEXT N
260 '
270 Z$=INKEY$
280 IF Z$="g"THEN CLS:CHAIN EX$
290 FOR Z=1 TO 7
300 IF Z$=A$(Z)THEN CLS:CHAIN B$(Z)
310 NEXT Z
320 GOTO 270
330 '
340 DATA a,mortgag,Mortgage Calculator
350 DATA b,loanpay,Monthly Time Payment Calculator (approximate)
360 DATA c,mortgag,Monthly Time Payment Calculator (exact)
370 DATA d,compint,Compound Interest Calculator
380 DATA e,deprec,Depreciation Schedule Calculator
390 DATA f,buysell,Markup/Markdown Calculator
400 DATA g,exit,EXIT
410 '
420 '.....preface
430 PRINT " These programs may be of interest if you are thinking about buyi";
440 PRINT "ng a new rig, "
450 PRINT " antenna farm, car or QTH."
460 RETURN
