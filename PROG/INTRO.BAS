1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$, V$, D$, Z$, X$
10 '\hamcalc\prog\INTRO - Page one of HAMCALC - 06 JUN 96 rev. 25 JAN 2011
20 CLS: KEY OFF
30 SCREEN 0, 0: KEY 10, "SYSTEM" + CHR$(13)
40 ''OMMON EX$,V$,D$
50 IF EX$ <> "" THEN 60
52 EX$ = "\hamcalc64\prog\hcal" : V$ = "." : D$ = ".": Z$ = ";": X$ = "A"
55 CHAIN EX$
60 P$ = STRING$(37, 32) + "-ùùù-" + STRING$(38, 32)
70 S$ = STRING$(80, 32)
80 COLOR 15, 2, 1
90 PRINT S$;
100 LOCATE CSRLIN - 1, 16
110 PRINT " H A M C A L C - PAINLESS MATH for RADIO AMATEURS "
120 COLOR 15, 1
130 COLOR 14, 4
140 PRINT " É"; STRING$(76, "Í"); "» ";
150 PRINT " º  THIS SOFTWARE IS FREE AND IS NOT TO BE SOLD! If you use these p";
160 PRINT "rograms, a º ";
170 PRINT " º  donation to the CANADIAN NATIONAL INSTITUTE for the BLIND - AMA";
180 PRINT "TEUR RADIO º ";
190 PRINT " º  PROGRAM, c/o the author at the address below, would be apprecia";
200 PRINT "ted.       º ";
210 PRINT " È"; STRING$(76, "Í"); "¼ ";
220 COLOR 15, 1
230 PRINT P$;
240 PRINT "HAMCALC is frequently being expanded and upgraded.";
250 COLOR 12, 1
260 PRINT " This is version "; V$; " release ";
270 PRINT "date:"; D$; ":";
280 COLOR 15, 1
290 PRINT " To find the number of the latest version contact the author.  ";
310 PRINT "Comments, suggestions and ideas for new programs are welcomed by the author>.   ";
330 PRINT S$;
340 PRINT "                             George Murphy, VE3ERP                 ";
350 PRINT "             ";
360 PRINT "                                77 McKenzie St.                    ";
370 PRINT "             ";
380 PRINT "                          Orillia, ON L3V6A6, Canada               ";
390 PRINT "             ";
400 PRINT "                          e-mail < ve3erp@rac.ca >                 ";
410 PRINT "             ";
420 PRINT P$;
430 COLOR 14, 4
440 PRINT " HAMCALC is available as a free download at <www.cq-amateur-radio.c";
450 PRINT "om>. Versions";
460 PRINT " offered by other sources are unauthorized, probably outdated and m";
470 PRINT "ay not run   ";
480 PRINT " properly.                                                         ";
490 PRINT "             ";
500 COLOR 15, 1
510 PRINT P$;
520 PRINT "The ARRL Handbook For Radio Amateurs, Antenna Book, Electronics Dat";
530 PRINT "a Book and   ";
540 PRINT "other ARRL publications are gratefully acknowledged as primary sour";
550 PRINT "ces of data  ";
560 PRINT "and reference material for these programs.                         ";
570 PRINT "             ";
580 PRINT P$;
590 LOCATE 25: PRINT S$;
600 COLOR 15, 2
610 LOCATE 25, 27
620 PRINT " Press any key to continue ";
630 COLOR 7, 0
640 Z$ = INKEY$: IF Z$ = "" THEN 640
650 CLS: EX$ = "\hamcalc64\prog\hcal": CHAIN EX$
