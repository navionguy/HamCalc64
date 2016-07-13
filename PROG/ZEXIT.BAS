1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'ZEXIT - Acknowledgements & return to DOS - 01 FEB 96 rev. 01 JUN 1998
20 CLS:KEY OFF
30 COLOR 15,1,0
40 VIEW PRINT 1 TO 24:CLS:VIEW PRINT:LOCATE 1
50 DIM A$(40)
60 '
70  '.....load file
80  OPEN"I",1,"\hamcalc\docfiles\acknow.fil"
90  N=0
100  IF EOF(1)THEN 130
110  N=N+1:INPUT #1,A$(N)
120  GOTO 100
130  CLOSE
140 '
150 T$=STRING$(6,32)
160 PRINT
170 PRINT T$;
180 PRINT "HAMCALC could never have been what and where it is without the"
190 PRINT T$;
200 PRINT "encouragement and input in terms of friendship, time, and expertise"
210 PRINT T$;
220 PRINT "from many people worldwide, especially"
230 PRINT
240 '
250 '.....print file
260 K=CINT(N/2)
270 FOR Z=1 TO K
280 PRINT TAB(2);A$(Z);TAB(42);A$(Z+K)
290 NEXT Z
300 '
310 PRINT
320 PRINT T$;
330 PRINT "and most especially my wonderful wife Anne, who has forgiven me for"
340 PRINT T$;
350 PRINT "letting HAMCALC turn what started out to be a part-time retirement"
360 PRINT T$;
370 PRINT "project into a most pleasurable (and almost full-time) occupation."
380 '
390 LOCATE 25,20:COLOR 14,4
400 PRINT " 73 de VE3ERP....Press any key to EXIT....";
410 IF INKEY$=""THEN 410
420 COLOR 7,0,0
430 CLS:SYSTEM
