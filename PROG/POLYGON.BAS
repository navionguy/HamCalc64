1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'POLYGON - Polygon Dimensions - 13 FEB 2002
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 
50 PI=4*ATN(1)  '3.141593
60 UL$=STRING$(80,205)
70 S$=" Length of each side"
80 H$=" Perpendicular distance from centre to mid point of each side"
90 R$=" Length of radial from centre to end of each side"
100 N$=" Number of sides/radials"
110 A$=" Angle between radials"
120 P$=" Perimeter"
130 GOTO 170
140 '
150 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN:RETURN
160 '
170 CLS:COLOR 15,2,1
180 N=0:H=0:R=0:S=0:P=0
190 PRINT " POLYGON DIMENSIONS (Regular polygons)";
200 PRINT TAB(57);"by George Murphy VE3ERP ";
210 COLOR 1,0:PRINT STRING$(80,223);
220 COLOR 7,0
230 '.....start
240 PRINT " Dimensions can be in any unit of measure.";
250 PRINT
260 PRINT " ENTER: ";N$;;", or 0 to EXIT";:INPUT N:LN=CSRLIN:GOSUB 150
270 IF N=0 THEN COLOR 7,0,0:CLS:CHAIN EX$
280 N=INT(N):IF N<3 THEN 260
290 A=2*PI/N:B=A/2
300 LN=3:GOSUB 150
310 PRINT " Press number in < > for known dimension:";
320 PRINT "   <1>";H$
330 PRINT "   <2>";R$
340 PRINT "   <3>";S$
350 Z$=INKEY$:IF Z$=""THEN 350
360 IF Z$="1"THEN GOSUB 150:GOTO 400
370 IF Z$="2"THEN GOSUB 150:GOTO 410
380 IF Z$="3"THEN GOSUB 150:GOTO 420
390 GOTO 350
400 PRINT " ENTER: ";H$;:INPUT H:GOSUB 150:GOSUB 440:GOTO 430
410 PRINT " ENTER: ";R$;:INPUT R:GOSUB 150:GOSUB 440:GOTO 430
420 PRINT " ENTER: ";S$;:INPUT S:GOSUB 150:GOSUB 440:GOTO 430
430 GOSUB 150:GOSUB 440
440 '.....calculate
450 IF B*S THEN H=S/2/TAN(B):R=S/2/SIN(B):GOTO 480
460 IF B*H THEN R=H/COS(B):S=2*TAN(B)*H:GOTO 480
470 IF B*R THEN H=R*COS(B):S=2*SIN(B)*R:GOTO 480
480 '.....display
490 LN=3:GOSUB 150
500 X=64
510 P=N*S
520 C$=" Area of polygon":C=S*H*N/2
530 D$="   Circumference of circle running thru mid points of sides"
540 E$="   Circumference of circle running thru end points of sides"
550 PRINT N$;STRING$(X-LEN(N$),".");USING " ##,###";N
560 PRINT H$;STRING$(X-LEN(H$),".");USING " ##,###.###";H
570 PRINT R$;STRING$(X-LEN(R$),".");USING " ##,###.###";R
580 PRINT A$;STRING$(X-LEN(A$),".");USING " ##,###.###ø";A*180/PI
590 PRINT S$;STRING$(X-LEN(S$),".");USING " ##,###.###";S
600 PRINT P$;STRING$(X-LEN(P$),".");USING " ##,###.###";P
610 PRINT D$;STRING$(X-LEN(D$),".");USING " ##,###.###";PI*2*H
620 PRINT E$;STRING$(X-LEN(E$),".");USING " ##,###.###";PI*2*R
630 PRINT C$;STRING$(X-LEN(C$),".");USING " ##,###.###";C
640 PRINT UL$;
650 '
660 '.....end
670 GOSUB 690:GOTO 170
680 '
690 '.....PRT
700 KEY OFF:GOSUB 770:LOCATE 25,5:COLOR 0,2
710 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
720 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
730 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 730 ELSE GOSUB 770
740 IF Z$="3"THEN RETURN
750 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
760 IF Z$="2"THEN LPRINT CHR$(12) ELSE 700
770 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
