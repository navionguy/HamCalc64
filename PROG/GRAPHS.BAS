1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'GRAPHS - 25 JAN 2006
20 CLS:KEY OFF:SCREEN 0
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 '.....title page
70 CLS
80 UL$=STRING$(80,205)
90 COLOR 15,2,1
100 PRINT " GRAPHS";TAB(63);"by Robert Dehoney ";
110 COLOR 1,0:PRINT STRING$(80,223);
120 COLOR 7,0
130 '
140 PRINT " This program generates a graph of any Y=�(X) equation";
150 PRINT UL$;
160 PRINT " ......press 1 to continue or 0 to EXIT.....";
170 Z$=INKEY$:IF Z$="0"THEN CLS:RUN EX$
180 IF Z$="1"THEN CLS:GOTO 200
190 GOTO 170
200 PRINT "1. You may change or replace previous equations in the space below."
210 PRINT "2. Enter new equations in the format Y=�(X)."
220 PRINT "3. Enter the max and min values of X over the range of X."
230 PRINT "4. Y vs X is then plotted"
240 PRINT UL$;
250 LN=CSRLIN
260 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE 18,1:PRINT UL$;
270 PRINT "Now, type `RUN 300'"; :LOCATE 2,1
280 LOCATE LN
290 '
300 COLOR 7,0,0:CLS :WXMAX=610 :WXMIN=60 :DWX=WXMAX-WXMIN :YMIN=1.000000E+16
310 WYMAX=320 :WYMIN=15 :DWY=WYMAX-WYMIN :DIM Y(1000)
320 INPUT "ENTER: minimum value of X";XMIN
330 INPUT "ENTER: maximum value of X";XMAX
340 DX=XMAX-XMIN
350 FOR N=0 TO DWX :X=N*DX/DWX
360 Y=SIN(X)
370 '
380 '
390 '
400 '
410 '
420 'Do not add line numbers higher than this.
430 IF Y>YMAX THEN YMAX=Y
440 IF Y<YMIN THEN YMIN=Y
450 Y(N)=Y :Y=0 :NEXT N :DY=YMAX-YMIN :IF DY=0 THEN PRINT "Cannot proceed"; :RUN
460 '...............Graphing routine
470 SCREEN 9
480 LINE (WXMIN,WYMIN)-(WXMAX,WYMAX),,B
490 '--------------Draw curve--------------
500 FOR N=0 TO DWX :X=N*DX/DWX+XMIN
510 XP=(X-XMIN)*DWX/DX+WXMIN : YP=WYMAX-DWY/DY*(Y(N)-YMIN)
520 PSET(XP,YP) :NEXT N 'draw curve
530 '--------------Plot Data Points (if any)--------------
540 'FOR N=1 TO NMAX
550 'XP=WXMAX-E-(DWX-2*E)/DX*(XMAX-X(N)) :YP=WYMIN+E+(DWY-2*E)/DY*(YMAX-Y(N))
560 'CIRCLE(XP,YP),3 :NEXT N 'plot data points
570 '------------------Print Scales----------------
580 LOCATE 24,8 :PRINT XMIN :LOCATE 23,74 :PRINT XMAX;:LOCATE 1,1
590 LOCATE 23,45 :PRINT "X"
600 LOCATE 22,1 :PRINT USING "##.##";YMIN :LOCATE 10,3 :PRINT "Y"
610 LOCATE 2,1       :PRINT USING "##.##";YMAX ' print scales
620 LOCATE 24,8 :PRINT ".....press any key when finished.....";
630 Z$=INKEY$:IF Z$=""THEN 630
640 SCREEN 0:GOTO 60
