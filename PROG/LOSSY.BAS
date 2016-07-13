1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'LOSSY - SWR vs. Line Loss  -  6 DEC 2003
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 PI=4*ATN(1)  '3.141593
70 UL$=STRING$(80,205)
80 '
90 '.....title page
100 CLS
110 COLOR 15,2,1
120 PRINT " SWR vs. Line Loss";TAB(60);"by Robert J. DeHoney ";
130 COLOR 1,0:PRINT STRING$(80,223);
140 COLOR 7,0
150 '
160 '.....start
170 PRINT "This program finds the attenuation factor in dB/100 feet for a ";
180 PRINT "transmission"
190 PRINT "line or feeder. It requires the user to input the length and ";
200 PRINT "measured SWR of "
210 PRINT "the line when terminated in a short or open condition."
220 PRINT
230 COLOR 0,7:PRINT " to run program....press 1 "
240 PRINT " to EXIT...........press 0 ";:COLOR 7,0
250 PRINT
260 Z$=INKEY$:IF Z$=""THEN 260
270 IF Z$="1"THEN 310
280 IF Z$="0"THEN CHAIN GO$
290 GOTO 260
300 '
310 '.....data input
320 INPUT "ENTER:Line or feeder length in feet........ ";L
330 INPUT "ENTER:SWR of the shorted or open line...... ";SWR
340 PRINT
350 ATT=LOG((SWR+1)/(SWR-1))/2/L 'nepers/foot
360 ATTDB=ATT*100/1.150999E-01 'dB/100ft
370 U$=" Line attenuation factor is ##.### dB/100ft"
380 COLOR 0,7:PRINT USING U$;ATTDB:COLOR 7,0
390 PRINT
400 GOTO 420
410 '
420 '.....end
430 GOSUB 450:GOTO 90
440 '
450 '.....PRT
460 KEY OFF:GOSUB 530:LOCATE 25,5:COLOR 0,2
470 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
480 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
490 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 490 ELSE GOSUB 530
500 IF Z$="3"THEN RETURN
510 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
520 IF Z$="2"THEN LPRINT CHR$(12) ELSE 460
530 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
