1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'HAMBANDS - Ham Band Edges - 21 APR 95 rev.11 APR 97
20 IF EX$=""THEN EX$="EXIT"
30 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
40 
50 CLS:KEY OFF
60 DIM F(15,3)
70 L$=" "+STRING$(78,205)
80 COLOR 15,2,1
90 PRINT " HAM BAND EDGE & CENTRE FREQUENCIES";
100 PRINT TAB(57);;"by George Murphy VE3ERP ";
110 COLOR 1,7
120 PRINT STRING$(80,223);
130 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3
140 COLOR 7,0
150 '
160 DATA 1.8, 2, 3.5, 4, 7, 7.3, 10.1, 10.15, 14, 14.35, 18.068, 18.168
170 DATA 21, 21.45, 24.89, 24.99, 28, 29.7, 50, 54, 144, 148, 222, 225
180 DATA 420, 450,902, 928, 1240, 1300
190 '
200 FOR Z=1 TO 15:READ F(Z,1):READ F(Z,3):NEXT Z
210 FOR Z=1 TO 15:F(Z,2)=SQR(F(Z,1)*F(Z,3)):NEXT Z
220 '
230 '.....screen display
240 COLOR 0,7
250 PRINT "        Ú                           Ú                           Ú "
260 PRINT " 160 m.Ä´*                    17 m.Ä´*                     2 m.Ä´*"
270 PRINT "  band  À                     band  À                     band  À "
280 PRINT L$;
290 PRINT "        Ú                           Ú                           Ú "
300 PRINT "  80 m.Ä´*                    15 m.Ä´*                  1.25 m.Ä´*"
310 PRINT "  band  À                     band  À                     band  À "
320 PRINT L$;
330 PRINT "        Ú                           Ú                           Ú "
340 PRINT "  40 m.Ä´*                    12 m.Ä´*                   70 cm.Ä´*"
350 PRINT "  band  À                     band  À                     band  À "
360 PRINT L$;
370 PRINT "        Ú                           Ú                           Ú "
380 PRINT "  30 m.Ä´*                    10 m.Ä´*                   33 cm.Ä´*"
390 PRINT "  band  À                     band  À                     band  À "
400 PRINT L$;
410 PRINT "        Ú                           Ú                           Ú "
420 PRINT "  20 m.Ä´*                     6 m.Ä´*                   23 cm.Ä´*"
430 PRINT "  band  À                     band  À                     band  À "
440 PRINT L$;
450 PRINT "  * Centre Frequency = û(Bottom Edge Frequency x Top Edge Frequency)"
460 PRINT L$;
470 COLOR 7,0
480 '
490 X=0
500 FOR Z=3 TO 19 STEP 4
510 X=X+1
520 COLOR 15,1
530 Y=F(X,1)   :GOSUB 800:LOCATE Z,11:PRINT USING U$;Y;:PRINT " MHz "
540 Y=F(X+5,1) :GOSUB 800:LOCATE Z,39:PRINT USING U$;Y;:PRINT " MHz "
550 Y=F(X+10,1):GOSUB 800:LOCATE Z,67:PRINT USING U$;Y;:PRINT " MHz "
560 COLOR 7,0
570 NEXT Z
580 '
590 X=0
600 FOR Z=4 TO 20 STEP 4
610 X=X+1
620 COLOR 7,1
630 Y=F(X,2)   :GOSUB 800:LOCATE Z,11:PRINT USING U$;Y;:PRINT " MHz "
640 Y=F(X+5,2) :GOSUB 800:LOCATE Z,39:PRINT USING U$;Y;:PRINT " MHz "
650 Y=F(X+10,2):GOSUB 800:LOCATE Z,67:PRINT USING U$;Y;:PRINT " MHz "
660 COLOR 7,0
670 NEXT Z
680 '
690 X=0
700 FOR Z=5 TO 21 STEP 4
710 X=X+1
720 COLOR 15,1
730 Y=F(X,3)   :GOSUB 800:LOCATE Z,11:PRINT USING U$;Y;:PRINT " MHz "
740 Y=F(X+5,3) :GOSUB 800:LOCATE Z,39:PRINT USING U$;Y;:PRINT " MHz "
750 Y=F(X+10,3):GOSUB 800:LOCATE Z,67:PRINT USING U$;Y;:PRINT " MHz "
760 COLOR 7,0
770 NEXT Z
780 GOTO 850
790 '
800 U$=               "#####.#"
810 IF Y<10^3 THEN U$="####.##"
820 IF Y<10^2 THEN U$="###.###"
830 RETURN
840 '
850 GOSUB 870:CLS:CHAIN GO$
860 '
870 'PRT
880 KEY OFF:GOSUB 950:LOCATE 25,5:COLOR 0,2
890 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
900 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
910 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 910 ELSE GOSUB 950
920 IF Z$="3"THEN RETURN
930 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
940 IF Z$="2"THEN LPRINT CHR$(12) ELSE 880
950 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
