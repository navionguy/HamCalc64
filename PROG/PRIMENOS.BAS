1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'PRIMENOS - 23 NOV 85 rev. 29 SEP 2005
20 IF EX$=""THEN EX$="EXIT"
30 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
40 
50 CLS:KEY OFF
60 COLOR 7,0,1
70 UL$=STRING$(80,205)
80 DIM PN(20)
90 '
100 '.....start
110 CLS
120 COLOR 15,2
130 PRINT " PRIME NUMBERS";TAB(57);;"by George Murphy VE3ERP ";
140 COLOR 1,0:PRINT STRING$(80,223);
150 COLOR 7,0
160 PRINT TAB(9);
170 PRINT "A prime number is a number evenly divisible only by itself and"
180 PRINT TAB(9);
190 PRINT "the number 1, and not by any other integer (whole number)."
200 PRINT UL$;
210 PRINT " Press number in < > to:"
220 PRINT UL$;
230 PRINT "   < 1 >  Display all prime numbers between any two integers"
240 PRINT "   < 2 >  Find prime factors of a number"
250 PRINT
260 PRINT "   < 0 >  EXIT"
270 Z$=INKEY$
280 IF Z$="0"THEN CLS:CHAIN GO$
290 IF Z$="1"THEN 330
300 IF Z$="2"THEN 650
310 GOTO 270
320 '
330 '.....prime numbers
340 CLS:COLOR ,,0
350 INPUT " ENTER: From first number...";N:N=INT(N)
360 INPUT " ENTER:   To last number....";L:L=INT(L)
370 IF L<N THEN SWAP L,N
380 NN=N
390 IF INT(N/2)=N/2 THEN N=N+1
400 CLS
410 PRINT " Prime numbers from";NN;;"to";L
420 PRINT UL$;
430 FLAG=16
440 IF NN>2 THEN 470
450 FOR N=1 TO 3:GOSUB 550:NEXT N:N=5
460 '
470 V=SQR(N):Y=3
480 X=N/Y:IF X=1 THEN GOSUB 550
490 IF X=INT(X)THEN 520
500 IF Y<=V THEN Y=Y+2:GOTO 480
510 GOSUB 550
520 N=N+2:IF N<=L THEN 470
530 GOSUB 610:END
540 '
550 FLAG=FLAG+1:PRINT USING " ######## ";N;
560 IF FLAG<192 THEN 590
570 GOSUB 840
580 FLAG=0:CLS
590 RETURN
600 '
610 '.....return to menu
620 GOSUB 840
630 GOTO 100   'start
640 '
650 '.....prime factors
660 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 2,23:COLOR 15,2
670 PRINT " (Algorithm by Erik Madsen OZ8EM) ";:COLOR 7,0
680 PRINT
690 INPUT " ENTER: Number to be resolved in primes";P
700 A=2:B=1:T=P:X=0
710 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 3:COLOR ,,0
720 PRINT " Analyzing";P;;"....please wait...."
730 FOR N=A TO T STEP B
750 IF T/N=INT(T/N)THEN X=X+1:PN(X)=N:T=T/N:GOTO 730
760 A=3:B=2
770 NEXT N
780 VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 4
790 PRINT " Prime factors of";P;;"= 1 ";
800 FOR Z=1 TO X:PRINT "x";PN(Z);
810 NEXT Z:PRINT ""
820 GOSUB 840:GOTO 100
830 '
840 'PRT
850 KEY OFF:GOSUB 920:LOCATE 25,5:COLOR 0,2
860 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
870 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
880 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 880 ELSE GOSUB 920
890 IF Z$="3"THEN RETURN
900 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
910 IF Z$="2"THEN LPRINT CHR$(12) ELSE 850
920 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
