1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'BATCHG - battery charger - 27 APR 07
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 '.....title page
70 CLS
80 COLOR 15,2,1
90 PRINT " BATTERY CHARGER";TAB(57);"by George Murphy VE3ERP ";
100 COLOR 1,0:PRINT STRING$(80,223);
110 COLOR 15,2:LOCATE CSRLIN-1,25:PRINT " Algorithm by R.J.Dehoney IEEE "
120 GOSUB 720:COLOR 7,0,0
130 PRINT:LN = CSRLIN
140 LOCATE 3,61:PRINT "D1=1N5817 Schottky
150 LOCATE 4,61:PRINT "Q1=IRF 511 MOSFET"
160 LOCATE 5,61:PRINT "IC=Maxim 641 ACPA"
170 LOCATE 6,61:PRINT "C1=220 ФF"
180 LOCATE 7,61:PRINT "C2=100 ФF"
190 LOCATE 8,61:PRINT "C3=330 ФF"
200 LOCATE 9,61:PRINT "R1=10 ohm"
210 LOCATE 10,61:PRINT "R1=2400 ohm"
220 LOCATE 11,61:PRINT "R3=220 ohm"
230 LOCATE LN
240 PRINT " This is a step-up switching circuit that transfers power from dr";
250 PRINT "y cells to"
260 PRINT " any rechargeable storage battery."
270 PRINT :PRINT " .....Press 1 to continue or 0 to exit......"
280 Z$=INKEY$:IF Z$=""THEN 280
290 IF Z$="0"THEN CLS:RUN GO$
300 IF Z$="1"THEN 330
310 GOTO 280
320 STOP
330 '.....start
340 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
350 INPUT "Enter maximum DC input voltage ",VMAX
360 INPUT "Enter minimum DC input voltage ",VMIN
370 INPUT "Enter output voltage ",VOUT
380 TP=2.789999E+01-7.799999E-01*VOUT :R=6.999999E-01 :TON=TP/2 :VD=6.000000E-01:IMIN=0:IOUT=0
390 VDC=VMIN :L=200 :X=TON*R/L :GOSUB 540 :IMIN=IOUT
400 X$="Enter maximum desired charging current, greater than "
410 PRINT X$;:PRINT USING "##.### amps";IMIN;:INPUT IX
420 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
430 FOR L=200 TO 4 STEP -1 :X=TON*R/L :VDC=VMAX :GOSUB 540
440 IF IX>IOUT THEN 445 else goto 450
445 next L
450 IF L>3 THEN 510
460 COLOR 12,0:PRINT " Cannot obtain";IX;"amps with this DC INPUT.":COLOR 7,0
470 PRINT USING " Maximum current with these voltages is ##.### amps";IOUT;
480 PRINT " with L=3ФH."
490 PRINT :PRINT " .....press any key....."
500 Z$=INKEY$:IF Z$=""THEN 500 ELSE 330
510 L0=L :IPKMAX=IPK :IOUTMAX=IOUT :IAVEMAX=IAVE
520 L=L0 :VDC=VMIN :GOSUB 540
530 IPKMIN=IPK :IOUTMIN=IOUT :IAVEMIN=IAVE :GOTO 590
540 '----------subroutine to find values--------
550 IM=VDC/R :IPK=IM*(1-EXP(-X))
560 TDIS=L*IPK/(VOUT+VD-VDC)
570 IAVE=IM-IPK/X
580 IOUT=IPK/2*TDIS/TP-7.000000E-03 :RETURN
590 '-----------subroutine to print values------------
600 LOCATE 13,61:PRINT "Vin=";VMIN;"volts
610 LOCATE 14,61:PRINT "  to";VMAX;"volts
620 LOCATE 15,61:PRINT "Vout=";VOUT;"volts"
630 LOCATE 12,61:PRINT "L1=";L;"ФH
640 LOCATE LN
650 PRINT USING " Minimum Peak Input Current....... ##.### amps";IPKMIN
660 PRINT USING " Maximum Peak Input Current....... ##.### amps";IPKMAX
670 PRINT USING " Minimum Average Input Current.... ##.### amps";IAVEMIN
680 PRINT USING " Maximum Average Input Current.... ##.### amps";IAVEMAX
690 PRINT USING " Minimum Average Output Current... ##.### amps";IOUTMIN
700 PRINT USING " Maximum Average Output Current... ##.### amps";IOUTMAX
710 GOTO 900
720 '.....Diagram
730 COLOR 0,7
740 PRINT "                             D1                           "
750 PRINT " + *дддддбдддддL1ддбддддддбдд>Ёддбддддддбддддд©           "
760 PRINT "         Ё         Ё      Ё      Ё      Ё     Ё           "
770 PRINT "         Ё         юд╢    Ё      Ё      Ё     Ё           "
780 PRINT "         Ё        Q1 Ёц©  R1     Ё      Ё     цддддд* +   "
790 PRINT "         Ё        здд╢ Ё  Ё      Ё      Ё     Ё           "
800 PRINT "         Ё+       Ё    Ё  Ё  здддадд©+  R2    Ё+          "
810 PRINT "  V in   C1       Ё  зд6дд4дд5д©   C2   Ё     C3   V out  "
820 PRINT "         Ё        Ё  Ё         Ё    Ё   Ё     Ё           "
830 PRINT "         Ё        Ё  Ё    IC   Ё    Ё   Ё     Ё           "
840 PRINT "         Ё        Ё  Ё         7ддддЁддд╢     цддддд* -   "
850 PRINT "         Ё        Ё  юдддд3дддды    Ё   R3    Ё           "
860 PRINT "         Ё        Ё       Ё         Ё   Ё     Ё           "
870 PRINT " - *дддддаддддддддадддддддадддддддддадддаддддды           "
880 COLOR 7,0
890 RETURN
900 '.....end
910 GOSUB 920:GOTO 60
920 '.....PRT
930 KEY OFF:GOSUB 1000:LOCATE 25,5:COLOR 0,2
940 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
950 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
960 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 960 ELSE GOSUB 1000
970 IF Z$="3"THEN RETURN
980 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
990 IF Z$="2"THEN LPRINT CHR$(12) ELSE 930
1000 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
