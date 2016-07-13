1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
10 'THRUBOOM - Yagi Element Lengths - 26 JUN 02
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 SL=2.997925E+02  'speed of light
50 UL$=STRING$(80,205)
60 '
70 '.....title page
80 CLS:B=0:C=0:BD=0:DL=0:NL=0
90 COLOR 15,2,1
100 PRINT " YAGI THRU-BOOM ELEMENT LENGTHS";TAB(57);"By George Murphy VE3ERP ";
110 COLOR 1,0:PRINT STRING$(80,223);
120 COLOR 7,0
130 '
140 '.....title page
150 PRINT " This program calculates new lengths of beam elements if mounted ";
160 PRINT "through, but  "
170 PRINT " insulated from, the boom."
180 PRINT
190 PRINT " Equations by GÅnter Hoch, DL6WU, and Ian White, G3SEK. The calcu";
200 PRINT "lations have  "
210 PRINT " proven to be accurate at 144 - 450 MHz for boom diameters up to ";
220 PRINT "about 0.055   "
230 PRINT " wavelength."
240 PRINT UL$;
250 LN=CSRLIN:T=18:GOSUB 1090
260 COLOR 0,7:PRINT:LOCATE ,23
270 PRINT " Press 1 to continue or 0 to EXIT. ";:COLOR 7,0
280 Z$=INKEY$:IF Z$=""THEN 280
290 IF Z$="0"THEN COLOR ,,0:CLS:CHAIN EX$
300 IF Z$="1"THEN CLS:GOTO 330
310 GOTO 280
320 '
330 '.....start
340 FLAG=0
350 CLS:COLOR ,,0:LOCATE 12
360 INPUT " ENTER: Frequency (144 - 450 MHz).....";F
370 IF F=0 THEN 350
380 IF F<144 THEN F$="below 144 MHz":GOTO 410
390 IF F>450 THEN F$="above 450 MHz":GOTO 410
400 GOTO 450
410 PRINT:BEEP:COLOR 0,7:FLAG=1
420 PRINT " Results may be inaccurate at frequencies ";F$:COLOR 7,0
430 PRINT :PRINT " Press any key . . . "
440 IF INKEY$=""THEN 440
450 CLS
460 W=SL/F*10^3    'wavelength in mm.
470 IF W>1000 THEN X=W/1000:X$="##.## m."ELSE X=W/100:X$="##.# cm.
480 T$="#####.### MHz ("+X$+") YAGI INSULATED THRU-BOOM ELEMENTS"
490 LOCATE,(80-LEN(T$))/2:PRINT USING T$;F;X
500 A=W*5.499999E-02:B=A/2.539999E+01
510 COLOR 0,7:LOCATE ,15
520 PRINT USING " For booms up to about ### mm. (##.# in.) diameter. ";A,B
530 COLOR 7,0
540 PRINT "Do you want measurements in (i)nches or (m)illimetres?   (i/m)"
550 Z$=INKEY$:IF Z$=""THEN 550
560 IF Z$="i"THEN UM$="in.":UM=2.539999E+01:T$="inches":T=29:GOTO 590
570 IF Z$="m"THEN UM$="mm.":UM=1:T$="millimetres":T=23:GOTO 590
580 GOTO 550
590 IF UM=1 THEN X$=" ####.## mm.≥":X=W::ELSE X$="  ###.## in.≥":X=W/2.539999E+01
600 LOCATE 3:PRINT "Wavelength: ";USING X$;X;
610 PRINT STRING$(55,32);
620 VIEW PRINT 4 TO 24:CLS:VIEW PRINT:LOCATE 4
630 PRINT "ENTER: Boom diameter (";UM$;;").......";:INPUT BD
640 IF BD<=A/UM THEN 700
650 BEEP:COLOR 0,7:FLAG=1
660 PRINT " Results may be inaccurate if boom diameter more than";A/UM;UM$
670 COLOR 7,0
680 PRINT " Press any key . . . ."
690 IF INKEY$=""THEN 690
700 LOCATE 2:PRINT STRING$(80,32);
710 X$=" Dimensions in "+T$
720 LOCATE 2,(80-LEN(X$))/2:PRINT X$
730 VIEW PRINT 4 TO 24:CLS:VIEW PRINT:LOCATE 4
740 B=BD*UM/W
750 '
760 C=1.259749E+01*B-1.145000E+02*B^2
770 '
780 LOCATE 3,26
790 PRINT USING " For a ##.### dia. boom lengthen all elements ##.###";BD,BD*C
800 PRINT UL$;
810 LN=CSRLIN
820 LOCATE CSRLIN-1,25:PRINT "œ"
830 IF FLAG=0 THEN 900
840 BEEP:COLOR 12
850 PRINT TAB(4);"WARNING: RESULTS FOR THIS FREQUENCY AND BOOM DIAMETER ";
860 PRINT "MAY NOT BE ACCURATE!";:COLOR 7,0
870 PRINT :PRINT "Press any key . . . ."
880 IF INKEY$=""THEN 880
890 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
900 LOCATE LN:PRINT "Want to calculate new element lengths?    (y/n)"
910 Z$=INKEY$:IF Z$=""THEN 910
920 IF Z$="y"THEN VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN:GOTO 950
930 IF Z$="n"THEN DL=0:GOTO 1010
940 GOTO 910
950 N=0
960 '
970 N=N+1
980 PRINT USING "(##) ";N;
990 PRINT "ENTER: design length of element (";UM$;;") or 0 (zero) to quit...";
1000 INPUT DL
1010 IF DL=0 THEN VIEW PRINT CSRLIN-1 TO 24:CLS:VIEW PRINT:GOTO 1260
1020 NL=DL+C*BD
1030 PRINT NL
1040 LOCATE LN,5
1050 T$="####.### "+UM$+" element becomes ####.### "+UM$+" long"+STRING$(30,32)
1060 PRINT USING T$;DL,NL
1070 LN=CSRLIN:GOTO 970
1080 '
1090 '.....diagram
1100 COLOR 0,7:LOCATE LN,T
1110 LOCATE ,T:PRINT "                     ¬/¬                     "
1120 LOCATE ,T:PRINT "                     ≥ ≥Æƒ element           "
1130 LOCATE ,T:PRINT "                   ⁄ƒ≈ƒ≈ƒø                   "
1140 LOCATE ,T:PRINT "  √ƒƒƒƒƒƒƒƒƒƒƒ ¬ ƒƒ¡¬|ƒ|¬¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥  "
1150 LOCATE ,T:PRINT "  √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ¥| |√ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥  "
1160 LOCATE ,T:PRINT "  |            ≥    ¿≈ƒ≈ŸÆø shouldered    |  "
1170 LOCATE ,T:PRINT "  |  boom dia.ƒ¥     ≥ ≥  √ bushing       |  "
1180 LOCATE ,T:PRINT "  |            ≥    ⁄≈ƒ≈øÆŸ insulators    |  "
1190 LOCATE ,T:PRINT "  √ƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ¥| |√ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥  "
1200 LOCATE ,T:PRINT "  √ƒƒƒƒƒƒƒƒƒƒƒ ¡ ƒƒ¬¡|ƒ|¡¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥  "
1210 LOCATE ,T:PRINT "                   ¿ƒ≈ƒ≈ƒŸ                   "
1220 LOCATE ,T:PRINT "                     ≥ ≥                     "
1230 LOCATE ,T:PRINT "                     ¡/¡                     "
1240 COLOR 7,0
1250 RETURN
1260 '.....end
1270 GOSUB 1290:GOTO 70
1280 '
1290 '.....PRT
1300 KEY OFF:GOSUB 1370:LOCATE 25,5:COLOR 0,2
1310 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1320 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1330 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1330 ELSE GOSUB 1370
1340 IF Z$="3"THEN RETURN
1350 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1360 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1300
1370 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
