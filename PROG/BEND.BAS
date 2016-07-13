1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'BEND - Bend Allowance - 19 FEB 2001 rev. 12 MAR 01,  04 JAN 08
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 
60 COLOR 7,0,0
70 PI=4*ATN(1)  '3.141593
80 UL$=STRING$(80,205)
90 '
100 '.....title page
110 CLS
120 COLOR 15,2
130 PRINT " BEND ALLOWANCE, Metal";TAB(57);"By George Murphy VE3ERP ";
140 COLOR 1,0:PRINT STRING$(80,223);
150 COLOR 7,0
160 COLOR 0,7:L=18
170 LOCATE ,L:PRINT "     leg         leg                        "
180 LOCATE ,L:PRINT " ³®ÄÄ A Ä¯³®C¯³®Ä B Ä¯³     Ûßßßßßßß A      "
190 LOCATE ,L:PRINT " ÚÄÄÄÄÄÄÄÄÂÄÂÄÂÄÄÄÄÄÄÄ¿     Û L             "
200 LOCATE ,L:PRINT " ³    bendÄ¯| |       ³     Û               "
210 LOCATE ,L:PRINT " ÀÄÄÄÄÄÄÄÄÁÄÁÄÁÄÄÄÄÄÄÄÙ     B               "
220 LOCATE ,L:PRINT "  BLANK BEFORE BENDING    ¯³T³®             "
230 LOCATE ,L:PRINT " T = Thickness of material                  "
240 LOCATE ,L:PRINT " R = Bending radius to inside face of blank "
250 LOCATE ,L:PRINT " L = Included angle of bend                 "
260 LOCATE ,L:PRINT " C = Bend allowance                         "
270 COLOR 7,0
280 PRINT UL$;
290 LN=CSRLIN
300 '
310 PRINT " In bending metal the problem is to find the length of straight s";
320 PRINT "tock required "
330 PRINT " for each bend; then these lengths are added to the lengths of th";
340 PRINT "e straight    "
350 PRINT " sections to obtain the total length of the material before bendi";
360 PRINT "ng.           "
370 PRINT "         (Machinery's Handbook, revised 21st edition, page 1928)."
380 PRINT " Bend allowance for pipe or tubing = radius to centreline of the ";
390 PRINT "material x    "
400 PRINT " included angle in degrees x 0.01745.                            ";
410 PRINT "         (Machinery's Handbook, revised 21st edition, page 2335)."
420 PRINT UL$;
430 PRINT " Press number in ( ) to:  (1) Run program with dimensions in ";
440 PRINT "millimetres"
450 PRINT "                          (2) Run program with dimensions in inches"
460 PRINT "                          (0) EXIT";
470 Z$=INKEY$:IF Z$=""THEN 470
480 IF Z$="0"THEN CLS:CHAIN EX$
490 IF Z$="1"THEN M=2.539999E+01:M$="mm":GOTO 520
500 IF Z$="2"THEN M=1   :M$="in.":GOTO 520
510 GOTO 470
520 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
530 PRINT " ENTER: Material thickness (";M$;")....................T=";:INPUT Z
550 T=Z/M
560 PRINT " ENTER: Inside radius to face of material (";M$;").....R=";:INPUT Z
580 R=Z/M
590 INPUT " ENTER: Included angle of bend (degrees)............L=";L
600 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
610 M=2.539999E+01
620 PRINT USING " Material thickness.......T= ###.# mm = ##.### in.";T*M,T
630 PRINT USING " Inside radius of bend....R= ###.# mm = ##.### in.";R*M,R
640 PRINT USING " Included angle...........L= ###.# degrees";L
650 K=PI/2*R
660 C1=5.500000E-01*T+K
670 C2=6.399999E-01*T+K
680 C3=7.099999E-01*T+K
690 PRINT " Bending allowance C:
700 M1$="  For brass & soft copper................................"
710 M2$="  For half-hard copper & brass, soft steel, & aluminum..."
720 M3$="  For bronze, hard copper, cold rolled & spring steel...."
730 PRINT M1$;:PRINT USING " ###.# mm = ##.### in.";C1*M,C1
740 PRINT M2$;:PRINT USING " ###.# mm = ##.### in.";C2*M,C2
750 PRINT M3$;:PRINT USING " ###.# mm = ##.### in.";C3*M,C3
760 GOSUB 790
770 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN:GOTO 310
780 '
790 'PRT
800 KEY OFF:GOSUB 870:LOCATE 25,5:COLOR 0,2
810 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
820 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
830 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 830 ELSE GOSUB 870
840 IF Z$="3"THEN RETURN
850 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
860 IF Z$="2"THEN LPRINT CHR$(12) ELSE 800
870 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
