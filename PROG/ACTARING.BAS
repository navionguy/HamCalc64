1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'OCTARING - Octagonal Loop Antenna - 23 NOV 2005
15 
20 CLS:KEY OFF
30 IF EX$=""THEN EX$="EXIT"
40 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
50 '
60 PI=4*ATN(1)  '3.141593
70 UL$=STRING$(80,205)
80 U1$="#####.###"
90 '
100 '.....start
110 CLS
120 COLOR 15,2,1
130 PRINT " OCTARING Miniature Octagonal Loop Antenna";
140 PRINT TAB(57);"by George Murphy VE3ERP ";
150 COLOR 1,0:PRINT STRING$(80,223);
160 COLOR 7,0
170 PRINT " The OctaRing is a version of the W2AHW Octaloop antenna. The Octa";
180 PRINT "loop program "
190 PRINT " contains some desigm data supplemental to that contained in the O";
200 PRINT "ctaRing      "
210 PRINT " program."
220 PRINT
230 PRINT " Press 1 to run OctaRing"
240 PRINT " Press 2 to run Octaloop (Contains more data also applicable to ";
250 PRINT " OctaRing)"
260 PRINT " Press 3 for design data applying to ALL small transmitting loops"
270 PRINT "press 4 for coaxial cable properties"
280 PRINT " Press 0 to EXIT"
290 Z$=INKEY$:IF Z$=""THEN 290
300 IF Z$="0"THEN COLOR 7,0,0:CLS:RUN EX$
310 IF Z$="1"THEN LN=2:GOSUB 370:GOSUB 990:GOSUB 870:GOTO  410
320 IF Z$="2"THEN CLS:CHAIN "octaloop"
330 IF Z$="3"THEN CHAIN"loopxmit"
340 IF Z$="4"THEN CHAIN"coaxchar"
350 GOTO 290
360 '
370 '.....erase previous line
380 VIEW PRINT LN TO 24:CLS:VIEW PRINT:LOCATE LN
390 RETURN
400 '
410 '.....enter data
420 INPUT " ENTER: Frequency (MHz)";F
430 LN=CSRLIN-1:GOSUB 370
440 PRINT USING " Frequency................. ###.### MHz.";F
450 COLOR 0,7
460 PRINT "``The larger the braid diameter the more efficient the antenna!''"
470 PRINT "...........Measure the O.D.of the cable you want to use.........."
480 COLOR 7,0
490 INPUT " ENTER: Outside diameter of braid (decimal inches)";DIA
500 LN=CSRLIN-3:GOSUB 370
510 PRINT USING " Diameter of braid......... ###.### in.";DIA
520 DCM=DIA*2.539999
530 CPF=2.460000E+01 'capacity per foot
540 INPUT " ENTER: Capacity per foot (from published tables) in pF";CPF
550 LN=CSRLIN-1:GOSUB 370
560 PRINT USING " Capacity per foot......... ###.### pF";CPF
570 CPI=CPF/12 'capacity per inch
580 B=0 'length in inches
590 '
600 '....iteration
610 B=B+1.000000E-03         'cable length in inches
620 CCAB=CPI/12*B*8 'capacity of cable
630 CLOOP=CCAB*8    'capacity of loop
640 X=10^6/(2*PI*F*CLOOP) 'reactance of loop
650 BCM=B*2.539999 'length of side in cm.
660 E1=1.600000E-02*BCM
670 E2=2.612999*BCM/2^DCM
680 E3=7.152999E-02*2^DCM/BCM
690 LUH=E1*(LOG(E2)+7.514299E-01+E3) 'inductance of loop
700 CRES=2.533028E+04/F^2/LUH 'capacity required to resonate
710 XL=10^6/(2*PI*F*CRES) 'reactance
720 IF SGN(X-XL)=-1 THEN 740
730 GOTO 610
740 P=B*8  'perimeter of loop
750 A=B/TAN(30*PI/180) : C=B/SIN(30*PI/180)
760 PRINT USING " Perimeter of loop........ ####.### in.";P
770 PRINT USING " Each side (dim. B)....... ####.### in.";B
780 PRINT USING " Side to side (dim. C).... ####.### in.";A
790 PRINT USING " Corner to corner......... ####.### in.";C
800 PRINT USING " Capacity of loop......... ####.### pF";CLOOP
810 PRINT USING " Inductance of loop....... ####.### ๊";LUH
820 PRINT USING " Reactance of loop........ ####.### ๆH";XL;
830 LOCATE 16,45:PRINT "Thread the cable through the 4 tee"
840 LOCATE 17,45:PRINT "heads of the PVC pipe frame       "
850 GOSUB 1250:GOTO 100
860 '
870 '.....prologue
880 PRINT " The OctaRing is a variation of Harold Kane's W2AHW miniature Oct";
890 PRINT "aloop antenna "
900 PRINT " which consists of a loop of coaxial cable whose braid is the rad";
910 PRINT "iating element"
920 PRINT " and the center conductor is left unconnected. It has a non-radia";
930 PRINT "ting cable    "
940 PRINT " `capacitor' tuning stub hanging from the loop at the gap (2). Th";
950 PRINT "e OctaRing is "
960 PRINT " the same but without the tuning stub."
970 RETURN
980 '
990 '.....diagram
1000 T=5
1010 COLOR 0,7
1020 LOCATE ,T:PRINT "    ษออออ(2)ออออป                                  "
1030 LOCATE ,T:PRINT "    บ           บ    1ฏ Coaxial cable loop         "
1040 LOCATE ,T:PRINT "  1ฏบ           บ    (2) 1 inch gap                "
1050 LOCATE ,T:PRINT "    บ           บ                                  "
1060 LOCATE ,T:PRINT "    บ  ณฎ-B-ฏณ  บ    B = 1/8 of loop perimeter     "
1070 LOCATE ,T:PRINT "    ศออัอออออัออผ                                  "
1080 LOCATE ,T:PRINT "       ภฤฟ ฺฤูฎฤDelta match to 50 ๊ coax feedline  "
1090 COLOR 0,3
1100 LOCATE ,T:PRINT "(4-sided loop shown, but octagonal loop described.)"
1110 COLOR 0,7
1120 LOCATE 2:T=58
1130 LOCATE ,T:PRINT "      อออหอออ      "
1140 LOCATE ,T:PRINT "         บ         "
1150 LOCATE ,T:PRINT " บ       บ       บ "
1160 LOCATE ,T:PRINT " ฬอออออออฮอออออออน "
1170 LOCATE ,T:PRINT " บ       บ       บ "
1180 LOCATE ,T:PRINT "         บ         "
1190 LOCATE ,T:PRINT "      อออฮอออ      "
1200 COLOR 0,3
1210 LOCATE ,T:PRINT "  PVC Pipe frame   "
1220 COLOR 7,0
1230 RETURN
1240 '
1250 '.....PRT
1260 KEY OFF:GOSUB 1330:LOCATE 25,5:COLOR 0,2
1270 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
1280 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
1290 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 1290 ELSE GOSUB 1330
1300 IF Z$="3"THEN RETURN
1310 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
1320 IF Z$="2"THEN LPRINT CHR$(12) ELSE 1260
1330 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
