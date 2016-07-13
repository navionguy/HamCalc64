1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$,PROG$
10 'COMFILT - 26 JAN 2010
20 'COMFILc.BAS rev 26 JAN 2010
30 CLS:KEY OFF
40 IF EX$=""THEN EX$="EXIT"
50 IF PROG$=""THEN GO$=EX$ ELSE GO$=PROG$
60 
70 '.....title page
80 COLOR 15,2
90 PRINT " COMPLEMENTARY FILTERS";TAB(61);"by R.J.Dehoney IEEE ";
100 COLOR 7,0
110 '.....start
120 PRINT
130 '---------------CONIMP2.BAS,rjd,1/22/10--------------
140 PRINT "Most filters obtain their insertion loss by reflecting the incoming energy back";
150 PRINT "to the source, thus their impedance match in the pass band is very good and";
160 PRINT "their VSWR in the reject band is very high. Some sources are ill behaved when";
170 PRINT "improperly loaded. This program gives element values for a filter with a";
180 PRINT "Butterworth characteristic that maintains a low input VSWR over a very wide";
190 PRINT "band. It consists of a low pass filter in parallel with a high pass filter,";
200 PRINT "with element values adjusted to maintain match. At low frequencies, the input";
210 PRINT "sees the desired load; at high frequencies, the input sees the intermal load.";:PRINT
220 PRINT "The program was taken from CP Hearn's RF Design, 9/95 article-Design";
230 PRINT "Parameters for 2 Thru 7 Pole Input Matched Lowpass/Highpass Filters."
240 PRINT "It uses his tabulated prototype values scaled to the desired F0 & Z0.";
250 MHZ=10^6:UH=9.999999E-07 :PF=9.999999E-13 :TP=8*ATN(1) :GOSUB 680 :PRINT
260 PRINT "Press <1> to run or <0> to EXIT.";
270 Z$=INKEY$:IF Z$=""THEN 270
280 IF Z$="0"THEN RUN GO$
290 IF Z$="1"THEN LOCATE CSRLIN-1:GOTO 310
300 GOTO 270
310 INPUT "Enter passband 3dB cutoff frequency, MHz ",F0 :FCO=F0*MHZ
320 INPUT "Enter desired Z0, ohms ",Z0
330 INPUT "Enter number of poles (2 to 7), or press <ENTER> to calculate N ",N
340 IF N=0 THEN 370
350 IF N<2 OR N>7 THEN 330
360 IF N>1 AND N<8 THEN 430
370 INPUT "Enter desired loss in dB for a frequency in the reject band ";LDB
380 INPUT "Enter reject band frequency in MHz";FR:K=FR/F0
390 KF=FR/F0
400 N=CINT(5.000000E-01*LOG(10^(LDB/10)-1)/LOG(KF))
410 PRINT "Number of poles=";N;";
420 INPUT "OK? ENTER:(y/n) ",A$ :IF A$="n" THEN 330
430 IF FR=0 THEN X=1+2^(2*N) :LDB=10*LOG(X)/LOG(10) :FR=2*F0
440 FOR P=1 TO N
450 IF N=2 THEN GL(P)=GL2(P) :GH(P)=GH2(P)
460 IF N=3 THEN GL(P)=GL3(P) :GH(P)=GH3(P)
470 IF N=4 THEN GL(P)=GL4(P) :GH(P)=GH4(P)
480 IF N=5 THEN GL(P)=GL5(P) :GH(P)=GH5(P)
490 IF N=6 THEN GL(P)=GL6(P) :GH(P)=GH6(P)
500 IF N=7 THEN GL(P)=GL7(P) :GH(P)=GH7(P)
510 NEXT P
520 '.....display
530 CLS
540 LOCATE 2,30:PRINT "COMPLEMENTARY FILTERS"
550 LOCATE 4
560 PRINT "     Series L        Shunt C           Shunt L        Series C";
570 FOR P=1 TO N STEP 2 :LL(P)=GL(P)*Z0/TP/FCO :CL(P+1)=GL(P+1)/TP/FCO/Z0
580 CH(P)=GH(P)/TP/FCO/Z0 :LH(P+1)=GH(P+1)*Z0/TP/FCO
590 PRINT USING " LL(#)###.###uH  CL(#)####.###pF      ";P;LL(P)/UH;P+1;CL(P+1)/PF;
600 PRINT USING " LH(#)###.###uH  CH(#)####.###pF";P+1;LH(P+1)/UH;P;CH(P)/PF
610 NEXT P :ON N-1 GOSUB 930,1030,1130,1230,1330,1430
620 PRINT USING " Passband 3 dB cutoff frequency....... ####.### MHz";FCO/MHZ
630 PRINT USING " Desired Z0........................... ####.### Й";Z0
640 PRINT USING " Number of poles...................... ####";N
650 PRINT USING " Reject band frequency................ ####.### Mhz";FR
660 PRINT USING " Loss in dB at reject band frequency.. ####.# dB";LDB
670 FR=0 :GOTO 820
680 '-----------subroutine to tabulate values-----------
690 GL2(1)=1.412999 :GL2(2)=7.063000E-01
700 GL3(1)=1.498999 :GL3(2)=1.332999 :GL3(3)=5.000000E-01
710 GL4(1)=1.531000 :GL4(2)=1.577999 :GL4(3)=1.085000 :GL4(4)=3.840999E-01
720 GL5(1)=1.542999 :GL5(2)=1.692000 :GL5(3)=1.383000 :GL5(4)=9.013000E-01 :GL5(5)=3.152000E-01
730 GL6(1)=1.552999 :GL6(2)=1.759999 :GL6(3)=1.554999 :GL6(4)=1.202000 :GL6(5)=7.548999E-01            :GL6(6)=2.561999E-01
740 GL7(1)=1.555999 :GL7(2)=1.799999 :GL7(3)=1.661999 :GL7(4)=1.398000 :GL7(5)=1.052000             :GL7(6)=6.366000E-01 :GL7(7)=2.239000E-01
750 GH2(1)=7.063000E-01 :GH2(2)=1.412999
760 GH3(1)=6.661000E-01 :GH3(2)=7.493000E-01 :GH3(3)=1.998000
770 GH4(1)=6.531999E-01 :GH4(2)=6.334999E-01 :GH4(3)=9.218999E-01 :GH4(4)=2.602999
780 GH5(1)=6.463999E-01 :GH5(2)=5.899000E-01 :GH5(3)=7.245000E-01 :GH5(4)=1.123000 :GH5(5)=3.253000
790 GH6(1)=6.437000E-01 :GH6(2)=5.674999E-01 :GH6(3)=6.407999E-01 :GH6(4)=8.206999E-01 :GH6(5)=1.279000            :GH6(6)=3.674999
800 GH7(1)=6.412000E-01 :GH7(2)=5.540000E-01 :GH7(3)=5.975999E-01 :GH7(4)=7.028999E-01 :GH7(5)=9.273999E-01           :GH7(6)=1.524999 :GH7(7)=4.698999
810 RETURN
820 '....end
830 GOSUB 840:CLS:GOTO 110
840 '.....PRT
850 KEY OFF:GOSUB 920:LOCATE 25,5:COLOR 0,2
860 PRINT " Send this page to:(1)Printer Queue? (2)Printout? ";
870 PRINT "(3)Next page? (1/2/3)";:COLOR 7,0
880 Z$=INKEY$:IF Z$<"1"OR Z$>"3"THEN 880 ELSE GOSUB 920
890 IF Z$="3"THEN RETURN
900 FOR I%=1 TO 24:FOR J%=1 TO 80:LPRINT CHR$(SCREEN(I%,J%));:NEXT J%:NEXT I%
910 IF Z$="2"THEN LPRINT CHR$(12) ELSE 850
920 LOCATE 25,1:PRINT STRING$(80,32);:RETURN
930 '--------2 pole schematic------
940 PRINT ";
950 PRINT ";
960 PRINT "              здддбдCH1дбдLL1дбдддд©╝ддд©";
970 PRINT "              Ё   Ё     Z0    Ё    Ё    Ё";
980 PRINT "            R=Z0 LH2    Ё    CL2 RL=Z0 Vout";
990 PRINT "              Ё   Ё    Vin    Ё    Ё    Ё";
1000 PRINT "              юдддадддддадддддадддды╝ддды";
1010 PRINT ";
1020 RETURN
1030 '--------3 pole schematic------
1040 PRINT ";
1050 PRINT ";
1060 PRINT "            зддCH3дбдCH1дбдLL1дбдLL3дд©╝ддд©";
1070 PRINT "            Ё      Ё     Z0    Ё      Ё    Ё";
1080 PRINT "          R=Z0    LH2    Ё    CL2   RL=Z0 Vout";
1090 PRINT "            Ё      Ё    Vin    Ё      Ё    Ё";
1100 PRINT "            юддддддадддддадддддадддддды╝ддды";
1110 PRINT ";
1120 RETURN
1130 '--------4 pole schematic------
1140 PRINT ";
1150 PRINT ";
1160 PRINT "         здддбдCH3дбдCH1дбдLL1дбдLL3дбдддд©╝ддд©";
1170 PRINT "         Ё   Ё     Ё     Z0    Ё     Ё    Ё    Ё";
1180 PRINT "       R=Z0 LH4   LH2    Ё    CL2   CL4 RL=Z0 Vout";
1190 PRINT "         Ё   Ё     Ё    Vin    Ё     Ё    Ё    Ё";
1200 PRINT "         юдддадддддадддддадддддадддддадддды╝ддды";
1210 PRINT ";
1220 RETURN
1230 '--------5 pole schematic------
1240 PRINT ";
1250 PRINT ";
1260 PRINT "        здCH5дбдCH3дбдCH1дбдLL1дбдLL3дбдLL5д©╝ддд©";
1270 PRINT "        Ё     Ё     Ё     Z0    Ё     Ё     Ё    Ё";
1280 PRINT "      R=Z0   LH4   LH2    Ё    CL2   CL4  RL=Z0 Vout";
1290 PRINT "        Ё     Ё     Ё    Vin    Ё     Ё     Ё    Ё";
1300 PRINT "        юдддддадддддадддддадддддадддддаддддды╝ддды";
1310 PRINT ";
1320 RETURN
1330 '--------6 pole schematic------
1340 PRINT ";
1350 PRINT ";
1360 PRINT "     здддбдCH5дбдCH3дбдCH1дбдLL1дбдLL3дбдLL5дбдддд©╝ддд©";
1370 PRINT "     Ё   Ё     Ё     Ё     Z0    Ё     Ё     Ё    Ё    Ё";
1380 PRINT "   R=Z0 LH6   LH4   LH2    Ё    CL2   CL4   CL6 RL=Z0 Vout";
1390 PRINT "     Ё   Ё     Ё     Ё    Vin    Ё     Ё     Ё    Ё    Ё";
1400 PRINT "     юдддадддддадддддадддддадддддадддддадддддадддды╝ддды";
1410 PRINT ";
1420 RETURN
1430 '--------7 pole schematic------
1440 PRINT ";
1450 PRINT ";
1460 PRINT "    здCH7дбдCH5дбдCH3дбдCH1дбдLL1дбдLL3дбдLL5дбдLL7д©╝ддд©";
1470 PRINT "    Ё     Ё     Ё     Ё     Z0    Ё     Ё     Ё     Ё    Ё";
1480 PRINT "  R=Z0   LH6   LH4   LH2    Ё    CL2   CL4   CL6  RL=Z0 Vout";
1490 PRINT "    Ё     Ё     Ё     Ё    Vin    Ё     Ё     Ё     Ё    Ё";
1500 PRINT "    юдддддадддддадддддадддддадддддадддддадддддаддддды╝ддды";
1510 PRINT ";
1520 RETURN
