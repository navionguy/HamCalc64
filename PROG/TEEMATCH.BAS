1 COMMON A, AL, B, BASEONLY, C, C$, C1, CAT$, CC, CKT$, D, D$, DD, DIA, DIMN$, DMS, E, EO, EX$, F, F$, FD, FF, FQ, FRQ, G$, GO$, I, I$, L, LATLONG, LD, LL, LN, LS, LW, LX, MAX, MENU, MIN, MX, N, NN, NT, OV, P, PI, PROG$, Q, QQ, QU, R, RA, RC, T, T$, U, U$, UH, UL$, V$, VC, W, WHIP, WIRD, WW, X, X$, X1, XS, Z$, ZP, ZS
5 'OMMON EX$
10 'TEEMATCH - HIGH PASS TEE MATCH with finite Q inductor, by Brian Eagan ZL1LE
20 '(TEE.BAS by ZL1LE edited for HAMCALC by George Murphy VE3ERP - 07 SEP 97)
30 '.....rev. 14 SEP 97
40 'Formulae derived from original material by Kevin Schmidt in QEX, July 1997
50 
60 IF EX$=""THEN EX$="EXIT"
70 CLS:COLOR 7,0,1:KEY OFF:PI=3.141592
80 UL$=STRING$(80,205)
90 J=20
100 PRINT " HIGH-PASS TEE TRANSMATCH (with finite Q inductor)"
110 PRINT UL$;
120 GOSUB 1140  'diagram
130 '
140 '.....input parameters
150 INPUT " ENTER: Input impedance in ohms......";R1
160 INPUT " ENTER: Load resistance in ohms......";RL
170 INPUT " ENTER: Load reactance in ohms.......";XL
180 INPUT " ENTER: Frequency in MHz.............";FREQ
190 PRINT UL$;
200 PRINT " Is there a limit to the size of capacitor C1 (y/n)?"
210 Y$=INKEY$:IF Y$="" THEN 210
220 IF Y$="y"THEN 230 ELSE GOTO 240
230 INPUT " ENTER: Limiting value of C1 in pF...";C1LIM:GOTO 250
240 C1LIM=1000
250 F=FREQ*10^6:W=2*PI*F
260 IF RL>R1 THEN GOSUB 1540 ELSE GOSUB 1610
270 J=20:VIEW PRINT 11 TO 24:CLS:VIEW PRINT:LOCATE 11
280 PRINT "   C1max = ";:PRINT USING "####.# pF";C1MAX;:PRINT
290 INPUT "   ENTER: Value of C1 in pF .....";C1
300 IF C1 >C1MAX THEN PRINT "   VALUE selected for C1 exceeds C1max!";:GOTO 290
310 X1=-1/W/C1/9.999999E-13:XT=(R1*R1+X1*X1)/X1:RT=(R1*R1+X1*X1)/R1
320 A=RL/R1*(R1*R1+X1*X1-R1*RL):NUM=RL*RL+A:RP=NUM/RL:XP=NUM/-SQR(A)
330 WL=-1/XT-1/XP:WL=1/WL:L=WL/W:X2=-XL-SQR(A):C2=-1/W/X2*9.999999E+11
340 J=20:K=1:VIEW PRINT 3 TO 24:CLS:VIEW PRINT:LOCATE 2
350 LOCATE ,20:PRINT "�";
360 LOCATE ,33:PRINT "�";
370 LOCATE ,46:PRINT "�";
380 LOCATE ,59:PRINT "�"
390 U$="####.# �":V$="####.### MHz "
400 PRINT " DESIGN PARAMETERS:� R1=";USING U$;R1;
410 PRINT " RL=";USING U$;RL;
420 PRINT " XL=";USING U$;XL;
430 PRINT " Freq.=";USING V$;FREQ
440 PRINT UL$;:LOCATE CSRLIN-1
450 LOCATE ,20:PRINT "�";
460 LOCATE ,33:PRINT "�";
470 LOCATE ,46:PRINT "�";
480 LOCATE ,59:PRINT "�"
490 L1=L*10^6:EFF=100:LOSS=0
500 PRINT TAB(23);"TABULATION OF TRANSMATCH PARAMETERS "
510 D1$="����������":D2$= "��������Ŀ"
520 PRINT TAB(10);"�";:FOR N=1 TO 5:PRINT D1$;:NEXT N:PRINT D2$
530 D3$="�    Q    �  L(�H)  �  C1(pF) �  C2(pF) �Loss (dB)� Eff.(%) �
540 PRINT TAB(10); D3$
550 D4$="����������":D5$= "��������Ĵ"
560 PRINT TAB(10);"�";:FOR N=1 TO 5:PRINT D4$;:NEXT N:PRINT D5$
570 PRINT TAB(10); "�Infinite � ";:Y=CSRLIN:X=POS(0):GOSUB 1300
580 N=0:GOSUB 1410
590 D6$="����������":D7$= "����������"
600 PRINT TAB(10);"�";:FOR N=1 TO 5:PRINT D6$;:NEXT N:PRINT D7$
610 PRINT UL$;
620 Y=CSRLIN:X=POS(0)
630 PRINT TAB(12);"PRESS 1 to try another value of C1 ....."
640 PRINT TAB(12);"  or  2 to calculate SWR for selected transmatch parameters ";
650 PRINT "....."
660 PRINT TAB(12);"  or  3 to continue ....."
670 Y$=INKEY$:IF Y$=""THEN 670
680 IF Y$="1"THEN VIEW PRINT 5 TO 24:CLS:VIEW PRINT:GOTO 260
690 IF Y$="3"THEN GOTO 1000
700 IF Y$="2"THEN GOSUB 1370 ELSE 670
710 PRINT " CALCULATION OF TRANSMATCH INPUT SWR ";
720 PRINT "(for selected transmatch parameters):"
730 PRINT
740 INPUT " ENTER: Value for Q (Enter `1e4' for DEFAULT value of infinity)";Q
750 INPUT " ENTER: Value for L in �H.....";L
760 INPUT " ENTER: Value for C1 in pF....";C1
770 INPUT " ENTER: Value for C2 in pF....";C2
780 XC1= -1/W/C1/9.999999E-13:XC2= -1/W/C2/9.999999E-13:XCL=XL+XC2:DEN =RL*RL+XCL*XCL
790 G1=RL/DEN:B1=-XCL/DEN:G2=1/W/L/9.999999E-07/Q:G=G1+G2:B=B1-1/W/L/9.999999E-07
800 YY=(G*G+B*B):RIN=G/YY:XIN=-B/YY+XC1:IF XIN > -5.000000E-01 AND  XIN <5.000000E-01 THEN XIN=0
810 VIEW PRINT Y+2 TO 24:CLS:VIEW PRINT
820 LOCATE Y+2:PRINT " For Q=";Q;;":";
830 LOCATE ,20:PRINT "L =";USING "####.### �H";L
840 LOCATE ,20:PRINT "C1=";USING "####.### pf";C1
850 LOCATE ,20:PRINT "C2=";USING "####.### pf";C2
860 U=RIN-R1:V=RIN+R1:P1=U*U+XIN*XIN:P2=V*V+XIN*XIN
870 GAMMA=SQR(P1/P2):SWR=(1+GAMMA)/(1-GAMMA)
880 LOCATE CSRLIN-3
890 LOCATE ,40:PRINT "Rin =";USING "####.## �";RIN
900 LOCATE ,40:PRINT "Xin =";USING "####.## �";XIN
910 LOCATE ,40:PRINT "SWR =";USING "####.##:1";SWR
920 PRINT
930 COLOR 0,7:PRINT " Another SWR calculation (y/n)? ";:COLOR 7,0
940 Y$=INKEY$:IF Y$="" THEN 940
950 IF Y$="Y" OR Y$="y" THEN GOSUB 1370:GOTO 710
960 IF Y$="n" OR Y$="N" THEN GOSUB 1370 ELSE 670
970 GOTO 620
980 END
990 '
1000 GOSUB 1370
1010 PRINT " NOTE: Formulae used in this program were derived from original ";
1020 PRINT "material by"
1030 PRINT "       Kevin Schmidt, W9CF, published in QEX, July 1997."
1040 PRINT UL$;
1050 PRINT TAB(10);"PRESS  1  to perform another transmatch design ......"
1060 PRINT TAB(10);"   or  2  to QUIT the program ......."
1070 Y$=INKEY$:IF Y$=""THEN 1070
1080 IF Y$="1"OR Y$="2"THEN 1090 ELSE 1070
1090 IF Y$="1"THEN 10
1100 IF PROG$=""THEN PROG$="exit"
1110 CLS:CHAIN PROG$
1120 END
1130 '
1140 '.....tuner diagram
1150 COLOR 0,7
1160 LOCATE 3:J=24
1170 LOCATE ,J:PRINT "                                  "
1180 LOCATE ,J:PRINT " Input >��Ĵ���������Ĵ����> Load "
1190 LOCATE ,J:PRINT "           C1    �    C2          "
1200 LOCATE ,J:PRINT "           �������                "
1210 LOCATE ,J:PRINT "           �  L                   "
1220 LOCATE ,J:PRINT "         \\�\\                    "
1230 LOCATE ,J:PRINT "                                  "
1240 COLOR 7,0
1250 PRINT UL$;
1260 RETURN
1270 '
1280 '     tabulate results
1290 PRINT TAB(10);"�  ";Q;:PRINT TAB(20);"� ";:Y=CSRLIN:X=POS(0)
1300 LOCATE Y,X:PRINT USING "##.###";L1;:PRINT TAB(30);"�  ";:Y=CSRLIN:X=POS(0)
1310 LOCATE Y,X:PRINT USING "###.#";C1;:PRINT TAB(40);"� ";:Y=CSRLIN:X=POS(0)
1320 LOCATE Y,X:PRINT USING "####.#";C2;:PRINT TAB(50);"� ";:Y=CSRLIN:X=POS(0)
1330 LOCATE Y,X:PRINT USING "##.##";LOSS;:PRINT TAB(60);"�  ";:Y=CSRLIN:X=POS(0)
1340 LOCATE Y,X:PRINT USING "###.#";EFF;:PRINT TAB(70);"�"
1350 RETURN
1360 '
1370 '.....clear window
1380 VIEW PRINT Y TO 24:CLS:VIEW PRINT:LOCATE Y,X
1390 RETURN
1400 '
1410 '.....Q Loop
1420 FOR Q=250 TO 50 STEP -50:RQ=W*L*Q:RP=RT*RQ/(RQ-RT)
1430 N=N+1
1440 IF RP<RL THEN GOTO 1520
1450 VAR=RL*RP-RL*RL:XP=RL*RP/-SQR(VAR):X2=-XL-SQR(VAR):C2=-1/W/X2*9.999999E+11
1460 WL=-1/XT-1/XP:WL=1/WL:L=WL/W:RQ=W*L*Q:RP=RT*RQ/(RQ-RT):EFF=RT/RP:L1=L*10^6
1470 IF ABS(EFF-REF)>9.999999E-05 THEN REF=EFF:GOTO 1430
1480 LOSS = -10/2.301000*LOG(EFF)
1490 C2=INT(10*C2)/10:EFF=INT(1000*EFF)/1000*100
1500 GOSUB 1280
1510 NEXT Q
1520 RETURN
1530 '
1540 '.....subroutine RL>R1
1550 C1A=1/W/SQR(R1*(RL-R1))*9.999999E+11:IF C1A>C1LIM THEN C1A=C1LIM
1560 C1B=1/W*SQR(RL/R1)/SQR(XL*XL+RL*(RL-R1))*9.999999E+11:IF C1B>C1LIM THEN C1B=C1LIM
1570 IF XL<=0 THEN C1MAX=C1B ELSE C1MAX=C1A
1580 IF XL>0 AND C1A>C1B THEN V=1
1590 RETURN
1600 '
1610 '.....subroutine RL<=R1
1620 IF (RL*RL+XL*XL)<=R1*RL THEN C1MAX=C1LIM:GOTO 1660
1630 C1B=1/W*SQR(RL/R1)/SQR(XL*XL+RL*(RL-R1))*9.999999E+11:IF C1B>C1LIM THEN C1B=C1LIM
1640 IF XL<=0 THEN C1MAX=C1B ELSE C1MAX=C1LIM
1650 IF XL>0 AND C1MAX>C1B THEN V=1:C1A=C1MAX
1660 RETURN
