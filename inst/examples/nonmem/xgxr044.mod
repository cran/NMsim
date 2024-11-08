$PROBLEM    043 as ADVAN13

$INPUT ROW ID NOMTIME TIME EVID CMT AMT DV FLAG STUDY
BLQ CYCLE DOSE PART PROFDAY PROFTIME WEIGHTB eff0

$DATA     ../data/xgxr2.csv IGNORE=@ IGNORE=(FLAG.NE.0) IGNORE(DOSE.LT.30)

$SUBROUTINE ADVAN13 TOL 9
$MODEL COMP=(DEPOT,DEFDOSE) COMP=(CENTRAL,DEFOBS)
       COMP=(PERIPH)

$PK
LTVKA=THETA(1)
LTVV2=THETA(2)
LTVCL=THETA(3)
LTVV3=THETA(4)
LTVQ=THETA(5)

MU_1=LTVKA
KA=EXP(MU_1+ETA(1))
MU_2=LTVV2
V2=EXP(MU_2+ETA(2))
MU_3=LTVCL
CL=EXP(MU_3+ETA(3))
MU_4=LTVV3
V3=EXP(MU_4+ETA(4))
MU_5 = LTVQ
Q =EXP(MU_5+ETA(5))
S2=V2/1000

$DES
DADT(1) = -KA*A(1)
DADT(2) = KA*A(1) + Q*(A(3)/V3-A(2)/V2) - CL*A(2)/V2
DADT(3) = -Q*(A(3)/V3-A(2)/V2)


$ERROR
  IPRED=A(2)/S2
  IRES=DV-IPRED

  IF (IPRED.GT.1) THEN
    W = SQRT(IPRED**2*SIGMA(1,1) + SIGMA(2,2))
  ELSE
    W=1
  ENDIF

  IWRES=IRES/W
  Y=IPRED+IPRED*ERR(1)+ERR(2)

;-----------------------INITIAL ESTIMATES---------------------------------
$THETA  (.1)             ; LTVKA
$THETA  (3)             ; LTVV2
$THETA  (3)             ; LTVCL
$THETA  (2)             ; LTVV3
$THETA  (-1)             ; LTVQ

$OMEGA 0 FIX
$OMEGA BLOCK (2) 0.1
.01
0.1
$OMEGA 0 FIX
$OMEGA 0 FIX

$SIGMA 0.1
$SIGMA 0 FIX

;; $ESTIMATION METHOD=1 POSTHOC INTER MAXEVAL=9999 NSIG=2 SIGL=9
;;            PRINT=10 NOABORT MSFO=xgxr033.msf
$ESTIMATION METHOD=SAEM INTERACTION NOABORT NBURN=1000 NITER=1000 CTYPE=3 MAX=99999 NSIG=3 SEED=3442 PRINT=10 RANMETHOD=P MSFO=xgxr044.msf

$ESTIMATION METHOD=IMP INTERACTION EONLY=1 NITER=20 PRINT=1 ISAMPLE=5000 RANMETHOD=P
																	    
$COV PRINT=E

$TABLE ROW LTVKA LTVV2 LTVV3 LTVCL LTVQ KA V2 V3 CL Q PRED IPRED Y NOPRINT FILE=xgxr044_res.txt
$TABLE ETAS(1:LAST) NOAPPEND NOPRINT FILE=xgxr044_etas.txt
