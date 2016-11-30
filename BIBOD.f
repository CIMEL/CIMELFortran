         BLOCK DATA  CHEKDO
C
C       CORRECT ANSWERS TO TEST PROBLEMS ( AS PRODUCED BY DISORT )
C
C
      PARAMETER  ( MXPROB = 9, MXCASE = 8, MAXTAU = 5,
     $             MAXMU = 10, MAXAZ = 3 )
      REAL  TSTFIR( MAXTAU, MXCASE, MXPROB ),
     $      TSTFDN( MAXTAU, MXCASE, MXPROB ),
     $      TSTFUP( MAXTAU, MXCASE, MXPROB ),
     $      TSTDFD( MAXTAU, MXCASE, MXPROB ),
     $      TSTUU ( MAXTAU, MAXMU, MAXAZ, MXCASE, MXPROB )
      COMMON / DOCHEK / TSTFIR, TSTFDN, TSTFUP, TSTDFD, TSTUU
C
C************************ TEST CASE 1A *********************************
      DATA ( TSTFIR( I,1,1 ), I = 1, 2 ) / 3.14159E+00, 2.29844E+00 /
      DATA ( TSTFDN( I,1,1 ), I = 1, 2 ) /-2.44249E-15, 7.94108E-02 /
      DATA ( TSTFUP( I,1,1 ), I = 1, 2 ) / 7.99451E-02, 8.32464E-17 /
      DATA ( TSTDFD( I,1,1 ), I = 1, 2 ) / 2.54067E+01, 1.86531E+01 /
      DATA ( ( TSTUU( I,J,1,1,1 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 1.17771E-01,
     $    2.64170E-02, 1.34041E-02, 1.33826E-02, 2.63324E-02,
     $    1.15898E-01, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 1B *********************************
      DATA ( TSTFIR( I,2,1 ), I = 1, 2 ) / 3.14159E+00, 2.29844E+00 /
      DATA ( TSTFDN( I,2,1 ), I = 1, 2 ) / 9.45730E-09, 4.20233E-01 /
      DATA ( TSTFUP( I,2,1 ), I = 1, 2 ) / 4.22922E-01, 1.09267E-08 /
      DATA ( TSTDFD( I,2,1 ), I = 1, 2 ) / 0.00000E+00, 0.00000E+00 /
      DATA ( ( TSTUU( I,J,1,2,1 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 6.22884E-01,
     $    1.39763E-01, 7.09192E-02, 7.08109E-02, 1.39337E-01,
     $    6.13458E-01, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 1C *********************************
      DATA ( TSTFIR( I,3,1 ), I = 1, 2 ) / 2*0.0 /
      DATA ( TSTFDN( I,3,1 ), I = 1, 2 ) / 3.14159E+00, 3.04897E+00 /
      DATA ( TSTFUP( I,3,1 ), I = 1, 2 ) / 9.06556E-02, 0.0 /
      DATA ( TSTDFD( I,3,1 ), I = 1, 2 ) / 6.66870E-02, 5.88936E-02 /
      DATA ( ( TSTUU( I,J,1,3,1 ), J = 1, 6 ), I = 1, 2 )
     $  / 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.33177E-01,
     $    2.99879E-02, 1.52233E-02, 9.84447E-01, 9.69363E-01,
     $    8.63946E-01, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 1D *********************************
      DATA ( TSTFIR( I,4,1 ), I = 1, 2 ) / 3.14159E+00, 0.00000E+00 /
      DATA ( TSTFDN( I,4,1 ), I = 1, 2 ) / 2*0.0 /
      DATA ( TSTFUP( I,4,1 ), I = 1, 2 ) / 2.59686E-01, 0.0 /
      DATA ( TSTDFD( I,4,1 ), I = 1, 2 ) / 2.57766E+01, 0.0 /
      DATA ( ( TSTUU( I,J,1,4,1 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 2.62972E-01,
     $    9.06967E-02, 5.02853E-02, 1.22980E-15, 1.30698E-17,
     $    6.88841E-18, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 1E *********************************
      DATA ( TSTFIR( I,5,1 ), I = 1, 2 ) / 3.14159E+00, 0.00000E+00 /
      DATA ( TSTFDN( I,5,1 ), I = 1, 2 ) / 0.0, 6.76954E-02 /
      DATA ( TSTFUP( I,5,1 ), I = 1, 2 ) / 3.07390E+00, 0.0 /
      DATA ( TSTDFD( I,5,1 ), I = 1, 2 ) / 0.00000E+00, 0.00000E+00 /
      DATA ( ( TSTUU( I,J,1,5,1 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 1.93321E+00,
     $    1.02732E+00, 7.97199E-01, 2.71316E-02, 1.87805E-02,
     $    1.16385E-02, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 1F *********************************
      DATA ( TSTFIR( I,6,1 ), I = 1, 2 ) / 2*0.0 /
      DATA ( TSTFDN( I,6,1 ), I = 1, 2 ) / 3.14159E+00, 4.60048E-03 /
      DATA ( TSTFUP( I,6,1 ), I = 1, 2 ) / 2.49618E+00, 0.0 /
      DATA ( TSTDFD( I,6,1 ), I = 1, 2 ) / 1.14239E-01, 7.93633E-05 /
      DATA ( ( TSTUU( I,J,1,6,1 ), J = 1, 6 ), I = 1, 2 )
     $  / 1.00000E+00, 1.00000E+00, 1.00000E+00, 8.77510E-01,
     $    8.15136E-01, 7.52715E-01, 1.86840E-03, 1.26492E-03,
     $    7.79281E-04, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C
C************************ TEST CASE 2A *********************************
      DATA ( TSTFIR( I,1,2 ), I = 1, 2 )
     $  / 2.52716E-01, 2.10311E-02 /
      DATA ( TSTFDN( I,1,2 ), I = 1, 2 )
     $  /-1.38778E-17, 4.41791E-02 /
      DATA ( TSTFUP( I,1,2 ), I = 1, 2 )
     $  / 5.35063E-02, 8.70947E-18 /
      DATA ( TSTDFD( I,1,2 ), I = 1, 2 )
     $  / 1.66570E+00, 1.89848E-01 /
      DATA ( ( TSTUU( I,J,1,1,2 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 1.61796E-01,
     $    2.11501E-02, 7.86713E-03, 7.71897E-03, 2.00778E-02,
     $    2.57685E-02, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 2B *********************************
      DATA ( TSTFIR( I,2,2 ), I = 1, 2 )
     $  / 2.52716E-01, 2.10311E-02 /
      DATA ( TSTFDN( I,2,2 ), I = 1, 2 )
     $  / 4.03470E-10, 1.06123E-01 /
      DATA ( TSTFUP( I,2,2 ), I = 1, 2 )
     $  / 1.25561E-01, 5.26549E-10 /
      DATA ( TSTDFD( I,2,2 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00 /
      DATA ( ( TSTUU( I,J,1,2,2 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 3.47678E-01,
     $    4.87120E-02, 1.89387E-02, 1.86027E-02, 4.64061E-02,
     $    6.77603E-02, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 2C *********************************
      DATA ( TSTFIR( I,3,2 ), I = 1, 2 )
     $  / 2.52716E-01, 2.56077E-28 /
      DATA ( TSTFDN( I,3,2 ), I = 1, 2 )
     $  / 1.38778E-17, 2.51683E-04 /
      DATA ( TSTFUP( I,3,2 ), I = 1, 2 )
     $  / 6.24730E-02, 6.14615E-21 /
      DATA ( TSTDFD( I,3,2 ), I = 1, 2 )
     $  / 1.67462E+00, 1.75464E-04 /
      DATA ( ( TSTUU( I,J,1,3,2 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 1.62566E-01,
     $    2.45786E-02, 1.01498E-02, 1.70004E-04, 3.97168E-05,
     $    1.32472E-05, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 2D *********************************
      DATA ( TSTFIR( I,4,2 ), I = 1, 2 )
     $  / 2.52716E-01, 2.56077E-28 /
      DATA ( TSTFDN( I,4,2 ), I = 1, 2 )
     $  / 3.98353E-10, 2.68008E-02 /
      DATA ( TSTFUP( I,4,2 ), I = 1, 2 )
     $  / 2.25915E-01, 4.51006E-10 /
      DATA ( TSTDFD( I,4,2 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00 /
      DATA ( ( TSTUU( I,J,1,4,2 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 3.64010E-01,
     $    8.26993E-02, 4.92370E-02, 1.05950E-02, 7.69337E-03,
     $    3.79276E-03, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C
C************************ TEST CASE 3A *********************************
      DATA ( TSTFIR( I,1,3 ), I = 1, 2 ) / 3.14159E+00, 1.42628E-04 /
      DATA ( TSTFDN( I,1,3 ), I = 1, 2 ) / 0.0, 4.70303E-02 /
      DATA ( TSTFUP( I,1,3 ), I = 1, 2 ) / 1.69951E-01, 0.0 /
      DATA ( TSTDFD( I,1,3 ), I = 1, 2 ) / 2.59221E+01, 7.43638E-02 /
      DATA ( ( TSTUU( I,J,1,1,3 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 4.86923E-01,
     $    5.08802E-02, 1.03351E-02, 7.88518E-03, 2.22081E-02,
     $    2.90003E-03, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 3B *********************************
      DATA ( TSTFIR( I,2,3 ), I = 1, 2 ) / 3.14159E+00, 1.42628E-04 /
      DATA ( TSTFDN( I,2,3 ), I = 1, 2 ) / 0.0, 1.31455E+00 /
      DATA ( TSTFUP( I,2,3 ), I = 1, 2 ) / 1.82690E+00, 0.0 /
      DATA ( TSTDFD( I,2,3 ), I = 1, 2 ) / 0.00000E+00, 0.00000E+00 /
      DATA ( ( TSTUU( I,J,1,2,3 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 3.72886E+00,
     $    6.32069E-01, 1.51211E-01, 2.13998E-01, 5.84010E-01,
     $    3.77162E-01, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 3C *********************************
      DATA ( TSTFIR( I,3,3 ), I = 1, 2 ) / 2*0.0 /
      DATA ( TSTFDN( I,3,3 ), I = 1, 2 ) / 3.14159E+00, 2.49660E+00 /
      DATA ( TSTFUP( I,3,3 ), I = 1, 2 ) / 5.83122E-01, 0.0 /
      DATA ( TSTDFD( I,3,3 ), I = 1, 2 ) / 8.06945E-02, 4.35243E-02 /
      DATA ( ( TSTUU( I,J,1,3,3 ), J = 1, 6 ), I = 1, 2 )
     $  / 1.00000E+00, 1.00000E+00, 1.00000E+00, 5.66618E-01,
     $    2.32213E-01, 7.60274E-02, 9.11175E-01, 7.44031E-01,
     $    4.03607E-01, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 3D *********************************
      DATA ( TSTFIR( I,4,3 ), I = 1, 2 ) / 3.14159E+00, 0.0 /
      DATA ( TSTFDN( I,4,3 ), I = 1, 2 ) / 0.0, 1.99813E-05 /
      DATA ( TSTFUP( I,4,3 ), I = 1, 2 ) / 1.70062E-01, 0.0 /
      DATA ( TSTDFD( I,4,3 ), I = 1, 2 ) / 2.59222E+01, 1.91941E-05 /
      DATA ( ( TSTUU( I,J,1,4,3 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 4.86926E-01,
     $    5.09190E-02, 1.03691E-02, 2.07944E-05, 9.76964E-07,
     $    1.88004E-07, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 3E *********************************
      DATA ( TSTFIR( I,5,3 ), I = 1, 2 ) / 3.14159E+00, 0.0 /
      DATA ( TSTFDN( I,5,3 ), I = 1, 2 ) / 0.0, 5.91973E-01 /
      DATA ( TSTFUP( I,5,3 ), I = 1, 2 ) / 2.54962E+00, 0.0 /
      DATA ( TSTDFD( I,5,3 ), I = 1, 2 ) / 0.00000E+00, 0.00000E+00 /
      DATA ( ( TSTUU( I,J,1,5,3 ), J = 1, 6 ), I = 1, 2 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 3.85776E+00,
     $    8.68015E-01, 3.79793E-01, 2.36362E-01, 1.64357E-01,
     $    9.15016E-02, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 3F *********************************
      DATA ( TSTFIR( I,6,3 ), I = 1, 2 ) / 2*0.0 /
      DATA ( TSTFDN( I,6,3 ), I = 1, 2 ) / 3.14159E+00, 1.01167E+00 /
      DATA ( TSTFUP( I,6,3 ), I = 1, 2 ) / 1.68750E+00, 0.0 /
      DATA ( TSTDFD( I,6,3 ), I = 1, 2 ) / 1.00459E-01, 1.71385E-02 /
      DATA ( ( TSTUU( I,J,1,6,3 ), J = 1, 6 ), I = 1, 2 )
     $  / 1.00000E+00, 1.00000E+00, 1.00000E+00, 7.57316E-01,
     $    5.87262E-01, 4.33051E-01, 4.16334E-01, 2.75956E-01,
     $    1.51785E-01, 0.00000E+00, 0.00000E+00, 0.00000E+00 /
C
C************************ TEST CASE 4A *********************************
      DATA ( TSTFIR( I,1,4 ), I = 1, 3 )
     $  / 3.14159E+00, 1.90547E+00, 1.15573E+00 /
      DATA ( TSTFDN( I,1,4 ), I = 1, 3 )
     $  / 0.0, 1.17401E+00, 1.81264E+00 /
      DATA ( TSTFUP( I,1,4 ), I = 1, 3 )
     $  / 1.73223E-01, 1.11113E-01, 0.0 /
      DATA ( TSTDFD( I,1,4 ), I = 1, 3 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00 /
      DATA ( ( TSTUU( I,J,1,1,4 ), J = 1, 6 ), I = 1, 3 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 9.29865E-02,
     $    6.61234E-02, 3.55365E-02, 2.49435E+00, 1.19070E-01,
     $    1.35138E-01, 1.24070E-01, 4.02884E-02, 1.73581E-02,
     $    3.34666E+00, 2.19624E-01, 1.57002E-01, 0.00000E+00,
     $    0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 4B *********************************
      DATA ( TSTFIR( I,2,4 ), I = 1, 3 )
     $  / 3.14159E+00, 1.90547E+00, 1.15573E+00 /
      DATA ( TSTFDN( I,2,4 ), I = 1, 3 )
     $  / 0.0, 1.01517E+00, 1.51554E+00 /
      DATA ( TSTFUP( I,2,4 ), I = 1, 3 )
     $  / 1.23666E-01, 7.88691E-02, 0.0 /
      DATA ( TSTDFD( I,2,4 ), I = 1, 3 )
     $  / 3.43724E-01, 3.52390E-01, 3.19450E-01 /
      DATA ( ( TSTUU( I,J,1,2,4 ), J = 1, 6 ), I = 1, 3 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 6.55782E-02,
     $    4.56643E-02, 2.74243E-02, 2.22302E+00, 9.64101E-02,
     $    9.62927E-02, 8.44926E-02, 2.80216E-02, 1.35087E-02,
     $    2.94684E+00, 1.67508E-01, 1.08212E-01, 0.00000E+00,
     $    0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 4C *********************************
      DATA ( TSTFIR( I,3,4 ), I = 1, 3 )
     $  / 1.57080E+00, 5.77864E-01, 2.12584E-01 /
      DATA ( TSTFDN( I,3,4 ), I = 1, 3 )
     $  / 0.0, 7.02764E-01, 8.03294E-01 /
      DATA ( TSTFUP( I,3,4 ), I = 1, 3 )
     $  / 2.25487E-01, 1.23848E-01, 0.0 /
      DATA ( TSTDFD( I,3,4 ), I = 1, 3 )
     $  / 3.85003E-01, 3.37317E-01, 2.16403E-01 /
      DATA ( ( ( TSTUU( I,J,K,3,4 ), J = 1, 6 ), I = 1, 3 ), K = 1, 3 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 8.70016E-01,
     $    2.24760E-01, 2.28322E-02, 4.76042E-02, 3.00258E+00,
     $    1.41140E+00, 6.97400E-01, 1.09065E-01, 9.35116E-03,
     $    8.37538E-02, 2.68792E+00, 8.76317E-01, 0.00000E+00,
     $    0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     $    0.00000E+00, 8.86109E-02, 5.76913E-02, 2.28322E-02,
     $    4.76042E-02, 5.82549E-02, 1.04636E-01, 9.15334E-02,
     $    2.95681E-02, 9.35116E-03, 8.37538E-02, 9.43348E-02,
     $    8.95961E-02, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     $    0.00000E+00, 0.00000E+00, 0.00000E+00, 6.95291E-02,
     $    4.93283E-02, 2.28322E-02, 4.76042E-02, 2.59416E-02,
     $    6.24961E-02, 5.90188E-02, 2.44592E-02, 9.35116E-03,
     $    8.37538E-02, 4.00024E-02, 4.66785E-02, 0.00000E+00,
     $    0.00000E+00, 0.00000E+00 /
C
C************************ TEST CASE 5A *********************************
      DATA ( TSTFIR( I,1,5 ), I = 1, 3 )
     $  / 3.14159E+00, 3.97856E-14, 5.03852E-28 /
      DATA ( TSTFDN( I,1,5 ), I = 1, 3 )
     $  / 0.0, 2.24768E+00, 4.79851E-01 /
      DATA ( TSTFUP( I,1,5 ), I = 1, 3 )
     $  / 2.66174E+00, 1.76783E+00, 0.0 /
      DATA ( TSTDFD( I,1,5 ), I = 1, 3 ) / 3*0.0 /
      DATA ( ( TSTUU( I,J,1,1,5 ), J = 1, 6 ), I = 1, 3 )
     $  / 0.00000E+00, 0.00000E+00, 0.00000E+00, 4.57660E-01,
     $    7.68942E-01, 1.03122E+00, 7.53662E-01, 6.96362E-01,
     $    6.50541E-01, 6.27631E-01, 5.81809E-01, 5.24532E-01,
     $    1.95230E-01, 1.31990E-01, 7.20655E-02, 0.00000E+00,
     $    0.00000E+00, 0.00000E+00 /
C************************ TEST CASE 5B *********************************
      DATA ( TSTFIR( I,2,5 ), I = 1, 3 )
     $  / 1.28058E-01, 8.67322E-06, 4.47729E-21 /
      DATA ( TSTFDN( I,2,5 ), I = 1, 3 )
     $  / 1.74767E+00, 2.33975E-01, 6.38347E-05 /
      DATA ( TSTFUP( I,2,5 ), I = 1, 3 )
     $  / 2.70485E-01, 3.74253E-02, 1.02904E-05 /
      DATA ( TSTDFD( I,2,5 ), I = 1, 3 )
     $  / 3.10129E-01, 4.52671E-02, 1.25022E-05 /
      DATA ( ( TSTUU( I,J,1,2,5 ), J = 1, 6 ), I = 1, 3 )
     $  / 1.50205E+01, 2.18778E-01, 1.36520E-01, 1.14001E-01,
     $    8.71231E-02, 8.55016E-02, 1.85090E-01, 4.92726E-02,
     $    2.65448E-02, 2.02154E-02, 1.29660E-02, 9.51226E-03,
     $    3.41287E-05, 1.39916E-05, 7.47042E-06, 5.65604E-06,
     $    3.58246E-06, 2.57859E-06 /
C
C************************ TEST CASE 6A *********************************
      DATA ( TSTFIR( I,1,6 ), I = 1, 2 ) / 2*100.0 /
      DATA ( TSTFDN( I,1,6 ), I = 1, 2 ) / 2*0.0 /
      DATA ( TSTFUP( I,1,6 ), I = 1, 2 ) / 2*0.0 /
      DATA ( TSTDFD( I,1,6 ), I = 1, 2 ) / 2*200.0 /
      DATA ( ( TSTUU( I,J,1,1,6 ), J = 1, 4 ), I = 1, 2 ) / 8*0.0 /
C************************ TEST CASE 6B *********************************
      DATA ( TSTFIR( I,2,6 ), I = 1, 3 )
     $  / 1.000000E+02, 3.67879E+01, 1.35335E+01 /
      DATA ( TSTFDN( I,2,6 ), I = 1, 3 ) / 3*0.0 /
      DATA ( TSTFUP( I,2,6 ), I = 1, 3 ) / 3*0.0 /
      DATA ( TSTDFD( I,2,6 ), I = 1, 3 )
     $  / 2.00000E+02, 7.35759E+01, 2.70671E+01 /
      DATA ( ( TSTUU( I,J,1,2,6 ), J = 1, 4 ), I = 1, 3 ) / 12*0.0 /
C************************ TEST CASE 6C *********************************
      DATA ( TSTFIR( I,3,6 ), I = 1, 3 )
     $  / 1.00000E+02, 3.67879E+01, 1.35335E+01 /
      DATA ( TSTFDN( I,3,6 ), I = 1, 3 )
     $  / 0.00000E+00, 1.39296E-07, 3.35148E-07 /
      DATA ( TSTFUP( I,3,6 ), I = 1, 3 )
     $  / 1.48450E+00, 2.99914E+00, 6.76676E+00 /
      DATA ( TSTDFD( I,3,6 ), I = 1, 3 )
     $  / 2.02010E+02, 7.79962E+01, 4.06006E+01 /
      DATA ( ( TSTUU( I,J,1,3,6 ), J = 1, 4 ), I = 1, 3 )
     $  / 0.00000E+00, 0.00000E+00, 9.77882E-05, 7.92386E-01,
     $    0.00000E+00, 0.00000E+00, 1.45131E-02, 1.30642E+00,
     $    0.00000E+00, 0.00000E+00, 2.15393E+00, 2.15393E+00 /
C************************ TEST CASE 6D *********************************
      DATA ( TSTFIR( I,4,6 ), I = 1, 3 )
     $  / 1.00000E+02, 3.67879E+01, 1.35335E+01 /
      DATA ( TSTFDN( I,4,6 ), I = 1, 3 )
     $  / 0.00000E+00, 5.16768E-08, 1.28869E-07 /
      DATA ( TSTFUP( I,4,6 ), I = 1, 3 )
     $  / 6.19105E-01, 1.33245E+00, 3.47790E+00 /
      DATA ( TSTDFD( I,4,6 ), I = 1, 3 )
     $  / 2.00899E+02, 7.57612E+01, 3.65186E+01 /
      DATA ( ( TSTUU( I,J,1,4,6 ), J = 1, 4 ), I = 1, 3 )
     $  / 0.00000E+00, 0.00000E+00, 5.07347E-05, 2.72396E-01,
     $    0.00000E+00, 0.00000E+00, 7.52970E-03, 4.49105E-01,
     $    0.00000E+00, 0.00000E+00, 1.11751E+00, 7.40449E-01 /
C************************ TEST CASE 6E *********************************
      DATA ( TSTFIR( I,5,6 ), I = 1, 3 )
     $  / 1.00000E+02, 3.67879E+01, 1.35335E+01 /
      DATA ( TSTFDN( I,5,6 ), I = 1, 3 )
     $  / 0.00000E+00, 7.94827E-06, 1.90008E-05 /
      DATA ( TSTFUP( I,5,6 ), I = 1, 3 )
     $  / 8.28836E+01, 1.65215E+02, 3.59895E+02 /
      DATA ( TSTDFD( I,5,6 ), I = 1, 3 )
     $  / 3.10552E+02, 3.11018E+02, 6.79533E+02 /
      DATA ( ( TSTUU( I,J,1,5,6 ), J = 1, 4 ), I = 1, 3 )
     $  / 0.00000E+00, 0.00000E+00, 3.20501E-03, 4.64836E+01,
     $    0.00000E+00, 0.00000E+00, 4.75666E-01, 7.66386E+01,
     $    0.00000E+00, 0.00000E+00, 7.05951E+01, 1.26356E+02 /
C************************ TEST CASE 6F *********************************
      DATA ( TSTFIR( I,6,6 ), I = 1, 3 )
     $  / 1.00000E+02, 3.67879E+01, 1.35335E+01 /
      DATA ( TSTFDN( I,6,6 ), I = 1, 3 )
     $  / 3.21497E+02, 1.42493E+02, 7.05306E+01 /
      DATA ( TSTFUP( I,6,6 ), I = 1, 3 )
     $  / 8.52994E+01, 1.70343E+02, 3.72842E+02 /
      DATA ( TSTDFD( I,6,6 ), I = 1, 3 )
     $  / 9.57001E+02, 5.29253E+02, 8.07923E+02 /
      DATA ( ( TSTUU( I,J,1,6,6 ), J = 1, 4 ), I = 1, 3 )
     $  / 1.02336E+02, 1.02336E+02, 3.58884E-03, 4.75119E+01,
     $    6.20697E+01, 6.89532E-01, 5.32631E-01, 7.83338E+01,
     $    3.76472E+01, 4.64603E-03, 7.90494E+01, 1.29151E+02 /
C************************ TEST CASE 6G *********************************
      DATA ( TSTFIR( I,7,6 ), I = 1, 3 )
     $  / 1.00000E+02, 3.67879E+01, 1.35335E+01 /
      DATA ( TSTFDN( I,7,6 ), I = 1, 3 )
     $  / 3.21497E+02, 3.04775E+02, 3.63632E+02 /
      DATA ( TSTFUP( I,7,6 ), I = 1, 3 )
     $  / 3.36115E+02, 4.13977E+02, 4.43050E+02 /
      DATA ( TSTDFD( I,7,6 ), I = 1, 3 )
     $  / 5.81342E+02, 1.28621E+02, -1.71119E+02 /
      DATA ( ( TSTUU( I,J,1,7,6 ), J = 1, 4 ), I = 1, 3 )
     $  / 1.02336E+02, 1.02336E+02, 7.80731E+01, 1.17126E+02,
     $    9.78748E+01, 1.01048E+02, 1.15783E+02, 1.36113E+02,
     $    1.10061E+02, 1.38631E+02, 1.33373E+02, 1.42865E+02 /
C************************ TEST CASE 6H *********************************
      DATA ( TSTFIR( I,8,6 ), I = 1, 3 )
     $  / 1.00000E+02, 1.35335E+01, 2.06115E-07 /
      DATA ( TSTFDN( I,8,6 ), I = 1, 3 )
     $  / 3.21497E+02, 2.55455E+02, 4.43444E+02 /
      DATA ( TSTFUP( I,8,6 ), I = 1, 3 )
     $  / 2.37350E+02, 2.61130E+02, 4.56205E+02 /
      DATA ( TSTDFD( I,8,6 ), I = 1, 3 )
     $  / 4.23780E+02, 6.19828E+01, -3.17719E+01 /
      DATA ( ( TSTUU( I,J,1,8,6 ), J = 1, 4 ), I = 1, 3 )
     $  / 1.02336E+02, 1.02336E+02, 7.12616E+01, 7.80737E+01,
     $    8.49992E+01, 7.73186E+01, 7.88310E+01, 8.56424E+01,
     $    1.38631E+02, 1.45441E+02, 1.44088E+02, 1.45547E+02 /
C
C************************ TEST CASE 7A *********************************
      DATA ( TSTFIR( I,1,7 ), I = 1, 3 )
     $  / 1.00000E+02, 3.67879E+01, 1.35335E+01 /
      DATA ( TSTFDN( I,1,7 ), I = 1, 3 )
     $  / 3.19830E+02, 3.54099E+02, 3.01334E+02 /
      DATA ( TSTFUP( I,1,7 ), I = 1, 3 )
     $  / 4.29572E+02, 4.47018E+02, 5.94576E+02 /
      DATA ( TSTDFD( I,1,7 ), I = 1, 3 )
     $  /-8.04270E+01, 2.51589E+02, 7.15964E+02 /
      DATA ( ( ( TSTUU( I,J,K,1,7 ), J = 1, 4 ), I = 1, 3 ), K = 1, 2 )
     $  / 1.01805E+02, 1.01805E+02, 1.41637E+02, 1.48416E+02,
     $    1.06346E+02, 1.34218E+02, 1.02514E+02, 1.58864E+02,
     $    9.64160E+01, 8.87832E+01, 1.89259E+02, 1.89259E+02,
     $    1.01805E+02, 1.01805E+02, 1.27778E+02, 1.48416E+02,
     $    1.06346E+02, 1.06238E+02, 9.41347E+01, 1.58864E+02,
     $    9.64160E+01, 7.48649E+01, 1.89259E+02, 1.89259E+02 /
C************************ TEST CASE 7B *********************************
      DATA ( TSTFIR( I,2,7 ), I = 1, 3 )
     $  / 1.00000E+02, 3.67879E+01, 1.35335E+01 /
      DATA ( TSTFDN( I,2,7 ), I = 1, 3 )
     $  / 3.19830E+02, 3.50555E+02, 2.92063E+02 /
      DATA ( TSTFUP( I,2,7 ), I = 1, 3 )
     $  / 3.12563E+02, 2.68126E+02, 3.05596E+02 /
      DATA ( TSTDFD( I,2,7 ), I = 1, 3 )
     $  /-1.68356E+02, 1.01251E+02, 4.09326E+02 /
      DATA ( ( ( TSTUU( I,J,K,2,7 ), J = 1, 4 ), I = 1, 3 ), K = 1, 2 )
     $  / 1.01805E+02, 1.01805E+02, 1.35840E+02, 9.56588E+01,
     $    1.05967E+02, 1.28779E+02, 9.00044E+01, 8.87624E+01,
     $    9.53651E+01, 7.47553E+01, 9.72743E+01, 9.72743E+01,
     $    1.01805E+02, 1.01805E+02, 1.21980E+02, 9.56588E+01,
     $    1.05967E+02, 1.00799E+02, 8.16247E+01, 8.87624E+01,
     $    9.53651E+01, 6.08370E+01, 9.72743E+01, 9.72743E+01 /
C************************ TEST CASE 7C *********************************
      DATA ( TSTFIR( I,3,7 ), I = 1, 3 )
     $  / 1.00000E+02, 3.67879E+01, 1.35335E+01 /
      DATA ( TSTFDN( I,3,7 ), I = 1, 3 )
     $  / 3.19830E+02, 3.53250E+02, 2.98583E+02 /
      DATA ( TSTFUP( I,3,7 ), I = 1, 3 )
     $  / 4.07040E+02, 4.11058E+02, 5.30504E+02 /
      DATA ( TSTDFD( I,3,7 ), I = 1, 3 )
     $  /-9.85693E+01, 2.17724E+02, 6.23936E+02 /
      DATA ( ( ( TSTUU( I,J,K,3,7 ), J = 1, 4 ), I = 1, 3 ), K = 1, 2 )
     $  / 1.01805E+02, 1.01805E+02, 1.40347E+02, 1.40534E+02,
     $    1.06259E+02, 1.32945E+02, 9.92793E+01, 1.48621E+02,
     $    9.61392E+01, 8.45884E+01, 1.54901E+02, 1.76268E+02,
     $    1.01805E+02, 1.01805E+02, 1.26343E+02, 1.40534E+02,
     $    1.06259E+02, 1.04823E+02, 9.02686E+01, 1.48621E+02,
     $    9.61392E+01, 6.97291E+01, 1.37958E+02, 1.76268E+02 /
C
C************************ TEST CASE 8A *********************************
      DATA ( TSTFIR( I,1,8 ), I = 1, 3 ) / 3*0.0 /
      DATA ( TSTFDN( I,1,8 ), I = 1, 3 ) /
     $  1.00000E+00, 7.22235E-01, 5.13132E-01 /
      DATA ( TSTFUP( I,1,8 ), I = 1, 3 ) /
     $  9.29634E-02, 2.78952E-02, 0.0 /
      DATA ( TSTDFD( I,1,8 ), I = 1, 3 ) /
     $  1.12474E+00, 6.51821E-01, 5.63361E-01 /
      DATA ( ( TSTUU( I,J,1,1,8 ), J = 1, 4 ), I = 1, 3 ) /
     $  3.18310E-01, 3.18310E-01, 5.62566E-02, 1.94423E-02,
     $  2.62711E-01, 1.36952E-01, 1.84909E-02, 5.52188E-03,
     $  2.10014E-01, 5.60376E-02, 2*0.0 /
C************************ TEST CASE 8B *********************************
      DATA ( TSTFIR( I,2,8 ), I = 1, 3 ) / 3*0.0 /
      DATA ( TSTFDN( I,2,8 ), I = 1, 3 ) /
     $  1.00000E+00, 7.95332E-01, 6.50417E-01 /
      DATA ( TSTFUP( I,2,8 ), I = 1, 3 ) /
     $  2.25136E-01, 1.26349E-01, 0.0 /
      DATA ( TSTDFD( I,2,8 ), I = 1, 3 ) /
     $  5.12692E-01, 3.56655E-01, 5.68095E-02 /
      DATA ( ( TSTUU( I,J,1,2,8 ), J = 1, 4 ), I = 1, 3 ) /
     $  3.18310E-01, 3.18310E-01, 1.23687E-01, 4.95581E-02,
     $  2.77499E-01, 1.83950E-01, 8.35695E-02, 2.50575E-02,
     $  2.40731E-01, 1.29291E-01, 2*0.0 /
C************************ TEST CASE 8C *********************************
      DATA ( TSTFIR( I,3,8 ), I = 1, 3 ) / 3*0.0 /
      DATA ( TSTFDN( I,3,8 ), I = 1, 3 ) /
     $  1.00000E+00, 4.86157E-01, 1.59984E-01 /
      DATA ( TSTFUP( I,3,8 ), I = 1, 3 ) /
     $  3.78578E-01, 2.43397E-01, 0.0 /
      DATA ( TSTDFD( I,3,8 ), I = 1, 3 ) /
     $  5.65095E-01, 2.76697E-01, 1.35679E-02 /
      DATA ( ( TSTUU( I,J,1,3,8 ), J = 1, 4 ), I = 1, 3 ) /
     $  3.18310E-01, 3.18310E-01, 1.49335E-01, 1.04766E-01,
     $  1.89020E-01, 9.88158E-02, 9.65192E-02, 6.54445E-02,
     $  6.84762E-02, 2.96698E-02, 2*0.0 /
C
C************************ TEST CASE 9A *********************************
      DATA ( TSTFIR( I,1,9 ), I = 1, 5 ) / 5*0.0 /
      DATA ( TSTFDN( I,1,9 ), I = 1, 5 ) /
     $  1.00000E+00, 3.55151E-01, 1.44265E-01, 6.71445E-03, 6.16968E-07/
      DATA ( TSTFUP( I,1,9 ), I = 1, 5 ) /
     $  2.27973E-01, 8.75098E-02, 3.61819E-02, 2.19291E-03, 0.0 /
      DATA ( TSTDFD( I,1,9 ), I = 1, 5 ) /
     $  8.82116E-01, 2.32366E-01, 9.33443E-02, 3.92782E-03, 1.02500E-07/
      DATA ( ( TSTUU( I,J,1,1,9 ), J = 1, 4 ), I = 1, 5 ) /
     $  3.18310E-01, 3.18310E-01, 9.98915E-02, 5.91345E-02,
     $  1.53507E-01, 5.09531E-02, 3.67006E-02, 2.31903E-02,
     $  7.06614E-02, 2.09119E-02, 1.48545E-02, 9.72307E-03,
     $  3.72784E-03, 1.08815E-03, 8.83317E-04, 5.94743E-04,
     $  2.87656E-07, 1.05921E-07, 2*0.0 /
C************************ TEST CASE 9B *********************************
      DATA ( TSTFIR( I,2,9 ), I = 1, 5 ) / 5*0.0 /
      DATA ( TSTFDN( I,2,9 ), I = 1, 5 ) /
     $  1.00000E+00, 4.52357E-01, 2.36473E-01, 2.76475E-02, 7.41854E-05/
      DATA ( TSTFUP( I,2,9 ), I = 1, 5 ) /
     $  1.00079E-01, 4.52015E-02, 2.41941E-02, 4.16017E-03, 0.0 /
      DATA ( TSTDFD( I,2,9 ), I = 1, 5 ) /
     $  8.04577E-01, 2.55330E-01, 1.30976E-01, 1.36227E-02, 1.22022E-05/
      DATA ( ( TSTUU( I,J,1,2,9 ), J = 1, 4 ), I = 1, 5 ) /
     $  3.18310E-01, 3.18310E-01, 7.39198E-02, 1.32768E-02,
     $  1.96609E-01, 5.92369E-02, 3.00230E-02, 7.05566E-03,
     $  1.15478E-01, 3.01809E-02, 1.52672E-02, 4.06932E-03,
     $  1.46177E-02, 3.85590E-03, 2.38301E-03, 7.77891E-04,
     $  3.37743E-05, 1.20858E-05, 2*0.0 /
C************************ TEST CASE 9C *********************************
      DATA ( TSTFIR( I,3,9 ), I = 1, 5 ) /
     $  1.57080E+00, 1.92354E-01, 2.35550E-02, 9.65131E-06, 9.03133E-19/
      DATA ( TSTFDN( I,3,9 ), I = 1, 5 ) /
     $  6.09217E+00, 4.97279E+00, 4.46616E+00, 4.22731E+00, 4.73767E+00/
      DATA ( TSTFUP( I,3,9 ), I = 1, 5 ) /
     $  4.68414E+00, 4.24381E+00, 4.16941E+00, 4.30667E+00, 5.11524E+00/
      DATA ( TSTDFD( I,3,9 ), I = 1, 5 ) /
     $  3.49563E+00, 8.81206E-01, 3.50053E-01, 1.93471E-02, 7.15349E-02/
      DATA ( ( ( TSTUU( I,J,K,3,9 ), J = 1, 4 ), I = 1, 5 ), K = 1, 3 )
     $ / 1.93920E+00, 1.93920E+00, 1.61855E+00, 1.43873E+00,
     $   1.66764E+00, 1.44453E+00, 1.38339E+00, 1.33890E+00,
     $   1.48511E+00, 1.35009E+00, 1.33079E+00, 1.32794E+00,
     $   1.34514E+00, 1.35131E+00, 1.35980E+00, 1.37918E+00,
     $   1.48927E+00, 1.54270E+00, 1.62823E+00, 1.62823E+00,
     $   1.93920E+00, 1.93920E+00, 1.57895E+00, 1.43873E+00,
     $   1.66764E+00, 1.42925E+00, 1.37317E+00, 1.33890E+00,
     $   1.48511E+00, 1.34587E+00, 1.32921E+00, 1.32794E+00,
     $   1.34514E+00, 1.35129E+00, 1.35979E+00, 1.37918E+00,
     $   1.48927E+00, 1.54270E+00, 1.62823E+00, 1.62823E+00,
     $   1.93920E+00, 1.93920E+00, 1.56559E+00, 1.43873E+00,
     $   1.66764E+00, 1.42444E+00, 1.37034E+00, 1.33890E+00,
     $   1.48511E+00, 1.34469E+00, 1.32873E+00, 1.32794E+00,
     $   1.34514E+00, 1.35128E+00, 1.35979E+00, 1.37918E+00,
     $   1.48927E+00, 1.54270E+00, 1.62823E+00, 1.62823E+00 /
C
      END



      SUBROUTINE  ALBTRN( ALBEDO, AMB, APB, ARRAY, B, BDR, CBAND, CC,
     $                    CMU, CWT, EVAL, EVECC, GL, GC, GU, IPVT,
     $                    KK, LL, NLYR, NN, NSTR, NUMU, PRNT, TAUCPR,
     $                    UMU, U0U, WK, YLMC, YLMU, Z,
     $                    MI, MI9M2, MAXULV, MAXUMU,
     $                    MXCMU, MXUMU, NNLYRI, ALBMED, TRNMED )
C
C        SPECIAL CASE TO GET ONLY ALBEDO AND TRANSMISSIVITY
C        OF ENTIRE MEDIUM AS A FUNCTION OF INCIDENT BEAM ANGLE
C        (MANY SIMPLIFICATIONS BECAUSE BOUNDARY CONDITION IS JUST
C        ISOTROPIC ILLUMINATION, THERE ARE NO THERMAL SOURCES, AND
C        PARTICULAR SOLUTIONS DO NOT NEED TO BE COMPUTED).  SEE
C        REF. S2 AND REFERENCES THEREIN FOR THEORY.
C
C        ROUTINES CALLED:  ALTRIN, LEPOLY, PRALTR, SETMTX, SOLVE1,
C                          SOLEIG, ZEROIT
C
      LOGICAL  PRNT(*)
      INTEGER  NLYR, NUMU, NSTR
      REAL     UMU(*), U0U( MAXUMU,* )
C
      INTEGER IPVT(*)
      REAL    ALBMED(*), AMB( MI,* ), APB( MI,*), ARRAY( MXCMU,* ),
     $        B(*), BDR( MI,0:* ), CBAND( MI9M2,* ), CC( MXCMU,* ),
     $        CMU(*), CWT(*), EVAL(*), EVECC( MXCMU,* ),
     $        GL( 0:MXCMU,* ), GC( MXCMU,MXCMU,* ), GU( MXUMU,MXCMU,* ),
     $        KK( MXCMU,* ), LL( MXCMU,* ), TAUCPR( 0:* ), TRNMED(*),
     $        WK(*), YLMC( 0:MXCMU,* ), YLMU( 0:MXCMU,* ), Z(*)
C
      LOGICAL  LAMBER, LYRCUT
C
C
C                    ** SET DISORT VARIABLES THAT ARE IGNORED IN THIS
C                    ** SPECIAL CASE BUT ARE NEEDED BELOW IN ARGUMENT
C                    ** LISTS OF SUBROUTINES SHARED WITH GENERAL CASE
      NCUT = NLYR
      LYRCUT = .FALSE.
      FISOT = 1.0
      LAMBER = .TRUE.
C
      MAZ = 0
      DELM0 = 1.0
C                          ** GET LEGENDRE POLYNOMIALS FOR COMPUTATIONAL
C                          ** AND USER POLAR ANGLE COSINES
C
      CALL  LEPOLY( NUMU, MAZ, MXCMU, NSTR-1, UMU, YLMU )
      CALL  LEPOLY( NN,   MAZ, MXCMU, NSTR-1, CMU, YLMC )
C
C                       ** EVALUATE LEGENDRE POLYNOMIALS WITH NEGATIVE
C                       ** -CMU- FROM THOSE WITH POSITIVE -CMU-;
C                       ** DAVE/ARMSTRONG EQ. (15)
      SGN  = -1.0
      DO  5  L = MAZ, NSTR-1
         SGN = - SGN
         DO  5  IQ = NN+1, NSTR
            YLMC( L,IQ ) = SGN * YLMC( L,IQ-NN )
    5 CONTINUE
C                                  ** ZERO BOTTOM REFLECTIVITY
C                                  ** (-ALBEDO- IS USED ONLY IN ANALYTIC
C                                  ** FORMULAE INVOLVING ALBEDO = 0
C                                  ** SOLUTIONS; EQS 16-17 OF REF S2)
      CALL  ZEROIT( BDR, MI*(MI+1) )
C
C
C ===================  BEGIN LOOP ON COMPUTATIONAL LAYERS  =============
C
      DO 100  LC = 1, NLYR
C
C                        ** SOLVE EIGENFUNCTION PROBLEM IN EQ. STWJ(8B)
C
         CALL  SOLEIG( AMB, APB, ARRAY, CMU, CWT, GL(0,LC), MI, MAZ,
     $                 MXCMU, NN, NSTR, WK, YLMC, CC, EVECC, EVAL,
     $                 KK(1,LC), GC(1,1,LC) )
C
C                          ** INTERPOLATE EIGENVECTORS TO USER ANGLES
C
         CALL  TERPEV( CWT, EVECC, GL(0,LC), GU(1,1,LC), MAZ, MXCMU,
     $                 MXUMU, NN, NSTR, NUMU, WK, YLMC, YLMU )
100   CONTINUE
C
C ===================  END LOOP ON COMPUTATIONAL LAYERS  ===============
C
C                      ** SET COEFFICIENT MATRIX OF EQUATIONS COMBINING
C                      ** BOUNDARY AND LAYER INTERFACE CONDITIONS
C
      CALL  SETMTX( BDR, CBAND, CMU, CWT, DELM0, GC, KK, LAMBER,
     $              LYRCUT, MI, MI9M2, MXCMU, NCOL, NCUT, NNLYRI,
     $              NN, NSTR, TAUCPR, WK )
C
      CALL  ZEROIT( U0U, MAXUMU*MAXULV )
C
      NHOM = 2
      IF( NLYR.EQ.1 )  NHOM = 1
      SPHALB = 0.0
      SPHTRN = 0.0
      DO 200  IHOM = 1, NHOM
C                             ** SOLVE FOR CONSTANTS OF INTEGRATION IN
C                             ** HOMOGENEOUS SOLUTION FOR ILLUMINATION
C                             ** FROM TOP (IHOM=1), THEN BOTTOM (IHOM=2)
C
         CALL  SOLVE1( B, CBAND, FISOT, IHOM, IPVT, LL, MI9M2, MXCMU,
     $                 NCOL, NLYR, NN, NNLYRI, NSTR, Z )
C
C                             ** COMPUTE AZIMUTHALLY-AVERAGED INTENSITY
C                             ** AT USER ANGLES; GIVES ALBEDO IF MULTI-
C                             ** LAYER (EQ. 9 OF REF S2); GIVES BOTH
C                             ** ALBEDO AND TRANSMISSIVITY IF SINGLE
C                             ** LAYER (EQS. 3-4 OF REF S2)
C
         CALL  ALTRIN( GU, KK, LL, MXCMU, MXUMU, MAXUMU, NLYR,
     $                 NN, NSTR, NUMU, TAUCPR, UMU, U0U, WK )
C
         IF ( IHOM.EQ.1 )  THEN
C                                   ** SAVE ALBEDOS;  FLIP TRANSMISSIV.
C                                   ** END OVER END TO CORRESPOND TO
C                                   ** POSITIVE -UMU- INST. OF NEGATIVE
            DO 120  IU = 1, NUMU/2
               ALBMED(IU) = U0U( IU + NUMU/2, 1 )
               IF( NLYR.EQ.1 )  TRNMED(IU) = U0U( NUMU/2+1-IU, 2 )
     $                          + EXP( - TAUCPR(NLYR) / UMU(IU+NUMU/2) )
120         CONTINUE
C                                    ** GET SPHERICAL ALBEDO AND, FOR 1
C                                    ** LAYER, SPHERICAL TRANSMISSIVITY
            IF( ALBEDO.GT.0.0 )
     $          CALL SPALTR( CMU, CWT, GC, KK, LL, MXCMU, NLYR,
     $                       NN, NSTR, TAUCPR, SPHALB, SPHTRN )
C
         ELSE IF ( IHOM.EQ.2 )  THEN
C                                      ** SAVE TRANSMISSIVITIES
            DO 140  IU = 1, NUMU/2
               TRNMED(IU) = U0U( IU + NUMU/2, 1 )
     $                      + EXP( - TAUCPR(NLYR) / UMU(IU+NUMU/2) )
140         CONTINUE
C                             ** GET SPHERICAL ALBEDO AND TRANSMISSIVITY
            IF( ALBEDO.GT.0.0 )
     $          CALL SPALTR( CMU, CWT, GC, KK, LL, MXCMU, NLYR,
     $                       NN, NSTR, TAUCPR, SPHTRN, SPHALB )
         END IF
200   CONTINUE
C
      IF ( ALBEDO.GT.0.0 )  THEN
C                                ** REF. S2, EQS. 16-17 (THESE EQS. HAVE
C                                ** A SIMPLE PHYSICAL INTERPRETATION
C                                ** LIKE THAT OF THE DOUBLING EQS.)
         DO 220  IU = 1, NUMU
            ALBMED(IU) = ALBMED(IU) + ( ALBEDO / (1.-ALBEDO*SPHALB) )
     $                                * SPHTRN * TRNMED(IU)
            TRNMED(IU) = TRNMED(IU) + ( ALBEDO / (1.-ALBEDO*SPHALB) )
     $                                * SPHALB * TRNMED(IU)
220      CONTINUE
      END IF
C                          ** RETURN -UMU- TO ALL POSITIVE VALUES, TO
C                          ** AGREE WITH ORDERING IN -ALBMED,TRNMED-
      NUMU = NUMU / 2
      DO 230  IU = 1, NUMU
        UMU(IU) = UMU(IU+NUMU)
 230  CONTINUE
C
      IF ( PRNT(6) )  CALL  PRALTR( UMU, NUMU, ALBMED, TRNMED )
C
      RETURN
      END
      SUBROUTINE  ALTRIN( GU, KK, LL, MXCMU, MXUMU, MAXUMU, NLYR,
     $                    NN, NSTR, NUMU, TAUCPR, UMU, U0U, WK )
C
C       COMPUTES AZIMUTHALLY-AVERAGED INTENSITY AT TOP AND BOTTOM
C       OF MEDIUM (RELATED TO ALBEDO AND TRANSMISSION OF MEDIUM BY
C       RECIPROCITY PRINCIPLES;  SEE REF S2).  USER POLAR ANGLES ARE
C       USED AS INCIDENT BEAM ANGLES. (THIS IS A VERY SPECIALIZED
C       VERSION OF 'USRINT')
C
C       ** NOTE **  USER INPUT VALUES OF -UMU- (ASSUMED POSITIVE) ARE
C                   TEMPORARILY IN UPPER LOCATIONS OF  -UMU-  AND
C                   CORRESPONDING NEGATIVES ARE IN LOWER LOCATIONS
C                   (THIS MAKES -GU- COME OUT RIGHT).  I.E. THE CONTENTS
C                   OF THE TEMPORARY -UMU- ARRAY ARE:
C
C                     -UMU(NUMU),..., -UMU(1), UMU(1),..., UMU(NUMU)
C
C   I N P U T    V A R I A B L E S:
C
C       GU     :  EIGENVECTORS INTERPOLATED TO USER POLAR ANGLES
C                 (i.e., g IN EQ. SC(1) )
C       KK     :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       LL     :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                 BY SOLVING SCALED VERSION OF EQ. SC(5);
C                 EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       NN     :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       TAUCPR :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C   O U T P U T    V A R I A B L E:
C
C       U0U  :    DIFFUSE AZIMUTHALLY-AVERAGED INTENSITY AT TOP AND
C                 BOTTOM OF MEDIUM (DIRECTLY TRANSMITTED COMPONENT,
C                 CORRESPONDING TO -BNDINT- IN 'USRINT', IS OMITTED).
C
C   I N T E R N A L    V A R I A B L E S:
C
C       DTAU   :  OPTICAL DEPTH OF A COMPUTATIONAL LAYER
C       PALINT :  NON-BOUNDARY-FORCED INTENSITY COMPONENT
C       UTAUPR :  OPTICAL DEPTHS OF USER OUTPUT LEVELS (DELTA-M SCALED)
C       WK     :  SCRATCH VECTOR FOR SAVING 'EXP' EVALUATIONS
C       ALL THE EXPONENTIAL FACTORS (i.e., EXP1, EXPN,... etc.)
C       COME FROM THE SUBSTITUTION OF CONSTANTS OF INTEGRATION IN
C       EQ. SC(12) INTO EQS. S1(8-9).  ALL HAVE NEGATIVE ARGUMENTS.
C+---------------------------------------------------------------------+
C
      REAL     UTAUPR( 2 )
      REAL     GU( MXUMU,MXCMU,* ), KK( MXCMU,* ), LL( MXCMU,* ), MU,
     $         TAUCPR( 0:* ), UMU(*), U0U( MAXUMU,* ), WK(*)
C
C
      UTAUPR(1) = 0.0
      UTAUPR(2) = TAUCPR( NLYR )
      DO 100  LU = 1, 2
         IF ( LU.EQ.1 )  THEN
            IUMIN = NUMU / 2 + 1
            IUMAX = NUMU
            SGN = 1.0
         ELSE
            IUMIN = 1
            IUMAX = NUMU / 2
            SGN = - 1.0
         END IF
C                                   ** LOOP OVER POLAR ANGLES AT WHICH
C                                   ** ALBEDOS/TRANSMISSIVITIES DESIRED
C                                   ** ( UPWARD ANGLES AT TOP BOUNDARY,
C                                   ** DOWNWARD ANGLES AT BOTTOM )
         DO 50  IU = IUMIN, IUMAX
            MU = UMU(IU)
C                                     ** INTEGRATE FROM TOP TO BOTTOM
C                                     ** COMPUTATIONAL LAYER
            PALINT = 0.0
            DO 30  LC = 1, NLYR
C
               DTAU = TAUCPR(LC) - TAUCPR(LC-1)
               EXP1 =  EXP( (UTAUPR(LU) - TAUCPR(LC-1)) / MU )
               EXP2 =  EXP( (UTAUPR(LU) - TAUCPR( LC )) / MU )
C
C                                      ** -KK- IS NEGATIVE
               DO 20  IQ = 1, NN
                  WK(IQ) = EXP( KK(IQ,LC) * DTAU )
                  DENOM = 1.0 + MU * KK(IQ,LC)
                  IF ( ABS(DENOM).LT.0.0001 ) THEN
C                                                   ** L'HOSPITAL LIMIT
                     EXPN = DTAU / MU * EXP2
                  ELSE
                     EXPN = ( EXP1 * WK(IQ) - EXP2 ) * SGN / DENOM
                  END IF
                  PALINT = PALINT + GU(IU,IQ,LC) * LL(IQ,LC) * EXPN
20             CONTINUE
C                                      ** -KK- IS POSITIVE
               DO 21  IQ = NN+1, NSTR
                  DENOM = 1.0 + MU * KK(IQ,LC)
                  IF ( ABS(DENOM).LT.0.0001 ) THEN
                     EXPN = - DTAU / MU * EXP1
                  ELSE
                     EXPN = ( EXP1 - EXP2 * WK(NSTR+1-IQ) ) *SGN / DENOM
                  END IF
                  PALINT = PALINT + GU(IU,IQ,LC) * LL(IQ,LC) * EXPN
21             CONTINUE
C
30          CONTINUE
C
            U0U( IU, LU ) = PALINT
C
 50      CONTINUE
100   CONTINUE
C
      RETURN
      END


      SUBROUTINE  ASYMTX( AA, EVEC, EVAL, M, IA, IEVEC, IER, WK )
C
C    =======  S I N G L E    P R E C I S I O N    V E R S I O N  ======
C
C       SOLVES EIGENFUNCTION PROBLEM FOR REAL ASYMMETRIC MATRIX
C       FOR WHICH IT IS KNOWN A PRIORI THAT THE EIGENVALUES ARE REAL.
C
C           ( C O M M E N T S    O M I T T E D )
C+---------------------------------------------------------------------+
C
      REAL              AA( IA,* ),  WK(*),  EVAL(*), EVEC( IEVEC,* )
      DOUBLE PRECISION  D1MACH2
      LOGICAL           NOCONV, NOTLAS
      DATA     C1/ 0.4375 /, C2/ 0.5 /, C3/ 0.75 /, C4/ 0.95 /,
     $         C5/ 16.0 /, C6/ 256.0 /, ZERO / 0.0 /, ONE / 1.0 /
C
C
      TOL = D1MACH2(4)
      IF ( M.LT.1 .OR. IA.LT.M .OR. IEVEC.LT.M )
     $     CALL ERRMSG( 'ASYMTX--BAD INPUT VARIABLE(S)', .TRUE. )
C
C                           ** HANDLE 1X1 AND 2X2 SPECIAL CASES
      IF ( M.EQ.1 )  THEN
         EVAL(1) = AA(1,1)
         EVEC(1,1) = ONE
         RETURN
C
      ELSE IF ( M.EQ.2 )  THEN
         DISCRI = ( AA(1,1) - AA(2,2) )**2 + 4. * AA(1,2) * AA(2,1)
         IF ( DISCRI.LT.ZERO )
     $        CALL ERRMSG( 'ASYMTX--COMPLEX EVALS IN 2X2 CASE', .TRUE. )
         SGN = ONE
         IF ( AA(1,1).LT.AA(2,2) )  SGN = - ONE
         EVAL(1) = 0.5 * ( AA(1,1) + AA(2,2) + SGN*SQRT(DISCRI) )
         EVAL(2) = 0.5 * ( AA(1,1) + AA(2,2) - SGN*SQRT(DISCRI) )
         EVEC(1,1) = ONE
         EVEC(2,2) = ONE
         IF ( AA(1,1).EQ.AA(2,2) .AND.
     $        (AA(2,1).EQ.ZERO.OR.AA(1,2).EQ.ZERO) )  THEN
            RNORM =   ABS(AA(1,1)) + ABS(AA(1,2)) + ABS(AA(2,1))
     $              + ABS(AA(2,2))
            W = TOL * RNORM
            EVEC(2,1) =   AA(2,1) / W
            EVEC(1,2) = - AA(1,2) / W
         ELSE
            EVEC(2,1) = AA(2,1) / ( EVAL(1) - AA(2,2) )
            EVEC(1,2) = AA(1,2) / ( EVAL(2) - AA(1,1) )
         ENDIF
C
         RETURN
C
      END IF
C                                        ** INITIALIZE OUTPUT VARIABLES
      IER = 0
      DO 20 I = 1, M
         EVAL(I) = ZERO
         DO 10 J = 1, M
            EVEC(I,J) = ZERO
10       CONTINUE
         EVEC(I,I) = ONE
20    CONTINUE
C                  ** BALANCE THE INPUT MATRIX AND REDUCE ITS NORM BY
C                  ** DIAGONAL SIMILARITY TRANSFORMATION STORED IN WK;
C                  ** THEN SEARCH FOR ROWS ISOLATING AN EIGENVALUE
C                  ** AND PUSH THEM DOWN
      RNORM = ZERO
      L  = 1
      K  = M
C
30    KKK = K
         DO 70  J = KKK, 1, -1
            ROW = ZERO
            DO 40 I = 1, K
               IF ( I.NE.J ) ROW = ROW + ABS( AA(J,I) )
40          CONTINUE
            IF ( ROW.EQ.ZERO ) THEN
               WK(K) = J
               IF ( J.NE.K ) THEN
                  DO 50 I = 1, K
                     REPL   = AA(I,J)
                     AA(I,J) = AA(I,K)
                     AA(I,K) = REPL
50                CONTINUE
                  DO 60 I = L, M
                     REPL   = AA(J,I)
                     AA(J,I) = AA(K,I)
                     AA(K,I) = REPL
60                CONTINUE
               END IF
               K = K - 1
               GO TO 30
            END IF
70       CONTINUE
C                                     ** SEARCH FOR COLUMNS ISOLATING AN
C                                       ** EIGENVALUE AND PUSH THEM LEFT
80    LLL = L
         DO 120 J = LLL, K
            COL = ZERO
            DO 90 I = L, K
               IF ( I.NE.J ) COL = COL + ABS( AA(I,J) )
90          CONTINUE
            IF ( COL.EQ.ZERO ) THEN
               WK(L) = J
               IF ( J.NE.L ) THEN
                  DO 100 I = 1, K
                     REPL   = AA(I,J)
                     AA(I,J) = AA(I,L)
                     AA(I,L) = REPL
100               CONTINUE
                  DO 110 I = L, M
                     REPL   = AA(J,I)
                     AA(J,I) = AA(L,I)
                     AA(L,I) = REPL
110               CONTINUE
               END IF
               L = L + 1
               GO TO 80
            END IF
120      CONTINUE
C                           ** BALANCE THE SUBMATRIX IN ROWS L THROUGH K
      DO 130 I = L, K
         WK(I) = ONE
130   CONTINUE
C
140   NOCONV = .FALSE.
         DO 200 I = L, K
            COL = ZERO
            ROW = ZERO
            DO 150 J = L, K
               IF ( J.NE.I ) THEN
                  COL = COL + ABS( AA(J,I) )
                  ROW = ROW + ABS( AA(I,J) )
               END IF
150         CONTINUE
            F = ONE
            G = ROW / C5
            H = COL + ROW
160         IF ( COL.LT.G ) THEN
               F   = F * C5
               COL = COL * C6
               GO TO 160
            END IF
            G = ROW * C5
170         IF ( COL.GE.G ) THEN
               F   = F / C5
               COL = COL / C6
               GO TO 170
            END IF
C                                                         ** NOW BALANCE
            IF ( (COL+ROW) / F .LT. C4 * H ) THEN
               WK(I)  = WK(I) * F
               NOCONV = .TRUE.
               DO 180 J = L, M
                  AA(I,J) = AA(I,J) / F
180            CONTINUE
               DO 190 J = 1, K
                  AA(J,I) = AA(J,I) * F
190            CONTINUE
            END IF
200      CONTINUE
C
      IF ( NOCONV ) GO TO 140
C                                  ** IS -A- ALREADY IN HESSENBERG FORM?
      IF ( K-1.LT.L+1 ) GO TO 350
C                                   ** TRANSFER -A- TO A HESSENBERG FORM
      DO 290 N = L+1, K-1
         H       = ZERO
         WK(N+M) = ZERO
         SCALE   = ZERO
C                                                        ** SCALE COLUMN
         DO 210 I = N, K
            SCALE = SCALE + ABS(AA(I,N-1))
210      CONTINUE
         IF ( SCALE.NE.ZERO ) THEN
            DO 220 I = K, N, -1
               WK(I+M) = AA(I,N-1) / SCALE
               H = H + WK(I+M) * WK(I+M)
220         CONTINUE
            G = - SIGN( SQRT(H),WK(N+M) )
            H = H - WK(N+M) * G
            WK(N+M) = WK(N+M) - G
C                                                 ** FORM (I-(U*UT)/H)*A
            DO 250 J = N, M
               F = ZERO
               DO 230  I = K, N, -1
                  F = F + WK(I+M) * AA(I,J)
230            CONTINUE
               DO 240 I = N, K
                  AA(I,J) = AA(I,J) - WK(I+M) * F / H
240            CONTINUE
250         CONTINUE
C                                    ** FORM (I-(U*UT)/H)*A*(I-(U*UT)/H)
            DO 280 I = 1, K
               F = ZERO
               DO 260  J = K, N, -1
                  F = F + WK(J+M) * AA(I,J)
260            CONTINUE
               DO 270 J = N, K
                  AA(I,J) = AA(I,J) - WK(J+M) * F / H
270            CONTINUE
280         CONTINUE
            WK(N+M)  = SCALE * WK(N+M)
            AA(N,N-1) = SCALE * G
         END IF
290   CONTINUE
C
      DO 340  N = K-2, L, -1
         N1 = N + 1
         N2 = N + 2
         F  = AA(N+1,N)
         IF ( F.NE.ZERO ) THEN
            F  = F * WK(N+1+M)
            DO 300 I = N+2, K
               WK(I+M) = AA(I,N)
300         CONTINUE
            IF ( N+1.LE.K ) THEN
               DO 330 J = 1, M
                  G = ZERO
                  DO 310 I = N+1, K
                     G = G + WK(I+M) * EVEC(I,J)
310               CONTINUE
                  G = G / F
                  DO 320 I = N+1, K
                     EVEC(I,J) = EVEC(I,J) + G * WK(I+M)
320               CONTINUE
330            CONTINUE
            END IF
         END IF
340   CONTINUE
C
350   CONTINUE
      N = 1
      DO 370 I = 1, M
         DO 360 J = N, M
            RNORM = RNORM + ABS(AA(I,J))
360      CONTINUE
         N = I
         IF ( I.LT.L .OR. I.GT.K ) EVAL(I) = AA(I,I)
370   CONTINUE
      N = K
      T = ZERO
C                                         ** SEARCH FOR NEXT EIGENVALUES
380   IF ( N.LT.L ) GO TO 530
      IN = 0
      N1 = N - 1
      N2 = N - 2
C                          ** LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
390   CONTINUE
      DO 400 I = L, N
         LB = N+L - I
         IF ( LB.EQ.L ) GO TO 410
         S = ABS( AA(LB-1,LB-1)) + ABS(AA(LB,LB) )
         IF ( S.EQ.ZERO ) S = RNORM
         IF ( ABS(AA(LB,LB-1)) .LE. TOL * S ) GO TO 410
400   CONTINUE
C                                                      ** ONE EVAL FOUND
410   X = AA(N,N)
      IF ( LB.EQ.N  ) THEN
         AA(N,N)  = X + T
         EVAL(N) = AA(N,N)
         N = N1
         GO TO 380
      END IF
C                                                     ** TWO EVALS FOUND
      Y = AA(N1,N1)
      W = AA(N,N1) * AA(N1,N)
      IF ( LB.EQ.N1 ) THEN
         P = (Y-X) * C2
         Q = P * P + W
         Z = SQRT(ABS(Q))
         AA(N,N) = X + T
         X = AA(N,N)
         AA(N1,N1) = Y + T
C                                                           ** REAL PAIR
         Z = P + SIGN(Z,P)
         EVAL(N1) = X + Z
         EVAL(N)  = EVAL(N1)
         IF ( Z.NE.ZERO ) EVAL(N) = X - W / Z
         X = AA(N,N1)
C                                  ** EMPLOY SCALE FACTOR IN CASE
C                                  ** X AND Z ARE VERY SMALL
         R = SQRT(X * X + Z * Z)
         P = X / R
         Q = Z / R
C                                                    ** ROW MODIFICATION
         DO 420 J = N1, M
            Z = AA(N1,J)
            AA(N1,J) = Q * Z + P * AA(N,J)
            AA(N,J)  = Q * AA(N,J) - P * Z
420      CONTINUE
C                                                 ** COLUMN MODIFICATION
         DO 430 I = 1, N
            Z = AA(I,N1)
            AA(I,N1) = Q * Z + P * AA(I,N)
            AA(I,N)  = Q * AA(I,N) - P * Z
430      CONTINUE
C                                          ** ACCUMULATE TRANSFORMATIONS
         DO 440 I = L, K
            Z = EVEC(I,N1)
            EVEC(I,N1) = Q * Z + P * EVEC(I,N)
            EVEC(I,N)  = Q * EVEC(I,N) - P * Z
440      CONTINUE
         N = N2
         GO TO 380
      END IF
C                    ** NO CONVERGENCE AFTER 30 ITERATIONS; SET ERROR
C                    ** INDICATOR TO THE INDEX OF THE CURRENT EIGENVALUE
C
      IF ( IN.EQ.30 ) THEN
         IER = 128 + N
         RETURN
      END IF
C                                                          ** FORM SHIFT
      IF ( IN.EQ.10 .OR. IN.EQ.20 ) THEN
         T = T + X
         DO 450 I = L, N
            AA(I,I) = AA(I,I) - X
450      CONTINUE
         S = ABS(AA(N,N1)) + ABS(AA(N1,N2))
         X = C3 * S
         Y = X
         W = -C1 * S * S
      END IF
C
      IN = IN + 1
C                ** LOOK FOR TWO CONSECUTIVE SMALL SUB-DIAGONAL ELEMENTS
C
      DO 460 J = LB, N2
         I = N2+LB - J
         Z = AA(I,I)
         R = X - Z
         S = Y - Z
         P = (R * S-W) / AA(I+1,I) + AA(I,I+1)
         Q = AA(I+1,I+1) - Z - R - S
         R = AA(I+2,I+1)
         S = ABS(P) + ABS(Q) + ABS(R)
         P = P / S
         Q = Q / S
         R = R / S
         IF ( I.EQ.LB ) GO TO 470
         UU = ABS( AA(I,I-1) ) * ( ABS(Q) + ABS(R) )
         VV = ABS(P) * ( ABS(AA(I-1,I-1)) + ABS(Z) + ABS(AA(I+1,I+1)) )
         IF ( UU .LE. TOL*VV ) GO TO 470
460   CONTINUE
C
470   CONTINUE
      AA(I+2,I) = ZERO
      DO 480 J = I+3, N
         AA(J,J-2) = ZERO
         AA(J,J-3) = ZERO
480   CONTINUE
C
C             ** DOUBLE QR STEP INVOLVING ROWS K TO N AND COLUMNS M TO N
C
      DO 520 KA = I, N1
         NOTLAS = KA.NE.N1
         IF ( KA.EQ.I ) THEN
            S = SIGN( SQRT( P*P + Q*Q + R*R ),P )
            IF ( LB.NE.I ) AA(KA,KA-1) = - AA(KA,KA-1)
         ELSE
            P = AA(KA,KA-1)
            Q = AA(KA+1,KA-1)
            R = ZERO
            IF ( NOTLAS ) R = AA(KA+2,KA-1)
            X = ABS(P) + ABS(Q) + ABS(R)
            IF ( X.EQ.ZERO ) GO TO 520
            P = P / X
            Q = Q / X
            R = R / X
            S = SIGN( SQRT( P*P + Q*Q + R*R ),P )
            AA(KA,KA-1) = - S * X
         END IF
         P = P + S
         X = P / S
         Y = Q / S
         Z = R / S
         Q = Q / P
         R = R / P
C                                                    ** ROW MODIFICATION
         DO 490 J = KA, M
            P = AA(KA,J) + Q * AA(KA+1,J)
            IF ( NOTLAS ) THEN
               P = P + R * AA(KA+2,J)
               AA(KA+2,J) = AA(KA+2,J) - P * Z
            END IF
            AA(KA+1,J) = AA(KA+1,J) - P * Y
            AA(KA,J)   = AA(KA,J)   - P * X
490      CONTINUE
C                                                 ** COLUMN MODIFICATION
         DO 500 II = 1, MIN0(N,KA+3)
            P = X * AA(II,KA) + Y * AA(II,KA+1)
            IF ( NOTLAS ) THEN
               P = P + Z * AA(II,KA+2)
               AA(II,KA+2) = AA(II,KA+2) - P * R
            END IF
            AA(II,KA+1) = AA(II,KA+1) - P * Q
            AA(II,KA)   = AA(II,KA) - P
500      CONTINUE
C                                          ** ACCUMULATE TRANSFORMATIONS
         DO 510 II = L, K
            P = X * EVEC(II,KA) + Y * EVEC(II,KA+1)
            IF ( NOTLAS ) THEN
               P = P + Z * EVEC(II,KA+2)
               EVEC(II,KA+2) = EVEC(II,KA+2) - P * R
            END IF
            EVEC(II,KA+1) = EVEC(II,KA+1) - P * Q
            EVEC(II,KA)   = EVEC(II,KA) - P
510      CONTINUE
520   CONTINUE
      GO TO 390
C                     ** ALL EVALS FOUND, NOW BACKSUBSTITUTE REAL VECTOR
530   CONTINUE
      IF ( RNORM.NE.ZERO ) THEN
         DO 560  N = M, 1, -1
            N2 = N
            AA(N,N) = ONE
            DO 550  I = N-1, 1, -1
               W = AA(I,I) - EVAL(N)
               IF ( W.EQ.ZERO ) W = TOL * RNORM
               R = AA(I,N)
               DO 540 J = N2, N-1
                  R = R + AA(I,J) * AA(J,N)
540            CONTINUE
               AA(I,N) = -R / W
               N2 = I
550         CONTINUE
560      CONTINUE
C                      ** END BACKSUBSTITUTION VECTORS OF ISOLATED EVALS
C
         DO 580 I = 1, M
            IF ( I.LT.L .OR. I.GT.K ) THEN
               DO 570 J = I, M
                  EVEC(I,J) = AA(I,J)
570            CONTINUE
            END IF
580      CONTINUE
C                                   ** MULTIPLY BY TRANSFORMATION MATRIX
         IF ( K.NE.0 ) THEN
            DO 610  J = M, L, -1
               DO 600 I = L, K
                  Z = ZERO
                  DO 590 N = L, MIN0(J,K)
                     Z = Z + EVEC(I,N) * AA(N,J)
590               CONTINUE
                  EVEC(I,J) = Z
600            CONTINUE
610         CONTINUE
         END IF
C
      END IF
C
      DO 620 I = L, K
         DO 620 J = 1, M
            EVEC(I,J) = EVEC(I,J) * WK(I)
620   CONTINUE
C                           ** INTERCHANGE ROWS IF PERMUTATIONS OCCURRED
      DO 640  I = L-1, 1, -1
         J = WK(I)
         IF ( I.NE.J ) THEN
            DO 630 N = 1, M
               REPL      = EVEC(I,N)
               EVEC(I,N) = EVEC(J,N)
               EVEC(J,N) = REPL
630         CONTINUE
         END IF
640   CONTINUE
C
      DO 660 I = K+1, M
         J = WK(I)
         IF ( I.NE.J ) THEN
            DO 650 N = 1, M
               REPL      = EVEC(I,N)
               EVEC(I,N) = EVEC(J,N)
               EVEC(J,N) = REPL
650         CONTINUE
         END IF
660   CONTINUE
C
      RETURN
      END

      SUBROUTINE  CHEKIN( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $                    WVNMHI, USRTAU, NTAU, UTAU, NSTR, USRANG,
     $                    NUMU, UMU, NPHI, PHI, IBCND, FBEAM, UMU0,
     $                    PHI0, FISOT, LAMBER, ALBEDO11, HL, BTEMP,
     $                    TTEMP, TEMIS, NOPLNK, ONLYFL, ACCUR, MAXCLY,
     $                    MAXULV, MAXUMU, MAXCMU, MAXPHI, MXCLY,
     $                    MXULV,  MXUMU,  MXCMU,  MXPHI, TAUC )
C
C           CHECKS THE INPUT DIMENSIONS AND VARIABLES
C
      LOGICAL  WRTBAD, WRTDIM
      LOGICAL  LAMBER, NOPLNK, ONLYFL, USRANG, USRTAU, INPERR
      INTEGER  IBCND, MAXCLY, MAXUMU, MAXULV, MAXCMU, MAXPHI, NLYR,
     $         NUMU, NSTR, NPHI, NTAU, MXCMU, MXUMU, MXPHI, MXCLY,
     $         MXULV
      REAL     ACCUR, ALBEDO11, BTEMP, DTAUC( MAXCLY ), FBEAM, FISOT,
     $         HL( 0:MAXCMU ), PHI( MAXPHI ), PMOM( 0:MAXCMU, MAXCLY ),
     $         PHI0, SSALB( MAXCLY ), TEMPER( 0:MAXCLY ), TEMIS, TTEMP,
     $         WVNMLO, WVNMHI, UMU( MAXUMU ), UMU0, UTAU( MAXULV ),
     $         TAUC( 0:* )
C
C 
      INPERR = .FALSE.
      IF ( NLYR.LT.1 ) INPERR = WRTBAD( 'NLYR' )
      IF ( NLYR.GT.MAXCLY ) INPERR = WRTBAD( 'MAXCLY' )
C
      DO 10  LC = 1, NLYR
         IF ( DTAUC(LC).LT.0.0 ) INPERR = WRTBAD( 'DTAUC' )
         IF ( SSALB(LC).LT.0.0 .OR. SSALB(LC).GT.1.0 )
     $        INPERR = WRTBAD( 'SSALB' )
         IF ( .NOT.NOPLNK .AND. IBCND.NE.1 )  THEN
            IF( LC.EQ.1 .AND. TEMPER(0).LT.0.0 )
     $          INPERR = WRTBAD( 'TEMPER' )
            IF( TEMPER(LC).LT.0.0 ) INPERR = WRTBAD( 'TEMPER' )
         ENDIF
         DO 5  K = 0, NSTR
            IF( PMOM(K,LC).LT.-1.0 .OR. PMOM(K,LC).GT.1.0 )
     $          INPERR = WRTBAD( 'PMOM' )
 5       CONTINUE
10    CONTINUE
C
      IF ( IBCND.EQ.1 )  THEN
         IF ( MAXULV.LT.2 ) INPERR = WRTBAD( 'MAXULV' )
      ELSE IF ( USRTAU )  THEN
         IF ( NTAU.LT.1 ) INPERR = WRTBAD( 'NTAU' )
         IF ( MAXULV.LT.NTAU ) INPERR = WRTBAD( 'MAXULV' )
         DO 20  LU = 1, NTAU
            IF( ABS(UTAU(LU)-TAUC(NLYR)).LE.1.E-4) UTAU(LU) = TAUC(NLYR)
            IF( UTAU(LU).LT.0.0 .OR. UTAU(LU).GT.TAUC(NLYR) )
     $          INPERR = WRTBAD( 'UTAU' )
20       CONTINUE
      ELSE
         IF ( MAXULV.LT.NLYR+1 ) INPERR = WRTBAD( 'MAXULV' )
      END IF
C
      IF ( NSTR.LT.2 .OR. MOD(NSTR,2).NE.0 ) INPERR = WRTBAD( 'NSTR' )
      IF ( NSTR.GT.MAXCMU ) INPERR = WRTBAD( 'MAXCMU' )
C
      IF ( USRANG )  THEN
         IF ( NUMU.LT.0 ) INPERR = WRTBAD( 'NUMU' )
         IF ( .NOT.ONLYFL .AND. NUMU.EQ.0 ) INPERR = WRTBAD( 'NUMU'  )
         IF ( NUMU.GT.MAXUMU ) INPERR = WRTBAD( 'MAXUMU' )
         IF ( IBCND.EQ.1 .AND. 2*NUMU.GT.MAXUMU )
     $        INPERR = WRTBAD( 'MAXUMU' )
         DO 30  IU = 1, NUMU
            IF( UMU(IU).LT.-1.0 .OR. UMU(IU).GT.1.0 .OR. UMU(IU).EQ.0.0)
     $           INPERR = WRTBAD( 'UMU' )
            IF( IBCND.EQ.1 .AND. UMU(IU).LT.0.0 )
     $           INPERR = WRTBAD( 'UMU' )
            IF( IU.GT.1 .AND. UMU(IU).LT.UMU(IU-1) )
     $           INPERR = WRTBAD( 'UMU' )
30       CONTINUE
      ELSE
         IF( MAXUMU.LT.NSTR ) INPERR = WRTBAD( 'MAXUMU' )
      END IF
C
      IF ( .NOT.ONLYFL .AND. IBCND.NE.1 )  THEN
         IF ( NPHI.LE.0 ) INPERR = WRTBAD( 'NPHI' )
         IF ( NPHI.GT.MAXPHI ) INPERR = WRTBAD( 'MAXPHI' )
         DO 40  J = 1, NPHI
            IF ( PHI(J).LT.0.0 .OR. PHI(J).GT.360.0 )
     $           INPERR = WRTBAD( 'PHI' )
40       CONTINUE
      END IF
C
      IF ( IBCND.LT.0 .OR. IBCND.GT.1 ) INPERR = WRTBAD( 'IBCND' )
      IF ( IBCND.EQ.0 )  THEN
         IF ( FBEAM.LT.0.0 ) INPERR = WRTBAD( 'FBEAM' )
         IF ( FBEAM.GT.0.0 .AND. ( UMU0.LE.0.0 .OR. UMU0.GT.1.0 ) )
     $        INPERR = WRTBAD( 'UMU0' )
            
         IF ( FBEAM.GT.0.0 .AND. ( PHI0.LT.0.0 .OR. PHI0.GT.360.0 ) )
     $        INPERR = WRTBAD( 'PHI0' )
         IF ( FISOT.LT.0.0 ) INPERR = WRTBAD( 'FISOT' )
         IF ( LAMBER )  THEN
            IF ( ALBEDO11.LT.0.0 .OR. ALBEDO11.GT.1.0 )
     $           INPERR = WRTBAD( 'ALBEDO' )
                 
         ELSE
C                    ** MAKE SURE FLUX ALBEDO AT DENSE MESH OF INCIDENT
C                    ** ANGLES DOES NOT ASSUME UNPHYSICAL VALUES
C
            DO 50  RMU = 0.0, 1.0, 0.01
               FLXALB = DREF( RMU, HL, NSTR )
               IF ( FLXALB.LT.0.0 .OR. FLXALB.GT.1.0 )
     $              INPERR = WRTBAD( 'HL' )
50          CONTINUE
         ENDIF
C
      ELSE IF ( IBCND.EQ.1 )  THEN
         IF ( ALBEDO11.LT.0.0 .OR. ALBEDO11.GT.1.0 )
     $        INPERR = WRTBAD( 'ALBEDO' )
                
      END IF
C
      IF ( .NOT.NOPLNK .AND. IBCND.NE.1 )  THEN
         IF ( WVNMLO.LT.0.0 .OR. WVNMHI.LE.WVNMLO )
     $        INPERR = WRTBAD( 'WVNMLO,HI' )
         IF ( TEMIS.LT.0.0 .OR. TEMIS.GT.1.0 )
     $        INPERR = WRTBAD( 'TEMIS' )
         IF ( BTEMP.LT.0.0 ) INPERR = WRTBAD( 'BTEMP' )
         IF ( TTEMP.LT.0.0 ) INPERR = WRTBAD( 'TTEMP' )
      END IF
C
      IF ( ACCUR.LT.0.0 .OR. ACCUR.GT.1.E-2 )
     $     INPERR = WRTBAD( 'ACCUR' )
C
      IF ( MXCLY.LT.NLYR ) INPERR = WRTDIM( 'MXCLY', NLYR )
      IF ( IBCND.NE.1 )  THEN
         IF ( USRTAU .AND. MXULV.LT.NTAU )
     $        INPERR = WRTDIM( 'MXULV', NTAU )
         IF ( .NOT.USRTAU .AND. MXULV.LT.NLYR+1 )
     $        INPERR = WRTDIM( 'MXULV', NLYR+1 )
      ELSE
         IF ( MXULV.LT.2 ) INPERR = WRTDIM( 'MXULV', 2 )
      END IF
      IF ( MXCMU.LT.NSTR ) INPERR = WRTDIM( 'MXCMU', NSTR )
      IF ( USRANG .AND. MXUMU.LT.NUMU )
     $     INPERR = WRTDIM( 'MXUMU', NUMU )
      IF ( USRANG .AND. IBCND.EQ.1 .AND. MXUMU.LT.2*NUMU )
     $     INPERR = WRTDIM( 'MXUMU', NUMU )
      IF ( .NOT.USRANG .AND. MXUMU.LT.NSTR )
     $      INPERR = WRTDIM( 'MXUMU', NSTR )
      IF ( .NOT.ONLYFL .AND. IBCND.NE.1 .AND. MXPHI.LT.NPHI )
     $      INPERR = WRTDIM( 'MXPHI', NPHI )
C
      IF ( INPERR )
     $   CALL ERRMSG( 'DISORT--INPUT AND/OR DIMENSION ERRORS', .TRUE. )
C
      DO 100  LC = 1, NLYR
         IF ( .NOT.NOPLNK .AND. ABS(TEMPER(LC)-TEMPER(LC-1)) .GT. 10.0 )
     $          CALL ERRMSG( 'CHEKIN--VERTICAL TEMPERATURE STEP MAY'
     $          // ' BE TOO LARGE FOR GOOD ACCURACY', .FALSE. )
100   CONTINUE
C
      RETURN
      END
      SUBROUTINE  CMPINT( FBEAM, GC, KK, LAYRU, LL, LYRCUT, MAZ,
     $                    MXCMU, MXULV, MXUMU, NCUT, NN, NSTR,
     $                    NOPLNK, NTAU, TAUCPR, UMU0, UTAUPR,
     $                    ZZ, ZPLK0, ZPLK1, UUM )
C
C       CALCULATES THE FOURIER INTENSITY COMPONENTS AT THE QUADRATURE
C       ANGLES FOR AZIMUTHAL EXPANSION TERMS (MAZ) IN EQ. SD(2)
C
C    I N P U T    V A R I A B L E S:
C
C       KK      :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       GC      :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       LL      :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                  BY SOLVING SCALED VERSION OF EQ. SC(5);
C                  EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       LYRCUT  :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       MAZ     :  ORDER OF AZIMUTHAL COMPONENT
C       NCUT    :  NUMBER OF COMPUTATIONAL LAYER WHERE ABSORPTION
C                  OPTICAL DEPTH EXCEEDS -ABSCUT-
C       NN      :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       TAUCPR  :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       UTAUPR  :  OPTICAL DEPTHS OF USER OUTPUT LEVELS IN DELTA-M
C                  COORDINATES;  EQUAL TO -UTAU- IF NO DELTA-M
C       ZZ      :  BEAM SOURCE VECTORS IN EQ. SS(19)
C       ZPLK0   :  THERMAL SOURCE VECTORS -Z0-, BY SOLVING EQ. SS(16)
C       ZPLK1   :  THERMAL SOURCE VECTORS -Z1-, BY SOLVING EQ. SS(16)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C    O U T P U T   V A R I A B L E S:
C
C       UUM     :  FOURIER COMPONENTS OF THE INTENSITY IN EQ.  SD(12)
C                   ( AT POLAR QUADRATURE ANGLES )
C
C    I N T E R N A L   V A R I A B L E S:
C
C       FACT    :  EXP( - UTAUPR / UMU0 )
C       ZINT    :  INTENSITY OF M=0 CASE, IN EQ. SC(1)
C+----------------------------------------------------------------------
C
       LOGICAL  LYRCUT, NOPLNK
       INTEGER  LAYRU(*)
       REAL     UUM( MXUMU, MXULV, 0:* )
       REAL     GC( MXCMU,MXCMU,* ), KK( MXCMU,* ), LL( MXCMU,* ),
     $          TAUCPR( 0:* ), UTAUPR(*), ZZ( MXCMU, *),
     $          ZPLK0( MXCMU,* ), ZPLK1( MXCMU,* )
C
C
C                                                  ** ZERO OUTPUT ARRAY
       CALL ZEROIT( UUM, MXUMU*MXULV*(MXCMU + 1) )
C
C                                       ** LOOP OVER USER LEVELS
       DO 100  LU = 1, NTAU
C
          LYU = LAYRU(LU)
          IF ( LYRCUT .AND. LYU.GT.NCUT )  GO TO 100
C
          DO 20  IQ = 1, NSTR
             ZINT = 0.0
             DO 10  JQ = 1, NN
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $                    EXP( - KK(JQ,LYU)*(UTAUPR(LU) - TAUCPR(LYU)) )
10           CONTINUE
             DO 11  JQ = NN+1, NSTR
                ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $                  EXP( - KK(JQ,LYU)*(UTAUPR(LU) - TAUCPR(LYU-1)) )
11           CONTINUE
C
             UUM(IQ,LU,MAZ) = ZINT
             IF ( FBEAM.GT.0.0 )
     $            UUM(IQ,LU,MAZ) = ZINT + ZZ(IQ,LYU)
     $                                    * EXP( - UTAUPR(LU) / UMU0 )
             IF ( .NOT.NOPLNK .AND. MAZ.EQ.0 )
     $            UUM(IQ,LU,MAZ) = UUM(IQ,LU,MAZ) + ZPLK0(IQ,LYU) +
     $                             ZPLK1(IQ,LYU) * UTAUPR(LU)
20        CONTINUE
C
100   CONTINUE
C
      RETURN
      END

      DOUBLE PRECISION FUNCTION D1MACH2(I)
C
C  DOUBLE-PRECISION MACHINE CONSTANTS (SEE R1MACH FOR DOCUMENTATION)
C
C  FOR IEEE-ARITHMETIC MACHINES (BINARY STANDARD), ONE OF THE FIRST
C  TWO SETS OF CONSTANTS BELOW SHOULD BE APPROPRIATE.
C
      INTEGER SMALL(4), LARGE(4), RIGHT(4), DIVER(4), LOG10(4), SC
      DOUBLE PRECISION DMACH(5), EPS, EPSNEW, S

      EQUIVALENCE (DMACH(1),SMALL(1)), (DMACH(2),LARGE(1)),
     $            (DMACH(3),RIGHT(1)), (DMACH(4),DIVER(1)),
     $            (DMACH(5),LOG10(1))

      LOGICAL  PASS1
      SAVE     PASS1
      DATA     PASS1/.TRUE./

C IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T 3B SERIES AND
C MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T PC 7300),
C IN WHICH THE MOST SIGNIFICANT BYTE IS STORED FIRST.

c     DATA (SMALL(N),N=1,2)/1048576,0/, (LARGE(N),N=1,2)/2146435071,-1/,
c    $  (RIGHT(N),N=1,2)/1017118720,0/, (DIVER(N),N=1,2)/1018167296,0/,
c    $  (LOG10(N),N=1,2)/1070810131,1352628735/, SC/987/

C IEEE ARITHMETIC MACHINES AND 8087-BASED MICROS, SUCH AS THE IBM PC
C AND AT&T 6300, IN WHICH THE LEAST SIGNIFICANT BYTE IS STORED FIRST.

      DATA (SMALL(N),N=1,2)/0,1048576/, (LARGE(N),N=1,2)/-1,2146435071/,
     $  (RIGHT(N),N=1,2)/0,1017118720/, (DIVER(N),N=1,2)/0,1018167296/,
     $  (LOG10(N),N=1,2)/1352628735,1070810131/, SC/987/

C AMDAHL MACHINES.

C      DATA (SMALL(N),N=1,2)/1048576,0/, (LARGE(N),N=1,2)/2147483647,-1/,
C     $ (RIGHT(N),N=1,2)/856686592,0/, (DIVER(N),N=1,2)/ 873463808,0/,
C     $ (LOG10(N),N=1,2)/1091781651,1352628735/, SC/987/

C BURROUGHS 1700 SYSTEM.

C      DATA (SMALL(N),N=1,2)/ZC00800000,Z000000000/,
C     $ (LARGE(N),N=1,2)/ZDFFFFFFFF,ZFFFFFFFFF/,
C     $ (RIGHT(N),N=1,2)/ZCC5800000,Z000000000/,
C     $ (DIVER(N),N=1,2)/ZCC6800000,Z000000000/,
C     $ (LOG10(N),N=1,2)/ZD00E730E7,ZC77800DC0/, SC/987/

C BURROUGHS 5700 SYSTEM.

C      DATA (SMALL(N),N=1,2)/O1771000000000000,O0000000000000000/,
C     $  (LARGE(N),N=1,2)/O0777777777777777,O0007777777777777/,
C     $  (RIGHT(N),N=1,2)/O1461000000000000,O0000000000000000/,
C     $  (DIVER(N),N=1,2)/O1451000000000000,O0000000000000000/,
C     $  (LOG10(N),N=1,2)/O1157163034761674,O0006677466732724/, SC/987/

C BURROUGHS 6700/7700 SYSTEMS.

C      DATA (SMALL(N),N=1,2)/O1771000000000000,O7770000000000000/,
C     $  (LARGE(N),N=1,2)/O0777777777777777,O7777777777777777/,
C     $  (RIGHT(N),N=1,2)/O1461000000000000,O0000000000000000/,
C     $  (DIVER(N),N=1,2)/O1451000000000000,O0000000000000000/,
C     $  (LOG10(N),N=1,2)/O1157163034761674,O0006677466732724/, SC/987/

C FTN4 ON THE CDC 6000/7000 SERIES.

C      DATA
C     $  (SMALL(N),N=1,2)/00564000000000000000B,00000000000000000000B/,
C     $  (LARGE(N),N=1,2)/37757777777777777777B,37157777777777777774B/,
C     $  (RIGHT(N),N=1,2)/15624000000000000000B,00000000000000000000B/,
C     $  (DIVER(N),N=1,2)/15634000000000000000B,00000000000000000000B/,
C     $  (LOG10(N),N=1,2)/17164642023241175717B,16367571421742254654B/,
C     $  SC/987/

C FTN5 ON THE CDC 6000/7000 SERIES.

C      DATA
C     $(SMALL(N),N=1,2)/O"00564000000000000000",O"00000000000000000000"/,
C     $(LARGE(N),N=1,2)/O"37757777777777777777",O"37157777777777777774"/,
C     $(RIGHT(N),N=1,2)/O"15624000000000000000",O"00000000000000000000"/,
C     $(DIVER(N),N=1,2)/O"15634000000000000000",O"00000000000000000000"/,
C     $(LOG10(N),N=1,2)/O"17164642023241175717",O"16367571421742254654"/,
C     $ SC/987/

C CONVEX C-1

C      DATA (SMALL(N),N=1,2)/'00100000'X,'00000000'X/,
C     $  (LARGE(N),N=1,2)/'7FFFFFFF'X,'FFFFFFFF'X/,
C     $  (RIGHT(N),N=1,2)/'3CC00000'X,'00000000'X/,
C     $  (DIVER(N),N=1,2)/'3CD00000'X,'00000000'X/,
C     $  (LOG10(N),N=1,2)/'3FF34413'X,'509F79FF'X/, SC/987/

C CRAY 1, XMP, 2, AND 3.

C      DATA
C     $ (SMALL(N),N=1,2)/201354000000000000000B,000000000000000000000B/,
C     $ (LARGE(N),N=1,2)/577767777777777777777B,000007777777777777776B/,
C     $ (RIGHT(N),N=1,2)/376434000000000000000B,000000000000000000000B/,
C     $ (DIVER(N),N=1,2)/376444000000000000000B,000000000000000000000B/,
C     $ (LOG10(N),N=1,2)/377774642023241175717B,000007571421742254654B/,
C     $ SC/987/

C DATA GENERAL ECLIPSE S/200
C NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING LINE -
C STATIC DMACH(5)

C      DATA SMALL/20K,3*0/, LARGE/77777K,3*177777K/,
C     $  RIGHT/31420K,3*0/, DIVER/32020K,3*0/,
C     $  LOG10/40423K,42023K,50237K,74776K/, SC/987/

C HARRIS SLASH 6 AND SLASH 7

C      DATA (SMALL(N),N=1,2)/'20000000,'00000201/,
C     $  (LARGE(N),N=1,2)/'37777777,'37777577/,
C     $  (RIGHT(N),N=1,2)/'20000000,'00000333/,
C     $  (DIVER(N),N=1,2)/'20000000,'00000334/,
C     $  (LOG10(N),N=1,2)/'23210115,'10237777/, SC/987/

C HONEYWELL DPS 8/70 SERIES.

C      DATA (SMALL(N),N=1,2)/O402400000000,O000000000000/,
C     $  (LARGE(N),N=1,2)/O376777777777,O777777777777/,
C     $  (RIGHT(N),N=1,2)/O604400000000,O000000000000/,
C     $  (DIVER(N),N=1,2)/O606400000000,O000000000000/,
C     $  (LOG10(N),N=1,2)/O776464202324,O117571775714/, SC/987/

C IBM 360/370 SERIES, XEROX SIGMA 5/7/9 AND THE SEL SYSTEMS 85/86.

C      DATA (SMALL(N),N=1,2)/Z00100000,Z00000000/,
C     $  (LARGE(N),N=1,2)/Z7FFFFFFF,ZFFFFFFFF/,
C     $  (RIGHT(N),N=1,2)/Z33100000,Z00000000/,
C     $  (DIVER(N),N=1,2)/Z34100000,Z00000000/,
C     $  (LOG10(N),N=1,2)/Z41134413,Z509F79FF/, SC/987/

C INTERDATA 8/32 WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
C FOR THE INTERDATA FORTRAN VII COMPILER REPLACE
C THE Z'S SPECIFYING HEX CONSTANTS WITH Y'S.

C      DATA (SMALL(N),N=1,2)/Z'00100000',Z'00000000'/,
C     $  (LARGE(N),N=1,2)/Z'7EFFFFFF',Z'FFFFFFFF'/,
C     $  (RIGHT(N),N=1,2)/Z'33100000',Z'00000000'/,
C     $  (DIVER(N),N=1,2)/Z'34100000',Z'00000000'/,
C     $  (LOG10(N),N=1,2)/Z'41134413',Z'509F79FF'/, SC/987/

C PDP-10 (KA PROCESSOR).

C      DATA (SMALL(N),N=1,2)/"033400000000,"000000000000/,
C     $  (LARGE(N),N=1,2)/"377777777777,"344777777777/,
C     $  (RIGHT(N),N=1,2)/"113400000000,"000000000000/,
C     $  (DIVER(N),N=1,2)/"114400000000,"000000000000/,
C     $  (LOG10(N),N=1,2)/"177464202324,"144117571776/, SC/987/

C PDP-10 (KI PROCESSOR).

C      DATA (SMALL(N),N=1,2)/"000400000000,"000000000000/,
C     $  (LARGE(N),N=1,2)/"377777777777,"377777777777/,
C     $  (RIGHT(N),N=1,2)/"103400000000,"000000000000/,
C     $  (DIVER(N),N=1,2)/"104400000000,"000000000000/,
C     $  (LOG10(N),N=1,2)/"177464202324,"047674776746/, SC/987/

C PDP-11 FORTRANS SUPPORTING 32-BIT INTEGERS
C (EXPRESSED IN INTEGER AND OCTAL).

C      DATA (SMALL(N),N=1,2)/8388608,0/, (LARGE(N),N=1,2)/2147483647,-1/,
C     $  (RIGHT(N),N=1,2)/612368384,0/, (DIVER(N),N=1,2)/620756992,0/,
C     $  (LOG10(N),N=1,2)/1067065498,-2063872008/, SC/987/

C      DATA (SMALL(N),N=1,2)/O00040000000,O00000000000/,
C     $  (LARGE(N),N=1,2)/O17777777777,O37777777777/,
C     $  (RIGHT(N),N=1,2)/O04440000000,O00000000000/,
C     $  (DIVER(N),N=1,2)/O04500000000,O00000000000/,
C     $  (LOG10(N),N=1,2)/O07746420232,O20476747770/, SC/987/

C PDP-11 FORTRANS SUPPORTING 16-BIT INTEGERS
C (EXPRESSED IN INTEGER AND OCTAL).

C      DATA SMALL/128,3*0/, LARGE/32767,3*-1/, RIGHT/9344,3*0/,
C     $  DIVER/9472,3*0/, LOG10/16282,8346,-31493,-12296/, SC/987/

C      DATA SMALL/O000200,3*O000000/, LARGE/O077777,3*O177777/,
C     $  RIGHT/O022200,3*O000000/, DIVER/O022400,3*O000000/,
C     $  LOG10/O037632,O020232,O102373,O147770/, SC/987/

C PRIME 50 SERIES SYSTEMS WITH 32-BIT INTEGERS AND 64V MODE
C INSTRUCTIONS, SUPPLIED BY IGOR BRAY.

C      DATA (SMALL(N),N=1,2)/:10000000000,:00000100001/,
C     $  (LARGE(N),N=1,2)/:17777777777,:37777677775/,
C     $  (RIGHT(N),N=1,2)/:10000000000,:00000000122/,
C     $  (DIVER(N),N=1,2)/:10000000000,:00000000123/,
C     $  (LOG10(N),N=1,2)/:11504046501,:07674600177/, SC/987/

C SEQUENT BALANCE 8000

C      DATA (SMALL(N),N=1,2)/$00000000, $00100000/,
C     $  (LARGE(N),N=1,2)/$FFFFFFFF, $7FEFFFFF/,
C     $  (RIGHT(N),N=1,2)/$00000000, $3CA00000/,
C     $  (DIVER(N),N=1,2)/$00000000, $3CB00000/,
C     $  (LOG10(N),N=1,2)/$509F79FF, $3FD34413/, SC/987/

C UNIVAC 1100 SERIES.

C      DATA (SMALL(N),N=1,2)/O000040000000,O000000000000/,
C     $  (LARGE(N),N=1,2)/O377777777777,O777777777777/,
C     $  (RIGHT(N),N=1,2)/O170540000000,O000000000000/,
C     $  (DIVER(N),N=1,2)/O170640000000,O000000000000/,
C     $  (LOG10(N),N=1,2)/O177746420232,O411757177572/, SC/987/

C VAX UNIX F77 COMPILER

C      DATA (SMALL(N),N=1,2)/128,0/, (LARGE(N),N=1,2)/-32769,-1/,
C     $  (RIGHT(N),N=1,2)/9344,0/, (DIVER(N),N=1,2)/9472,0/,
C     $  (LOG10(N),N=1,2)/546979738,-805796613/, SC/987/

C VAX-11 WITH FORTRAN IV-PLUS COMPILER

C      DATA (SMALL(N),N=1,2)/Z00000080,Z00000000/,
C     $  (LARGE(N),N=1,2)/ZFFFF7FFF,ZFFFFFFFF/,
C     $  (RIGHT(N),N=1,2)/Z00002480,Z00000000/,
C     $  (DIVER(N),N=1,2)/Z00002500,Z00000000/,
C     $  (LOG10(N),N=1,2)/Z209A3F9A,ZCFF884FB/, SC/987/

C VAX/VMS VERSION 2.2

C      DATA (SMALL(N),N=1,2)/'80'X,'0'X/,
C     $  (LARGE(N),N=1,2)/'FFFF7FFF'X,'FFFFFFFF'X/,
C     $  (RIGHT(N),N=1,2)/'2480'X,'0'X/, (DIVER(N),N=1,2)/'2500'X,'0'X/,
C     $  (LOG10(N),N=1,2)/'209A3F9A'X,'CFF884FB'X/, SC/987/

C      DMACH(1)=2.22507386E-308  
C      DMACH(2)=1.79769313E+308  
      DMACH(3)=1.11022302E-16  
      DMACH(4)=2.22044605E-16
      DMACH(1)=2.3D-308  
      DMACH(2)=1.7D+308


      IF( PASS1 )  THEN

         IF (SC.NE.987)
     $       CALL ERRMSG( 'D1MACH--NO DATA STATEMENTS ACTIVE',.TRUE.)

C                        ** CALCULATE MACHINE PRECISION
         EPSNEW = 0.01D0
   10    EPS = EPSNEW
            EPSNEW = EPSNEW / 1.1D0
C                               ** IMPORTANT TO STORE 'S' SINCE MAY BE
C                               ** KEPT IN HIGHER PRECISION IN REGISTERS
            S = 1.D0 + EPSNEW
            IF( S.GT.1.D0 ) GO TO 10
         IF( EPS/DMACH(4).LT.0.5D0 .OR. EPS/DMACH(4).GT.2.D0 )
     $       CALL ERRMSG( 'D1MACH--TABULATED PRECISION WRONG',.TRUE.)

      END IF

      IF (I.LT.1.OR.I.GT.5)
     $    CALL ERRMSG( 'D1MACH--ARGUMENT OUT OF BOUNDS',.TRUE.)
      D1MACH2 = DMACH(I)
      RETURN
      END

      SUBROUTINE  DISORT( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $                    WVNMHI, USRTAU, NTAU, UTAU, NSTR, USRANG,
     $                    NUMU, UMU, NPHI, PHI, IBCND, FBEAM, UMU0,
     $                    PHI0, FISOT, LAMBER, ALBEDO, HL, BTEMP,
     $                    TTEMP, TEMIS, DELTAM, NOPLNK, ONLYFL,
     $                    ACCUR, PRNT, HEADER, MAXCLY, MAXULV,
     $                    MAXUMU, MAXCMU, MAXPHI, RFLDIR, RFLDN,
     $                    FLUP, DFDT, UAVG, UU, U0U, ALBMED, TRNMED,
     $                    VAL,MXCLY,MXULV,MXCMU,MXUMU,MXPHI,
     $                    MI, MI9M2 , NNLYRI ,
     $                    IPVT,LAYRU,ALBSAV,AMB, APB, ARRAY, B, BDR,
     $                    BEM, CBAND, CC, CMU, CWT, EMU, EVAL,
     $                    EVECC, EXPBEA, FLYR, FLDN, FLDIR, GL,
     $                    GC, GU, HLPR, KK, LL, OPRIM, PHIRAD, PKAG,
     $                    PSI, RMU, TAUC, TAUCPR, U0C, UTAUPR,
     $                    UUM, WK, XR0,XR1, YLM0, YLMC, YLMU, Z, Z0,
     $                    Z0U, Z1, Z1U, ZJ, ZZ, ZPLK0, ZPLK1, ZBEAM)
 
C
C    MODIF : PASS1=FALSE
C            EPSIL
C            TEST SUR SSALB
C            RPD+PI+EPSIL DEVANT IF(PASS1...
C            SETDIS  ARGUMENT MXCLY
C            ARGUMENT VAL
C            REPERAGE PAR LES NIVEAUX
C            PARAMETRES DEFINIS DAND LE PROGRAMME PRINCIPAL
C **********************************************************************
C       PLANE-PARALLEL DISCRETE ORDINATES RADIATIVE TRANSFER PROGRAM
C             ( SEE DISORT.DOC FOR COMPLETE DOCUMENTATION )
C **********************************************************************
C
C+---------------------------------------------------------------------+
C------------------    I/O VARIABLE SPECIFICATIONS     -----------------
C+---------------------------------------------------------------------+
C
      CHARACTER  HEADER*127
      LOGICAL  DELTAM, LAMBER, NOPLNK, ONLYFL, PRNT(7), USRANG, USRTAU
      INTEGER  IBCND, MAXCLY, MAXUMU, MAXULV, MAXCMU, MAXPHI, NLYR,
     $         NUMU, NSTR, NPHI, NTAU
      REAL     ACCUR, ALBEDO, BTEMP, DTAUC( MAXCLY ), FBEAM, FISOT,
     $         HL( 0:MAXCMU ), PHI( MAXPHI ), PMOM( 0:MAXCMU, MAXCLY ),
     $         PHI0, SSALB( MAXCLY ), TEMPER( 0:MAXCLY ), TEMIS, TTEMP,
     $         WVNMLO, WVNMHI, UMU( MAXUMU ), UMU0, UTAU( MAXULV )
C
      REAL     RFLDIR( MAXULV ), RFLDN( MAXULV ), FLUP( MAXULV ),
     $         UAVG( MAXULV ), DFDT( MAXULV ), U0U( MAXUMU, MAXULV ),
     $         UU( MAXUMU, MAXULV, MAXPHI ), ALBMED( MAXUMU ),
     $         TRNMED( MAXUMU )
C
C+---------------------------------------------------------------------+
C      ROUTINES CALLED (IN ORDER):  SLFTST, ZEROAL, CHEKIN, SETDIS,
C                                   PRTINP, ALBTRN, LEPOLY, SURFAC,
C                                   SOLEIG, UPBEAM, UPISOT, TERPEV,
C                                   TERPSO, SETMTX, SOLVE0, FLUXES,
C                                   USRINT, PRAVIN, PRTINT
C+---------------------------------------------------------------------+
C
C  INDEX CONVENTIONS (FOR ALL DO-LOOPS AND ALL VARIABLE DESCRIPTIONS):
C
C     IU     :  FOR USER POLAR ANGLES
C
C  IQ,JQ,KQ  :  FOR COMPUTATIONAL POLAR ANGLES ('QUADRATURE ANGLES')
C
C   IQ/2     :  FOR HALF THE COMPUTATIONAL POLAR ANGLES (JUST THE ONES
C               IN EITHER 0-90 DEGREES, OR 90-180 DEGREES)
C
C     J      :  FOR USER AZIMUTHAL ANGLES
C
C     K,L    :  FOR LEGENDRE EXPANSION COEFFICIENTS OR, ALTERNATIVELY,
C               SUBSCRIPTS OF ASSOCIATED LEGENDRE POLYNOMIALS
C
C     LU     :  FOR USER LEVELS
C
C     LC     :  FOR COMPUTATIONAL LAYERS (EACH HAVING A DIFFERENT
C               SINGLE-SCATTER ALBEDO AND/OR PHASE FUNCTION)
C
C    LEV     :  FOR COMPUTATIONAL LEVELS
C
C    MAZ     :  FOR AZIMUTHAL COMPONENTS IN FOURIER COSINE EXPANSION
C               OF INTENSITY AND PHASE FUNCTION
C
C+---------------------------------------------------------------------+
C               I N T E R N A L    V A R I A B L E S
C
C   ALBSAV(IU)        TEMPORARY STORAGE FOR MEDIUM ALBEDO IN IBCND = 1
C                     SPECIAL CASE
C
C   AMB(IQ/2,IQ/2)    FIRST MATRIX FACTOR IN REDUCED EIGENVALUE PROBLEM
C                     OF EQS. SS(12), STWJ(8E)  (USED ONLY IN 'SOLEIG')
C
C   APB(IQ/2,IQ/2)    SECOND MATRIX FACTOR IN REDUCED EIGENVALUE PROBLEM
C                     OF EQS. SS(12), STWJ(8E)  (USED ONLY IN 'SOLEIG')
C
C   ARRAY(IQ,IQ)      SCRATCH MATRIX FOR 'SOLEIG', 'UPBEAM' AND 'UPISOT'
C                     (SEE EACH SUBROUTINE FOR DEFINITION)
C
C   B()               RIGHT-HAND SIDE VECTOR OF EQ. SC(5) GOING INTO
C                     *SOLVE0,1*;  RETURNS AS SOLUTION VECTOR
C                     VECTOR CAPITAL-L, THE CONSTANTS OF INTEGRATION
C
C   BDR(IQ/2,0:IQ/2)  BOTTOM-BOUNDARY BIDIRECTIONAL REFLECTIVITY FOR A
C                     GIVEN AZIMUTHAL COMPONENT.  FIRST INDEX ALWAYS
C                     REFERS TO A COMPUTATIONAL ANGLE.  SECOND INDEX:
C                     IF ZERO, REFERS TO INCIDENT BEAM ANGLE -UMU0-;
C                     IF NON-ZERO, REFERS TO A COMPUTATIONAL ANGLE.
C
C   BEM(IQ/2)         BOTTOM-BOUNDARY DIRECTIONAL EMISSIVITY AT COMPU-
C                     TATIONAL ANGLES.
C
C   BPLANK            INTENSITY EMITTED FROM BOTTOM BOUNDARY
C
C   CBAND()           MATRIX OF LEFT-HAND SIDE OF THE LINEAR SYSTEM
C                     EQ. SC(5), SCALED BY EQ. SC(12);  IN BANDED
C                     FORM REQUIRED BY LINPACK SOLUTION ROUTINES
C
C   CC(IQ,IQ)         CAPITAL-C-SUB-IJ IN EQ. SS(5)
C
C   CMU(IQ)           COMPUTATIONAL POLAR ANGLES (GAUSSIAN)
C
C   CWT(IQ)           QUADRATURE WEIGHTS CORRESP. TO -CMU-
C
C   DELM0             KRONECKER DELTA, DELTA-SUB-M0, WHERE 'M' = MAZ
C                     IS THE NUMBER OF THE FOURIER COMPONENT IN THE
C                     AZIMUTH COSINE EXPANSION
C
C   EMU(IU)           BOTTOM-BOUNDARY DIRECTIONAL EMISSIVITY AT USER
C                     ANGLES.
C
C   EVAL(IQ)          TEMPORARY STORAGE FOR EIGENVALUES OF EQ. SS(12)
C
C   EVECC(IQ,IQ)      COMPLETE EIGENVECTORS OF SS(7) ON RETURN FROM
C                     *SOLEIG* ; STORED PERMANENTLY IN -GC-
C
C   EXPBEA(LC)        TRANSMISSION OF DIRECT BEAM IN DELTA-M OPTICAL
C                     DEPTH COORDINATES
C
C   FLYR(LC)          TRUNCATED FRACTION IN DELTA-M METHOD
C
C   GL(K,LC)          PHASE FUNCTION LEGENDRE POLY. EXPANSION
C                     COEFFICIENTS, CALCULATED FROM 'PMOM' BY
C                     INCLUDING SINGLE-SCATTERING ALBEDO, FACTOR
C                     2K+1, AND (IF DELTAM=TRUE) THE DELTA-M
C                     SCALING
C
C   GC(IQ,IQ,LC)      EIGENVECTORS AT POLAR QUADRATURE ANGLES,
C                     LITTLE-G  IN EQ. SC(1)
C
C   GU(IU,IQ,LC)      EIGENVECTORS INTERPOLATED TO USER POLAR ANGLES
C                     ( LITTLE-G  IN EQS. SC(3) AND S1(8-9), i.e.
C                     CAPITAL-G WITHOUT THE CAPITAL-L FACTOR )
C
C   HLPR()            LEGENDRE COEFFICIENTS OF BOTTOM BIDIRECTIONAL
C                     REFLECTIVITY (AFTER INCLUSION OF 2K+1 FACTOR)
C
C   IPVT(LC*IQ)       INTEGER VECTOR OF PIVOT INDICES FOR LINPACK
C                     ROUTINES
C
C   KK(IQ,LC)         EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C
C   KCONV             COUNTER IN AZIMUTH CONVERGENCE TEST
C
C   LAYRU(LU)         COMPUTATIONAL LAYER IN WHICH USER OUTPUT LEVEL
C                     -UTAU(LU)- IS LOCATED
C
C   LL(IQ,LC)         CONSTANTS OF INTEGRATION CAPITAL-L IN EQ. SC(1),
C                     OBTAINED BY SOLVING SCALED VERSION OF EQ. SC(5)
C
C   LYRCUT            TRUE, RADIATION IS ASSUMED ZERO BELOW LAYER
C                     -NCUT- BECAUSE OF ALMOST COMPLETE ABSORPTION
C
C   NAZ               NUMBER OF AZIMUTHAL COMPONENTS CONSIDERED
C
C   NCUT              COMPUTATIONAL LAYER NUMBER IN WHICH ABSORPTION
C                     OPTICAL DEPTH FIRST EXCEEDS -ABSCUT-
C
C   OPRIM(LC)         SINGLE SCATTERING ALBEDO AFTER DELTA-M SCALING
C
C   PASS1             TRUE ON FIRST ENTRY, FALSE THEREAFTER
C
C   PKAG(0:LC)        INTEGRATED PLANCK FUNCTION FOR INTERNAL EMISSION
C
C   PSI(IQ)           SUM JUST AFTER SQUARE BRACKET IN  EQ. SD(9)
C
C   RMU(IU,0:IQ)      BOTTOM-BOUNDARY BIDIRECTIONAL REFLECTIVITY FOR A
C                     GIVEN AZIMUTHAL COMPONENT.  FIRST INDEX ALWAYS
C                     REFERS TO A USER ANGLE.  SECOND INDEX:
C                     IF ZERO, REFERS TO INCIDENT BEAM ANGLE -UMU0-;
C                     IF NON-ZERO, REFERS TO A COMPUTATIONAL ANGLE.
C
C   TAUC(0:LC)        CUMULATIVE OPTICAL DEPTH (UN-DELTA-M-SCALED)
C
C   TAUCPR(0:LC)      CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED IF
C                     DELTAM = TRUE, OTHERWISE EQUAL TO -TAUC-)
C
C   TPLANK            INTENSITY EMITTED FROM TOP BOUNDARY
C
C   UUM(IU,LU,MAZ)    COMPONENTS OF THE INTENSITY (U-SUPER-M) WHEN
C                     EXPANDED IN FOURIER COSINE SERIES IN AZIMUTH ANGLE
C
C   U0C(IQ,LU)        AZIMUTHALLY-AVERAGED INTENSITY
C
C   UTAUPR(LU)        OPTICAL DEPTHS OF USER OUTPUT LEVELS IN DELTA-M
C                     COORDINATES;  EQUAL TO  -UTAU(LU)- IF NO DELTA-M
C
C   WK()              SCRATCH ARRAY
C
C   XR0(LC)           X-SUB-ZERO IN EXPANSION OF THERMAL SOURCE FUNC-
C                     TION PRECEDING EQ. SS(14) (HAS NO MU-DEPENDENCE)
C
C   XR1(LC)           X-SUB-ONE IN EXPANSION OF THERMAL SOURCE FUNC-
C                     TION;  SEE  EQS. SS(14-16)
C
C   YLM0(L)           NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                     OF SUBSCRIPT 'L' AT THE BEAM ANGLE (NOT SAVED
C                     AS FUNCTION OF SUPERSCIPT 'M')
C
C   YLMC(L,IQ)        NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                     OF SUBSCRIPT 'L' AT THE COMPUTATIONAL ANGLES
C                     (NOT SAVED AS FUNCTION OF SUPERSCIPT 'M')
C
C   YLMU(L,IU)        NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                     OF SUBSCRIPT 'L' AT THE USER ANGLES
C                     (NOT SAVED AS FUNCTION OF SUPERSCIPT 'M')
C
C   Z()               SCRATCH ARRAY USED IN *SOLVE0,1* TO SOLVE A
C                     LINEAR SYSTEM FOR THE CONSTANTS OF INTEGRATION
C
C   Z0(IQ)            SOLUTION VECTORS Z-SUB-ZERO OF EQ. SS(16)
C
C   Z0U(IU,LC)        Z-SUB-ZERO IN EQ. SS(16) INTERPOLATED TO USER
C                     ANGLES FROM AN EQUATION DERIVED FROM SS(16)
C
C   Z1(IQ)            SOLUTION VECTORS Z-SUB-ONE  OF EQ. SS(16)
C
C   Z1U(IU,LC)        Z-SUB-ONE IN EQ. SS(16) INTERPOLATED TO USER
C                     ANGLES FROM AN EQUATION DERIVED FROM SS(16)
C
C   ZBEAM(IU,LC)      PARTICULAR SOLUTION FOR BEAM SOURCE
C
C   ZJ(IQ)            RIGHT-HAND SIDE VECTOR CAPITAL-X-SUB-ZERO IN
C                     EQ. SS(19), ALSO THE SOLUTION VECTOR CAPITAL
C                     -Z-SUB-ZERO AFTER SOLVING THAT SYSTEM
C
C   ZZ(IQ,LC)         PERMANENT STORAGE FOR THE BEAM SOURCE VECTORS -ZJ-
C
C   ZPLK0(IQ,LC)      PERMANENT STORAGE FOR THE THERMAL SOURCE
C                     VECTORS  -Z0-  OBTAINED BY SOLVING  EQ. SS(16)
C
C   ZPLK1(IQ,LC)      PERMANENT STORAGE FOR THE THERMAL SOURCE
C                     VECTORS  -Z1-  OBTAINED BY SOLVING  EQ. SS(16)
C
C+---------------------------------------------------------------------+
C   LOCAL SYMBOLIC DIMENSIONS:
C
C       MXCLY  = MAX NO. OF COMPUTATIONAL LAYERS
C       MXULV  = MAX NO. OF OUTPUT LEVELS
C       MXCMU  = MAX NO. OF COMPUTATION POLAR ANGLES
C       MXUMU  = MAX NO. OF OUTPUT POLAR ANGLES
C       MXPHI  = MAX NO. OF OUTPUT AZIMUTHAL ANGLES
C+---------------------------------------------------------------------+
C
      LOGICAL LYRCUT, PASS1
      INTEGER IPVT( NNLYRI ), LAYRU( MXULV ),VAL(MAXULV)
      REAL    ALBSAV( MXUMU ), AMB( MI,MI ), APB( MI,MI ),
     $        ARRAY( MXCMU,MXCMU ), B( NNLYRI ), BDR( MI,0:MI ),
     $        BEM( MI ), CBAND( MI9M2,NNLYRI ), CC( MXCMU,MXCMU ),
     $        CMU( MXCMU ), CWT( MXCMU ), EMU( MXUMU ), EVAL( MI ),
     $        EVECC( MXCMU, MXCMU ), EXPBEA( 0:MXCLY ), FLYR( MXCLY ),
     $        FLDN( MXULV ), FLDIR( MXULV ), GL( 0:MXCMU,MXCLY ),
     $        GC( MXCMU,MXCMU,MXCLY ), GU( MXUMU,MXCMU,MXCLY ),
     $        HLPR( 0:MXCMU ), KK( MXCMU,MXCLY ), LL( MXCMU,MXCLY ),
     $        OPRIM( MXCLY ), PHIRAD( MXPHI ), PKAG( 0:MXCLY ),
     $        PSI( MXCMU ), RMU( MXUMU,0:MI ), TAUC( 0:MXCLY ),
     $        TAUCPR( 0:MXCLY ), U0C( MXCMU,MXULV ), UTAUPR( MXULV ),
     $        UUM( MXUMU,MXULV,0:MXCMU ), WK( MXCMU ), XR0( MXCLY ),
     $        XR1( MXCLY ), YLM0( 0:MXCMU ), YLMC( 0:MXCMU,MXCMU ),
     $        YLMU( 0:MXCMU,MXUMU ), Z( NNLYRI ), Z0( MXCMU ),
     $        Z0U( MXUMU,MXCLY ), Z1( MXCMU ),
     $        Z1U( MXUMU,MXCLY ), ZJ( MXCMU ), ZZ( MXCMU,MXCLY ),
     $        ZPLK0( MXCMU,MXCLY ), ZPLK1( MXCMU,MXCLY ),
     $        ZBEAM( MXUMU,MXCLY )
      DOUBLE PRECISION   D1MACH2
C
      SAVE  PASS1
      DATA  PASS1 / .FALSE. /
C

         PI = 2. * ASIN(1.0)
         EPSIL = 10.*D1MACH2(4)
         RPD = PI / 180.0
C
      IF ( PASS1 )  THEN
         PI = 2. * ASIN(1.0)
         EPSIL = 10.*D1MACH2(4)
         RPD = PI / 180.0
C                                ** INSERT INPUT VALUES FOR SELF-TEST
C                                ** NOTE: SELF-TEST MUST NOT USE IBCND=1
C
         CALL  SLFTST( ACCUR, ALBEDO, BTEMP, DELTAM, DTAUC( 1 ), FBEAM,
     $                 FISOT, IBCND, LAMBER, NLYR, NOPLNK, NPHI,
     $                 NUMU, NSTR, NTAU, ONLYFL, PHI( 1 ), PHI0, PMOM,
     $                 PRNT, SSALB( 1 ), TEMIS, TEMPER, TTEMP, UMU( 1 ),
     $                 USRANG, USRTAU, UTAU( 1 ), UMU0, WVNMHI, WVNMLO,
     $                 .FALSE., DUM, DUM, DUM, DUM )
      END IF
C
   1  CONTINUE
      IF ( PRNT(1) )  WRITE( *,1010 )  HEADER
C
C                         ** ZERO SOME ARRAYS (NOT STRICTLY NECESSARY,
C                         ** BUT OTHERWISE UNUSED PARTS OF ARRAYS
C                         ** COLLECT GARBAGE)
      DO 10 I = 1, NNLYRI
         IPVT(I) = 0
10    CONTINUE
      CALL  ZEROAL( AMB, APB, ARRAY, CC, CMU, CWT,  EVAL, EVECC,
     $              GC, GU, HLPR, KK, LL, PSI, WK, XR0, XR1,
     $              YLM0, YLMC, YLMU, Z, Z0, Z1, ZJ, ZZ, ZPLK0,
     $              ZPLK1, Z0U, Z1U, ZBEAM, MI, MXCMU, MXCLY,
     $              NNLYRI, MXUMU, MXULV )
C
C                                  ** CALCULATE CUMULATIVE OPTICAL DEPTH
C                                  ** AND DITHER SINGLE-SCATTER ALBEDO
C                                  ** TO IMPROVE NUMERICAL BEHAVIOR OF
C                                  ** EIGENVALUE/VECTOR COMPUTATION
      TAUC( 0 ) = 0.
      CALL  ZEROIT( TAUC(0), MXCLY+1 )
      DO 20  LC = 1, NLYR
C        IF( SSALB(LC).EQ.1.0 )  SSALB(LC) = 1.0 - EPSIL
C--------------------------------------------------------
         IF( SSALB(LC).GT.0.99999 )  SSALB(LC) = 0.99999
C--------------------------------------------------------
         TAUC(LC) = TAUC(LC-1) + DTAUC(LC)
20    CONTINUE
C
C-------------------  MODIF ---------------------
C
      IF(USRTAU)THEN
                DO 31 I1=1,NTAU
                   LC=VAL(I1)-1
                   UTAU(I1)=TAUC(LC)
 31             CONTINUE
      ENDIF
C-------------------------------------------------
C                                ** CHECK INPUT DIMENSIONS AND VARIABLES
C
C	WRITE(*,*) '9.2'
C	write(*,*) UMU0, ALBEDO
	
      CALL  CHEKIN( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $              WVNMHI, USRTAU, NTAU, UTAU, NSTR, USRANG,
     $              NUMU, UMU, NPHI, PHI, IBCND, FBEAM, UMU0,
     $              PHI0, FISOT, LAMBER, ALBEDO, HL, BTEMP,
     $              TTEMP, TEMIS, NOPLNK, ONLYFL, ACCUR, MAXCLY,
     $              MAXULV, MAXUMU, MAXCMU, MAXPHI, MXCLY,
     $              MXULV,  MXUMU,  MXCMU,  MXPHI, TAUC )
C
C                                 ** PERFORM VARIOUS SETUP OPERATIONS
C
      CALL  SETDIS( CMU, CWT, DELTAM, DTAUC, EXPBEA, FBEAM, FLYR,
     $              GL, HL, HLPR, IBCND, LAMBER, LAYRU, LYRCUT,
     $              MAXUMU, MAXCMU, MXCMU, NCUT, NLYR, NTAU, NN,
     $              NSTR, NOPLNK, NUMU, ONLYFL, OPRIM, PMOM, SSALB,
     $              TAUC, TAUCPR, UTAU, UTAUPR, UMU, UMU0, USRTAU,
     $              USRANG, MXCLY )
C
C                                             ** PRINT INPUT INFORMATION
      IF ( PRNT(1) )
     $     CALL PRTINP( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $                  WVNMHI, NTAU, UTAU, NSTR, NUMU, UMU,
     $                  NPHI, PHI, IBCND, FBEAM, UMU0, PHI0,
     $                  FISOT, LAMBER, ALBEDO, HL, BTEMP, TTEMP,
     $                  TEMIS, DELTAM, NOPLNK, ONLYFL, ACCUR,
     $                  FLYR, LYRCUT, OPRIM, TAUC, TAUCPR,
     $                  MAXCMU, PRNT(7) )
C
C
      IF ( IBCND.EQ.1 )  THEN
C                              ** HANDLE SPECIAL CASE FOR GETTING ALBEDO
C                              ** AND TRANSMISSIVITY OF MEDIUM FOR MANY
C                              ** BEAM ANGLES AT ONCE
C
         CALL  ALBTRN( ALBEDO, AMB, APB, ARRAY, B, BDR, CBAND, CC,
     $                 CMU, CWT, EVAL, EVECC, GL, GC, GU, IPVT,
     $                 KK, LL, NLYR, NN, NSTR, NUMU, PRNT, TAUCPR,
     $                 UMU, U0U, WK, YLMC, YLMU, Z,
     $                 MI, MI9M2, MAXULV, MAXUMU,
     $                 MXCMU, MXUMU, NNLYRI, ALBMED, TRNMED )
         RETURN
      ENDIF
C                                   ** CALCULATE PLANCK FUNCTIONS
      IF ( NOPLNK )  THEN
         BPLANK = 0.0
         TPLANK = 0.0
         CALL  ZEROIT( PKAG, MXCLY+1 )
      ELSE
         TPLANK = TEMIS * PLKAVG( WVNMLO, WVNMHI, TTEMP )
         BPLANK =         PLKAVG( WVNMLO, WVNMHI, BTEMP )
         DO 40  LEV = 0, NLYR
            PKAG( LEV ) = PLKAVG( WVNMLO, WVNMHI, TEMPER(LEV) )
 40      CONTINUE
      END IF
C
C
C ========  BEGIN LOOP TO SUM AZIMUTHAL COMPONENTS OF INTENSITY  =======
C ========  (EQ STWJ 5)
C
      KCONV = 0
      NAZ = NSTR-1
C                                            ** AZIMUTH-INDEPENDENT CASE
C
      IF ( FBEAM.EQ.0.0 .OR. (1.-UMU0).LT.1.E-5 .OR. ONLYFL .OR.
     $     (NUMU.EQ.1.AND.(1.-UMU(1)).LT.1.E-5 ) )
     $   NAZ = 0
C
      CALL  ZEROIT( UU, MAXUMU*MAXULV*MAXPHI )
      DO  200  MAZ = 0, NAZ
C
      IF ( MAZ.EQ.0 )  DELM0 = 1.0
      IF ( MAZ.GT.0 )  DELM0 = 0.0
C                                  ** GET NORMALIZED ASSOCIATED LEGENDRE
C                          ** POLYNOMIALS FOR INCIDENT BEAM ANGLE COSINE
      IF ( FBEAM.GT.0.0 )
     $     CALL  LEPOLY( 1, MAZ, MXCMU, NSTR-1, -UMU0, YLM0 )
C
C                                  ** GET NORMALIZED ASSOCIATED LEGENDRE
C                                      ** POLYNOMIALS FOR COMPUTATIONAL
C                                      ** AND USER POLAR ANGLE COSINES
      IF ( .NOT.ONLYFL .AND. USRANG )
     $ CALL  LEPOLY( NUMU, MAZ, MXCMU, NSTR-1, UMU, YLMU )
       CALL  LEPOLY( NN,   MAZ, MXCMU, NSTR-1, CMU, YLMC )
C
C                       ** EVALUATE NORMALIZED ASSOCIATED LEGENDRE
C                       ** POLYNOMIALS WITH NEGATIVE -CMU- FROM THOSE
C                       ** WITH POSITIVE -CMU-; DAVE/ARMSTRONG EQ. (15)
      SGN  = - 1.0
      DO  50  L = MAZ, NSTR-1
         SGN = - SGN
         DO  50  IQ = NN+1, NSTR
            YLMC( L,IQ ) = SGN * YLMC( L,IQ-NN )
 50   CONTINUE
C                                 ** SPECIFY USER'S BOTTOM REFLECTIVITY
C                                 ** AND EMISSIVITY PROPERTIES
      IF ( .NOT.LYRCUT )
     $   CALL  SURFAC( ALBEDO, CMU, CWT, DELM0, FBEAM, HLPR, LAMBER,
     $                 MI, MAZ, MXCMU, MXUMU, NN, NUMU, NSTR, ONLYFL,
     $                 UMU, USRANG, YLM0, YLMC, YLMU, BDR, EMU, BEM,
     $                 RMU )
C
C ===================  BEGIN LOOP ON COMPUTATIONAL LAYERS  =============
C
      DO 100  LC = 1, NCUT
C
C                        ** SOLVE EIGENFUNCTION PROBLEM IN EQ. STWJ(8B);
C                        ** RETURN EIGENVALUES AND EIGENVECTORS
C
         CALL  SOLEIG( AMB, APB, ARRAY, CMU, CWT, GL(0,LC), MI, MAZ,
     $                 MXCMU, NN, NSTR, WK, YLMC, CC, EVECC, EVAL,
     $                 KK(1,LC), GC(1,1,LC) )
C
C                                  ** CALCULATE PARTICULAR SOLUTIONS OF
C                                  ** EQ.SS(18) FOR INCIDENT BEAM SOURCE
         IF ( FBEAM.GT.0.0 )
     $        CALL  UPBEAM( ARRAY, CC, CMU, DELM0, FBEAM, GL(0,LC),
     $                      IPVT, MAZ, MXCMU, NN, NSTR, PI, UMU0, WK,
     $                      YLM0, YLMC, ZJ, ZZ(1,LC) )
C
         IF ( .NOT.NOPLNK .AND. MAZ.EQ.0 ) THEN
C
C                              ** CALCULATE PARTICULAR SOLUTIONS OF
C                              ** EQ. SS(15) FOR THERMAL EMISSION SOURCE
C
            DELTAT = TAUCPR(LC) - TAUCPR(LC-1)
            XR1( LC ) = 0.0
            IF ( DELTAT.GT.0.0 ) XR1( LC ) = ( PKAG(LC) - PKAG(LC-1) )
     $                                       / DELTAT
            XR0( LC ) = PKAG(LC-1) - XR1(LC) * TAUCPR(LC-1)
C
            CALL UPISOT( ARRAY, CC, CMU, IPVT, MXCMU, NN, NSTR,
     $                   OPRIM(LC), WK, XR0(LC), XR1(LC), Z0, Z1,
     $                   ZPLK0(1,LC), ZPLK1(1,LC) )
         END IF
C
C
         IF ( .NOT.ONLYFL .AND. USRANG ) THEN
C                                            ** INTERPOLATE EIGENVECTORS
C                                            ** TO USER ANGLES
C
            CALL  TERPEV( CWT, EVECC, GL(0,LC), GU(1,1,LC), MAZ, MXCMU,
     $                    MXUMU, NN, NSTR, NUMU, WK, YLMC, YLMU)
C
C                                            ** INTERPOLATE SOURCE TERMS
C                                            ** TO USER ANGLES
C
            CALL  TERPSO( CWT, DELM0, FBEAM, GL(0,LC), MAZ,
     $                    MXCMU, MXUMU, NOPLNK, NUMU, NSTR, OPRIM(LC),
     $                    PI, YLM0, YLMC, YLMU, PSI, XR0(LC), XR1(LC),
     $                    Z0, ZJ, ZBEAM(1,LC), Z0U(1,LC), Z1U(1,LC) )
         END IF
C
100   CONTINUE
C
C ===================  END LOOP ON COMPUTATIONAL LAYERS  ===============
C
C                      ** SET COEFFICIENT MATRIX OF EQUATIONS COMBINING
C                      ** BOUNDARY AND LAYER INTERFACE CONDITIONS
C
      CALL  SETMTX( BDR, CBAND, CMU, CWT, DELM0, GC, KK, LAMBER,
     $              LYRCUT, MI, MI9M2, MXCMU, NCOL, NCUT, NNLYRI,
     $              NN, NSTR, TAUCPR, WK )
C
C                      ** SOLVE FOR CONSTANTS OF INTEGRATION IN HOMO-
C                      ** GENEOUS SOLUTION (GENERAL BOUNDARY CONDITIONS)
C
      CALL  SOLVE0( B, BDR, BEM, BPLANK, CBAND, CMU, CWT, EXPBEA,
     $              FBEAM, FISOT, IPVT, LAMBER, LL, LYRCUT,
     $              MAZ, MI, MI9M2, MXCMU, NCOL, NCUT, NN, NSTR,
     $              NNLYRI, PI, TPLANK, TAUCPR, UMU0, Z, ZZ,
     $              ZPLK0, ZPLK1 )
C
C                                  ** COMPUTE UPWARD AND DOWNWARD FLUXES
      IF ( MAZ.EQ.0 )
     $     CALL FLUXES( CMU, CWT, FBEAM, GC, KK, LAYRU, LL, LYRCUT,
     $                  MXCMU, MXULV, NCUT, NN, NSTR, NTAU, PI,
     $                  PRNT, SSALB, TAUCPR, UMU0, UTAU, UTAUPR,
     $                  XR0, XR1, ZZ, ZPLK0, ZPLK1, DFDT, FLUP,
     $                  FLDN, FLDIR, RFLDIR, RFLDN, UAVG, U0C,
     $                  MAXULV )
C
      IF ( ONLYFL )  THEN
         IF( MAXUMU.GE.NSTR )  THEN
C                                         ** SAVE AZIM-AVGD INTENSITIES
C                                         ** AT QUADRATURE ANGLES
            DO 120 LU = 1, NTAU
               DO 120 IQ = 1, NSTR
                  U0U( IQ,LU ) = U0C( IQ,LU )
120         CONTINUE
         ELSE
               CALL  ZEROIT( U0U, MAXUMU*MAXULV )
         ENDIF
         GO TO 210
      ENDIF
C
      IF ( USRANG ) THEN
C                                     ** COMPUTE AZIMUTHAL INTENSITY
C                                     ** COMPONENTS AT USER ANGLES
C
         CALL  USRINT( BPLANK, CMU, CWT, DELM0, EMU, EXPBEA,
     $                 FBEAM, FISOT, GC, GU, KK, LAMBER, LAYRU, LL,
     $                 LYRCUT, MAZ, MXCMU, MXULV, MXUMU, NCUT,
     $                 NLYR, NN, NSTR, NOPLNK, NUMU, NTAU, PI, RMU,
     $                 TAUCPR, TPLANK, UMU, UMU0, UTAUPR, WK,
     $                 ZBEAM, Z0U, Z1U, ZZ, ZPLK0, ZPLK1, UUM )
C
      ELSE
C                                     ** COMPUTE AZIMUTHAL INTENSITY
C                                     ** COMPONENTS AT QUADRATURE ANGLES
C
         CALL  CMPINT( FBEAM, GC, KK, LAYRU, LL, LYRCUT, MAZ,
     $                 MXCMU, MXULV, MXUMU, NCUT, NN, NSTR,
     $                 NOPLNK, NTAU, TAUCPR, UMU0, UTAUPR,
     $                 ZZ, ZPLK0, ZPLK1, UUM )
      END IF
C
      IF( MAZ.EQ.0 ) THEN
C
         DO  140  J = 1, NPHI
            PHIRAD( J ) = RPD * ( PHI(J) - PHI0 )
 140     CONTINUE
C                               ** SAVE AZIMUTHALLY AVERAGED INTENSITIES
         DO 160  LU = 1, NTAU
            DO 160  IU = 1, NUMU
               U0U( IU,LU ) = UUM( IU,LU,0 )
 160     CONTINUE
C                              ** PRINT AZIMUTHALLY AVERAGED INTENSITIES
C                              ** AT USER ANGLES
         IF ( PRNT(4) )
     $        CALL PRAVIN( UMU, NUMU, MAXUMU, UTAU, NTAU, U0U )
C
      END IF
C                                ** INCREMENT INTENSITY BY CURRENT
C                                ** AZIMUTHAL COMPONENT (FOURIER
C                                ** COSINE SERIES);  EQ SD(2)
      AZERR = 0.0
      DO 180  J = 1, NPHI
         COSPHI = COS( MAZ * PHIRAD(J) )
         DO 180  LU = 1, NTAU
            DO 180  IU = 1, NUMU
               AZTERM = UUM( IU,LU,MAZ ) * COSPHI
               UU( IU,LU,J ) = UU( IU,LU,J ) + AZTERM
               AZERR = AMAX1( RATIO( ABS(AZTERM), ABS(UU(IU,LU,J)) ),
     $                        AZERR )
180   CONTINUE
      IF ( AZERR.LE.ACCUR )  KCONV = KCONV + 1
      IF ( KCONV.GE.2 )      GOTO 210
C
200   CONTINUE
C
C ===================  END LOOP ON AZIMUTHAL COMPONENTS  ===============
C
C
C                                                 ** PRINT INTENSITIES
C
 210  IF ( PRNT(5) .AND. .NOT.ONLYFL )
     $     CALL  PRTINT( UU, UTAU, NTAU, UMU, NUMU, PHI, NPHI,
     $                   MAXULV, MAXUMU )
C
C
      IF ( PASS1 )  THEN
C                                      ** COMPARE TEST CASE RESULTS WITH
C                                    ** CORRECT ANSWERS AND ABORT IF BAD
C
         CALL SLFTST( ACCUR, ALBEDO, BTEMP, DELTAM, DTAUC( 1 ), FBEAM,
     $                FISOT, IBCND, LAMBER, NLYR, NOPLNK, NPHI,
     $                NUMU, NSTR, NTAU, ONLYFL, PHI( 1 ), PHI0, PMOM,
     $                PRNT, SSALB( 1 ), TEMIS, TEMPER, TTEMP, UMU( 1 ),
     $                USRANG, USRTAU, UTAU( 1 ), UMU0, WVNMHI, WVNMLO,
     $                .TRUE., FLUP( 1 ), RFLDIR( 1 ), RFLDN( 1 ),
     $                UU( 1,1,1 ) )
C
         PASS1 = .FALSE.
         GO TO 1
      END IF
C
      RETURN
C
1010  FORMAT ( ////, 1X, 120('*'), /, 25X,
     $  'DISCRETE ORDINATES RADIATIVE TRANSFER PROGRAM, VERSION 1.0',
     $  /, 1X, A, /, 1X, 120('*') )
      END

      REAL FUNCTION  DREF( MU, HL, NSTR )
C
C        EXACT FLUX ALBEDO FOR GIVEN ANGLE OF INCIDENCE, GIVEN
C        A BIDIRECTIONAL REFLECTIVITY CHARACTERIZED BY ITS
C        LEGENDRE COEFFICIENTS ( NOTE** THESE WILL ONLY AGREE
C        WITH BOTTOM-BOUNDARY ALBEDOS CALCULATED BY 'DISORT' IN
C        THE LIMIT AS NUMBER OF STREAMS GO TO INFINITY, BECAUSE
C        'DISORT' EVALUATES THE INTEGRAL 'CL' ONLY APPROXIMATELY,
C        BY QUADRATURE, WHILE THIS ROUTINE CALCULATES IT EXACTLY. )
C
C      INPUT :   MU     COSINE OF INCIDENCE ANGLE
C                HL     LEGENDRE COEFFICIENTS OF BIDIRECTIONAL REF'Y
C              NSTR     NUMBER OF ELEMENTS OF 'HL' TO CONSIDER
C
C      INTERNAL VARIABLES (P-SUB-L IS THE L-TH LEGENDRE POLYNOMIAL) :
C
C              CL    INTEGRAL FROM 0 TO 1 OF  MU * P-SUB-L(MU)
C                       (VANISHES FOR  L = 3, 5, 7, ... )
C              PL    P-SUB-L
C            PLM1    P-SUB-(L-1)
C            PLM2    P-SUB-(L-2)
C
      PARAMETER  ( MAXTRM = 100 )
      LOGICAL      PASS1
      REAL         MU, HL( 0:* )
      REAL         C( MAXTRM )
      DATA  PASS1 / .TRUE. /
C
C
      IF ( PASS1 )  THEN
         PASS1 = .FALSE.
         CL = 0.125
         C(2) = 10. * CL
         DO 1  L = 4, MAXTRM, 2
            CL = - CL * (L-3) / (L+2)
            C(L) = 2. * (2*L+1) * CL
    1    CONTINUE
      END IF
C
      IF ( NSTR.GT.MAXTRM )  CALL
     $     ERRMSG( 'DREF--PARAMETER MAXTRM TOO SMALL', .TRUE. )
C
      DREF = HL(0) - 2.*HL(1) * MU
      PLM2 = 1.0
      PLM1 = - MU
      DO 10  L = 2, NSTR-1
C                                ** LEGENDRE POLYNOMIAL RECURRENCE
C
         PL = ( (2*L-1) * (-MU) * PLM1 - (L-1) * PLM2 ) / L
         IF( MOD(L,2).EQ.0 )  DREF = DREF + C(L) * HL(L) * PL
         PLM2 = PLM1
         PLM1 = PL
   10 CONTINUE
C
      RETURN
      END


      SUBROUTINE  FLUXES( CMU, CWT, FBEAM, GC, KK, LAYRU, LL, LYRCUT,
     $                    MXCMU, MXULV, NCUT, NN, NSTR, NTAU, PI,
     $                    PRNT, SSALB, TAUCPR, UMU0, UTAU, UTAUPR,
     $                    XR0, XR1, ZZ, ZPLK0, ZPLK1, DFDT, FLUP,
     $                    FLDN, FLDIR, RFLDIR, RFLDN, UAVG, U0C,
     $                    MAXULV )
C
C       MODIF LOA: FLUX SPHERIQUES
C
C       CALCULATES THE RADIATIVE FLUXES, MEAN INTENSITY, AND FLUX
C       DERIVATIVE WITH RESPECT TO OPTICAL DEPTH FROM THE M=0 INTENSITY
C       COMPONENTS (THE AZIMUTHALLY-AVERAGED INTENSITY)
C
C    I N P U T     V A R I A B L E S:
C
C       CMU      :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT      :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       GC       :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       KK       :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       LAYRU    :  LAYER NUMBER OF USER LEVEL -UTAU-
C       LL       :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                   BY SOLVING SCALED VERSION OF EQ. SC(5);
C                   EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       LYRCUT   :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       NN       :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       NCUT     :  NUMBER OF COMPUTATIONAL LAYER WHERE ABSORPTION
C                     OPTICAL DEPTH EXCEEDS -ABSCUT-
C       TAUCPR   :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       UTAUPR   :  OPTICAL DEPTHS OF USER OUTPUT LEVELS IN DELTA-M
C                     COORDINATES;  EQUAL TO  -UTAU- IF NO DELTA-M
C       XR0      :  EXPANSION OF THERMAL SOURCE FUNCTION IN EQ. SS(14)
C       XR1      :  EXPANSION OF THERMAL SOURCE FUNCTION EQS. SS(16)
C       ZZ       :  BEAM SOURCE VECTORS IN EQ. SS(19)
C       ZPLK0    :  THERMAL SOURCE VECTORS -Z0-, BY SOLVING EQ. SS(16)
C       ZPLK1    :  THERMAL SOURCE VECTORS -Z1-, BY SOLVING EQ. SS(16)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C   O U T P U T     V A R I A B L E S:
C
C       U0C      :  AZIMUTHALLY AVERAGED INTENSITIES
C                   ( AT POLAR QUADRATURE ANGLES )
C       (RFLDIR, RFLDN, FLUP, DFDT, UAVG ARE 'DISORT' OUTPUT VARIABLES)
C
C   I N T E R N A L       V A R I A B L E S:
C
C       DIRINT   :  DIRECT INTENSITY ATTENUATED
C       FDNTOT   :  TOTAL DOWNWARD FLUX (DIRECT + DIFFUSE)
C       FLDIR    :  DIRECT-BEAM FLUX (DELTA-M SCALED)
C       FLDN     :  DIFFUSE DOWN-FLUX (DELTA-M SCALED)
C       FNET     :  NET FLUX (TOTAL-DOWN - DIFFUSE-UP)
C       FACT     :  EXP( - UTAUPR / UMU0 )
C       PLSORC   :  PLANCK SOURCE FUNCTION (THERMAL)
C       ZINT     :  INTENSITY OF m = 0 CASE, IN EQ. SC(1)
C+---------------------------------------------------------------------+
C
      LOGICAL LYRCUT, PRNT(*)
      REAL    DFDT(*), FLUP(*), FLDIR(*), FLDN(*), RFLDIR(*), RFLDN(* ),
     $        U0C( MXCMU,MXULV ), UAVG(*)
      INTEGER LAYRU(*)
      REAL    CMU(*), CWT(*), GC( MXCMU,MXCMU,* ), KK( MXCMU,* ),
     $        LL( MXCMU,* ), SSALB(*), TAUCPR( 0:* ),
     $        UTAU(*), UTAUPR(*), XR0(*), XR1(*), ZZ( MXCMU,* ),
     $        ZPLK0( MXCMU,* ), ZPLK1( MXCMU,* )
C
C
      IF ( PRNT(2) )  WRITE( *,1010 )
C                                          ** ZERO DISORT OUTPUT ARRAYS
      CALL  ZEROIT( U0C, MXULV*MXCMU )
      CALL  ZEROIT( RFLDIR, MAXULV )
      CALL  ZEROIT( FLDIR,  MXULV )
      CALL  ZEROIT( RFLDN,  MAXULV )
      CALL  ZEROIT( FLDN,   MXULV )
      CALL  ZEROIT( FLUP,   MAXULV )
      CALL  ZEROIT( UAVG,   MAXULV )
      CALL  ZEROIT( DFDT,   MAXULV )
C                                        ** LOOP OVER USER LEVELS
      DO 100  LU = 1, NTAU
C
         LYU = LAYRU(LU)
C
         IF ( LYRCUT .AND. LYU.GT.NCUT ) THEN
C                                                ** NO RADIATION REACHES
C                                                ** THIS LEVEL
            FDNTOT = 0.0
            FNET   = 0.0
            PLSORC = 0.0
            GO TO 90
         END IF
C
         IF ( FBEAM.GT.0.0 )  THEN
            FACT  = EXP( - UTAUPR(LU) / UMU0 )
            DIRINT = FBEAM * FACT
            FLDIR(  LU ) = UMU0 * ( FBEAM * FACT )
            RFLDIR( LU ) = UMU0 * FBEAM * EXP( - UTAU( LU ) / UMU0 )
         ELSE
            DIRINT = 0.0
            FLDIR(  LU ) = 0.0
            RFLDIR( LU ) = 0.0
         END IF
C
         DO 20  IQ = 1, NN
C
            ZINT = 0.0
            DO 10  JQ = 1, NN
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $                EXP( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU)) )
10          CONTINUE
            DO 11  JQ = NN+1, NSTR
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $                EXP( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU-1)) )
11          CONTINUE
C
            U0C( IQ,LU ) = ZINT
            IF ( FBEAM.GT.0.0 )  U0C( IQ,LU ) = ZINT + ZZ(IQ,LYU) * FACT
            U0C( IQ,LU ) = U0C( IQ,LU ) + ZPLK0(IQ,LYU)
     $                     + ZPLK1(IQ,LYU) * UTAUPR(LU)
            UAVG(LU) = UAVG(LU) + CWT(NN+1-IQ) * U0C( IQ,LU )
            FLDN(LU) = FLDN(LU) + CWT(NN+1-IQ)*CMU(NN+1-IQ) * U0C(IQ,LU)
20       CONTINUE
C
         DO 40  IQ = NN+1, NSTR
C
            ZINT = 0.0
            DO 30  JQ = 1, NN
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $                EXP( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU)) )
30          CONTINUE
            DO 31  JQ = NN+1, NSTR
               ZINT = ZINT + GC(IQ,JQ,LYU) * LL(JQ,LYU) *
     $                EXP( - KK(JQ,LYU) * (UTAUPR(LU) - TAUCPR(LYU-1)) )
31          CONTINUE
C
            U0C( IQ,LU ) = ZINT
            IF ( FBEAM.GT.0.0 )  U0C( IQ,LU ) = ZINT + ZZ(IQ,LYU) * FACT
            U0C( IQ,LU ) = U0C( IQ,LU ) + ZPLK0(IQ,LYU)
     $                     + ZPLK1(IQ,LYU) * UTAUPR(LU)
            UAVG(LU) = UAVG(LU) + CWT(IQ-NN) * U0C( IQ,LU )
            FLUP(LU) = FLUP(LU) + CWT(IQ-NN) * CMU(IQ-NN) * U0C( IQ,LU )
40       CONTINUE
C
         FLUP( LU )  = 2.0 * PI * FLUP( LU )
         FLDN( LU )  = 2.0 * PI * FLDN( LU )
         FDNTOT = FLDN( LU ) + FLDIR( LU )
         FNET   = FDNTOT - FLUP( LU )
         RFLDN( LU ) = FDNTOT - RFLDIR( LU )
C        UAVG( LU ) = ( 2.0 * PI * UAVG(LU) + DIRINT ) / ( 4.*PI )
C     MODIFICATION FLUX SPHERIQUES
         UAVG( LU ) = ( 2.0 * PI * UAVG(LU) + DIRINT )
         PLSORC =  XR0(LYU) + XR1(LYU) * UTAUPR(LU)
         DFDT( LU ) = ( 1.0-SSALB(LYU) ) * 4.*PI* ( UAVG(LU) - PLSORC )
 90      IF( PRNT(2) )  WRITE( *,1020 ) UTAU(LU), LYU, RFLDIR(LU),
     $                                 RFLDN(LU), FDNTOT, FLUP(LU),
     $                                 FNET, UAVG(LU), PLSORC, DFDT(LU)
100   CONTINUE
C
      IF ( PRNT(3) )  THEN
         WRITE ( *,1100 )
         DO 200  LU = 1, NTAU
            WRITE( *,1110 )  UTAU( LU )
            DO  200  IQ = 1, NN
               ANG1 = 180./PI * ACOS( CMU(2*NN-IQ+1) )
               ANG2 = 180./PI * ACOS( CMU(IQ) )
               WRITE( *,1120 ) ANG1, CMU(2*NN-IQ+1), U0C(IQ,LU),
     $                         ANG2, CMU(IQ),        U0C(IQ+NN,LU)
200      CONTINUE
      END IF
C
1010  FORMAT( //, 21X,
     $ '<----------------------- FLUXES ----------------------->', /,
     $ '   OPTICAL  COMPU    DOWNWARD    DOWNWARD    DOWNWARD     ',
     $ ' UPWARD                    MEAN      PLANCK   D(NET FLUX)', /,
     $ '     DEPTH  LAYER      DIRECT     DIFFUSE       TOTAL     ',
     $ 'DIFFUSE         NET   INTENSITY      SOURCE   / D(OP DEP)', / )
1020  FORMAT( F10.4, I7, 1P,7E12.3, E14.3 )
1100  FORMAT( //, ' ******** AZIMUTHALLY AVERAGED INTENSITIES',
     $      ' ( AT POLAR QUADRATURE ANGLES ) *******' )
1110  FORMAT( /, ' OPTICAL DEPTH =', F10.4, //,
     $  '     ANGLE (DEG)   COS(ANGLE)     INTENSITY',
     $  '     ANGLE (DEG)   COS(ANGLE)     INTENSITY' )
1120  FORMAT( 2( 0P,F16.4, F13.5, 1P,E14.3 ) )
C
      RETURN
      END

      SUBROUTINE  GETMOM( IPHAS, GG, NMOM, PMOM )
C
C        CALCULATE PHASE FUNCTION LEGENDRE EXPANSION COEFFICIENTS
C        IN VARIOUS SPECIAL CASES
C
      INTEGER  IPHAS, NMOM
      REAL     GG, PMOM( 0:* )
C
C       INPUT: IPHAS   PHASE FUNCTION OPTIONS
C                      1 : ISOTROPIC
C                      2 : RAYLEIGH
C                      3 : HENYEY-GREENSTEIN WITH ASYMMETRY FACTOR -GG-
C                      4 : HAZE L AS SPECIFIED BY GARCIA/SIEWERT
C                      5 : CLOUD C.1 AS SPECIFIED BY GARCIA/SIEWERT
C
C              GG      ASYMMETRY FACTOR FOR HENYEY-GREENSTEIN CASE
C
C              NMOM    INDEX OF HIGHEST LEGENDRE COEFFICIENT NEEDED
C                        ( = NUMBER OF STREAMS 'NSTR'  CHOSEN
C                         FOR THE DISCRETE ORDINATE METHOD)
C
C      OUTPUT: PMOM(K)  LEGENDRE EXPANSION COEFFICIENTS (K=0 TO NMOM)
C                         (BE SURE TO DIMENSION '0:maxval' IN CALLING
C                          PROGRAM)
C
C      REFERENCE:  GARCIA, R. AND C. SIEWERT, 1985: BENCHMARK RESULTS
C                     IN RADIATIVE TRANSFER, TRANSP. THEORY AND STAT.
C                     PHYSICS 14, 437-484, TABLES 10 AND 17
C
      REAL  HAZELM( 82 ), CLDMOM( 299 )
C
      DATA HAZELM /  2.41260, 3.23047, 3.37296, 3.23150, 2.89350,
     A               2.49594, 2.11361, 1.74812, 1.44692, 1.17714,
     B               0.96643, 0.78237, 0.64114, 0.51966, 0.42563,
     C               0.34688, 0.28351, 0.23317, 0.18963, 0.15788,
     D               0.12739, 0.10762, 0.08597, 0.07381, 0.05828,
     E               0.05089, 0.03971, 0.03524, 0.02720, 0.02451,
     F               0.01874, 0.01711, 0.01298, 0.01198, 0.00904,
     G               0.00841, 0.00634, 0.00592, 0.00446, 0.00418,
     H               0.00316, 0.00296, 0.00225, 0.00210, 0.00160,
     I               0.00150, 0.00115, 0.00107, 0.00082, 0.00077,
     J               0.00059, 0.00055, 0.00043, 0.00040, 0.00031,
     K               0.00029, 0.00023, 0.00021, 0.00017, 0.00015,
     L               0.00012, 0.00011, 0.00009, 0.00008, 0.00006,
     M               0.00006, 0.00005, 0.00004, 0.00004, 0.00003,
     N               0.00003, 3*0.00002, 8*0.00001 /
C
      DATA  ( CLDMOM(K), K = 1, 159 ) /
     A  2.544,  3.883,  4.568,  5.235,  5.887,  6.457,  7.177,  7.859,
     B  8.494,  9.286,  9.856, 10.615, 11.229, 11.851, 12.503, 13.058,
     C 13.626, 14.209, 14.660, 15.231, 15.641, 16.126, 16.539, 16.934,
     D 17.325, 17.673, 17.999, 18.329, 18.588, 18.885, 19.103, 19.345,
     E 19.537, 19.721, 19.884, 20.024, 20.145, 20.251, 20.330, 20.401,
     F 20.444, 20.477, 20.489, 20.483, 20.467, 20.427, 20.382, 20.310,
     G 20.236, 20.136, 20.036, 19.909, 19.785, 19.632, 19.486, 19.311,
     H 19.145, 18.949, 18.764, 18.551, 18.348, 18.119, 17.901, 17.659,
     I 17.428, 17.174, 16.931, 16.668, 16.415, 16.144, 15.883, 15.606,
     J 15.338, 15.058, 14.784, 14.501, 14.225, 13.941, 13.662, 13.378,
     K 13.098, 12.816, 12.536, 12.257, 11.978, 11.703, 11.427, 11.156,
     L 10.884, 10.618, 10.350, 10.090,  9.827,  9.574,  9.318,  9.072,
     M  8.822, 8.584, 8.340, 8.110, 7.874, 7.652, 7.424, 7.211, 6.990,
     N  6.785, 6.573, 6.377, 6.173, 5.986, 5.790, 5.612, 5.424, 5.255,
     O  5.075, 4.915, 4.744, 4.592, 4.429, 4.285, 4.130, 3.994, 3.847,
     P  3.719, 3.580, 3.459, 3.327, 3.214, 3.090, 2.983, 2.866, 2.766,
     Q  2.656, 2.562, 2.459, 2.372, 2.274, 2.193, 2.102, 2.025, 1.940,
     R  1.869, 1.790, 1.723, 1.649, 1.588, 1.518, 1.461, 1.397, 1.344,
     S  1.284, 1.235, 1.179, 1.134, 1.082, 1.040, 0.992, 0.954, 0.909 /
      DATA  ( CLDMOM(K), K = 160, 299 ) /
     T  0.873, 0.832, 0.799, 0.762, 0.731, 0.696, 0.668, 0.636, 0.610,
     U  0.581, 0.557, 0.530, 0.508, 0.483, 0.463, 0.440, 0.422, 0.401,
     V  0.384, 0.364, 0.349, 0.331, 0.317, 0.301, 0.288, 0.273, 0.262,
     W  0.248, 0.238, 0.225, 0.215, 0.204, 0.195, 0.185, 0.177, 0.167,
     X  0.160, 0.151, 0.145, 0.137, 0.131, 0.124, 0.118, 0.112, 0.107,
     Y  0.101, 0.097, 0.091, 0.087, 0.082, 0.079, 0.074, 0.071, 0.067,
     Z  0.064, 0.060, 0.057, 0.054, 0.052, 0.049, 0.047, 0.044, 0.042,
     A  0.039, 0.038, 0.035, 0.034, 0.032, 0.030, 0.029, 0.027, 0.026,
     B  0.024, 0.023, 0.022, 0.021, 0.020, 0.018, 0.018, 0.017, 0.016,
     C  0.015, 0.014, 0.013, 0.013, 0.012, 0.011, 0.011, 0.010, 0.009,
     D  0.009, 3*0.008, 2*0.007, 3*0.006, 4*0.005, 4*0.004, 6*0.003,
     E  9*0.002, 18*0.001 /
C
C
      IF ( IPHAS.LT.1 .OR. IPHAS.GT.5 )
     $     CALL ERRMSG( 'GETMOM--BAD INPUT VARIABLE IPHAS', .TRUE. )
      IF ( IPHAS.EQ.3 .AND. (GG.LE.-1.0 .OR. GG.GE.1.0) )
     $     CALL ERRMSG( 'GETMOM--BAD INPUT VARIABLE GG', .TRUE. )
      IF ( NMOM.LT.2 )
     $     CALL ERRMSG( 'GETMOM--BAD INPUT VARIABLE NMOM', .TRUE. )
C
C
      PMOM(0) = 1.0
      DO  10  K = 1, NMOM
         PMOM(K) = 0.0
   10 CONTINUE
C
      IF ( IPHAS.EQ.2 )  THEN
C                                       ** RAYLEIGH PHASE FUNCTION
         PMOM(2) = 0.1
C
      ELSE IF ( IPHAS.EQ.3 ) THEN
C                                       ** HENYEY-GREENSTEIN PHASE FCN
         DO  20  K = 1, NMOM
            PMOM(K) = GG**K
   20    CONTINUE
C
      ELSE IF ( IPHAS.EQ.4 ) THEN
C                                        ** HAZE-L PHASE FUNCTION
         DO  30  K = 1, MIN0(82,NMOM)
            PMOM(K) = HAZELM(K) / ( 2*K+1 )
   30    CONTINUE
C
      ELSE IF ( IPHAS.EQ.5 ) THEN
C                                        ** CLOUD C.1 PHASE FUNCTION
         DO  40  K = 1, MIN0(298,NMOM)
            PMOM(K) = CLDMOM(K) / ( 2*K+1 )
40       CONTINUE
C
      END IF
C
      RETURN
      END


      INTEGER FUNCTION  ISAMAX( N, SX, INCX )
C
C  --INPUT--  N  NUMBER OF ELEMENTS IN VECTOR OF INTEREST
C            SX  SING-PREC ARRAY, LENGTH 1+(N-1)*INCX, CONTAINING VECTOR
C          INCX  SPACING OF VECTOR ELEMENTS IN 'SX'
C
C --OUTPUT-- ISAMAX   FIRST I, I = 1 TO N, TO MAXIMIZE
C                         ABS(SX(1+(I-1)*INCX))
C
      REAL SX(*), SMAX, XMAG
C
C
      IF( N.LE.0 ) THEN
         ISAMAX = 0
      ELSE IF( N.EQ.1 ) THEN
         ISAMAX = 1
      ELSE
         SMAX = 0.0
         II = 1
         DO 20  I = 1, 1+(N-1)*INCX, INCX
            XMAG = ABS(SX(I))
            IF( SMAX.LT.XMAG ) THEN
               SMAX = XMAG
               ISAMAX = II
            ENDIF
            II = II + 1
   20    CONTINUE
      ENDIF
C
      RETURN
      END

      SUBROUTINE  LEPOLY( NMU, M, MAXMU, TWONM1, MU, YLM )
C
C       COMPUTES THE NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL,
C       DEFINED IN TERMS OF THE ASSOCIATED LEGENDRE POLYNOMIAL
C       PLM = P-SUB-L-SUPER-M AS
C
C             YLM(MU) = SQRT( (L-M)!/(L+M)! ) * PLM(MU)
C
C       FOR FIXED ORDER -M- AND ALL DEGREES FROM L = M TO TWONM1.
C       WHEN M.GT.0, ASSUMES THAT Y-SUB(M-1)-SUPER(M-1) IS AVAILABLE
C       FROM A PRIOR CALL TO THE ROUTINE.
C
C       REFERENCE: Dave, J.V. and B.H. Armstrong, Computations of
C                  High-Order Associated Legendre Polynomials,
C                  J. Quant. Spectrosc. Radiat. Transfer 10,
C                  557-562, 1970.  (hereafter D/A)
C
C       METHOD: Varying degree recurrence relationship.
C
C       NOTE 1: The D/A formulas are transformed by
C               setting  M = n-1; L = k-1.
C       NOTE 2: Assumes that routine is called first with  M = 0,
C               then with  M = 1, etc. up to  M = TWONM1.
C       NOTE 3: Loops are written in such a way as to vectorize.
C
C  I N P U T     V A R I A B L E S:
C
C       NMU    :  NUMBER OF ARGUMENTS OF -YLM-
C       M      :  ORDER OF -YLM-
C       MAXMU  :  FIRST DIMENSION OF -YLM-
C       TWONM1 :  MAX DEGREE OF -YLM-
C       MU(I)  :  I = 1 TO NMU, ARGUMENTS OF -YLM-
C       IF M.GT.0, YLM(M-1,I) FOR I = 1 TO NMU IS REQUIRED
C
C  O U T P U T     V A R I A B L E:
C
C       YLM(L,I) :  L = M TO TWONM1, NORMALIZED ASSOCIATED LEGENDRE
C                   POLYNOMIALS EVALUATED AT ARGUMENT -MU(I)-
C+---------------------------------------------------------------------+
      REAL     MU(*), YLM( 0:MAXMU,* )
      INTEGER  M, NMU, TWONM1
      PARAMETER  ( MAXSQT = 1000 )
      REAL     SQT( MAXSQT )
      LOGICAL  PASS1
      SAVE  SQT, PASS1
      DATA  PASS1 / .TRUE. /
C
C
      IF ( PASS1 )  THEN
         PASS1 = .FALSE.
         DO 1  NS = 1, MAXSQT
            SQT( NS ) = SQRT( FLOAT(NS) )
    1    CONTINUE
      ENDIF
C
      IF ( 2*TWONM1 .GT. MAXSQT )
     $   CALL ERRMSG( 'LEPOLY--NEED TO INCREASE PARAM MAXSQT', .TRUE. )
C
      IF ( M .EQ. 0 )  THEN
C                             ** UPWARD RECURRENCE FOR ORDINARY
C                             ** LEGENDRE POLYNOMIALS
         DO  10  I = 1, NMU
            YLM( 0,I ) = 1.
            YLM( 1,I ) = MU( I )
10       CONTINUE
         DO  20  L = 2, TWONM1
            DO  20  I = 1, NMU
               YLM( L,I ) = ( ( 2*L-1 ) * MU(I) * YLM( L-1,I )
     $                      - ( L-1 ) * YLM( L-2,I ) ) / L
20       CONTINUE
C
      ELSE
C
         DO  30  I = 1, NMU
C                               ** Y-SUB-M-SUPER-M; DERIVED FROM
C                               ** D/A EQS. (11,12)
C
            YLM( M,I) = - SQT( 2*M-1 ) / SQT( 2*M )
     $                  * SQRT( 1. - MU(I)**2 ) * YLM( M-1,I )
C
C                              ** Y-SUB-(M+1)-SUPER-M; DERIVED FROM
C                              ** D/A EQS. (13,14) USING EQS. (11,12)
C
            YLM( M+1,I ) = SQT( 2*M+1 ) * MU(I) * YLM( M,I )
30       CONTINUE
C                                   ** UPWARD RECURRENCE; D/A EQ. (10)
         DO  40  L = M+2, TWONM1
            TMP1 = SQT( L-M ) * SQT( L+M )
            TMP2 = SQT( L-M-1 ) * SQT( L+M-1 )
            DO  40  I = 1, NMU
               YLM( L,I ) = ( ( 2*L-1 ) * MU(I) * YLM( L-1,I )
     $                        - TMP2 * YLM( L-2,I ) ) / TMP1
40       CONTINUE
C
      END IF
C
      RETURN
      END

      REAL FUNCTION  PLKAVG ( WNUMLO, WNUMHI, T )
C
C        COMPUTES PLANCK FUNCTION INTEGRATED BETWEEN TWO WAVENUMBERS
C
C  NOTE ** CHANGE 'R1MACH' TO 'D1MACH' TO RUN IN DOUBLE PRECISION
C
C  I N P U T :  WNUMLO : LOWER WAVENUMBER ( INV CM ) OF SPECTRAL
C                           INTERVAL
C               WNUMHI : UPPER WAVENUMBER
C               T      : TEMPERATURE (K)
C
C  O U T P U T :  PLKAVG : INTEGRATED PLANCK FUNCTION ( WATTS/SQ M )
C                           = INTEGRAL (WNUMLO TO WNUMHI) OF
C                              2H C**2  NU**3 / ( EXP(HC NU/KT) - 1)
C                              (WHERE H=PLANCKS CONSTANT, C=SPEED OF
C                              LIGHT, NU=WAVENUMBER, T=TEMPERATURE,
C                              AND K = BOLTZMANN CONSTANT)
C
C  REFERENCE : SPECIFICATIONS OF THE PHYSICAL WORLD: NEW VALUE
C                 OF THE FUNDAMENTAL CONSTANTS, DIMENSIONS/N.B.S.,
C                 JAN. 1974
C
C  METHOD :  FOR  -WNUMLO-  CLOSE TO  -WNUMHI-, A SIMPSON-RULE
C            QUADRATURE IS DONE TO AVOID ILL-CONDITIONING; OTHERWISE
C
C            (1)  FOR WAVENUMBER (WNUMLO OR WNUMHI) SMALL,
C                 INTEGRAL(0 TO WNUM) IS CALCULATED BY EXPANDING
C                 THE INTEGRAND IN A POWER SERIES AND INTEGRATING
C                 TERM BY TERM;
C
C            (2)  OTHERWISE, INTEGRAL(WNUMLO/HI TO INFINITY) IS
C                 CALCULATED BY EXPANDING THE DENOMINATOR OF THE
C                 INTEGRAND IN POWERS OF THE EXPONENTIAL AND
C                 INTEGRATING TERM BY TERM.
C
C  ACCURACY :  AT LEAST 6 SIGNIFICANT DIGITS, ASSUMING THE
C              PHYSICAL CONSTANTS ARE INFINITELY ACCURATE
C
C  ERRORS WHICH ARE NOT TRAPPED:
C
C      * POWER OR EXPONENTIAL SERIES MAY UNDERFLOW, GIVING NO
C        SIGNIFICANT DIGITS.  THIS MAY OR MAY NOT BE OF CONCERN,
C        DEPENDING ON THE APPLICATION.
C
C      * SIMPSON-RULE SPECIAL CASE IS SKIPPED WHEN DENOMINATOR OF
C        INTEGRAND WILL CAUSE OVERFLOW.  IN THAT CASE THE NORMAL
C        PROCEDURE IS USED, WHICH MAY BE INACCURATE IF THE
C        WAVENUMBER LIMITS (WNUMLO, WNUMHI) ARE CLOSE TOGETHER.
C ----------------------------------------------------------------------
C                                   *** ARGUMENTS
      REAL     T, WNUMLO, WNUMHI
C                                   *** LOCAL VARIABLES
C
C        A1,2,... :  POWER SERIES COEFFICIENTS
C        C2       :  H * C / K, IN UNITS CM*K (H = PLANCKS CONSTANT,
C                      C = SPEED OF LIGHT, K = BOLTZMANN CONSTANT)
C        D(I)     :  EXPONENTIAL SERIES EXPANSION OF INTEGRAL OF
C                       PLANCK FUNCTION FROM WNUMLO (I=1) OR WNUMHI
C                       (I=2) TO INFINITY
C        EPSIL    :  SMALLEST NUMBER SUCH THAT 1+EPSIL .GT. 1 ON
C                       COMPUTER
C        EX       :  EXP( - V(I) )
C        EXM      :  EX**M
C        MMAX     :  NO. OF TERMS TO TAKE IN EXPONENTIAL SERIES
C        MV       :  MULTIPLES OF 'V(I)'
C        P(I)     :  POWER SERIES EXPANSION OF INTEGRAL OF
C                       PLANCK FUNCTION FROM ZERO TO WNUMLO (I=1) OR
C                       WNUMHI (I=2)
C        PI       :  3.14159...
C        SIGMA    :  STEFAN-BOLTZMANN CONSTANT (W/M**2/K**4)
C        SIGDPI   :  SIGMA / PI
C        SMALLV   :  NUMBER OF TIMES THE POWER SERIES IS USED (0,1,2)
C        V(I)     :  C2 * (WNUMLO(I=1) OR WNUMHI(I=2)) / TEMPERATURE
C        VCUT     :  POWER-SERIES CUTOFF POINT
C        VCP      :  EXPONENTIAL SERIES CUTOFF POINTS
C        VMAX     :  LARGEST ALLOWABLE ARGUMENT OF 'EXP' FUNCTION
C
      PARAMETER  ( A1 = 1./3., A2 = -1./8., A3 = 1./60., A4 = -1./5040.,
     $             A5 = 1./272160., A6 = -1./13305600. )
      INTEGER  SMALLV
      REAL     C2, CONC, D(2), EPSIL, EX, MV, P(2), SIGMA, SIGDPI,
     $         V(2), VCUT, VCP(7), VSQ
      DOUBLE PRECISION   D1MACH2
      SAVE     CONC, VMAX, EPSIL, SIGDPI
      DATA     C2 / 1.438786 /,  SIGMA / 5.67032E-8 /,
     $         VCUT / 1.5 /, VCP / 10.25, 5.7, 3.9, 2.9, 2.3, 1.9, 0.0 /
      DATA     PI / 0.0 /
      F(X) = X**3 / ( EXP(X) - 1 )
C
C
      IF ( PI.EQ.0.0 )  THEN
         PI = 2. * ASIN( 1.0 )
C$$$$$$$$$$$$$$$        VMAX = ALOG( D1MACH(2) )
         VMAX = DLOG( D1MACH2(2) )
         EPSIL = D1MACH2(4)
         SIGDPI = SIGMA / PI
         CONC = 15. / PI**4
      END IF
C
      IF( T.LT.0.0 .OR. WNUMHI.LE.WNUMLO .OR. WNUMLO.LT.0. )
     $    CALL ERRMSG( 'PLKAVG--TEMPERATURE OR WAVENUMS. WRONG', .TRUE.)
C
      IF ( T.LT.1.E-4 )  THEN
         PLKAVG = 0.0
         RETURN
      ENDIF
C
      V(1) = C2 * WNUMLO / T
      V(2) = C2 * WNUMHI / T
      IF ( V(1).GT.EPSIL .AND. V(2).LT.VMAX .AND.
     $     (WNUMHI-WNUMLO)/WNUMHI .LT. 1.E-2 )  THEN
C
C                          ** WAVENUMBERS ARE VERY CLOSE.  GET INTEGRAL
C                          ** BY ITERATING SIMPSON RULE TO CONVERGENCE.
         HH = V(2) - V(1)
         OLDVAL = 0.0
         VAL0 = F( V(1) ) + F( V(2) )
C
         DO  2  N = 1, 10
            DEL = HH / (2*N)
            VAL = VAL0
            DO  1  K = 1, 2*N-1
               VAL = VAL + 2*(1+MOD(K,2)) * F( V(1) + K*DEL )
    1       CONTINUE
            VAL = DEL/3. * VAL
            IF ( ABS( (VAL-OLDVAL)/VAL ) .LE. 1.E-6 )  GO TO 3
            OLDVAL = VAL
    2    CONTINUE
         CALL ERRMSG( 'PLKAVG--SIMPSON RULE DIDNT CONVERGE', .FALSE. )
C
    3    PLKAVG = SIGDPI * T**4 * CONC * VAL
         RETURN
      END IF
C
      SMALLV = 0
      DO  50  I = 1, 2
C
         IF( V(I).LT.VCUT )  THEN
C                                   ** USE POWER SERIES
            SMALLV = SMALLV + 1
            VSQ = V(I)**2
            P(I) =  CONC * VSQ * V(I) * ( A1 + V(I) * ( A2 + V(I) *
     $                ( A3 + VSQ * ( A4 + VSQ * ( A5 + VSQ*A6 ) ) ) ) )
         ELSE
C                    ** USE EXPONENTIAL SERIES
            MMAX = 0
C                                ** FIND UPPER LIMIT OF SERIES
   20       MMAX = MMAX + 1
               IF ( V(I).LT.VCP( MMAX ) )  GO TO 20
C
            EX = EXP( - V(I) )
            EXM = 1.0
            D(I) = 0.0
C
            DO  30  M = 1, MMAX
               MV = M * V(I)
               EXM = EX * EXM
               D(I) = D(I) +
     $                EXM * ( 6. + MV*( 6. + MV*( 3. + MV ) ) ) / M**4
   30       CONTINUE
C
            D(I) = CONC * D(I)
         END IF
C
   50 CONTINUE
C
      IF ( SMALLV .EQ. 2 ) THEN
C                                    ** WNUMLO AND WNUMHI BOTH SMALL
         PLKAVG = P(2) - P(1)
C
      ELSE IF ( SMALLV .EQ. 1 ) THEN
C                                    ** WNUMLO SMALL, WNUMHI LARGE
         PLKAVG = 1. - P(1) - D(2)
C
      ELSE
C                                    ** WNUMLO AND WNUMHI BOTH LARGE
         PLKAVG = D(1) - D(2)
C
      END IF
C
      PLKAVG = SIGDPI * T**4 * PLKAVG
      IF( PLKAVG.EQ.0.0 )
     $    CALL ERRMSG( 'PLKAVG--RETURNS ZERO; POSSIBLE UNDERFLOW',
     $                 .FALSE. )
C
      RETURN
      END


      SUBROUTINE  PRALTR( UMU, NUMU, ALBMED, TRNMED )
C
C        PRINT PLANAR ALBEDO AND TRANSMISSIVITY OF MEDIUM
C        AS A FUNCTION OF INCIDENT BEAM ANGLE
C
      REAL     UMU(*), ALBMED(*), TRNMED(*)
C
C
      WRITE( *,110 )
      DO 20  IU = 1, NUMU
         ANGL = 180.0/3.14159265 * ACOS( UMU(IU) )
         WRITE(*,111)  ANGL, UMU(IU), ALBMED(IU), TRNMED(IU)
 20   CONTINUE
C
      RETURN
C
110   FORMAT( ///, ' *******  FLUX ALBEDO AND/OR TRANSMISSIVITY OF ',
     $ 'ENTIRE MEDIUM  ********', //,
     $ ' BEAM ZEN ANG   COS(BEAM ZEN ANG)      ALBEDO   TRANSMISSIVITY')
111   FORMAT( 0P,F13.4, F20.6, F12.5, 1P,E17.4 )
      END
      SUBROUTINE  PRAVIN( UMU, NUMU, MAXUMU, UTAU, NTAU, U0U )
C
C        PRINT AZIMUTHALLY AVERAGED INTENSITIES AT USER ANGLES
C
      REAL     UMU(*), UTAU(*), U0U( MAXUMU,* )
C
C
      WRITE ( *, '(//,A)' )
     $         ' *********  AZIMUTHALLY AVERAGED INTENSITIES '
     $       // '(USER POLAR ANGLES)  *********'
      LENFMT = 8
      NPASS = 1 + NUMU / LENFMT
      IF ( MOD(NUMU,LENFMT) .EQ. 0 )  NPASS = NPASS - 1
      DO 10  NP = 1, NPASS
         IUMIN = 1 + LENFMT * (NP-1)
         IUMAX = MIN0( LENFMT*NP, NUMU )
         WRITE ( *,101 )  ( UMU(IU), IU = IUMIN, IUMAX )
         DO 10  LU = 1, NTAU
            WRITE( *,102 ) UTAU(LU), ( U0U(IU,LU), IU=IUMIN,IUMAX)
 10   CONTINUE
C
      RETURN
C
101   FORMAT( /, 3X,'OPTICAL   POLAR ANGLE COSINES',
     $        /, 3X,'  DEPTH', 8F14.5 )
102   FORMAT( 0P,F10.4, 1P,8E14.4 )
      END
      SUBROUTINE  PRTFIN( UTAU, NTAU, UMU, NUMU, PHI, NPHI, MAXULV,
     $                    MAXUMU, ONLYFL, AZMAVG, RFLDIR, RFLDN, FLUP,
     $                    DFDT, U0U, UU, TSTFIR, TSTFDN, TSTFUP,
     $                    TSTDFD, TSTUU, MAXTAU, MAXMU, MAXAZ )
C
C        PRINT 'DISORT' RESULTS AND, DIRECTLY BENEATH THEM, THEIR
C        RATIOS TO THE CORRECT ANSWERS;  PRINT NUMBER OF NON-UNIT
C        RATIOS THAT OCCUR.
C
C     INPUT :   TSTFIR  CORRECT DIRECT FLUX
C               TSTFDN  CORRECT DIFFUSE DOWN FLUX
C               TSTFUP  CORRECT DIFFUSE UP FLUX
C               TSTDFD  CORRECT D(FLUX)/D(OPTICAL DEPTH)
C               TSTUU   CORRECT INTENSITY OR AZIM-AVG INTENSITY
C               AZMAVG  TRUE, CHECK AZIMUTHALLY-AVERAGED INTENSITY ONLY
C                       FALSE, CHECK FULL INTENSITY
C               (REMAINING INPUT = 'DISORT' I/O VARIABLES)
C
      LOGICAL  ONLYFL, AZMAVG, BADRAT
      REAL     DFDT(*), RFLDIR(*), RFLDN(*), FLUP(*), PHI(*), UMU(*),
     $         UTAU(*), U0U( MAXUMU,* ), UU( MAXUMU,MAXULV,* ),
     $         TSTFIR(*), TSTFDN(*), TSTFUP(*), TSTDFD(*),
     $         TSTUU ( MAXTAU, MAXMU, MAXAZ )
      PARAMETER  ( MAXRAT = 100 )
      REAL     RATV( MAXRAT )
      BADRAT(RAT) = RAT.LT.0.99 .OR. RAT.GT.1.01
C
C
      IF ( NTAU.GT.MAXTAU .OR. NUMU.GT.MAXMU .OR. NPHI.GT.MAXAZ )  CALL
     $   ERRMSG( 'PRTFIN--OUT OF BOUNDS IN COMPARATOR ARRAYS', .TRUE.)
C
      NUMBAD = 0
      WRITE( *,110 )
      DO 10  LU = 1, NTAU
         WRITE( *,120 )  UTAU(LU), RFLDIR(LU), RFLDN(LU), FLUP(LU),
     $                   DFDT(LU)
         RAT1 = RATIO( RFLDIR(LU), TSTFIR(LU) )
         RAT2 = RATIO( RFLDN(LU),  TSTFDN(LU) )
         RAT3 = RATIO(  FLUP(LU),  TSTFUP(LU) )
         RAT4 = RATIO(  DFDT(LU),  TSTDFD(LU) )
         WRITE( *,130 )  RAT1, RAT2, RAT3, RAT4
         IF( BADRAT(RAT1) )  NUMBAD = NUMBAD + 1
         IF( BADRAT(RAT2) )  NUMBAD = NUMBAD + 1
         IF( BADRAT(RAT3) )  NUMBAD = NUMBAD + 1
         IF( BADRAT(RAT4) )  NUMBAD = NUMBAD + 1
10    CONTINUE
C
      IF ( ONLYFL )  GO TO 100
C
      IF ( NUMU.GT.MAXRAT .OR. NPHI.GT.MAXRAT )
     $     CALL ERRMSG( 'PRTFIN--INCREASE PARAMETER MAXRAT', .TRUE. )
C
      IF ( AZMAVG ) THEN
C                                          ** PRINT AZIMUTHALLY-AVERAGED
C                                          ** INTENSITIES AT USER ANGLES
         IF ( NUMU.GT.8 )
     $        CALL ERRMSG( 'PRTFIN--FORMATS INADEQUATE', .FALSE. )
         WRITE ( *,140 )  ( UMU(IU), IU = 1, NUMU )
         DO 20  LU = 1, NTAU
            WRITE( *,160 ) UTAU(LU), ( U0U(IU,LU), IU = 1, NUMU )
            DO 18  IU = 1, NUMU
               RATV(IU) = RATIO( U0U(IU,LU), TSTUU(LU,IU,1) )
               IF( BADRAT(RATV(IU)) )  NUMBAD = NUMBAD + 1
   18       CONTINUE
            WRITE( *,170 )  ( RATV(IU), IU = 1, NUMU )
   20    CONTINUE
C
      ELSE
C                                  ** PRINT INTENSITIES AT USER ANGLES
         IF ( NPHI.GT.8 )
     $        CALL ERRMSG( 'PRTFIN--INADEQUATE PRINT FORMATS', .FALSE. )
         WRITE( *,180 )  ( PHI(J), J = 1, NPHI )
         DO 40  LU = 1, NTAU
            DO 40  IU = 1, NUMU
               IF( IU.EQ.1 )  WRITE( *,200 )  UTAU(LU), UMU(IU),
     $                        ( UU( IU,LU,J ), J = 1, NPHI )
               IF( IU.GT.1 )  WRITE( *,201 )            UMU(IU),
     $                        ( UU( IU,LU,J ), J = 1, NPHI )
               DO 38  J = 1, NPHI
                  RATV(J) = RATIO( UU(IU,LU,J), TSTUU(LU,IU,J) )
                  IF( BADRAT(RATV(J)) )  NUMBAD = NUMBAD + 1
   38          CONTINUE
               WRITE( *,210 )  ( RATV(J), J = 1, NPHI )
   40    CONTINUE
C
      END IF
C                             ** PRINT OUT NUMBER OF NON-UNIT RATIOS
  100 CONTINUE
      WRITE( *,300 )  NUMBAD
      RETURN
C
110   FORMAT( //,
     $  '                  <-------------- FLUXES -------------->', /,
     $  '    OPTICAL       DOWNWARD       DOWNWARD         UPWARD',
     $  '    D(NET FLUX)'/,
     $  '      DEPTH         DIRECT        DIFFUSE        DIFFUSE',
     $  '    / D(OP DEP)' )
120   FORMAT( 0P,F11.4, 1P,4E15.5 )
130   FORMAT( 11X, 4( '     (',F8.5,')' ) )
140   FORMAT( //,
     $  ' ********* AZIMUTHALLY AVERAGED INTENSITIES  *********', //,
     $  '   OPTICAL   POLAR ANGLE COSINES', /,
     $  '     DEPTH', 8F14.5 )
160   FORMAT( 0P,F10.3, 1P,8E14.5 )
170   FORMAT( 10X, 8( :,'    (',F8.5,')' ) )
180   FORMAT( //, ' ********  I N T E N S I T I E S  *********', //,
     $            '             POLAR   AZIMUTHAL ANGLES (DEGREES)', /,
     $            '   OPTICAL   ANGLE', /,
     $            '     DEPTH  COSINE', 8(F10.1,4X) )
200   FORMAT( /, 0P,F10.3, F8.3, 1P,8E14.5 )
201   FORMAT( 10X,      0P,F8.3, 1P,8E14.5 )
210   FORMAT( 18X, 8( :,'    (',F8.5,')' ) )
300   FORMAT( //,1X,45('='),/,' ====  ',I4,
     $   '  SERIOUSLY NON-UNIT RATIOS    ====',
     $         /,1X,45('=') )
C
      END
      SUBROUTINE  PRTINP( NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $                    WVNMHI, NTAU, UTAU, NSTR, NUMU, UMU,
     $                    NPHI, PHI, IBCND, FBEAM, UMU0, PHI0,
     $                    FISOT, LAMBER, ALBEDO, HL, BTEMP, TTEMP,
     $                    TEMIS, DELTAM, NOPLNK, ONLYFL, ACCUR,
     $                    FLYR, LYRCUT, OPRIM, TAUC, TAUCPR,
     $                    MAXCMU, PRTMOM )
C
C        PRINT VALUES OF INPUT VARIABLES
C
      LOGICAL  DELTAM, LAMBER, LYRCUT, NOPLNK, ONLYFL, PRTMOM
      REAL     UMU(*), FLYR(*), DTAUC(*), OPRIM(*), PHI(*),
     $         PMOM( 0:MAXCMU,* ), SSALB(*), UTAU(*), TAUC( 0:* ),
     $         TAUCPR( 0:* ), TEMPER( 0:* ), HL( 0:MAXCMU )
C
C
      WRITE( *,1010 )  NSTR, NLYR
      IF ( IBCND.NE.1 ) WRITE( *,1030 )  NTAU, (UTAU(LU), LU = 1, NTAU)
      IF ( .NOT.ONLYFL )
     $      WRITE( *,1040 )  NUMU, ( UMU(IU), IU = 1, NUMU )
      IF ( .NOT.ONLYFL .AND. IBCND.NE.1 )
     $      WRITE( *,1050 )  NPHI, ( PHI(J), J = 1, NPHI )
      IF ( NOPLNK .OR. IBCND.EQ.1 )  WRITE( *,1100 )
      WRITE( *,1055 )  IBCND
      IF ( IBCND.EQ.0 )  THEN
         WRITE( *,1060 ) FBEAM, UMU0, PHI0, FISOT
         IF ( LAMBER )   WRITE( *,1080 ) ALBEDO
         IF ( .NOT.LAMBER )  WRITE( *,1090 ) ( HL(K), K = 0, NSTR )
         IF ( .NOT.NOPLNK )  WRITE( *,1110 ) WVNMLO, WVNMHI, BTEMP,
     $                                       TTEMP, TEMIS
      ELSE IF ( IBCND.EQ.1 )  THEN
         WRITE( *,1080 ) ALBEDO
      ENDIF
      IF ( DELTAM )      WRITE( *,1120 )
      IF ( .NOT.DELTAM ) WRITE( *,1130 )
      IF ( IBCND.EQ.1 )  THEN
         WRITE( *,1135 )
      ELSE IF ( ONLYFL )  THEN
         WRITE( *,1140 )
      ELSE
         WRITE( *,1150 )
      ENDIF
      WRITE( *,1160 )  ACCUR
      IF ( LYRCUT )  WRITE( *,1170 )
      IF( .NOT.NOPLNK )  WRITE ( *,1190 )
      IF(      NOPLNK )  WRITE ( *,1191 )
      YESSCT = 0.0
      DO 10 LC = 1, NLYR
         YESSCT = YESSCT + SSALB(LC)
         IF( .NOT.NOPLNK )
     $       WRITE( *,1200 )  LC, DTAUC(LC), TAUC(LC), SSALB(LC),
     $                    FLYR(LC), TAUCPR(LC)-TAUCPR(LC-1), TAUCPR(LC),
     $                    OPRIM(LC), PMOM(1,LC), TEMPER(LC-1)
         IF( NOPLNK )
     $       WRITE( *,1200 )  LC, DTAUC(LC), TAUC(LC), SSALB(LC),
     $                    FLYR(LC), TAUCPR(LC)-TAUCPR(LC-1), TAUCPR(LC),
     $                    OPRIM(LC), PMOM(1,LC)
 10   CONTINUE
      IF( .NOT.NOPLNK )  WRITE( *,1210 ) TEMPER(NLYR)
C
      IF( PRTMOM .AND. YESSCT.GT.0.0 )  THEN
         WRITE( *, '(/,A)' )  ' LAYER   PHASE FUNCTION MOMENTS'
         DO 20 LC = 1, NLYR
            IF( SSALB(LC).GT.0.0 )
     $          WRITE( *,1300 )  LC, ( PMOM(K,LC), K = 0, NSTR )
 20      CONTINUE
      ENDIF
C
      RETURN
C
1010  FORMAT ( /, ' NO. STREAMS =', I4,
     $  '     NO. COMPUTATIONAL LAYERS =', I4 )
1030  FORMAT( I4,' USER OPTICAL DEPTHS :',10F10.4, /, (26X,10F10.4) )
1040  FORMAT( I4,' USER POLAR ANGLE COSINES :',10F9.5,/,(31X,10F9.5) )
1050  FORMAT( I4,' USER AZIMUTHAL ANGLES :', 10F9.2, /, (28X,10F9.2) )
1055  FORMAT( ' BOUNDARY CONDITION FLAG: IBCND =', I2 )
1060  FORMAT( '    INCIDENT BEAM WITH INTENSITY =', 1P,E11.3, ' AND',
     $ ' POLAR ANGLE COSINE = ', 0P,F8.5,'  AND AZIMUTH ANGLE =', F7.2,
     $ /,'    PLUS ISOTROPIC INCIDENT INTENSITY =', 1P,E11.3 )
1070  FORMAT( '    ISOTROPIC ILLUMINATION FROM TOP AND BOTTOM OF',
     $   ' INTENSITY =', 1P,E11.3 )
1080  FORMAT( '    BOTTOM ALBEDO (LAMBERTIAN) =', 0P,F8.4 )
1090  FORMAT( '    LEGENDRE COEFFS OF BOTTOM BIDIRECTIONAL',
     $ ' REFLECTIVITY :', /, (10X,10F9.5) )
1100  FORMAT( ' NO THERMAL EMISSION' )
1110  FORMAT( '    THERMAL EMISSION IN WAVENUMBER INTERVAL :', 2F14.4,/,
     $   '    BOTTOM TEMPERATURE =', F10.2, '     TOP TEMPERATURE =',
     $   F10.2,'    TOP EMISSIVITY =', F8.4 )
1120  FORMAT( ' USES DELTA-M METHOD' )
1130  FORMAT( ' DOES NOT USE DELTA-M METHOD' )
1135  FORMAT( ' CALCULATE ALBEDO AND TRANSMISSIVITY OF MEDIUM',
     $   ' VS. INCIDENT BEAM ANGLE' )
1140  FORMAT( ' CALCULATE FLUXES AND AZIM-AVERAGED INTENSITIES ONLY' )
1150  FORMAT( ' CALCULATE FLUXES AND INTENSITIES' )
1160  FORMAT( ' RELATIVE CONVERGENCE CRITERION FOR AZIMUTH SERIES =',
     $   1P,E11.2 )
1170  FORMAT( ' SETS RADIATION = 0 BELOW ABSORPTION OPTICAL DEPTH 10' )
1190  FORMAT( /, 37X, '<------------- DELTA-M --------------->', /,
     $'                   TOTAL    SINGLE                           ',
     $               'TOTAL    SINGLE', /,
     $'       OPTICAL   OPTICAL   SCATTER   TRUNCATED   ',
     $   'OPTICAL   OPTICAL   SCATTER    ASYMM', /,
     $'         DEPTH     DEPTH    ALBEDO    FRACTION     ',
     $     'DEPTH     DEPTH    ALBEDO   FACTOR   TEMPERATURE' )
1191  FORMAT( /, 37X, '<------------- DELTA-M --------------->', /,
     $'                   TOTAL    SINGLE                           ',
     $               'TOTAL    SINGLE', /,
     $'       OPTICAL   OPTICAL   SCATTER   TRUNCATED   ',
     $   'OPTICAL   OPTICAL   SCATTER    ASYMM', /,
     $'         DEPTH     DEPTH    ALBEDO    FRACTION     ',
     $     'DEPTH     DEPTH    ALBEDO   FACTOR' )
1200  FORMAT( I4, 2F10.4, F10.5, F12.5, 2F10.4, F10.5, F9.4,F14.3 )
1210  FORMAT( 85X, F14.3 )
1300  FORMAT( I6, 10F11.6, /, (6X,10F11.6) )
C
      END
      SUBROUTINE  PRTINT( UU, UTAU, NTAU, UMU, NUMU, PHI, NPHI,
     $                    MAXULV, MAXUMU )
C
C         PRINTS THE INTENSITY AT USER POLAR AND AZIMUTHAL ANGLES
C
C     ALL ARGUMENTS ARE DISORT INPUT OR OUTPUT VARIABLES
C
C+---------------------------------------------------------------------+
      REAL   PHI(*), UMU(*), UTAU(*), UU( MAXUMU, MAXULV, * )
C
C
      WRITE ( *, '(//,A)' )
     $         ' *********  I N T E N S I T I E S  *********'
      LENFMT = 10
      NPASS = 1 + NPHI / LENFMT
      IF ( MOD(NPHI,LENFMT) .EQ. 0 )  NPASS = NPASS - 1
      DO 10  LU = 1, NTAU
         DO 10  NP = 1, NPASS
            JMIN = 1 + LENFMT * (NP-1)
            JMAX = MIN0( LENFMT*NP, NPHI )
            WRITE( *,101 )  ( PHI(J), J = JMIN, JMAX )
            DO 10  IU = 1, NUMU
               IF( IU.EQ.1 )  WRITE( *,102 )  UTAU(LU), UMU(IU),
     $           ( UU( IU,LU,J ), J = JMIN, JMAX )
               IF( IU.GT.1 )  WRITE( *,103 )  UMU(IU),
     $           ( UU( IU,LU,J ), J = JMIN, JMAX )
10    CONTINUE
C
      RETURN
C
101   FORMAT( /, 3X,'          POLAR   AZIMUTH ANGLES (DEGREES)',
     $        /, 3X,'OPTICAL   ANGLE',
     $        /, 3X,' DEPTH   COSINE', 10F11.2 )
102   FORMAT( F10.4, F8.4, 1P,10E11.3 )
103   FORMAT( 10X,   F8.4, 1P,10E11.3 )
      END
      SUBROUTINE  QGAUSN1( M, GMU, GWT )
C
C       COMPUTE WEIGHTS AND ABSCISSAE FOR ORDINARY GAUSSIAN QUADRATURE
C       (NO WEIGHT FUNCTION INSIDE INTEGRAL) ON THE INTERVAL (0,1)
C
C       REFERENCE:  Davis, P.J. and P. Rabinowitz, Methods of Numerical
C                   Integration, Academic Press, New York, pp. 87, 1975.
C
C          METHOD:  Compute the abscissae as roots of the Legendre
C                   Polynomial P-SUB-N using a cubically convergent
C                   refinement of Newton's method.  Compute the
C                   weights from EQ. 2.7.3.8 of Davis/Rabinowitz.
C
C        ACCURACY:  at least 13 significant digits
C
C
C  I N P U T :    M       ORDER OF QUADRATURE RULE
C
C  O U T P U T :  GMU(I)  I = 1 TO M,    ARRAY OF ABSCISSAE
C                 GWT(I)  I = 1 TO M,    ARRAY OF WEIGHTS
C
C  I N T E R N A L    V A R I A B L E S:
C
C    PM2,PM1,P : 3 SUCCESSIVE LEGENDRE POLYNOMIALS
C    PPR       : DERIVATIVE OF LEGENDRE POLYNOMIAL
C    P2PRI     : 2ND DERIVATIVE OF LEGENDRE POLYNOMIAL
C    TOL       : CONVERGENCE CRITERION FOR LEGENDRE POLY ROOT ITERATION
C    X,XI      : SUCCESSIVE ITERATES IN CUBICALLY-
C                CONVERGENT VERSION OF NEWTON'S METHOD
C                ( SEEKING ROOTS OF LEGENDRE POLYNOMIAL )
C+---------------------------------------------------------------------+
      REAL     CONA, GMU(*), GWT(*), PI, T
      INTEGER  LIM, M, NP1
      DOUBLE   PRECISION  D1MACH2
      DOUBLE   PRECISION  EN, NNP1, P, PM1, PM2, PPR, P2PRI, PROD,
     $                    TMP, TOL, X, XI
      SAVE PI
      DATA     PI / 0.0 /
C
C
      IF ( PI.EQ.0.0 )  THEN
         PI = 2. * ASIN(1.0)
      END IF
         TOL = 10. * D1MACH2(4)
C
      IF ( M.LE.1 )  THEN
         M = 1
         GMU( 1 ) = 0.5
         GWT( 1 ) = 1.0
         RETURN
      END IF
C
      EN   = M
      NP1  = M + 1
      NNP1 = M * NP1
      CONA = FLOAT( M-1 ) / ( 8 * M**3 )
C                                        ** INITIAL GUESS FOR K-TH ROOT
C                                        ** OF LEGENDRE POLYNOMIAL, FROM
C                                        ** DAVIS/RABINOWITZ (2.7.3.3A)
      LIM  = M / 2
      DO 30  K = 1, LIM
         T = ( 4*K - 1 ) * PI / ( 4*M + 2 )
         X = COS ( T + CONA / TAN( T ) )
C                                        ** RECURSION RELATION FOR
C                                        ** LEGENDRE POLYNOMIALS
10       PM2 = 1.D0
         PM1 = X
         DO 20 NN = 2, M
            P   = ( ( 2*NN - 1 ) * X * PM1 - ( NN-1 ) * PM2 ) / NN
            PM2 = PM1
            PM1 = P
20       CONTINUE
C
         TMP   = 1.D0 / ( 1.D0 - X**2 )
         PPR   = EN * ( PM2 - X * P ) * TMP
         P2PRI = ( 2.D0 * X * PPR - NNP1 * P ) * TMP
         XI    = X - ( P / PPR ) * ( 1.D0 +
     $               ( P / PPR ) * P2PRI / ( 2.D0 * PPR ) )
C
C                                              ** CHECK FOR CONVERGENCE
         IF ( DABS(XI-X) .GT. TOL ) THEN
            X = XI
            GO TO 10
         END IF
C                          ** ITERATION FINISHED--CALC. WEIGHTS,
C                          ** ABSCISSAE FOR (-1,1)
         GMU( K ) = - X
         GWT( K ) = 2.D0 / ( TMP * ( EN * PM2 )**2 )
         GMU( NP1 - K ) = - GMU( K )
         GWT( NP1 - K ) =   GWT( K )
30    CONTINUE
C                                    ** SET MIDDLE ABSCISSA AND WEIGHT
C                                    ** FOR RULES OF ODD ORDER
      IF ( MOD( M,2 ) .NE. 0 )  THEN
         GMU( LIM + 1 ) = 0.0
         PROD = 1.D0
         DO 40 K = 3, M, 2
            PROD = PROD * K / ( K-1 )
40       CONTINUE
         GWT( LIM + 1 ) = 2.D0 / PROD**2
      END IF
C                                        ** CONVERT FROM (-1,1) TO (0,1)
      DO 50  K = 1, M
         GMU( K ) = 0.5 * GMU( K ) + 0.5
         GWT( K ) = 0.5 * GWT( K )
50    CONTINUE
C
      RETURN
      END
      REAL FUNCTION R1MACH(I)
C
C  SINGLE-PRECISION MACHINE CONSTANTS
C
C  ASSUME FLOATING-POINT NUMBERS ARE REPRESENTED IN THE T-DIGIT,
C  BASE-B FORM
C
C     (SIGN) (B**E) * ( (X(1)/B) + ... + (X(T)/B**T) )
C
C  WHERE 0.LE.X(I).LT.B, I=1,...,T, X(1).GT.0; EMIN.LE.E.LE.EMAX.
C  FOR EXAMPLE, IN BASE 10, THE X(I) WOULD JUST BE THE DIGITS
C  FOLLOWING THE DECIMAL POINT, AND CLEARLY THE FIRST DIGIT (X(1))
C  WOULD HAVE TO BE NONZERO OR THE DECIMAL POINT COULD BE MOVED
C  OVER WITH A CORRESPONDING CHANGE IN 'E'.  THEN
C
C  R1MACH(1) = B**(EMIN-1), SMALLEST POSITIVE MAGNITUDE
C                 (E = EMIN, X(1) = 1, ALL OTHER X(I) = 0).
C  R1MACH(2) = B**EMAX*(1 - B**(-T)), LARGEST MAGNITUDE
C                 (E = EMAX, X(I) = B-1).
C  R1MACH(3) = 1/B**T, SMALLEST RELATIVE SPACING.
C  R1MACH(4) = 1/B**(T-1), MACHINE PRECISION (X(1)=X(T)=1,ALL OTHER
C                 X(I) = 0); SMALLEST POSITIVE EPS SUCH THAT 1+EPS.NE.1
C  R1MACH(5) = LOG10(B)
C
C  REFERENCE: FOX P.A., HALL A.D., SCHRYER N.L.,'FRAMEWORK FOR A
C               PORTABLE LIBRARY', ACM TRANSACTIONS ON MATHEMATICAL
C               SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
C
C  TO ALTER THIS FUNCTION FOR A PARTICULAR ENVIRONMENT,
C  THE DESIRED SET OF DATA STATEMENTS SHOULD BE ACTIVATED BY
C  DELETING THE C FROM COLUMN 1.
C  ON RARE MACHINES A STATIC STATEMENT MAY NEED TO BE ADDED.
C  (BUT PROBABLY MORE SYSTEMS PROHIBIT IT THAN REQUIRE IT.)
C
C  FOR IEEE-ARITHMETIC MACHINES (BINARY STANDARD), THE FIRST
C  SET OF CONSTANTS BELOW SHOULD BE APPROPRIATE.
C
C  WHERE POSSIBLE, DECIMAL, OCTAL OR HEXADECIMAL CONSTANTS ARE USED
C  TO SPECIFY THE CONSTANTS EXACTLY.  SOMETIMES THIS REQUIRES USING
C  EQUIVALENT INTEGER ARRAYS.  IF YOUR COMPILER USES HALF-WORD
C  INTEGERS BY DEFAULT (SOMETIMES CALLED INTEGER*2), YOU MAY NEED TO
C  CHANGE INTEGER TO INTEGER*4 OR OTHERWISE INSTRUCT YOUR COMPILER
C  TO USE FULL-WORD INTEGERS IN THE NEXT 5 DECLARATIONS.
C
      INTEGER SMALL(2), LARGE(2), RIGHT(2), DIVER(2), LOG10(2), SC
      REAL RMACH(5)

      EQUIVALENCE (RMACH(1),SMALL(1)), (RMACH(2),LARGE(1)),
     $            (RMACH(3),RIGHT(1)), (RMACH(4),DIVER(1)),
     $            (RMACH(5),LOG10(1))

      LOGICAL  PASS1
      SAVE     PASS1
      DATA     PASS1/.TRUE./

C IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
C 3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
C PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).

C      DATA SMALL(1)/8388608/, LARGE(1)/2139095039/,
C     $     RIGHT(1)/864026624/, DIVER(1)/872415232/,
C     $     LOG10(1)/ 1050288283/, SC/987/

C AMDAHL MACHINES.

C      DATA SMALL(1)/1048576/, LARGE(1)/2147483647/,
C     $     RIGHT(1)/990904320/, DIVER(1)/1007681536/,
C     $     LOG10(1)/1091781651/, SC/987/

C BURROUGHS 1700 SYSTEM.

C      DATA RMACH/Z400800000,Z5FFFFFFFF,Z4E9800000,Z4EA800000,
C     $             Z500E730E8/, SC/987/

C BURROUGHS 5700/6700/7700 SYSTEMS.

C      DATA RMACH/O1771000000000000,O0777777777777777,O1311000000000000,
C     $             O1301000000000000,O1157163034761675/, SC/987/

C FTN4 ON CDC 6000/7000 SERIES.

C      DATA RMACH/00564000000000000000B,37767777777777777776B,
C     $ 16414000000000000000B,16424000000000000000B,
C     $ 17164642023241175720B/, SC/987/

C FTN5 ON CDC 6000/7000 SERIES.

C      DATA RMACH/O"00564000000000000000",O"37767777777777777776",
C     $ O"16414000000000000000",O"16424000000000000000",
C     $ O"17164642023241175720"/, SC/987/

C CONVEX C-1.

C      DATA RMACH/'00800000'X,'7FFFFFFF'X,'34800000'X,
C     $ '35000000'X,'3F9A209B'X/, SC/987/

C CRAY 1, XMP, 2, AND 3.

C      DATA RMACH/200034000000000000000B,577767777777777777776B,
C     $ 377224000000000000000B,377234000000000000000B,
C     $ 377774642023241175720B/, SC/987/

C DATA GENERAL ECLIPSE S/200.
C NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING LINE -
C STATIC RMACH(5)

C      DATA SMALL/20K,0/, LARGE/77777K,177777K/, RIGHT/35420K,0/,
C     $  DIVER/36020K,0/, LOG10/40423K,42023K/, SC/987/

C HARRIS SLASH 6 AND SLASH 7.

C      DATA SMALL/'20000000,'00000201/, LARGE/'37777777,'00000177/,
C     $  RIGHT/'20000000,'00000352/, DIVER/'20000000,'00000353/,
C     $  LOG10/'23210115,'00000377/, SC/987/

C HONEYWELL DPS 8/70 SERIES.

C      DATA RMACH/O402400000000,O376777777777,O714400000000,
C     $ O716400000000,O776464202324/, SC/987/

C IBM 360/370 SERIES,
C XEROX SIGMA 5/7/9 AND SEL SYSTEMS 85/86.

C      DATA RMACH/Z00100000,Z7FFFFFFF,Z3B100000,Z3C100000,
C     $ Z41134413/, SC/987/

C INTERDATA 8/32 WITH UNIX SYSTEM FORTRAN 77 COMPILER.
C FOR INTERDATA FORTRAN VII COMPILER REPLACE
C Z'S SPECIFYING HEX CONSTANTS WITH Y'S.

C      DATA RMACH/Z'00100000',Z'7EFFFFFF',Z'3B100000',Z'3C100000',
C     $ Z'41134413'/, SC/987/

C PDP-10 (KA OR KI PROCESSOR).

C      DATA RMACH/"000400000000,"377777777777,"146400000000,
C     $ "147400000000,"177464202324/, SC/987/

C PDP-11 FORTRANS SUPPORTING 32-BIT INTEGERS
C (EXPRESSED IN INTEGER AND OCTAL).

C      DATA SMALL(1)/8388608/, LARGE(1)/2147483647/,
C     $  RIGHT(1)/880803840/, DIVER(1)/889192448/,
C     $  LOG10(1)/1067065499/, SC/987/

C      DATA RMACH/O00040000000,O17777777777,O06440000000,
C     $ O06500000000,O07746420233/, SC/987/

C PDP-11 FORTRANS SUPPORTING 16-BIT INTEGERS
C (EXPRESSED IN INTEGER AND OCTAL).

C      DATA SMALL/128,0/, LARGE/32767,-1/, RIGHT/13440,0/,
C     $  DIVER/13568,0/, LOG10/16282,8347/, SC/987/

C      DATA SMALL/O000200,O000000/, LARGE/O077777,O177777/,
C     $  RIGHT/O032200,O000000/, DIVER/O032400,O000000/,
C     $  LOG10/O037632,O020233/, SC/987/

C SEQUENT BALANCE 8000.

C      DATA SMALL(1)/$00800000/, LARGE(1)/$7F7FFFFF/,
C     $  RIGHT(1)/$33800000/, DIVER(1)/$34000000/,
C     $  LOG10(1)/$3E9A209B/, SC/987/

C UNIVAC 1100 SERIES.

C      DATA RMACH/O000400000000,O377777777777,O146400000000,
C     $ O147400000000,O177464202324/, SC/987/

C VAX UNIX F77 COMPILER.

C      DATA SMALL(1)/128/, LARGE(1)/-32769/, RIGHT(1)/13440/,
C     $  DIVER(1)/13568/, LOG10(1)/547045274/, SC/987/

C VAX-11 WITH FORTRAN IV-PLUS COMPILER.

C      DATA RMACH/Z00000080,ZFFFF7FFF,Z00003480,Z00003500,
C     $ Z209B3F9A/, SC/987/

C VAX/VMS VERSION 2.2.

C      DATA RMACH/'80'X,'FFFF7FFF'X,'3480'X,'3500'X,
C     $ '209B3F9A'X/, SC/987/

      IF( PASS1 )  THEN

         IF (SC.NE.987)
     $       CALL ERRMSG( 'R1MACH--NO DATA STATEMENTS ACTIVE',.TRUE.)

C                      ** CALCULATE MACHINE PRECISION
         EPSNEW = 0.01
   10    EPS = EPSNEW
            EPSNEW = EPSNEW / 1.1
C                               ** IMPORTANT TO STORE 'S' SINCE MAY BE
C                               ** KEPT IN HIGHER PRECISION IN REGISTERS
            S = 1.0 + EPSNEW
            IF( S.GT.1.0 ) GO TO 10
         RATIO = EPS / RMACH(4)
         IF( RATIO.LT.0.5 .OR. RATIO.GT.2.0 )
     $       CALL ERRMSG( 'R1MACH--TABULATED PRECISION WRONG',.TRUE.)

      END IF

      IF (I.LT.1.OR.I.GT.5)
     $    CALL ERRMSG( 'R1MACH--ARGUMENT OUT OF BOUNDS',.TRUE.)
      R1MACH = RMACH(I)
      RETURN
      END
      REAL FUNCTION  RATIO( A, B )
C
C        CALCULATE RATIO  A/B  WITH OVER- AND UNDER-FLOW PROTECTION
C
         IF ( ABS(A).LT.1.0E-8 .AND. ABS(B).LT.1.0E-8 )  THEN
            RATIO = 1.0
         ELSE IF ( B.EQ.0.0 )  THEN
            RATIO = 1.E+20
         ELSE
            RATIO = A / B
         END IF
C
      RETURN
      END
      REAL FUNCTION  SASUM( N, SX, INCX )
C
C  --INPUT--  N  NUMBER OF ELEMENTS IN VECTOR TO BE SUMMED
C            SX  SING-PREC ARRAY, LENGTH 1+(N-1)*INCX, CONTAINING VECTOR
C          INCX  SPACING OF VECTOR ELEMENTS IN 'SX'
C
C --OUTPUT-- SASUM   SUM FROM 0 TO N-1 OF  ABS(SX(1+I*INCX))
C
      REAL SX(*)
C
C
      SASUM = 0.0
      IF( N.LE.0 )  RETURN
      IF( INCX.NE.1 ) THEN
C                                          ** NON-UNIT INCREMENTS
          DO 10 I = 1, 1+(N-1)*INCX, INCX
             SASUM = SASUM + ABS(SX(I))
   10     CONTINUE
      ELSE
C                                          ** UNIT INCREMENTS
         M = MOD(N,6)
         IF( M.NE.0 ) THEN
C                             ** CLEAN-UP LOOP SO REMAINING VECTOR
C                             ** LENGTH IS A MULTIPLE OF 6.
            DO 30  I = 1, M
              SASUM = SASUM + ABS(SX(I))
   30       CONTINUE
         ENDIF
C                              ** UNROLL LOOP FOR SPEED
         DO 50  I = M+1, N, 6
           SASUM = SASUM + ABS(SX(I))   + ABS(SX(I+1)) + ABS(SX(I+2))
     $                   + ABS(SX(I+3)) + ABS(SX(I+4)) + ABS(SX(I+5))
   50    CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE     SAXPY( N, SA, SX, INCX, SY, INCY )
C
C          Y = A*X + Y  (X, Y = VECTORS, A = SCALAR)
C
C  --INPUT--
C        N  NUMBER OF ELEMENTS IN INPUT VECTORS 'X' AND 'Y'
C       SA  SINGLE PRECISION SCALAR MULTIPLIER 'A'
C       SX  SING-PREC ARRAY CONTAINING VECTOR 'X'
C     INCX  SPACING OF ELEMENTS OF VECTOR 'X' IN 'SX'
C       SY  SING-PREC ARRAY CONTAINING VECTOR 'Y'
C     INCY  SPACING OF ELEMENTS OF VECTOR 'Y' IN 'SY'
C
C --OUTPUT--
C       SY   FOR I = 0 TO N-1, OVERWRITE  SY(LY+I*INCY) WITH
C                 SA*SX(LX+I*INCX) + SY(LY+I*INCY),
C            WHERE LX = 1          IF INCX .GE. 0,
C                     = (-INCX)*N  IF INCX .LT. 0
C            AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
C
      REAL SX(*), SY(*), SA
C
C
      IF( N.LE.0 .OR. SA.EQ.0.0 ) RETURN
C
      IF ( INCX.EQ.INCY .AND. INCX.GT.1 )  THEN
C
          DO 10  I = 1, 1+(N-1)*INCX, INCX
             SY(I) = SY(I) + SA * SX(I)
   10     CONTINUE
C
      ELSE IF ( INCX.EQ.INCY .AND. INCX.EQ.1 )  THEN
C
C                                        ** EQUAL, UNIT INCREMENTS
         M = MOD(N,4)
         IF( M .NE. 0 ) THEN
C                            ** CLEAN-UP LOOP SO REMAINING VECTOR LENGTH
C                            ** IS A MULTIPLE OF 4.
            DO 20  I = 1, M
              SY(I) = SY(I) + SA * SX(I)
   20       CONTINUE
         ENDIF
C                              ** UNROLL LOOP FOR SPEED
         DO 30  I = M+1, N, 4
            SY(I)   = SY(I)   + SA * SX(I)
            SY(I+1) = SY(I+1) + SA * SX(I+1)
            SY(I+2) = SY(I+2) + SA * SX(I+2)
            SY(I+3) = SY(I+3) + SA * SX(I+3)
   30    CONTINUE
C
      ELSE
C               ** NONEQUAL OR NONPOSITIVE INCREMENTS.
         IX = 1
         IY = 1
         IF( INCX.LT.0 )  IX = 1 + (N-1)*(-INCX)
         IF( INCY.LT.0 )  IY = 1 + (N-1)*(-INCY)
         DO 40  I = 1, N
            SY(IY) = SY(IY) + SA*SX(IX)
            IX = IX + INCX
            IY = IY + INCY
   40    CONTINUE
C
      ENDIF
C
      RETURN
      END
      REAL FUNCTION  SDOT( N, SX, INCX, SY, INCY )
C
C          S.P. DOT PRODUCT OF VECTORS  'X'  AND  'Y'
C
C  --INPUT--
C        N  NUMBER OF ELEMENTS IN INPUT VECTORS 'X' AND 'Y'
C       SX  SING-PREC ARRAY CONTAINING VECTOR 'X'
C     INCX  SPACING OF ELEMENTS OF VECTOR 'X' IN 'SX'
C       SY  SING-PREC ARRAY CONTAINING VECTOR 'Y'
C     INCY  SPACING OF ELEMENTS OF VECTOR 'Y' IN 'SY'
C
C --OUTPUT--
C     SDOT   SUM FOR I = 0 TO N-1 OF  SX(LX+I*INCX) * SY(LY+I*INCY),
C            WHERE  LX = 1          IF INCX .GE. 0,
C                      = (-INCX)*N  IF INCX .LT. 0,
C            AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
C
      REAL SX(*), SY(*)
C
C
      SDOT = 0.0
      IF( N.LE.0 )  RETURN
C
      IF ( INCX.EQ.INCY .AND. INCX.GT.1 )  THEN
C
          DO 10  I = 1, 1+(N-1)*INCX, INCX
             SDOT = SDOT + SX(I) * SY(I)
   10     CONTINUE
C
      ELSE IF ( INCX.EQ.INCY .AND. INCX.EQ.1 )  THEN
C
C                                        ** EQUAL, UNIT INCREMENTS
         M = MOD(N,5)
         IF( M .NE. 0 ) THEN
C                            ** CLEAN-UP LOOP SO REMAINING VECTOR LENGTH
C                            ** IS A MULTIPLE OF 4.
            DO 20  I = 1, M
               SDOT = SDOT + SX(I) * SY(I)
   20       CONTINUE
         ENDIF
C                              ** UNROLL LOOP FOR SPEED
         DO 30  I = M+1, N, 5
            SDOT = SDOT + SX(I)*SY(I)     + SX(I+1)*SY(I+1)
     $                  + SX(I+2)*SY(I+2) + SX(I+3)*SY(I+3)
     $                  + SX(I+4)*SY(I+4)
   30    CONTINUE
C
      ELSE
C               ** NONEQUAL OR NONPOSITIVE INCREMENTS.
         IX = 1
         IY = 1
         IF( INCX.LT.0 )  IX = 1 + (N-1)*(-INCX)
         IF( INCY.LT.0 )  IY = 1 + (N-1)*(-INCY)
         DO 40  I = 1, N
            SDOT = SDOT + SX(IX) * SY(IY)
            IX = IX + INCX
            IY = IY + INCY
   40    CONTINUE
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE  SETDIS( CMU, CWT, DELTAM, DTAUC, EXPBEA, FBEAM, FLYR,
     $                    GL, HL, HLPR, IBCND, LAMBER, LAYRU, LYRCUT,
     $                    MAXUMU, MAXCMU, MXCMU, NCUT, NLYR, NTAU, NN,
     $                    NSTR, NOPLNK, NUMU, ONLYFL, OPRIM, PMOM,SSALB,
     $                    TAUC, TAUCPR, UTAU, UTAUPR, UMU, UMU0, USRTAU,
     $                    USRANG, MXCLY )
C
C  MODIF : MXCLY EN ARGUMENT
C
C          PERFORM MISCELLANEOUS SETTING-UP OPERATIONS
C
C       ROUTINES CALLED:  ERRMSG, QGAUSN1, ZEROIT
C
C       INPUT :  ALL ARE 'DISORT' INPUT VARIABLES (SEE DOC FILE)
C
C       OUTPUT:  NTAU,UTAU   IF USRTAU = FALSE
C                NUMU,UMU    IF USRANG = FALSE
C                CMU,CWT     COMPUTATIONAL POLAR ANGLES AND
C                               CORRESPONDING QUADRATURE WEIGHTS
C                EXPBEA      TRANSMISSION OF DIRECT BEAM
C                FLYR        TRUNCATED FRACTION IN DELTA-M METHOD
C                GL          PHASE FUNCTION LEGENDRE COEFFICIENTS MULTI-
C                              PLIED BY (2L+1) AND SINGLE-SCATTER ALBEDO
C                HLPR        LEGENDRE MOMENTS OF SURFACE BIDIRECTIONAL
C                              REFLECTIVITY, TIMES 2K+1
C                LAYRU       COMPUTATIONAL LAYER IN WHICH -UTAU- FALLS
C                LYRCUT      FLAG AS TO WHETHER RADIATION WILL BE ZEROED
C                              BELOW LAYER -NCUT-
C                NCUT        COMPUTATIONAL LAYER WHERE ABSORPTION
C                              OPTICAL DEPTH FIRST EXCEEDS -ABSCUT-
C                NN          NSTR / 2
C                OPRIM       DELTA-M-SCALED SINGLE-SCATTER ALBEDO
C                TAUCPR      DELTA-M-SCALED OPTICAL DEPTH
C                UTAUPR      DELTA-M-SCALED VERSION OF -UTAU-
C
      LOGICAL  DELTAM, LAMBER, LYRCUT, NOPLNK, ONLYFL, USRTAU, USRANG
      INTEGER  LAYRU(*)
      REAL     CMU(*), CWT(*), DTAUC(*), EXPBEA(0:*), FLYR(*),
     $         GL(0:MXCMU,*), HL(0:*), HLPR(0:*), OPRIM(*),
     $         PMOM(0:MAXCMU,*), SSALB(*), TAUC(0:*), TAUCPR(0:*),
     $         UTAU(*), UTAUPR(*), UMU(*)
      DATA  ABSCUT / 10. /
C
C
      IF ( .NOT.USRTAU ) THEN
C                              ** SET OUTPUT LEVELS AT COMPUTATIONAL
C                              ** LAYER BOUNDARIES
         NTAU = NLYR + 1
         DO 30  LC = 0, NTAU-1
            UTAU(LC+1) = TAUC(LC)
30       CONTINUE
      END IF
C                        ** APPLY DELTA-M SCALING AND MOVE DESCRIPTION
C                        ** OF COMPUTATIONAL LAYERS TO LOCAL VARIABLES
      EXPBEA( 0 ) = 1.0
      CALL  ZEROIT( TAUCPR(0), MXCLY+1 )
      CALL  ZEROIT( EXPBEA(1), MXCLY )
      CALL  ZEROIT( FLYR, MXCLY )
      CALL  ZEROIT( GL, (MXCMU+1)*MXCLY )
      CALL  ZEROIT( OPRIM, MXCLY )
      ABSTAU = 0.0
      DO  60  LC = 1, NLYR
         PMOM(0,LC) = 1.0
         IF ( ABSTAU.LT.ABSCUT )  NCUT = LC
         ABSTAU = ABSTAU + ( 1. - SSALB(LC) ) * DTAUC(LC)
C
         IF ( .NOT.DELTAM )  THEN
            OPRIM(LC) = SSALB(LC)
            TAUCPR(LC) = TAUC(LC)
            DO 40  K = 0, NSTR-1
               GL(K,LC) = (2*K+1) * OPRIM(LC) * PMOM(K,LC)
 40         CONTINUE
            F = 0.0
         ELSE
C                                    ** DO DELTA-M TRANSFORMATION
            F = PMOM( NSTR,LC )
            OPRIM(LC) = SSALB(LC) * ( 1. - F ) / ( 1. - F * SSALB(LC) )
            TAUCPR(LC) = TAUCPR(LC-1) + ( 1. - F*SSALB(LC) ) * DTAUC(LC)
            DO 50  K = 0, NSTR-1
               GL(K,LC) = (2*K+1) * OPRIM(LC) * (PMOM(K,LC)-F) / (1.-F)
 50         CONTINUE
         ENDIF
C
         FLYR(LC) = F
         EXPBEA(LC) = 0.0
         IF ( FBEAM.GT.0.0 )  EXPBEA(LC) = EXP( - TAUCPR(LC) / UMU0 )
60    CONTINUE
C                      ** IF NO THERMAL EMISSION, CUT OFF MEDIUM BELOW
C                      ** ABSORPTION OPTICAL DEPTH = ABSCUT ( NOTE THAT
C                      ** DELTA-M TRANSFORMATION LEAVES ABSORPTION
C                      ** OPTICAL DEPTH INVARIANT ).  NOT WORTH THE
C                      ** TROUBLE FOR ONE-LAYER PROBLEMS, THOUGH.
      LYRCUT = .FALSE.
      IF ( ABSTAU.GE.ABSCUT .AND. NOPLNK .AND. IBCND.NE.1
     $     .AND. NLYR.GT.1 )  LYRCUT =.TRUE.
      IF ( .NOT.LYRCUT )  NCUT = NLYR
C
C                             ** SET ARRAYS DEFINING LOCATION OF USER
C                             ** OUTPUT LEVELS WITHIN DELTA-M-SCALED
C                             ** COMPUTATIONAL MESH
      DO 90  LU = 1, NTAU
         DO 70 LC = 1, NLYR
            IF ( UTAU(LU).GE.TAUC(LC-1) .AND. UTAU(LU).LE.TAUC(LC) )
     $           GO TO 80
70       CONTINUE
         LC = NLYR
C
80       UTAUPR(LU) = UTAU(LU)
         IF(DELTAM) UTAUPR(LU) = TAUCPR(LC-1) + (1.-SSALB(LC)*FLYR(LC))
     $                                        * (UTAU(LU) - TAUC(LC-1))
         LAYRU(LU) = LC
90    CONTINUE
C                      ** CALCULATE COMPUTATIONAL POLAR ANGLE COSINES
C                      ** AND ASSOCIATED QUADRATURE WEIGHTS FOR GAUSSIAN
C                      ** QUADRATURE ON THE INTERVAL (0,1) (UPWARD)
      NN = NSTR / 2
      CALL  QGAUSN1( NN, CMU, CWT )
C                                  ** DOWNWARD (NEG) ANGLES AND WEIGHTS
      DO 100  IQ = 1, NN
         CMU(IQ+NN) = - CMU(IQ)
         CWT(IQ+NN) =   CWT(IQ)
100   CONTINUE
C
      IF ( FBEAM.GT.0.0 )  THEN
C                               ** COMPARE BEAM ANGLE TO COMPU'L ANGLES
         DO 110  IQ = 1, NN
            IF ( ABS(UMU0-CMU(IQ))/UMU0 .LT. 1.E-4 )  CALL ERRMSG
     $         ( 'SETDIS--BEAM ANGLE=COMPUTATIONAL ANGLE; CHANGE NSTR',
     $            .TRUE. )
  110    CONTINUE
      END IF
C
      IF ( .NOT.USRANG .OR. (ONLYFL .AND. MAXUMU.GE.NSTR) )  THEN
C
C                                   ** SET OUTPUT POLAR ANGLES TO
C                                   ** COMPUTATIONAL POLAR ANGLES
            NUMU = NSTR
            DO 120  IU = 1, NN
               UMU(IU) = - CMU(NN+1-IU)
120         CONTINUE
            DO 121  IU = NN+1, NSTR
               UMU(IU) = CMU(IU-NN)
121         CONTINUE
      END IF
C
      IF ( USRANG .AND. IBCND.EQ.1 )  THEN
C
C                               ** SHIFT POSITIVE USER ANGLE COSINES TO
C                               ** UPPER LOCATIONS AND PUT NEGATIVES
C                               ** IN LOWER LOCATIONS
         DO 140  IU = 1, NUMU
            UMU(IU+NUMU) = UMU(IU)
140      CONTINUE
         DO 141  IU = 1, NUMU
            UMU(IU) = - UMU( 2*NUMU+1-IU)
141      CONTINUE
         NUMU = 2*NUMU
      END IF
C
      IF ( .NOT.LYRCUT .AND. .NOT.LAMBER )  THEN
         DO 160  K = 0, NSTR
            HLPR(K) = (2*K+1) * HL(K)
160      CONTINUE
      END IF
C
      RETURN
      END
      SUBROUTINE  SETMTX( BDR, CBAND, CMU, CWT, DELM0, GC, KK, LAMBER,
     $                    LYRCUT, MI, MI9M2, MXCMU, NCOL, NCUT, NNLYRI,
     $                    NN, NSTR, TAUCPR, WK )
C
C        CALCULATE COEFFICIENT MATRIX FOR THE SET OF EQUATIONS
C        OBTAINED FROM THE BOUNDARY CONDITIONS AND THE CONTINUITY-
C        OF-INTENSITY-AT-LAYER-INTERFACE EQUATIONS;  STORE IN THE
C        SPECIAL BANDED-MATRIX FORMAT REQUIRED BY LINPACK ROUTINES
C
C     ROUTINES CALLED:  ZEROIT
C
C     I N P U T      V A R I A B L E S:
C
C       BDR      :  SURFACE BIDIRECTIONAL REFLECTIVITY
C       CMU      :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT      :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       DELM0    :  KRONECKER DELTA, DELTA-SUB-M0
C       GC       :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       KK       :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       LYRCUT   :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       NN       :  NUMBER OF STREAMS IN A HEMISPHERE (NSTR/2)
C       NCUT     :  TOTAL NUMBER OF COMPUTATIONAL LAYERS CONSIDERED
C       TAUCPR   :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C   O U T P U T     V A R I A B L E S:
C
C       CBAND    :  LEFT-HAND SIDE MATRIX OF LINEAR SYSTEM EQ. SC(5),
C                   SCALED BY EQ. SC(12); IN BANDED FORM REQUIRED
C                   BY LINPACK SOLUTION ROUTINES
C       NCOL     :  COUNTS OF COLUMNS IN -CBAND-
C
C   I N T E R N A L    V A R I A B L E S:
C
C       IROW     :  POINTS TO ROW IN  -CBAND-
C       JCOL     :  POINTS TO POSITION IN LAYER BLOCK
C       LDA      :  ROW DIMENSION OF -CBAND-
C       NCD      :  NUMBER OF DIAGONALS BELOW OR ABOVE MAIN DIAGONAL
C       NCOL     :  COUNTS OF COLUMNS IN -CBAND-
C       NSHIFT   :  FOR POSITIONING NUMBER OF ROWS IN BAND STORAGE
C       WK       :  TEMPORARY STORAGE FOR 'EXP' EVALUATIONS
C ---------------------------------------------------------------------+
      LOGICAL LAMBER, LYRCUT
      REAL    BDR( MI,0:* ), CBAND( MI9M2,NNLYRI ), CMU(*), CWT(*),
     $        GC( MXCMU,MXCMU,* ), KK( MXCMU,* ), TAUCPR( 0:* ), WK(*)
C
C
      CALL  ZEROIT( CBAND, MI9M2*NNLYRI )
      NCD    = 3*NN - 1
      LDA    = 3*NCD + 1
      NSHIFT = LDA - 2*NSTR + 1
      NCOL   = 0
C                         ** USE CONTINUITY CONDITIONS OF EQ. STWJ(17)
C                         ** TO FORM COEFFICIENT MATRIX IN STWJ(20);
C                         ** EMPLOY SCALING TRANSFORMATION STWJ(22)
      DO 30  LC = 1, NCUT
C
         DO 4  IQ = 1, NN
            WK(IQ) = EXP( KK(IQ,LC) * (TAUCPR(LC) - TAUCPR(LC-1)) )
 4       CONTINUE
C
         JCOL = 0
         DO 10  IQ = 1, NN
            NCOL = NCOL + 1
            IROW = NSHIFT - JCOL
            DO 5  JQ = 1, NSTR
               CBAND(IROW+NSTR,NCOL) =   GC(JQ,IQ,LC)
               CBAND(IROW,     NCOL) = - GC(JQ,IQ,LC) * WK(IQ)
               IROW = IROW + 1
 5          CONTINUE
            JCOL = JCOL + 1
10       CONTINUE
C
         DO 20  IQ = NN+1, NSTR
            NCOL = NCOL + 1
            IROW = NSHIFT - JCOL
            DO 15  JQ = 1, NSTR
               CBAND(IROW+NSTR,NCOL) =   GC(JQ,IQ,LC) * WK(NSTR+1-IQ)
               CBAND(IROW,     NCOL) = - GC(JQ,IQ,LC)
               IROW = IROW + 1
15          CONTINUE
            JCOL = JCOL + 1
20       CONTINUE
C
30    CONTINUE
C                  ** USE TOP BOUNDARY CONDITION OF STWJ(20A) FOR
C                  ** FIRST LAYER
      JCOL = 0
      DO 40  IQ = 1, NN
         EXPA = EXP( KK(IQ,1) * TAUCPR(1) )
         IROW = NSHIFT - JCOL + NN
         DO 35  JQ = NN, 1, -1
            CBAND(IROW,JCOL+1) = GC(JQ,IQ,1) * EXPA
            IROW = IROW+1
35       CONTINUE
         JCOL = JCOL+1
40    CONTINUE
C
      DO 50  IQ = NN+1, NSTR
         IROW = NSHIFT - JCOL + NN
         DO 45  JQ = NN, 1, -1
            CBAND(IROW,JCOL+1) = GC(JQ,IQ,1)
            IROW = IROW+1
45       CONTINUE
         JCOL = JCOL+1
50    CONTINUE
C                           ** USE BOTTOM BOUNDARY CONDITION OF
C                           ** STWJ(20C) FOR LAST LAYER
      NNCOL = NCOL - NSTR
      JCOL  = 0
      DO 70  IQ = 1, NN
         NNCOL = NNCOL + 1
         IROW  = NSHIFT - JCOL + NSTR
C
         DO 60  JQ = NN+1, NSTR
            IF ( LYRCUT .OR. (LAMBER .AND. DELM0.EQ.0) ) THEN
C
C                          ** NO AZIMUTHAL-DEPENDENT INTENSITY IF LAM-
C                          ** BERT SURFACE; NO INTENSITY COMPONENT IF
C                          ** TRUNCATED BOTTOM LAYER
C
               CBAND(IROW,NNCOL) = GC(JQ,IQ,NCUT)
            ELSE
               SUM = 0.0
               DO 55  K = 1, NN
                  SUM = SUM + CWT(K) * CMU(K) * BDR(JQ-NN,K)
     $                        * GC(NN+1-K,IQ,NCUT)
55             CONTINUE
               CBAND(IROW,NNCOL) = GC(JQ,IQ,NCUT) - (1.+DELM0) * SUM
            END IF
C
            IROW = IROW + 1
60       CONTINUE
         JCOL = JCOL + 1
70    CONTINUE
C
      DO 90  IQ = NN+1, NSTR
         NNCOL = NNCOL + 1
         IROW  = NSHIFT - JCOL + NSTR
         EXPA = WK(NSTR+1-IQ)
C
         DO 80  JQ = NN+1, NSTR
C
            IF ( LYRCUT .OR. (LAMBER .AND. DELM0.EQ.0) ) THEN
               CBAND(IROW,NNCOL) = GC(JQ,IQ,NCUT) * EXPA
            ELSE
               SUM = 0.0
               DO 75  K = 1, NN
                  SUM = SUM + CWT(K) * CMU(K) * BDR(JQ-NN,K)
     $                        * GC(NN+1-K,IQ,NCUT)
75             CONTINUE
               CBAND(IROW,NNCOL) = ( GC(JQ,IQ,NCUT)
     $                               - (1.+DELM0) * SUM ) * EXPA
            END IF
C
            IROW = IROW + 1
80       CONTINUE
         JCOL = JCOL + 1
90    CONTINUE
C
      RETURN
      END
      SUBROUTINE  SGBCO( ABD, LDA, N, ML, MU, IPVT, RCOND, Z )
C
C         FACTORS A REAL BAND MATRIX BY GAUSSIAN ELIMINATION
C         AND ESTIMATES THE CONDITION OF THE MATRIX.
C
C         PART OF LINPACK.
C
C         REVISION DATE:  8/1/82
C         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)
C
C     IF  RCOND  IS NOT NEEDED, SGBFA IS SLIGHTLY FASTER.
C     TO SOLVE  A*X = B , FOLLOW SBGCO BY SGBSL.
C     TO COMPUTE  INVERSE(A)*C , FOLLOW SBGCO BY SGBSL.
C     TO COMPUTE  DETERMINANT(A) , FOLLOW SBGCO BY SGBDI.
C
C     ON ENTRY
C
C        ABD     REAL(LDA, N)
C                CONTAINS THE MATRIX IN BAND STORAGE.  THE COLUMNS
C                OF THE MATRIX ARE STORED IN THE COLUMNS OF  ABD  AND
C                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS
C                ML+1 THROUGH 2*ML+MU+1 OF  ABD .
C                SEE THE COMMENTS BELOW FOR DETAILS.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  ABD .
C                LDA MUST BE .GE. 2*ML + MU + 1 .
C
C        N       INTEGER
C                THE ORDER OF THE ORIGINAL MATRIX.
C
C        ML      INTEGER
C                NUMBER OF DIAGONALS BELOW THE MAIN DIAGONAL.
C                0 .LE. ML .LT. N .
C
C        MU      INTEGER
C                NUMBER OF DIAGONALS ABOVE THE MAIN DIAGONAL.
C                0 .LE. MU .LT. N .
C                MORE EFFICIENT IF  ML .LE. MU .
C
C     ON RETURN
C
C        ABD     AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND
C                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        RCOND   REAL
C                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .
C                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS
C                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE
C                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND .
C                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION
C                           1.0 + RCOND .EQ. 1.0
C                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING
C                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF
C                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
C                UNDERFLOWS.
C
C        Z       REAL(N)
C                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.
C                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS
C                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
C
C     BAND STORAGE
C
C           IF  A  IS A BAND MATRIX, THE FOLLOWING PROGRAM SEGMENT
C           WILL SET UP THE INPUT.
C
C                   ML = (BAND WIDTH BELOW THE DIAGONAL)
C                   MU = (BAND WIDTH ABOVE THE DIAGONAL)
C                   M = ML + MU + 1
C                   DO 20 J = 1, N
C                      I1 = MAX0(1, J-MU)
C                      I2 = MIN0(N, J+ML)
C                      DO 10 I = I1, I2
C                         K = I - J + M
C                         ABD(K,J) = A(I,J)
C                10    CONTINUE
C                20 CONTINUE
C
C           THIS USES ROWS  ML+1  THROUGH  2*ML+MU+1  OF  ABD .
C           IN ADDITION, THE FIRST  ML  ROWS IN  ABD  ARE USED FOR
C           ELEMENTS GENERATED DURING THE TRIANGULARIZATION.
C           THE TOTAL NUMBER OF ROWS NEEDED IN  ABD  IS  2*ML+MU+1 .
C           THE  ML+MU BY ML+MU  UPPER LEFT TRIANGLE AND THE
C           ML BY ML  LOWER RIGHT TRIANGLE ARE NOT REFERENCED.
C
C     EXAMPLE:  IF THE ORIGINAL MATRIX IS
C
C           11 12 13  0  0  0
C           21 22 23 24  0  0
C            0 32 33 34 35  0
C            0  0 43 44 45 46
C            0  0  0 54 55 56
C            0  0  0  0 65 66
C
C      THEN  N = 6, ML = 1, MU = 2, LDA .GE. 5  AND ABD SHOULD CONTAIN
C
C            *  *  *  +  +  +  , * = NOT USED
C            *  * 13 24 35 46  , + = USED FOR PIVOTING
C            * 12 23 34 45 56
C           11 22 33 44 55 66
C           21 32 43 54 65  *
C
C
C     ROUTINES CALLED:  FROM LINPACK: SGBFA
C                       FROM BLAS:    SAXPY, SDOT, SSCAL, SASUM
C                       FROM FORTRAN: ABS, AMAX1, MAX0, MIN0, SIGN
C
      INTEGER  LDA, N, ML, MU, IPVT(*)
      REAL     ABD(LDA,*), Z(*)
      REAL     RCOND
C
      REAL     SDOT, EK, T, WK, WKM
      REAL     ANORM, S, SASUM, SM, YNORM
      INTEGER  IS, INFO, J, JU, K, KB, KP1, L, LA, LM, LZ, M, MM
C
C
C                       ** COMPUTE 1-NORM OF A
      ANORM = 0.0E0
      L = ML + 1
      IS = L + MU
      DO 10 J = 1, N
         ANORM = AMAX1(ANORM, SASUM(L,ABD(IS,J), 1))
         IF (IS .GT. ML + 1) IS = IS - 1
         IF (J .LE. MU) L = L + 1
         IF (J .GE. N - ML) L = L - 1
   10 CONTINUE
C                                               ** FACTOR
      CALL SGBFA(ABD, LDA, N, ML, MU, IPVT, INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
C     OVERFLOW.
C
C                     ** SOLVE TRANS(U)*W = E
      EK = 1.0E0
      DO 20 J = 1, N
         Z(J) = 0.0E0
   20 CONTINUE
C
      M = ML + MU + 1
      JU = 0
      DO 100 K = 1, N
         IF (Z(K) .NE. 0.0E0) EK = SIGN(EK, -Z(K))
         IF (ABS(EK-Z(K)) .GT. ABS(ABD(M,K))) THEN
            S = ABS(ABD(M,K))/ABS(EK-Z(K))
            CALL SSCAL(N, S, Z, 1)
            EK = S*EK
         ENDIF
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = ABS(WK)
         SM = ABS(WKM)
         IF (ABD(M,K) .NE. 0.0E0) THEN
            WK  = WK /ABD(M,K)
            WKM = WKM/ABD(M,K)
         ELSE
            WK  = 1.0E0
            WKM = 1.0E0
         ENDIF
         KP1 = K + 1
         JU = MIN0(MAX0(JU, MU+IPVT(K)), N)
         MM = M
         IF (KP1 .LE. JU) THEN
            DO 60 J = KP1, JU
               MM = MM - 1
               SM = SM + ABS(Z(J)+WKM*ABD(MM,J))
               Z(J) = Z(J) + WK*ABD(MM,J)
               S = S + ABS(Z(J))
   60       CONTINUE
            IF (S .LT. SM) THEN
               T = WKM - WK
               WK = WKM
               MM = M
               DO 70 J = KP1, JU
                  MM = MM - 1
                  Z(J) = Z(J) + T*ABD(MM,J)
   70          CONTINUE
            ENDIF
         ENDIF
         Z(K) = WK
  100 CONTINUE
C
      S = 1.0E0 / SASUM(N, Z, 1)
      CALL SSCAL(N, S, Z, 1)
C
C                         ** SOLVE TRANS(L)*Y = W
      DO 120 KB = 1, N
         K = N + 1 - KB
         LM = MIN0(ML, N-K)
         IF (K .LT. N) Z(K) = Z(K) + SDOT(LM, ABD(M+1,K), 1, Z(K+1), 1)
         IF (ABS(Z(K)) .GT. 1.0E0) THEN
            S = 1.0E0 / ABS(Z(K))
            CALL SSCAL(N, S, Z, 1)
         ENDIF
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  120 CONTINUE
C
      S = 1.0E0 / SASUM(N, Z, 1)
      CALL SSCAL(N, S, Z, 1)
C
      YNORM = 1.0E0
C                         ** SOLVE L*V = Y
      DO 140 K = 1, N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         LM = MIN0(ML, N-K)
         IF (K .LT. N) CALL SAXPY(LM, T, ABD(M+1,K), 1, Z(K+1), 1)
         IF (ABS(Z(K)) .GT. 1.0E0) THEN
            S = 1.0E0 / ABS(Z(K))
            CALL SSCAL(N, S, Z, 1)
            YNORM = S*YNORM
         ENDIF
  140 CONTINUE
C
      S = 1.0E0/SASUM(N, Z, 1)
      CALL SSCAL(N, S, Z, 1)
      YNORM = S*YNORM
C                           ** SOLVE  U*Z = W
      DO 160 KB = 1, N
         K = N + 1 - KB
         IF (ABS(Z(K)) .GT. ABS(ABD(M,K))) THEN
            S = ABS(ABD(M,K)) / ABS(Z(K))
            CALL SSCAL(N, S, Z, 1)
            YNORM = S*YNORM
         ENDIF
         IF (ABD(M,K) .NE. 0.0E0) Z(K) = Z(K)/ABD(M,K)
         IF (ABD(M,K) .EQ. 0.0E0) Z(K) = 1.0E0
         LM = MIN0(K, M) - 1
         LA = M - LM
         LZ = K - LM
         T = -Z(K)
         CALL SAXPY(LM, T, ABD(LA,K), 1, Z(LZ), 1)
  160 CONTINUE
C                              ** MAKE ZNORM = 1.0
      S = 1.0E0 / SASUM(N, Z, 1)
      CALL SSCAL(N, S, Z, 1)
      YNORM = S*YNORM
C
      IF (ANORM .NE. 0.0E0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0E0) RCOND = 0.0E0
      RETURN
      END
      SUBROUTINE  SGBFA( ABD, LDA, N, ML, MU, IPVT, INFO )
C
C         FACTORS A REAL BAND MATRIX BY ELIMINATION.
C
C         PART OF LINPACK.
C
C         REVISION DATE:  8/1/82
C         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)
C
C     SGBFA IS USUALLY CALLED BY SBGCO, BUT IT CAN BE CALLED
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
C
C     ON ENTRY
C
C        ABD     REAL(LDA, N)
C                CONTAINS THE MATRIX IN BAND STORAGE.  THE COLUMNS
C                OF THE MATRIX ARE STORED IN THE COLUMNS OF  ABD  AND
C                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS
C                ML+1 THROUGH 2*ML+MU+1 OF  ABD .
C                SEE THE COMMENTS BELOW FOR DETAILS.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  ABD .
C                LDA MUST BE .GE. 2*ML + MU + 1 .
C
C        N       INTEGER
C                THE ORDER OF THE ORIGINAL MATRIX.
C
C        ML      INTEGER
C                NUMBER OF DIAGONALS BELOW THE MAIN DIAGONAL.
C                0 .LE. ML .LT. N .
C
C        MU      INTEGER
C                NUMBER OF DIAGONALS ABOVE THE MAIN DIAGONAL.
C                0 .LE. MU .LT. N .
C                MORE EFFICIENT IF  ML .LE. MU .
C     ON RETURN
C
C        ABD     AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND
C                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U , WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        INFO    INTEGER
C                = 0  NORMAL VALUE.
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
C                     INDICATE THAT SGBSL WILL DIVIDE BY ZERO IF
C                     CALLED.  USE  RCOND  IN SBGCO FOR A RELIABLE
C                     INDICATION OF SINGULARITY.
C
C     BAND STORAGE
C
C           IF  A  IS A BAND MATRIX, THE FOLLOWING PROGRAM SEGMENT
C           WILL SET UP THE INPUT.
C
C                   ML = (BAND WIDTH BELOW THE DIAGONAL)
C                   MU = (BAND WIDTH ABOVE THE DIAGONAL)
C                   M = ML + MU + 1
C                   DO 20 J = 1, N
C                      I1 = MAX0(1, J-MU)
C                      I2 = MIN0(N, J+ML)
C                      DO 10 I = I1, I2
C                         K = I - J + M
C                         ABD(K,J) = A(I,J)
C                10    CONTINUE
C                20 CONTINUE
C
C           THIS USES ROWS  ML+1  THROUGH  2*ML+MU+1  OF  ABD .
C           IN ADDITION, THE FIRST  ML  ROWS IN  ABD  ARE USED FOR
C           ELEMENTS GENERATED DURING THE TRIANGULARIZATION.
C           THE TOTAL NUMBER OF ROWS NEEDED IN  ABD  IS  2*ML+MU+1 .
C           THE  ML+MU BY ML+MU  UPPER LEFT TRIANGLE AND THE
C           ML BY ML  LOWER RIGHT TRIANGLE ARE NOT REFERENCED.
C
C
C     ROUTINES CALLED:  FROM BLAS:    SAXPY, SSCAL, ISAMAX
C                       FROM FORTRAN: MAX0, MIN0
C
      INTEGER  LDA, N, ML, MU, IPVT(*), INFO
      REAL     ABD(LDA,*)
C
      REAL     T
      INTEGER  I,ISAMAX,I0,J,JU,JZ,J0,J1,K,KP1,L,LM,M,MM,NM1
C
C
      M = ML + MU + 1
      INFO = 0
C                        ** ZERO INITIAL FILL-IN COLUMNS
      J0 = MU + 2
      J1 = MIN0(N, M) - 1
      DO 20 JZ = J0, J1
         I0 = M + 1 - JZ
         DO 10 I = I0, ML
            ABD(I,JZ) = 0.0E0
   10    CONTINUE
   20 CONTINUE
      JZ = J1
      JU = 0
C
C                       ** GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
      NM1 = N - 1
      DO 120 K = 1, NM1
         KP1 = K + 1
C                                  ** ZERO NEXT FILL-IN COLUMN
         JZ = JZ + 1
         IF (JZ .LE. N) THEN
            DO 40 I = 1, ML
               ABD(I,JZ) = 0.0E0
   40       CONTINUE
         ENDIF
C                                  ** FIND L = PIVOT INDEX
         LM = MIN0(ML, N-K)
         L = ISAMAX(LM+1, ABD(M,K), 1) + M - 1
         IPVT(K) = L + K - M
C
         IF (ABD(L,K) .EQ. 0.0E0) THEN
C                                      ** ZERO PIVOT IMPLIES THIS COLUMN
C                                      ** ALREADY TRIANGULARIZED
            INFO = K
         ELSE
C                                ** INTERCHANGE IF NECESSARY
            IF (L .NE. M) THEN
               T = ABD(L,K)
               ABD(L,K) = ABD(M,K)
               ABD(M,K) = T
            ENDIF
C                                   ** COMPUTE MULTIPLIERS
            T = -1.0E0 / ABD(M,K)
            CALL SSCAL(LM, T, ABD(M+1,K), 1)
C
C                               ** ROW ELIMINATION WITH COLUMN INDEXING
C
            JU = MIN0(MAX0(JU, MU+IPVT(K)), N)
            MM = M
            DO 80 J = KP1, JU
               L = L - 1
               MM = MM - 1
               T = ABD(L,J)
               IF (L .NE. MM) THEN
                  ABD(L,J) = ABD(MM,J)
                  ABD(MM,J) = T
               ENDIF
               CALL SAXPY(LM, T, ABD(M+1,K), 1, ABD(MM+1,J), 1)
   80       CONTINUE
C
         ENDIF
C
  120 CONTINUE
C
      IPVT(N) = N
      IF (ABD(M,N) .EQ. 0.0E0) INFO = N
      RETURN
      END
      SUBROUTINE  SGBSL( ABD, LDA, N, ML, MU, IPVT, B, JOB )
C
C         SOLVES THE REAL BAND SYSTEM
C            A * X = B  OR  TRANS(A) * X = B
C         USING THE FACTORS COMPUTED BY SBGCO OR SGBFA.
C
C         PART OF LINPACK.
C
C         REVISION DATE:  8/1/82
C         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)
C
C     ON ENTRY
C
C        ABD     REAL(LDA, N)
C                THE OUTPUT FROM SBGCO OR SGBFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  ABD .
C
C        N       INTEGER
C                THE ORDER OF THE ORIGINAL MATRIX.
C
C        ML      INTEGER
C                NUMBER OF DIAGONALS BELOW THE MAIN DIAGONAL.
C
C        MU      INTEGER
C                NUMBER OF DIAGONALS ABOVE THE MAIN DIAGONAL.
C
C        IPVT    INTEGER(N)
C                THE PIVOT VECTOR FROM SBGCO OR SGBFA.
C
C        B       REAL(N)
C                THE RIGHT HAND SIDE VECTOR.
C
C        JOB     INTEGER
C                = 0         TO SOLVE  A*X = B ,
C                = NONZERO   TO SOLVE  TRANS(A)*X = B , WHERE
C                            TRANS(A)  IS THE TRANSPOSE.
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY, THIS INDICATES SINGULARITY,
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
C        CALLED CORRECTLY AND IF SBGCO HAS SET RCOND .GT. 0.0
C        OR SGBFA HAS SET INFO .EQ. 0 .
C
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
C     WITH  P  COLUMNS
C           CALL SGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z)
C           IF (RCOND IS TOO SMALL) GO TO ...
C           DO 10 J = 1, P
C              CALL SGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0)
C        10 CONTINUE
C
C     ROUTINES CALLED:  FROM BLAS:    SAXPY, SDOT
C                       FROM FORTRAN: MIN0
C
      INTEGER  LDA, N, ML, MU, IPVT(*), JOB
      REAL     ABD(LDA,*), B(*)
C
      REAL     SDOT,T
      INTEGER  K,KB,L,LA,LB,LM,M,NM1
C
C
      M = MU + ML + 1
      NM1 = N - 1
      IF (JOB .EQ. 0) THEN
C                               ** JOB = 0 , SOLVE  A * X = B
C                               ** FIRST SOLVE L*Y = B
         IF (ML .NE. 0) THEN
            DO 20 K = 1, NM1
               LM = MIN0(ML, N-K)
               L = IPVT(K)
               T = B(L)
               IF (L .NE. K) THEN
                  B(L) = B(K)
                  B(K) = T
               ENDIF
               CALL SAXPY( LM, T, ABD(M+1,K), 1, B(K+1), 1 )
   20       CONTINUE
         ENDIF
C                           ** NOW SOLVE  U*X = Y
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K) / ABD(M,K)
            LM = MIN0(K, M) - 1
            LA = M - LM
            LB = K - LM
            T = -B(K)
            CALL SAXPY(LM, T, ABD(LA,K), 1, B(LB), 1)
   40    CONTINUE
C
      ELSE
C                          ** JOB = NONZERO, SOLVE  TRANS(A) * X = B
C                                  ** FIRST SOLVE  TRANS(U)*Y = B
         DO 60 K = 1, N
            LM = MIN0(K, M) - 1
            LA = M - LM
            LB = K - LM
            T = SDOT(LM, ABD(LA,K), 1, B(LB), 1)
            B(K) = (B(K) - T)/ABD(M,K)
   60    CONTINUE
C                                  ** NOW SOLVE TRANS(L)*X = Y
         IF (ML .NE. 0) THEN
            DO 80 KB = 1, NM1
               K = N - KB
               LM = MIN0(ML, N-K)
               B(K) = B(K) + SDOT(LM, ABD(M+1,K), 1, B(K+1), 1)
               L = IPVT(K)
               IF (L .NE. K) THEN
                  T = B(L)
                  B(L) = B(K)
                  B(K) = T
               ENDIF
   80       CONTINUE
         ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE  SGECO( A, LDA, N,IPVT, RCOND, Z )
C
C         FACTORS A REAL MATRIX BY GAUSSIAN ELIMINATION
C         AND ESTIMATES THE CONDITION OF THE MATRIX.
C
C         PART OF LINPACK.
C
C         REVISION DATE:  8/1/82
C         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)
C
C         IF  RCOND  IS NOT NEEDED, SGEFA IS SLIGHTLY FASTER.
C         TO SOLVE  A*X = B , FOLLOW SGECO BY SGESL.
C         TO COMPUTE  INVERSE(A)*C , FOLLOW SGECO BY SGESL.
C         TO COMPUTE  DETERMINANT(A) , FOLLOW SGECO BY SGEDI.
C         TO COMPUTE  INVERSE(A) , FOLLOW SGECO BY SGEDI.
C
C     ON ENTRY
C
C        A       REAL(LDA, N)
C                THE MATRIX TO BE FACTORED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     ON RETURN
C
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
C                WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U , WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        RCOND   REAL
C                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .
C                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS
C                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE
C                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND .
C                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION
C                           1.0 + RCOND .EQ. 1.0
C                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING
C                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF
C                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
C                UNDERFLOWS.
C
C        Z       REAL(N)
C                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.
C                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS
C                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
C
C     ROUTINES CALLED:  FROM LINPACK: SGEFA
C                       FROM BLAS:    SAXPY, SDOT, SSCAL, SASUM
C                       FROM FORTRAN: ABS, AMAX1, SIGN
C
      INTEGER  LDA, N, IPVT(*)
      REAL     A(LDA,*), Z(*)
      REAL     RCOND
C
      REAL     SDOT,EK,T,WK,WKM
      REAL     ANORM,S,SASUM,SM,YNORM
      INTEGER  INFO,J,K,KB,KP1,L
C
C
C                        ** COMPUTE 1-NORM OF A
      ANORM = 0.0E0
      DO 10 J = 1, N
         ANORM = AMAX1( ANORM, SASUM(N,A(1,J),1) )
   10 CONTINUE
C                                      ** FACTOR
      CALL SGEFA(A,LDA,N,IPVT,INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
C     OVERFLOW.
C
C                        ** SOLVE TRANS(U)*W = E
      EK = 1.0E0
      DO 20 J = 1, N
         Z(J) = 0.0E0
   20 CONTINUE
C
      DO 100 K = 1, N
         IF (Z(K) .NE. 0.0E0) EK = SIGN(EK, -Z(K))
         IF (ABS(EK-Z(K)) .GT. ABS(A(K,K))) THEN
            S = ABS(A(K,K)) / ABS(EK-Z(K))
            CALL SSCAL(N, S, Z, 1)
            EK = S*EK
         ENDIF
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = ABS(WK)
         SM = ABS(WKM)
         IF (A(K,K) .NE. 0.0E0) THEN
            WK  = WK  / A(K,K)
            WKM = WKM / A(K,K)
         ELSE
            WK  = 1.0E0
            WKM = 1.0E0
         ENDIF
         KP1 = K + 1
         IF (KP1 .LE. N) THEN
            DO 60 J = KP1, N
               SM = SM + ABS(Z(J)+WKM*A(K,J))
               Z(J) = Z(J) + WK*A(K,J)
               S = S + ABS(Z(J))
   60       CONTINUE
            IF (S .LT. SM) THEN
               T = WKM - WK
               WK = WKM
               DO 70 J = KP1, N
                  Z(J) = Z(J) + T*A(K,J)
   70          CONTINUE
            ENDIF
         ENDIF
         Z(K) = WK
  100 CONTINUE
C
      S = 1.0E0 / SASUM(N, Z, 1)
      CALL SSCAL(N, S, Z, 1)
C                                ** SOLVE TRANS(L)*Y = W
      DO 120 KB = 1, N
         K = N + 1 - KB
         IF (K .LT. N) Z(K) = Z(K) + SDOT(N-K, A(K+1,K), 1, Z(K+1), 1)
         IF (ABS(Z(K)) .GT. 1.0E0) THEN
            S = 1.0E0/ABS(Z(K))
            CALL SSCAL(N, S, Z, 1)
         ENDIF
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  120 CONTINUE
C
      S = 1.0E0 / SASUM(N, Z, 1)
      CALL SSCAL(N, S, Z, 1)
C                                 ** SOLVE L*V = Y
      YNORM = 1.0E0
      DO 140 K = 1, N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         IF (K .LT. N) CALL SAXPY(N-K, T, A(K+1,K), 1, Z(K+1), 1)
         IF (ABS(Z(K)) .GT. 1.0E0) THEN
            S = 1.0E0/ABS(Z(K))
            CALL SSCAL(N, S, Z, 1)
            YNORM = S*YNORM
         ENDIF
  140 CONTINUE
C
      S = 1.0E0 / SASUM(N, Z, 1)
      CALL SSCAL(N, S, Z, 1)
C                                  ** SOLVE  U*Z = V
      YNORM = S*YNORM
      DO 160 KB = 1, N
         K = N + 1 - KB
         IF (ABS(Z(K)) .GT. ABS(A(K,K))) THEN
            S = ABS(A(K,K))/ABS(Z(K))
            CALL SSCAL(N, S, Z, 1)
            YNORM = S*YNORM
         ENDIF
         IF (A(K,K) .NE. 0.0E0) Z(K) = Z(K)/A(K,K)
         IF (A(K,K) .EQ. 0.0E0) Z(K) = 1.0E0
         T = -Z(K)
         CALL SAXPY(K-1, T, A(1,K), 1, Z(1), 1)
  160 CONTINUE
C                                   ** MAKE ZNORM = 1.0
      S = 1.0E0 / SASUM(N, Z, 1)
      CALL SSCAL(N, S, Z, 1)
      YNORM = S*YNORM
C
      IF (ANORM .NE. 0.0E0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0E0) RCOND = 0.0E0
      RETURN
      END
      SUBROUTINE  SGEFA( A, LDA, N, IPVT, INFO )
C
C         FACTORS A REAL MATRIX BY GAUSSIAN ELIMINATION.
C
C         PART OF LINPACK.
C
C         REVISION DATE:  8/1/82
C         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)
C
C     SGEFA IS USUALLY CALLED BY SGECO, BUT IT CAN BE CALLED
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
C     (TIME FOR SGECO) = (1 + 9/N)*(TIME FOR SGEFA) .
C
C     ON ENTRY
C
C        A       REAL(LDA, N)
C                THE MATRIX TO BE FACTORED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     ON RETURN
C
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
C                WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U , WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        INFO    INTEGER
C                = 0  NORMAL VALUE.
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
C                     INDICATE THAT SGESL OR SGEDI WILL DIVIDE BY ZERO
C                     IF CALLED.  USE  RCOND  IN SGECO FOR A RELIABLE
C                     INDICATION OF SINGULARITY.
C
C     ROUTINES CALLED:  FROM BLAS:    SAXPY, SSCAL, ISAMAX
C
      INTEGER  LDA, N, IPVT(*), INFO
      REAL     A(LDA,*)
C
      REAL     T
      INTEGER  ISAMAX,J,K,KP1,L,NM1
C
C
C                      ** GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
      INFO = 0
      NM1 = N - 1
      DO 60 K = 1, NM1
         KP1 = K + 1
C                                            ** FIND L = PIVOT INDEX
         L = ISAMAX( N-K+1, A(K,K), 1) + K-1
         IPVT(K) = L
C
         IF (A(L,K) .EQ. 0.0E0) THEN
C                                     ** ZERO PIVOT IMPLIES THIS COLUMN
C                                     ** ALREADY TRIANGULARIZED
            INFO = K
         ELSE
C                                     ** INTERCHANGE IF NECESSARY
            IF (L .NE. K) THEN
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
            ENDIF
C                                     ** COMPUTE MULTIPLIERS
            T = -1.0E0 / A(K,K)
            CALL SSCAL( N-K, T, A(K+1,K), 1 )
C
C                              ** ROW ELIMINATION WITH COLUMN INDEXING
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .NE. K) THEN
                  A(L,J) = A(K,J)
                  A(K,J) = T
               ENDIF
               CALL SAXPY( N-K, T, A(K+1,K), 1, A(K+1,J), 1 )
   30       CONTINUE
C
         ENDIF
C
   60 CONTINUE
C
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0E0) INFO = N
      RETURN
      END
      SUBROUTINE  SGESL( A, LDA, N,IPVT, B, JOB )
C
C         SOLVES THE REAL SYSTEM
C            A * X = B  OR  TRANS(A) * X = B
C         USING THE FACTORS COMPUTED BY SGECO OR SGEFA.
C
C         PART OF LINPACK.
C
C         REVISION DATE:  8/1/82
C         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)
C
C     ON ENTRY
C
C        A       REAL(LDA, N)
C                THE OUTPUT FROM SGECO OR SGEFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        IPVT    INTEGER(N)
C                THE PIVOT VECTOR FROM SGECO OR SGEFA.
C
C        B       REAL(N)
C                THE RIGHT HAND SIDE VECTOR.
C
C        JOB     INTEGER
C                = 0         TO SOLVE  A*X = B ,
C                = NONZERO   TO SOLVE  TRANS(A)*X = B  WHERE
C                            TRANS(A)  IS THE TRANSPOSE.
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY, THIS INDICATES SINGULARITY,
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
C        CALLED CORRECTLY AND IF SGECO HAS SET RCOND .GT. 0.0
C        OR SGEFA HAS SET INFO .EQ. 0 .
C
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
C     WITH  P  COLUMNS
C           CALL SGECO(A,LDA,N,IPVT,RCOND,Z)
C           IF (RCOND IS TOO SMALL) GO TO ...
C           DO 10 J = 1, P
C              CALL SGESL(A,LDA,N,IPVT,C(1,J),0)
C        10 CONTINUE
C
C
C     ROUTINES CALLED:  FROM BLAS:    SAXPY, SDOT
C
      INTEGER  LDA, N, IPVT(*), JOB
      REAL     A(LDA,*), B(*)
C
      REAL     SDOT,T
      INTEGER  K,KB,L,NM1
C
C
      NM1 = N - 1
      IF (JOB .EQ. 0) THEN
C                                 ** JOB = 0 , SOLVE  A * X = B
C                                     ** FIRST SOLVE  L*Y = B
         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .NE. K) THEN
               B(L) = B(K)
               B(K) = T
            ENDIF
            CALL SAXPY( N-K, T, A(K+1,K), 1, B(K+1), 1 )
   20    CONTINUE
C                                    ** NOW SOLVE  U*X = Y
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K) / A(K,K)
            T = -B(K)
            CALL SAXPY( K-1, T, A(1,K), 1, B(1), 1 )
   40    CONTINUE
C
      ELSE
C                         ** JOB = NONZERO, SOLVE  TRANS(A) * X = B
C                                    ** FIRST SOLVE  TRANS(U)*Y = B
         DO 60 K = 1, N
            T = SDOT( K-1, A(1,K), 1, B(1), 1 )
            B(K) = (B(K) - T) / A(K,K)
   60    CONTINUE
C                                    ** NOW SOLVE  TRANS(L)*X = Y
         DO 80 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + SDOT( N-K, A(K+1,K), 1, B(K+1), 1 )
            L = IPVT(K)
            IF (L .NE. K) THEN
               T = B(L)
               B(L) = B(K)
               B(K) = T
            ENDIF
   80    CONTINUE
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE  SLFTST( ACCUR, ALBEDO11, BTEMP, DELTAM, DTAUC, FBEAM,
     $                    FISOT, IBCND, LAMBER, NLYR, NOPLNK, NPHI,
     $                    NUMU, NSTR, NTAU, ONLYFL, PHI, PHI0, PMOM,
     $                    PRNT, SSALB, TEMIS, TEMPER, TTEMP, UMU,
     $                    USRANG, USRTAU, UTAU, UMU0, WVNMHI, WVNMLO,
     $                    COMPAR, FLUP, RFLDIR, RFLDN, UU )
C
C       IF  COMPAR = FALSE, SAVE USER INPUT VALUES THAT WOULD OTHERWISE
C       BE DESTROYED AND REPLACE THEM WITH INPUT VALUES FOR SELF-TEST.
C       IF  COMPAR = TRUE, COMPARE SELF-TEST CASE RESULTS WITH CORRECT
C       ANSWERS AND RESTORE USER INPUT VALUES IF TEST IS PASSED.
C
C       (SEE FILE 'DISORT.DOC' FOR VARIABLE DEFINITIONS.)
C
C     I N T E R N A L    V A R I A B L E S:
C
C         ACC     RELATIVE ACCURACY REQUIRED FOR PASSING SELF-TEST
C         ERRORn  RELATIVE ERRORS IN 'DISORT' OUTPUT VARIABLES
C         OK      LOGICAL VARIABLE FOR DETERMINING FAILURE OF SELF-TEST
C         ALL VARIABLES ENDING IN 'S':  TEMPORARY 'S'TORAGE FOR INPUT
C+---------------------------------------------------------------------+
      REAL     PMOM( 0:* ), TEMPER( 0:* )
      LOGICAL  COMPAR, DELTAM, LAMBER, NOPLNK, OK, ONLYFL, PRNT(*),
     $         USRANG, USRTAU
      REAL     PMOMS( 0:4 ), TEMPES (0:1 )
      LOGICAL  DELTAS, LAMBES, NOPLNS, ONLYFS, PRNTS ( 7 ), USRANS,
     $         USRTAS, TSTBAD
      SAVE     NLYRS, DTAUCS, SSALBS, PMOMS, NSTRS, USRANS, NUMUS,
     $         UMUS, USRTAS, NTAUS, UTAUS, NPHIS, PHIS, IBCNDS,
     $         FBEAMS, UMU0S, PHI0S, FISOTS, LAMBES, ALBEDS, DELTAS,
     $         ONLYFS, ACCURS, NOPLNS, WVNMLS, WVNMHS, BTEMPS, TTEMPS,
     $         TEMISS, TEMPES, PRNTS
      DATA     ACC / 1.E-4 /
C
C
      IF  ( .NOT.COMPAR )  THEN
C                                              ** SAVE USER INPUT VALUES
         NLYRS = NLYR
         DTAUCS = DTAUC
         SSALBS = SSALB
         DO 1  N = 0, 4
            PMOMS(N) = PMOM(N)
 1       CONTINUE
         NSTRS = NSTR
         USRANS = USRANG
         NUMUS  = NUMU
         UMUS  = UMU
         USRTAS = USRTAU
         NTAUS = NTAU
         UTAUS  = UTAU
         NPHIS = NPHI
         PHIS  = PHI
         IBCNDS = IBCND
         FBEAMS = FBEAM
         UMU0S = UMU0
         PHI0S = PHI0
         FISOTS  = FISOT
         LAMBES = LAMBER
         ALBEDS = ALBEDO11
         DELTAS = DELTAM
         ONLYFS = ONLYFL
         ACCURS = ACCUR
         NOPLNS = NOPLNK
         WVNMLS = WVNMLO
         WVNMHS = WVNMHI
         BTEMPS = BTEMP
         TTEMPS = TTEMP
         TEMISS = TEMIS
         TEMPES( 0 ) = TEMPER( 0 )
         TEMPES( 1 ) = TEMPER( 1 )
         DO 3 I = 1, 7
            PRNTS( I ) = PRNT( I )
    3    CONTINUE
C                                     ** SET INPUT VALUES FOR SELF-TEST
         NLYR = 1
         DTAUC = 1.0
         SSALB = 0.9
C                          ** HAZE L MOMENTS
         PMOM(0) = 1.0
         PMOM(1) = 0.8042
         PMOM(2) = 0.646094
         PMOM(3) = 0.481851
         PMOM(4) = 0.359056
         NSTR = 4
         USRANG = .TRUE.
         NUMU  = 1
         UMU  = 0.5
         USRTAU = .TRUE.
         NTAU = 1
         UTAU  = 0.5
         NPHI = 1
         PHI  = 90.0
         IBCND = 0
         FBEAM = 3.14159265
         UMU0 = 0.866
         PHI0 = 0.0
         FISOT  = 1.0
         LAMBER = .TRUE.
         ALBEDO11 = 0.7
         DELTAM = .TRUE.
         ONLYFL = .FALSE.
         ACCUR = 1.E-4
         NOPLNK = .FALSE.
         WVNMLO = 0.0
         WVNMHI = 50000.
         BTEMP = 300.0
         TTEMP = 100.0
         TEMIS = 0.8
         TEMPER( 0 ) = 210.0
         TEMPER( 1 ) = 200.0
         DO 5 I = 1, 7
            PRNT( I ) = .FALSE.
    5    CONTINUE
C
      ELSE
C                                    ** COMPARE TEST CASE RESULTS WITH
C                                    ** CORRECT ANSWERS AND ABORT IF BAD
         OK = .TRUE.
         ERROR1 = ( UU  - 47.86005 ) / 47.86005
         ERROR2 = ( RFLDIR - 1.527286 ) / 1.527286
         ERROR3 = ( RFLDN - 28.37223 ) / 28.37223
         ERROR4 = ( FLUP   - 152.5853 ) / 152.5853
         IF( ABS(ERROR1).GT.ACC ) OK = TSTBAD( 'UU',     ERROR1 )
         IF( ABS(ERROR2).GT.ACC ) OK = TSTBAD( 'RFLDIR', ERROR2 )
         IF( ABS(ERROR3).GT.ACC ) OK = TSTBAD( 'RFLDN',  ERROR3 )
         IF( ABS(ERROR4).GT.ACC ) OK = TSTBAD( 'FLUP',   ERROR4 )
C
         IF( .NOT. OK )
     $       CALL ERRMSG( 'DISORT--SELF-TEST FAILED', .TRUE. )
C
C                                           ** RESTORE USER INPUT VALUES
         NLYR = NLYRS
         DTAUC = DTAUCS
         SSALB = SSALBS
         DO 11  N = 0, 4
            PMOM(N) = PMOMS(N)
 11      CONTINUE
         NSTR = NSTRS
         USRANG = USRANS
         NUMU  = NUMUS
         UMU  = UMUS
         USRTAU = USRTAS
         NTAU = NTAUS
         UTAU  = UTAUS
         NPHI = NPHIS
         PHI  = PHIS
         IBCND = IBCNDS
         FBEAM = FBEAMS
         UMU0 = UMU0S
         PHI0 = PHI0S
         FISOT  = FISOTS
         LAMBER = LAMBES
         ALBEDO11 = ALBEDS
         DELTAM = DELTAS
         ONLYFL = ONLYFS
         ACCUR = ACCURS
         NOPLNK = NOPLNS
         WVNMLO = WVNMLS
         WVNMHI = WVNMHS
         BTEMP = BTEMPS
         TTEMP = TTEMPS
         TEMIS = TEMISS
         TEMPER( 0 ) = TEMPES( 0 )
         TEMPER( 1 ) = TEMPES( 1 )
         DO 13  I = 1, 7
            PRNT( I ) = PRNTS( I )
   13    CONTINUE
      END IF
C
      RETURN
      END
      SUBROUTINE  SOLEIG( AMB, APB, ARRAY, CMU, CWT, GL, MI, MAZ,
     $                    MXCMU, NN, NSTR, WK, YLMC, CC, EVECC, EVAL,
     $                    KK, GC )
C
C         SOLVES EIGENVALUE/VECTOR PROBLEM NECESSARY TO CONSTRUCT
C         HOMOGENEOUS PART OF DISCRETE ORDINATE SOLUTION; STWJ(8B)
C         ** NOTE ** EIGENVALUE PROBLEM IS DEGENERATE WHEN SINGLE
C                    SCATTERING ALBEDO = 1;  PRESENT WAY OF DOING IT
C                    SEEMS NUMERICALLY MORE STABLE THAN ALTERNATIVE
C                    METHODS THAT WE TRIED
C
C     ROUTINES CALLED:  ASYMTX
C
C   I N P U T     V A R I A B L E S:
C
C       GL     :  DELTA-M SCALED LEGENDRE COEFFICIENTS OF PHASE FUNCTION
C                    (INCLUDING FACTORS 2L+1 AND SINGLE-SCATTER ALBEDO)
C       CMU    :  COMPUTATIONAL POLAR ANGLE COSINES
C       CWT    :  WEIGHTS FOR QUADRATURE OVER POLAR ANGLE COSINE
C       MAZ    :  ORDER OF AZIMUTHAL COMPONENT
C       NN     :  HALF THE TOTAL NUMBER OF STREAMS
C       YLMC   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE QUADRATURE ANGLES -CMU-
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C   O U T P U T    V A R I A B L E S:
C
C       CC     :  CAPITAL-C-SUB-IJ IN EQ. SS(5); NEEDED IN SS(15&18)
C       EVAL   :  -NN- EIGENVALUES OF EQ. SS(12) ON RETURN FROM 'ASYMTX'
C                    BUT THEN SQUARE ROOTS TAKEN
C       EVECC  :  -NN- EIGENVECTORS  (G+) - (G-)  ON RETURN
C                    FROM 'ASYMTX' ( COLUMN J CORRESPONDS TO -EVAL(J)- )
C                    BUT THEN  (G+) + (G-)  IS CALCULATED FROM SS(10),
C                    G+  AND  G-  ARE SEPARATED, AND  G+  IS STACKED ON
C                    TOP OF  G-  TO FORM -NSTR- EIGENVECTORS OF SS(7)
C       GC     :  PERMANENT STORAGE FOR ALL -NSTR- EIGENVECTORS, BUT
C                    IN AN ORDER CORRESPONDING TO -KK-
C       KK     :  PERMANENT STORAGE FOR ALL -NSTR- EIGENVALUES OF SS(7),
C                    BUT RE-ORDERED WITH NEGATIVE VALUES FIRST ( SQUARE
C                    ROOTS OF -EVAL- TAKEN AND NEGATIVES ADDED )
C
C   I N T E R N A L   V A R I A B L E S:
C
C       AMB,APB :  MATRICES (ALPHA-BETA), (ALPHA+BETA) IN REDUCED
C                    EIGENVALUE PROBLEM
C       ARRAY   :  COMPLETE COEFFICIENT MATRIX OF REDUCED EIGENVALUE
C                    PROBLEM: (ALFA+BETA)*(ALFA-BETA)
C       GPPLGM  :  (G+) + (G-) (CF. EQS. SS(10-11))
C       GPMIGM  :  (G+) - (G-) (CF. EQS. SS(10-11))
C       WK      :  SCRATCH ARRAY REQUIRED BY 'ASYMTX'
C+---------------------------------------------------------------------+
      REAL    AMB( MI,* ), APB( MI,* ), ARRAY( MI,* ), CC( MXCMU,* ),
     $        CMU(*), CWT(*), EVAL(*), EVECC( MXCMU,* ), GC( MXCMU,* ),
     $        GL(0:*), KK(*), WK(*), YLMC( 0:MXCMU,* )
C
C
C                             ** CALCULATE QUANTITIES IN EQS. SS(5-6)
      DO 40 IQ  = 1, NN
C
         DO 20  JQ = 1, NSTR
            SUM = 0.0
            DO 10  L = MAZ, NSTR-1
               SUM = SUM + GL(L) * YLMC(L,IQ) * YLMC(L,JQ)
10          CONTINUE
            CC(IQ,JQ) = 0.5 * SUM * CWT(JQ)
20       CONTINUE
C
         DO 30  JQ = 1, NN
C                             ** FILL REMAINDER OF ARRAY USING SYMMETRY
C                             ** RELATIONS  C(-MUI,MUJ) = C(MUI,-MUJ)
C                             ** AND        C(-MUI,-MUJ) = C(MUI,MUJ)
C
            CC(IQ+NN,JQ) = CC(IQ,JQ+NN)
            CC(IQ+NN,JQ+NN) = CC(IQ,JQ)
C                                      ** GET FACTORS OF COEFF. MATRIX
C                                      ** OF REDUCED EIGENVALUE PROBLEM
            ALPHA =   CC(IQ,JQ) / CMU(IQ)
            BETA = CC(IQ,JQ+NN) / CMU(IQ)
            AMB(IQ,JQ) = ALPHA - BETA
            APB(IQ,JQ) = ALPHA + BETA
30       CONTINUE
         AMB(IQ,IQ) = AMB(IQ,IQ) - 1.0 / CMU(IQ)
         APB(IQ,IQ) = APB(IQ,IQ) - 1.0 / CMU(IQ)
C
40    CONTINUE
C                      ** FINISH CALCULATION OF COEFFICIENT MATRIX OF
C                      ** REDUCED EIGENVALUE PROBLEM:  GET MATRIX
C                      ** PRODUCT (ALFA+BETA)*(ALFA-BETA); SS(12)
      DO 70  IQ = 1, NN
         DO 70  JQ = 1, NN
            SUM = 0.
            DO 60  KQ = 1, NN
               SUM = SUM + APB(IQ,KQ) * AMB(KQ,JQ)
60          CONTINUE
            ARRAY(IQ,JQ) = SUM
70    CONTINUE
C                      ** FIND (REAL) EIGENVALUES AND EIGENVECTORS
C
      CALL  ASYMTX( ARRAY, EVECC, EVAL, NN, MI, MXCMU, IER, WK )
C
      IF ( IER.GT.0 )  THEN
         WRITE( *, '(//,A,I4,A)' )  ' ASYMTX--EIGENVALUE NO. ', IER,
     $     '  DIDNT CONVERGE.  LOWER-NUMBERED EIGENVALUES WRONG.'
         CALL  ERRMSG( 'ASYMTX--CONVERGENCE PROBLEMS', .TRUE. )
      END IF
C
      DO 75  IQ = 1, NN
         EVAL(IQ) = SQRT( ABS( EVAL(IQ) ) )
         KK( IQ+NN ) = EVAL(IQ)
C                                             ** ADD NEGATIVE EIGENVALUE
         KK( NN+1-IQ ) = - EVAL(IQ)
75    CONTINUE
C                          ** FIND EIGENVECTORS (G+) + (G-) FROM SS(10)
C                          ** AND STORE TEMPORARILY IN -APB- ARRAY
      DO 90  JQ = 1, NN
         DO 90  IQ = 1, NN
            SUM = 0.
            DO 80  KQ = 1,NN
               SUM = SUM + AMB(IQ,KQ) * EVECC(KQ,JQ)
80          CONTINUE
            APB(IQ,JQ) = SUM / EVAL(JQ)
90    CONTINUE
C
      DO 100  JQ = 1, NN
         DO 100  IQ = 1, NN
            GPPLGM = APB(IQ,JQ)
            GPMIGM = EVECC(IQ,JQ)
C                                ** RECOVER EIGENVECTORS G+,G- FROM
C                                ** THEIR SUM AND DIFFERENCE; STACK THEM
C                                ** TO GET EIGENVECTORS OF FULL SYSTEM
C                                ** SS(7) (JQ = EIGENVECTOR NUMBER)
C
            EVECC(IQ,      JQ) = 0.5 * ( GPPLGM + GPMIGM )
            EVECC(IQ+NN,   JQ) = 0.5 * ( GPPLGM - GPMIGM )
C
C                                ** EIGENVECTORS CORRESPONDING TO
C                                ** NEGATIVE EIGENVALUES (CORRESP. TO
C                                ** REVERSING SIGN OF 'K' IN SS(10) )
            GPPLGM = - GPPLGM
            EVECC(IQ,   JQ+NN) = 0.5 * ( GPPLGM + GPMIGM )
            EVECC(IQ+NN,JQ+NN) = 0.5 * ( GPPLGM - GPMIGM )
            GC( IQ+NN,   JQ+NN )   = EVECC( IQ,    JQ )
            GC( NN+1-IQ, JQ+NN )   = EVECC( IQ+NN, JQ )
            GC( IQ+NN,   NN+1-JQ ) = EVECC( IQ,    JQ+NN )
            GC( NN+1-IQ, NN+1-JQ ) = EVECC( IQ+NN, JQ+NN )
100   CONTINUE
C
      RETURN
      END
      SUBROUTINE  SOLVE0( B, BDR, BEM, BPLANK, CBAND, CMU, CWT, EXPBEA,
     $                    FBEAM, FISOT, IPVT, LAMBER, LL, LYRCUT,
     $                    MAZ, MI, MI9M2, MXCMU, NCOL, NCUT, NN, NSTR,
     $                    NNLYRI, PI, TPLANK, TAUCPR, UMU0, Z, ZZ,
     $                    ZPLK0, ZPLK1 )
C
C        CONSTRUCT RIGHT-HAND SIDE VECTOR -B- FOR GENERAL BOUNDARY
C        CONDITIONS STWJ(17) AND SOLVE SYSTEM OF EQUATIONS OBTAINED
C        FROM THE BOUNDARY CONDITIONS AND THE
C        CONTINUITY-OF-INTENSITY-AT-LAYER-INTERFACE EQUATIONS.
C        THERMAL EMISSION CONTRIBUTES ONLY IN AZIMUTHAL INDEPENDENCE.
C
C     ROUTINES CALLED:  SGBCO, SGBSL, ZEROIT
C
C     I N P U T      V A R I A B L E S:
C
C       BDR      :  SURFACE BIDIRECTIONAL REFLECTIVITY
C       BEM      :  SURFACE BIDIRECTIONAL EMISSIVITY
C       BPLANK   :  BOTTOM BOUNDARY THERMAL EMISSION
C       CBAND    :  LEFT-HAND SIDE MATRIX OF LINEAR SYSTEM EQ. SC(5),
C                   SCALED BY EQ. SC(12); IN BANDED FORM REQUIRED
C                   BY LINPACK SOLUTION ROUTINES
C       CMU      :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT      :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       EXPBEA   :  TRANSMISSION OF INCIDENT BEAM, EXP(-TAUCPR/UMU0)
C       LYRCUT   :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       MAZ      :  ORDER OF AZIMUTHAL COMPONENT
C       NCOL     :  COUNTS OF COLUMNS IN -CBAND-
C       NN       :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       NCUT     :  TOTAL NUMBER OF COMPUTATIONAL LAYERS CONSIDERED
C       TPLANK   :  TOP BOUNDARY THERMAL EMISSION
C       TAUCPR   :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       ZZ       :  BEAM SOURCE VECTORS IN EQ. SS(19)
C       ZPLK0    :  THERMAL SOURCE VECTORS -Z0-, BY SOLVING EQ. SS(16)
C       ZPLK1    :  THERMAL SOURCE VECTORS -Z1-, BY SOLVING EQ. SS(16)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C   O U T P U T     V A R I A B L E S:
C
C       B        :  RIGHT-HAND SIDE VECTOR OF EQ. SC(5) GOING INTO
C                   *SGBSL*; RETURNS AS SOLUTION VECTOR OF EQ.
C                   SC(12), CONSTANTS OF INTEGRATION WITHOUT
C                   EXPONENTIAL TERM
C      LL        :  PERMANENT STORAGE FOR -B-, BUT RE-ORDERED
C
C   I N T E R N A L    V A R I A B L E S:
C
C       IPVT     :  INTEGER VECTOR OF PIVOT INDICES
C       IT       :  POINTER FOR POSITION IN  -B-
C       NCD      :  NUMBER OF DIAGONALS BELOW OR ABOVE MAIN DIAGONAL
C       RCOND    :  INDICATOR OF SINGULARITY FOR -CBAND-
C       Z        :  SCRATCH ARRAY REQUIRED BY *SGBCO*
C+---------------------------------------------------------------------+
C
      LOGICAL  LAMBER, LYRCUT
      INTEGER  IPVT(*)
      REAL     B(*), BDR( MI,0:* ), BEM(*), CBAND( MI9M2,NNLYRI ),
     $         CMU(*), CWT(*), EXPBEA(0:*), LL( MXCMU,* ),
     $         TAUCPR( 0:* ), Z(*), ZZ( MXCMU,* ), ZPLK0( MXCMU,* ),
     $         ZPLK1( MXCMU,* )
C
C
      CALL  ZEROIT( B, NNLYRI )
C                             ** CONSTRUCT -B-,  STWJ(20A,C) FOR
C                             ** PARALLEL BEAM + BOTTOM REFLECTION +
C                             ** THERMAL EMISSION AT TOP AND/OR BOTTOM
C
      IF ( MAZ.GT.0 .AND. FBEAM.GT.0.0 )  THEN
C
C                                         ** AZIMUTH-DEPENDENT CASE
C                                         ** (NEVER CALLED IF FBEAM = 0)
         IF ( LYRCUT .OR. LAMBER ) THEN
C
C               ** NO AZIMUTHAL-DEPENDENT INTENSITY FOR LAMBERT SURFACE;
C               ** NO INTENSITY COMPONENT FOR TRUNCATED BOTTOM LAYER
C
            DO 10  IQ = 1, NN
C                                                     ** TOP BOUNDARY
               B(IQ) = - ZZ(NN+1-IQ,1)
C                                                  ** BOTTOM BOUNDARY
               B(NCOL-NN+IQ) = - ZZ(IQ+NN,NCUT) * EXPBEA(NCUT)
10          CONTINUE
C
         ELSE
C
            DO 20  IQ = 1, NN
               B(IQ) = - ZZ(NN+1-IQ,1)
C
               SUM = 0.
               DO 15  JQ = 1, NN
                  SUM = SUM + CWT(JQ) * CMU(JQ) * BDR(IQ,JQ)
     $                        * ZZ(NN+1-JQ,NCUT) * EXPBEA(NCUT)
15             CONTINUE
               B(NCOL-NN+IQ) = SUM
               IF ( FBEAM.GT.0.0 )
     $              B(NCOL-NN+IQ) = SUM + ( BDR(IQ,0) * UMU0*FBEAM/PI
     $                                 - ZZ(IQ+NN,NCUT) ) * EXPBEA(NCUT)
20          CONTINUE
         END IF
C                             ** CONTINUITY CONDITION FOR LAYER
C                             ** INTERFACES OF EQ. STWJ(20B)
         IT = NN
         DO 40  LC = 1, NCUT-1
            DO 30  IQ = 1, NSTR
               IT    = IT + 1
               B(IT) = ( ZZ(IQ,LC+1) - ZZ(IQ,LC) ) * EXPBEA(LC)
30          CONTINUE
40       CONTINUE
C
      ELSE
C                                   ** AZIMUTH-INDEPENDENT CASE
         IF ( FBEAM.EQ.0.0 )  THEN
C
            DO 50 IQ = 1, NN
C                                      ** TOP BOUNDARY
C
               B(IQ) = - ZPLK0(NN+1-IQ,1) + FISOT + TPLANK
50          CONTINUE
C
            IF ( LYRCUT ) THEN
C                               ** NO INTENSITY COMPONENT FOR TRUNCATED
C                               ** BOTTOM LAYER
               DO 60 IQ = 1, NN
C                                      ** BOTTOM BOUNDARY
C
                  B(NCOL-NN+IQ) = - ZPLK0(IQ+NN,NCUT)
     $                            - ZPLK1(IQ+NN,NCUT) * TAUCPR(NCUT)
60             CONTINUE
C
            ELSE
C
               DO 80 IQ = 1, NN
C
                  SUM = 0.
                  DO 70 JQ = 1, NN
                     SUM = SUM + CWT(JQ) * CMU(JQ) * BDR(IQ,JQ)
     $                          * ( ZPLK0(NN+1-JQ,NCUT)
     $                            + ZPLK1(NN+1-JQ,NCUT) * TAUCPR(NCUT) )
70                CONTINUE
                  B(NCOL-NN+IQ) = 2.*SUM + BEM(IQ) * BPLANK
     $                            - ZPLK0(IQ+NN,NCUT)
     $                            - ZPLK1(IQ+NN,NCUT) * TAUCPR(NCUT)
80             CONTINUE
            END IF
C                             ** CONTINUITY CONDITION FOR LAYER
C                             ** INTERFACES, STWJ(20B)
            IT = NN
            DO 100  LC = 1, NCUT-1
               DO 90  IQ = 1, NSTR
                  IT    = IT + 1
                  B(IT) = ZPLK0(IQ,LC+1) - ZPLK0(IQ,LC) +
     $                  ( ZPLK1(IQ,LC+1) - ZPLK1(IQ,LC) ) * TAUCPR(LC)
90             CONTINUE
100         CONTINUE
C
         ELSE
C
            DO 150 IQ = 1, NN
               B(IQ) = - ZZ(NN+1-IQ,1) - ZPLK0(NN+1-IQ,1) +FISOT +TPLANK
150         CONTINUE
C
            IF ( LYRCUT ) THEN
C
               DO 160 IQ = 1, NN
                  B(NCOL-NN+IQ) = - ZZ(IQ+NN,NCUT) * EXPBEA(NCUT)
     $                            - ZPLK0(IQ+NN,NCUT)
     $                            - ZPLK1(IQ+NN,NCUT) * TAUCPR(NCUT)
160            CONTINUE
C
            ELSE
C
               DO 180 IQ = 1, NN
C
                  SUM = 0.
                  DO 170 JQ = 1, NN
                     SUM = SUM + CWT(JQ) * CMU(JQ) * BDR(IQ,JQ)
     $                          * ( ZZ(NN+1-JQ,NCUT) * EXPBEA(NCUT)
     $                            + ZPLK0(NN+1-JQ,NCUT)
     $                            + ZPLK1(NN+1-JQ,NCUT) * TAUCPR(NCUT) )
170               CONTINUE
                  B(NCOL-NN+IQ) = 2.*SUM + ( BDR(IQ,0) * UMU0*FBEAM/PI
     $                                 - ZZ(IQ+NN,NCUT) ) * EXPBEA(NCUT)
     $                            + BEM(IQ) * BPLANK
     $                            - ZPLK0(IQ+NN,NCUT)
     $                            - ZPLK1(IQ+NN,NCUT) * TAUCPR(NCUT)
180            CONTINUE
            END IF
C
            IT = NN
            DO 200  LC = 1, NCUT-1
               DO 190  IQ = 1, NSTR
                  IT    = IT + 1
                  B(IT) = ( ZZ(IQ,LC+1) - ZZ(IQ,LC) ) * EXPBEA(LC)
     $                    + ZPLK0(IQ,LC+1) - ZPLK0(IQ,LC) +
     $                    ( ZPLK1(IQ,LC+1) - ZPLK1(IQ,LC) ) * TAUCPR(LC)
190            CONTINUE
200         CONTINUE
C
         END IF
C
      END IF
C                     ** FIND L-U (LOWER/UPPER TRIANGULAR) DECOMPOSITION
C                     ** OF BAND MATRIX -CBAND- AND TEST IF IT IS NEARLY
C                     ** SINGULAR (NOTE: -CBAND- IS DESTROYED)
C                     ** (-CBAND- IS IN LINPACK PACKED FORMAT)
      RCOND = 0.0
      NCD = 3*NN - 1
      CALL  SGBCO( CBAND, MI9M2, NCOL, NCD, NCD, IPVT, RCOND, Z )
      IF ( 1.0+RCOND .EQ. 1.0 )  CALL  ERRMSG
     $   ( 'SOLVE0--SGBCO SAYS MATRIX NEAR SINGULAR',.FALSE.)
C
C                   ** SOLVE LINEAR SYSTEM WITH COEFF MATRIX -CBAND-
C                   ** AND R.H. SIDE(S) -B- AFTER -CBAND- HAS BEEN L-U
C                   ** DECOMPOSED.  SOLUTION IS RETURNED IN -B-.
C
      CALL  SGBSL( CBAND, MI9M2, NCOL, NCD, NCD, IPVT, B, 0 )
C
C                   ** ZERO -CBAND- (IT MAY CONTAIN 'FOREIGN'
C                   ** ELEMENTS UPON RETURNING FROM LINPACK);
C                   ** NECESSARY TO PREVENT ERRORS
C
      CALL  ZEROIT( CBAND, MI9M2*NNLYRI )
C
      DO 220  LC = 1, NCUT
         IPNT = LC*NSTR - NN
         DO 220  IQ = 1, NN
            LL(NN+1-IQ,LC) = B(IPNT+1-IQ)
            LL(IQ+NN,  LC) = B(IQ+IPNT)
220   CONTINUE
C
      RETURN
      END
      SUBROUTINE  SOLVE1( B, CBAND, FISOT, IHOM, IPVT, LL, MI9M2, MXCMU,
     $                    NCOL, NCUT, NN, NNLYRI, NSTR, Z )
C
C        CONSTRUCT RIGHT-HAND SIDE VECTOR -B- FOR ISOTROPIC INCIDENCE
C        (ONLY) ON EITHER TOP OR BOTTOM BOUNDARY AND SOLVE SYSTEM
C        OF EQUATIONS OBTAINED FROM THE BOUNDARY CONDITIONS AND THE
C        CONTINUITY-OF-INTENSITY-AT-LAYER-INTERFACE EQUATIONS
C
C     ROUTINES CALLED:  SGBCO, SGBSL, ZEROIT
C
C     I N P U T      V A R I A B L E S:
C
C       CBAND    :  LEFT-HAND SIDE MATRIX OF LINEAR SYSTEM EQ. SC(5),
C                   SCALED BY EQ. SC(12); IN BANDED FORM REQUIRED
C                   BY LINPACK SOLUTION ROUTINES
C       IHOM     :  DIRECTION OF ILLUMINATION FLAG
C       NCOL     :  COUNTS OF COLUMNS IN -CBAND-
C       NN       :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C    O U T P U T     V A R I A B L E S:
C
C       B        :  RIGHT-HAND SIDE VECTOR OF EQ. SC(5) GOING INTO
C                   *SGBSL*; RETURNS AS SOLUTION VECTOR OF EQ.
C                   SC(12), CONSTANTS OF INTEGRATION WITHOUT
C                   EXPONENTIAL TERM
C       LL      :   PERMANENT STORAGE FOR -B-, BUT RE-ORDERED
C
C   I N T E R N A L    V A R I A B L E S:
C
C       IPVT     :  INTEGER VECTOR OF PIVOT INDICES
C       NCD      :  NUMBER OF DIAGONALS BELOW OR ABOVE MAIN DIAGONAL
C       RCOND    :  INDICATOR OF SINGULARITY FOR -CBAND-
C       Z        :  SCRATCH ARRAY REQUIRED BY *SGBCO*
C----------------------------------------------------------------------+
      INTEGER  IPVT(*)
      REAL     B( NNLYRI ), CBAND( MI9M2,NNLYRI ), LL( MXCMU,* ), Z(*)
C
C
      CALL  ZEROIT( B, NNLYRI )
      NCD = 3*NN - 1
C
      IF ( IHOM.EQ.1 )  THEN
C                             ** BECAUSE THERE ARE NO BEAM OR EMISSION
C                             ** SOURCES, REMAINDER OF -B- ARRAY IS ZERO
         DO 10  I = 1, NN
            B(I) = FISOT
            B( NCOL-NN+I ) = 0.0
10       CONTINUE
C
         RCOND = 0.0
         CALL  SGBCO( CBAND, MI9M2, NCOL, NCD, NCD, IPVT, RCOND, Z )
         IF ( 1.0+RCOND .EQ. 1.0 )  CALL  ERRMSG
     $         ( 'SOLVE1--SGBCO SAYS MATRIX NEAR SINGULAR', .FALSE. )
C
      ELSE IF ( IHOM.EQ.2 )  THEN
C
         DO 20 I = 1, NN
            B(I) = 0.0
            B( NCOL-NN+I ) = FISOT
20       CONTINUE
C
      END IF
C
      CALL  SGBSL( CBAND, MI9M2, NCOL, NCD, NCD, IPVT, B, 0 )
C
C                          ** ZERO -CBAND- TO GET RID OF 'FOREIGN'
C                          ** ELEMENTS PUT IN BY LINPACK
      DO 30  LC = 1, NCUT
         IPNT = LC*NSTR - NN
         DO 30  IQ = 1, NN
            LL( NN+1-IQ, LC) = B( IPNT+1-IQ )
            LL( IQ+NN,   LC) = B( IQ+IPNT )
30    CONTINUE
C
      RETURN
      END
      SUBROUTINE  SPALTR( CMU, CWT, GC, KK, LL, MXCMU, NLYR,
     $                    NN, NSTR, TAUCPR, SFLUP, SFLDN )
C
C       CALCULATES SPHERICAL ALBEDO AND TRANSMISSIVITY FOR THE ENTIRE
C       MEDIUM FROM THE M=0 INTENSITY COMPONENTS
C       (THIS IS A VERY SPECIALIZED VERSION OF 'FLUXES')
C
C    I N P U T    V A R I A B L E S:
C
C       CMU     :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT     :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       KK      :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       GC      :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       LL      :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                  BY SOLVING SCALED VERSION OF EQ. SC(5);
C                  EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       NN      :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C    O U T P U T   V A R I A B L E S:
C
C       SFLUP   :  UP-FLUX AT TOP (EQUIVALENT TO SPHERICAL ALBEDO DUE TO
C                  RECIPROCITY).  FOR ILLUMINATION FROM BELOW IT GIVES
C                  SPHERICAL TRANSMISSIVITY
C       SFLDN   :  DOWN-FLUX AT BOTTOM (FOR SINGLE LAYER
C                  EQUIVALENT TO SPHERICAL TRANSMISSIVITY
C                  DUE TO RECIPROCITY)
C
C    I N T E R N A L   V A R I A B L E S:
C
C       ZINT    :  INTENSITY OF M=0 CASE, IN EQ. SC(1)
C+----------------------------------------------------------------------
C
      REAL  CMU(*), CWT(*), GC( MXCMU,MXCMU,* ), KK( MXCMU,* ),
     $      LL( MXCMU,* ), TAUCPR( 0:* )
C
C
      SFLUP = 0.0
      DO 20  IQ = NN+1, NSTR
         ZINT  = 0.0
         DO 10   JQ = 1, NN
            ZINT = ZINT + GC(IQ,JQ,1) * LL(JQ,1) *
     $                    EXP( KK(JQ,1) * TAUCPR(1) )
10       CONTINUE
         DO 11  JQ = NN+1, NSTR
            ZINT = ZINT + GC(IQ,JQ,1) * LL(JQ,1)
11       CONTINUE
C
         SFLUP = SFLUP + CWT(IQ-NN) * CMU(IQ-NN) * ZINT
20    CONTINUE
C
      SFLDN  = 0.0
      DO 40  IQ = 1, NN
         ZINT   = 0.0
         DO 30  JQ = 1, NN
             ZINT = ZINT + GC(IQ,JQ,NLYR) * LL(JQ,NLYR)
30       CONTINUE
         DO 31  JQ = NN+1, NSTR
             ZINT = ZINT + GC(IQ,JQ,NLYR) * LL(JQ,NLYR) *
     $              EXP( - KK(JQ,NLYR)*(TAUCPR(NLYR) - TAUCPR(NLYR-1)) )
31       CONTINUE
C
         SFLDN = SFLDN + CWT(NN+1-IQ) * CMU(NN+1-IQ) * ZINT
40    CONTINUE
C
      SFLUP = 2.0 * SFLUP
      SFLDN = 2.0 * SFLDN
C
      RETURN
      END
      SUBROUTINE     SSCAL( N, SA, SX, INCX )
C
C         CALCULATE  X = A*X  (X = VECTOR, A = SCALAR)
C
C  --INPUT--  N  NUMBER OF ELEMENTS IN VECTOR
C            SA  SINGLE PRECISION SCALE FACTOR
C            SX  SING-PREC ARRAY, LENGTH 1+(N-1)*INCX, CONTAINING VECTOR
C          INCX  SPACING OF VECTOR ELEMENTS IN 'SX'
C
C --OUTPUT-- SX  REPLACE  SX(1+I*INCX)  WITH  SA * SX(1+I*INCX)
C                FOR I = 0 TO N-1
C
      REAL SA, SX(*)
C
C
      IF( N.LE.0 ) RETURN
C
      IF( INCX.NE.1 ) THEN
C
          DO 10  I = 1, 1+(N-1)*INCX, INCX
             SX(I) = SA * SX(I)
   10     CONTINUE
C
      ELSE
C
         M = MOD(N,5)
         IF( M.NE.0 ) THEN
C                           ** CLEAN-UP LOOP SO REMAINING VECTOR LENGTH
C                           ** IS A MULTIPLE OF 5.
            DO 30  I = 1, M
               SX(I) = SA * SX(I)
   30       CONTINUE
         ENDIF
C                             ** UNROLL LOOP FOR SPEED
         DO 50  I = M+1, N, 5
            SX(I)   = SA * SX(I)
            SX(I+1) = SA * SX(I+1)
            SX(I+2) = SA * SX(I+2)
            SX(I+3) = SA * SX(I+3)
            SX(I+4) = SA * SX(I+4)
   50    CONTINUE
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE  SURFAC( ALBEDO, CMU, CWT, DELM0, FBEAM, HLPR, LAMBER,
     $                    MI, MAZ, MXCMU, MXUMU, NN, NUMU, NSTR, ONLYFL,
     $                    UMU, USRANG, YLM0, YLMC, YLMU, BDR, EMU, BEM,
     $                    RMU )
C
C       SPECIFIES USER'S SURFACE BIDIRECTIONAL PROPERTIES, STWJ(21)
C   MODIF : QGAUSN EN COMMENTAIRE
C
C   I N P U T     V A R I A B L E S:
C
C       DELM0  :  KRONECKER DELTA, DELTA-SUB-M0
C       CMU    :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT    :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       HLPR   :  LEGENDRE MOMENTS OF SURFACE BIDIRECTIONAL REFLECTIVITY
C                    (WITH 2K+1 FACTOR INCLUDED)
C       MAZ    :  ORDER OF AZIMUTHAL COMPONENT
C       NN     :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       YLM0   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE BEAM ANGLE
C       YLMC   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS
C                 AT THE QUADRATURE ANGLES
C       YLMU   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS
C                 AT THE USER ANGLES
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C    O U T P U T     V A R I A B L E S:
C
C       BDR :  SURFACE BIDIRECTIONAL REFLECTIVITY (COMPUTATIONAL ANGLES)
C       RMU :  SURFACE BIDIRECTIONAL REFLECTIVITY (USER ANGLES)
C       BEM :  SURFACE DIRECTIONAL EMISSIVITY (COMPUTATIONAL ANGLES)
C       EMU :  SURFACE DIRECTIONAL EMISSIVITY (USER ANGLES)
C
C    I N T E R N A L     V A R I A B L E S:
C
C       DREF      DIRECTIONAL REFLECTIVITY
C       NMUG   :  NUMBER OF ANGLE COSINE QUADRATURE POINTS
C                 ON (0,1) FOR INTEGRATING BIDIRECTIONAL REFLECTIVITY
C                 TO GET DIRECTIONAL EMISSIVITY (IT IS NECESSARY TO USE
C                 A QUADRATURE SET DISTINCT FROM THE COMPUTATIONAL
C                 ANGLES, BECAUSE THE COMPUTATIONAL ANGLES MAY NOT BE
C                 DENSE ENOUGH -- I.E. 'NSTR' MAY BE TOO SMALL-- TO GIVE
C                 AN ACCURATE APPROXIMATION FOR THE INTEGRATION).
C       GMU    :  THE 'NMUG' ANGLE COSINE QUADRATURE POINTS ON (0,1)
C       GWT    :  THE 'NMUG' ANGLE COSINE QUADRATURE WEIGHTS ON (0,1)
C       YLMG   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS
C                 AT THE 'NMUG' QUADRATURE ANGLES
C+---------------------------------------------------------------------+
      LOGICAL  LAMBER, ONLYFL, USRANG
      REAL     BDR( MI,0:* ), BEM(*), CMU(*), CWT(*), EMU(*),
     $         HLPR(0:*), RMU( MXUMU,0:* ), UMU(*),
     $         YLM0(0:*), YLMC( 0:MXCMU,* ), YLMU( 0:MXCMU,* )
      PARAMETER  ( NMUG = 10, MAXSTR = 100 )
      LOGICAL  PASS1
      REAL     GMU( NMUG ), GWT( NMUG ), YLMG( 0:MAXSTR, NMUG )
      DATA  PASS1 / .TRUE. /
C
C
      IF ( PASS1 )  THEN
         PASS1 = .FALSE.
C        CALL QGAUSN( NMUG, GMU, GWT )
C
         CALL LEPOLY( NMUG, 0, MAXSTR, MAXSTR, GMU, YLMG )
C                       ** CONVERT LEGENDRE POLYS. TO NEGATIVE -GMU-
         SGN  = - 1.0
         DO 1  K = 0, MAXSTR
            SGN = - SGN
            DO 1  JG = 1, NMUG
               YLMG( K,JG ) = SGN * YLMG( K,JG )
 1       CONTINUE
C
      END IF
C
      CALL  ZEROIT( BDR, MI*(MI+1) )
      CALL  ZEROIT( BEM, MI )
C
      IF ( LAMBER .AND. MAZ.EQ.0 ) THEN
C
         DO 20 IQ = 1, NN
            BEM(IQ) = 1.0 - ALBEDO
            DO 20 JQ = 0, NN
               BDR(IQ,JQ) = ALBEDO
20       CONTINUE
C
      ELSE IF ( .NOT.LAMBER ) THEN
C                                  ** COMPUTE SURFACE BIDIRECTIONAL
C                                  ** PROPERTIES AT COMPUTATIONAL ANGLES
         DO 60 IQ = 1, NN
C
            DO 40 JQ = 1, NN
              SUM = 0.0
              DO 30 K = MAZ, NSTR-1
                 SUM = SUM + HLPR(K) * YLMC(K,IQ) * YLMC(K,JQ+NN)
30            CONTINUE
              BDR(IQ,JQ) = (2.-DELM0) * SUM
40          CONTINUE
C
            IF ( FBEAM.GT.0.0 )  THEN
               SUM = 0.0
               DO 50 K = MAZ, NSTR-1
                  SUM = SUM + HLPR(K) * YLMC(K,IQ) * YLM0(K)
50             CONTINUE
               BDR(IQ,0) = (2.-DELM0) * SUM
            ENDIF
C
60       CONTINUE
C
         IF ( MAZ.EQ.0 ) THEN
C
            IF ( NSTR.GT.MAXSTR )  CALL
     $           ERRMSG( 'SURFAC--PARAMETER MAXSTR TOO SMALL', .TRUE. )
C
C                              ** INTEGRATE BIDIRECTIONAL REFLECTIVITY
C                              ** AT REFLECTION POLAR ANGLES -CMU- AND
C                              ** INCIDENT ANGLES -GMU- TO GET
C                              ** DIRECTIONAL EMISSIVITY AT
C                              ** COMPUTATIONAL ANGLES -CMU-.
            DO 100  IQ = 1, NN
               DREF = 0.0
               DO 90  JG = 1, NMUG
                  SUM = 0.0
                  DO 80  K = 0, NSTR-1
                     SUM = SUM + HLPR(K) * YLMC(K,IQ) * YLMG(K,JG)
80                CONTINUE
                  DREF = DREF + 2.* GWT(JG) * GMU(JG) * SUM
90             CONTINUE
               BEM(IQ) = 1.0 - DREF
100         CONTINUE
C
         END IF
C
      END IF
C                                       ** COMPUTE SURFACE BIDIRECTIONAL
C                                       ** PROPERTIES AT USER ANGLES
C
      IF ( .NOT.ONLYFL .AND. USRANG )  THEN
C
         CALL  ZEROIT( EMU, MXUMU )
         CALL  ZEROIT( RMU, MXUMU*(MI+1) )
C
         DO 170 IU = 1, NUMU
            IF ( UMU(IU).GT.0.0 )  THEN
C
               IF ( LAMBER .AND. MAZ.EQ.0 )  THEN
                  DO 110 IQ = 0, NN
                     RMU(IU,IQ) = ALBEDO
110               CONTINUE
                  EMU(IU) = 1.0 - ALBEDO
C
               ELSE IF ( .NOT.LAMBER ) THEN
                  DO 130 IQ = 1, NN
                     SUM = 0.0
                     DO 120 K = MAZ, NSTR-1
                        SUM = SUM + HLPR(K) * YLMU(K,IU) * YLMC(K,IQ+NN)
120                  CONTINUE
                     RMU(IU,IQ) = (2.-DELM0) * SUM
130               CONTINUE
C
                  IF ( FBEAM.GT.0.0 )  THEN
                     SUM = 0.0
                     DO 140 K = MAZ, NSTR-1
                        SUM = SUM + HLPR(K) * YLMU(K,IU) * YLM0(K)
140                  CONTINUE
                     RMU(IU,0) = (2.-DELM0) * SUM
                  END IF
C
                  IF ( MAZ.EQ.0 ) THEN
C
C                               ** INTEGRATE BIDIRECTIONAL REFLECTIVITY
C                               ** AT REFLECTION ANGLES -UMU- AND
C                               ** INCIDENT ANGLES -GMU- TO GET
C                               ** DIRECTIONAL EMISSIVITY AT
C                               ** USER ANGLES -UMU-.
                     DREF = 0.0
                     DO 160 JG = 1, NMUG
                        SUM = 0.0
                        DO 150 K = 0, NSTR-1
                           SUM = SUM + HLPR(K) * YLMU(K,IU) * YLMG(K,JG)
150                     CONTINUE
                        DREF = DREF + 2.* GWT(JG) * GMU(JG) * SUM
160                  CONTINUE
C
                     EMU(IU) = 1.0 - DREF
                  END IF
C
               END IF
            END IF
170      CONTINUE
C
      END IF
C
      RETURN
      END
      SUBROUTINE  TERPEV( CWT, EVECC, GL, GU, MAZ, MXCMU, MXUMU,
     $                    NN, NSTR, NUMU, WK, YLMC, YLMU )
C
C         INTERPOLATE EIGENVECTORS TO USER ANGLES; EQ SD(8)
C
      REAL  CWT(*), EVECC( MXCMU,* ), GL(0:*), GU(  MXUMU,* ), WK(*),
     $      YLMC(  0:MXCMU,* ), YLMU(  0:MXCMU,* )
C
C
      DO 50  IQ = 1, NSTR
C
         DO 20  L = MAZ, NSTR-1
C                                       ** INNER SUM IN SD(8) TIMES ALL
C                                   ** FACTORS IN OUTER SUM BUT PLM(MU)
            SUM = 0.0
            DO 10  JQ = 1, NSTR
               SUM = SUM + CWT(JQ) * YLMC(L,JQ) * EVECC(JQ,IQ)
10          CONTINUE
            WK(L+1) = 0.5 * GL(L) * SUM
20       CONTINUE
C                                    ** FINISH OUTER SUM IN SD(8)
C                                    ** AND STORE EIGENVECTORS
         DO 40  IU = 1, NUMU
            SUM = 0.
            DO 30  L = MAZ, NSTR-1
               SUM = SUM + WK(L+1) * YLMU(L,IU)
30          CONTINUE
            IF ( IQ.LE.NN )  GU( IU, IQ+NN     ) = SUM
            IF ( IQ.GT.NN )  GU( IU, NSTR+1-IQ ) = SUM
40       CONTINUE
C
50    CONTINUE
C
      RETURN
      END
      SUBROUTINE  TERPSO( CWT, DELM0, FBEAM, GL, MAZ, MXCMU, MXUMU,
     $                    NOPLNK, NUMU, NSTR, OPRIM, PI, YLM0, YLMC,
     $                    YLMU, PSI, XR0, XR1, Z0, ZJ, ZBEAM, Z0U,
     $                    Z1U )
C
C         INTERPOLATES SOURCE FUNCTIONS TO USER ANGLES
C
C    I N P U T      V A R I A B L E S:
C
C       CWT    :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       DELM0  :  KRONECKER DELTA, DELTA-SUB-M0
C       GL     :  DELTA-M SCALED LEGENDRE COEFFICIENTS OF PHASE FUNCTION
C                    (INCLUDING FACTORS 2L+1 AND SINGLE-SCATTER ALBEDO)
C       MAZ    :  ORDER OF AZIMUTHAL COMPONENT
C       OPRIM  :  SINGLE SCATTERING ALBEDO
C       XR0    :  EXPANSION OF THERMAL SOURCE FUNCTION
C       XR1    :  EXPANSION OF THERMAL SOURCE FUNCTION EQS.SS(14-16)
C       YLM0   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE BEAM ANGLE
C       YLMC   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE QUADRATURE ANGLES
C       YLMU   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE USER ANGLES
C       Z0     :  SOLUTION VECTORS Z-SUB-ZERO OF EQ. SS(16)
C       ZJ     :  SOLUTION VECTOR CAPITAL -Z-SUB-ZERO AFTER SOLVING
C                 EQ. SS(19)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C    O U T P U T     V A R I A B L E S:
C
C       ZBEAM  :  INCIDENT-BEAM SOURCE FUNCTION AT USER ANGLES
C       Z0U,Z1U:  COMPONENTS OF A LINEAR-IN-OPTICAL-DEPTH-DEPENDENT
C                    SOURCE (APPROXIMATING THE PLANCK EMISSION SOURCE)
C
C   I N T E R N A L       V A R I A B L E S:
C
C       PSI    :  SUM JUST AFTER SQUARE BRACKET IN  EQ. SD(9)
C+---------------------------------------------------------------------+
      LOGICAL  NOPLNK
      REAL     CWT(*), GL(0:*), PSI(*),  YLM0(0:*), YLMC( 0:MXCMU,* ),
     $         YLMU( 0:MXCMU,*), Z0(*), ZJ(*), ZBEAM(*), Z0U(*),
     $         Z1U(*)
C
C
      IF ( FBEAM.GT.0.0 )  THEN
C                                  ** BEAM SOURCE TERMS; EQ. SD(9)
         DO 20  IQ = MAZ, NSTR-1
            PSUM = 0.
            DO 10  JQ = 1, NSTR
               PSUM = PSUM + CWT(JQ) * YLMC(IQ,JQ) * ZJ(JQ)
10          CONTINUE
            PSI(IQ+1) = 0.5 * GL(IQ) * PSUM
20       CONTINUE
C
         FACT = ( 2. - DELM0 ) * FBEAM / (4.0*PI)
         DO 40  IU = 1, NUMU
            SUM = 0.
            DO 30 IQ = MAZ, NSTR-1
               SUM = SUM + YLMU(IQ,IU) *
     $                    ( PSI(IQ+1) + FACT * GL(IQ) * YLM0(IQ) )
30          CONTINUE
            ZBEAM(IU) = SUM
40       CONTINUE
      END IF
C
      IF ( .NOT.NOPLNK .AND. MAZ.EQ.0 )  THEN
C
C                                ** THERMAL SOURCE TERMS, STWJ(27C)
         DO 80  IQ = MAZ, NSTR-1
            PSUM = 0.0
            DO 70  JQ = 1, NSTR
               PSUM = PSUM + CWT(JQ) * YLMC(IQ,JQ) * Z0(JQ)
 70         CONTINUE
            PSI(IQ+1) = 0.5 * GL(IQ) * PSUM
 80       CONTINUE
C
          DO 100  IU = 1, NUMU
            SUM = 0.0
            DO 90   IQ = MAZ, NSTR-1
               SUM = SUM + YLMU(IQ,IU) * PSI(IQ+1)
90          CONTINUE
            Z0U(IU) = SUM + (1.-OPRIM) * XR0
            Z1U(IU) = XR1
100      CONTINUE
C
      END IF
C
      RETURN
      END
      LOGICAL FUNCTION  TSTBAD( VARNAM, RELERR )
C
C       WRITE NAME (-VARNAM-) OF VARIABLE FAILING SELF-TEST AND ITS
C       PERCENT ERROR FROM THE CORRECT VALUE;  RETURN  'FALSE'.
C
      CHARACTER*(*)  VARNAM
      REAL           RELERR
C
C
      TSTBAD = .FALSE.
      WRITE( *, '(/,3A,1P,E11.2,A)' )
     $       ' OUTPUT VARIABLE  ', VARNAM,'  DIFFERED BY', 100.*RELERR,
     $       '  PER CENT FROM CORRECT VALUE.  SELF-TEST FAILED.'
      RETURN
      END
      SUBROUTINE  UPBEAM( ARRAY, CC, CMU, DELM0, FBEAM, GL, IPVT, MAZ,
     $                    MXCMU, NN, NSTR, PI, UMU0, WK, YLM0, YLMC, ZJ,
     $                    ZZ )
C
C         FINDS THE INCIDENT-BEAM PARTICULAR SOLUTION  OF SS(18)
C
C     ROUTINES CALLED:  SGECO, SGESL
C
C   I N P U T    V A R I A B L E S:
C
C       CC     :  CAPITAL-C-SUB-IJ IN EQ. SS(5)
C       CMU    :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       DELM0  :  KRONECKER DELTA, DELTA-SUB-M0
C       GL     :  DELTA-M SCALED LEGENDRE COEFFICIENTS OF PHASE FUNCTION
C                    (INCLUDING FACTORS 2L+1 AND SINGLE-SCATTER ALBEDO)
C       MAZ    :  ORDER OF AZIMUTHAL COMPONENT
C       YLM0   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE BEAM ANGLE
C       YLMC   :  NORMALIZED ASSOCIATED LEGENDRE POLYNOMIAL
C                 AT THE QUADRATURE ANGLES
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C   O U T P U T    V A R I A B L E S:
C
C       ZJ     :  RIGHT-HAND SIDE VECTOR CAPITAL-X-SUB-ZERO IN SS(19);
C                 ALSO THE SOLUTION VECTOR CAPITAL-Z-SUB-ZERO
C                 AFTER SOLVING THAT SYSTEM
C       ZZ     :  PERMANENT STORAGE FOR -ZJ-, BUT RE-ORDERED
C
C   I N T E R N A L    V A R I A B L E S:
C
C       ARRAY  :  COEFFICIENT MATRIX IN LEFT-HAND SIDE OF EQ. SS(19)
C       IPVT   :  INTEGER VECTOR OF PIVOT INDICES REQUIRED BY *LINPACK*
C       WK     :  SCRATCH ARRAY REQUIRED BY *LINPACK*
C+---------------------------------------------------------------------+
C
      INTEGER  IPVT(*)
      REAL     ARRAY( MXCMU,* ), CC( MXCMU,* ), CMU(*), GL(0:*),
     $         WK(*), YLM0(0:*), YLMC( 0:MXCMU,* ), ZJ(*), ZZ(*)
C
C
      DO 40  IQ = 1, NSTR
C
         DO 10  JQ = 1, NSTR
            ARRAY(IQ,JQ) = - CC(IQ,JQ)
10       CONTINUE
         ARRAY(IQ,IQ) = 1. + CMU(IQ) / UMU0 + ARRAY(IQ,IQ)
C
         SUM = 0.
         DO 20  K = MAZ, NSTR-1
            SUM = SUM + GL(K) * YLMC(K,IQ) * YLM0(K)
20       CONTINUE
         ZJ(IQ) = ( 2. - DELM0 ) * FBEAM * SUM / (4.0*PI)
40    CONTINUE
C                  ** FIND L-U (LOWER/UPPER TRIANGULAR) DECOMPOSITION
C                  ** OF -ARRAY- AND SEE IF IT IS NEARLY SINGULAR
C                  ** (NOTE:  -ARRAY- IS DESTROYED)
      RCOND = 0.0
      CALL  SGECO( ARRAY, MXCMU, NSTR, IPVT, RCOND, WK )
      IF ( 1.0+RCOND .EQ. 1.0 )  CALL  ERRMSG
     $   ( 'UPBEAM--SGECO SAYS MATRIX NEAR SINGULAR',.FALSE.)
C
C                ** SOLVE LINEAR SYSTEM WITH COEFF MATRIX -ARRAY-
C                ** (ASSUMED ALREADY L-U DECOMPOSED) AND R.H. SIDE(S)
C                ** -ZJ-;  RETURN SOLUTION(S) IN -ZJ-
      JOB = 0
      CALL  SGESL( ARRAY, MXCMU, NSTR, IPVT, ZJ, JOB )
C
      DO 50  IQ = 1, NN
         ZZ( IQ+NN )   = ZJ( IQ )
         ZZ( NN+1-IQ ) = ZJ( IQ+NN )
50    CONTINUE
C
      RETURN
      END
      SUBROUTINE  UPISOT( ARRAY, CC, CMU, IPVT, MXCMU, NN, NSTR, OPRIM,
     $                    WK, XR0, XR1, Z0, Z1, ZPLK0, ZPLK1 )
C
C       FINDS THE PARTICULAR SOLUTION OF THERMAL RADIATION OF SS(15)
C
C     ROUTINES CALLED:  SGECO, SGESL
C
C   I N P U T     V A R I A B L E S:
C
C       CC     :  CAPITAL-C-SUB-IJ IN EQ. SS(5)
C       CMU    :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       OPRIM  :  DELTA-M SCALED SINGLE SCATTERING ALBEDO
C       XR0    :  EXPANSION OF THERMAL SOURCE FUNCTION
C       XR1    :  EXPANSION OF THERMAL SOURCE FUNCTION EQS. SS(14-16)
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C    O U T P U T    V A R I A B L E S:
C
C       Z0     :  SOLUTION VECTORS Z-SUB-ZERO OF EQ. SS(16)
C       Z1     :  SOLUTION VECTORS Z-SUB-ONE  OF EQ. SS(16)
C       ZPLK0, :  PERMANENT STORAGE FOR -Z0,Z1-, BUT RE-ORDERED
C        ZPLK1
C
C   I N T E R N A L    V A R I A B L E S:
C
C       ARRAY  :  COEFFICIENT MATRIX IN LEFT-HAND SIDE OF EQ. SS(16)
C       IPVT   :  INTEGER VECTOR OF PIVOT INDICES REQUIRED BY *LINPACK*
C       WK     :  SCRATCH ARRAY REQUIRED BY *LINPACK*
C+---------------------------------------------------------------------+
C
      INTEGER IPVT(*)
      REAL    ARRAY( MXCMU,* ), CC( MXCMU,* ), CMU(*), WK(*),
     $        Z0(*), Z1(*), ZPLK0(*), ZPLK1(*)
C
C
      DO 20 IQ = 1, NSTR
C
         DO 10 JQ = 1, NSTR
            ARRAY(IQ,JQ) = - CC(IQ,JQ)
10       CONTINUE
         ARRAY(IQ,IQ) = 1.0 + ARRAY(IQ,IQ)
C
         Z1(IQ) = XR1
         Z0(IQ) = (1.-OPRIM) * XR0 + CMU(IQ) * Z1(IQ)
20    CONTINUE
C                       ** SOLVE LINEAR EQUATIONS: SAME AS IN *UPBEAM*,
C                       ** EXCEPT -ZJ- REPLACED BY -Z0-
      RCOND = 0.0
      CALL  SGECO( ARRAY, MXCMU, NSTR, IPVT, RCOND, WK )
      IF ( 1.0+RCOND .EQ. 1.0 )  CALL  ERRMSG
     $   ( 'UPISOT--SGECO SAYS MATRIX NEAR SINGULAR',.FALSE.)
C
      CALL  SGESL( ARRAY, MXCMU, NSTR, IPVT, Z0, 0 )
C
      DO 30  IQ = 1, NN
         ZPLK0( IQ+NN )   = Z0( IQ )
         ZPLK1( IQ+NN )   = Z1( IQ )
         ZPLK0( NN+1-IQ ) = Z0( IQ+NN )
         ZPLK1( NN+1-IQ ) = Z1( IQ+NN )
30    CONTINUE
C
      RETURN
      END
      SUBROUTINE  USRINT( BPLANK, CMU, CWT, DELM0, EMU, EXPBEA,
     $                    FBEAM, FISOT, GC, GU, KK, LAMBER, LAYRU, LL,
     $                    LYRCUT, MAZ, MXCMU, MXULV, MXUMU, NCUT,
     $                    NLYR, NN, NSTR, NOPLNK, NUMU, NTAU, PI, RMU,
     $                    TAUCPR, TPLANK, UMU, UMU0, UTAUPR, WK,
     $                    ZBEAM, Z0U, Z1U, ZZ, ZPLK0, ZPLK1, UUM )
C
C       COMPUTES INTENSITY COMPONENTS AT USER OUTPUT ANGLES
C       FOR AZIMUTHAL EXPANSION TERMS IN EQ. SD(2)
C
C   I N P U T    V A R I A B L E S:
C
C       BPLANK :  INTEGRATED PLANCK FUNCTION FOR EMISSION FROM
C                 BOTTOM BOUNDARY
C       CMU    :  ABSCISSAE FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       CWT    :  WEIGHTS FOR GAUSS QUADRATURE OVER ANGLE COSINE
C       DELM0  :  KRONECKER DELTA, DELTA-SUB-M0
C       EMU    :  SURFACE DIRECTIONAL EMISSIVITY (USER ANGLES)
C       EXPBEA :  TRANSMISSION OF INCIDENT BEAM, EXP(-TAUCPR/UMU0)
C       GC     :  EIGENVECTORS AT POLAR QUADRATURE ANGLES, SC(1)
C       GU     :  EIGENVECTORS INTERPOLATED TO USER POLAR ANGLES
C                 (i.e., g IN EQ. SC(1) )
C       KK     :  EIGENVALUES OF COEFF. MATRIX IN EQ. SS(7)
C       LAYRU  :  LAYER NUMBER OF USER LEVEL -UTAU-
C       LL     :  CONSTANTS OF INTEGRATION IN EQ. SC(1), OBTAINED
C                 BY SOLVING SCALED VERSION OF EQ. SC(5);
C                 EXPONENTIAL TERM OF EQ. SC(12) NOT INCLUDED
C       LYRCUT :  LOGICAL FLAG FOR TRUNCATION OF COMPUT. LAYER
C       MAZ    :  ORDER OF AZIMUTHAL COMPONENT
C       NCUT   :  TOTAL NUMBER OF COMPUTATIONAL LAYERS CONSIDERED
C       NN     :  ORDER OF DOUBLE-GAUSS QUADRATURE (NSTR/2)
C       RMU    :  SURFACE BIDIRECTIONAL REFLECTIVITY (USER ANGLES)
C       TAUCPR :  CUMULATIVE OPTICAL DEPTH (DELTA-M-SCALED)
C       TPLANK :  INTEGRATED PLANCK FUNCTION FOR EMISSION FROM
C                 TOP BOUNDARY
C       UTAUPR :  OPTICAL DEPTHS OF USER OUTPUT LEVELS IN DELTA-M
C                    COORDINATES;  EQUAL TO  -UTAU- IF NO DELTA-M
C       Z0U    :  Z-SUB-ZERO IN EQ. SS(16) INTERPOLATED TO USER
C                 ANGLES FROM AN EQUATION DERIVED FROM SS(16)
C       Z1U    :  Z-SUB-ONE IN EQ. SS(16) INTERPOLATED TO USER
C                 ANGLES FROM AN EQUATION DERIVED FROM SS(16)
C       ZZ     :  BEAM SOURCE VECTORS IN EQ. SS(19)
C       ZPLK0  :  THERMAL SOURCE VECTORS -Z0-, BY SOLVING EQ. SS(16)
C       ZPLK1  :  THERMAL SOURCE VECTORS -Z1-, BY SOLVING EQ. SS(16)
C       ZBEAM  :  INCIDENT-BEAM SOURCE VECTORS
C       (REMAINDER ARE 'DISORT' INPUT VARIABLES)
C
C   O U T P U T    V A R I A B L E S:
C
C       UUM  :  AZIMUTHAL COMPONENTS OF THE INTENSITY IN EQ. STWJ(5)
C
C   I N T E R N A L    V A R I A B L E S:
C
C       BNDDIR :  DIRECT INTENSITY DOWN AT THE BOTTOM BOUNDARY
C       BNDDFU :  DIFFUSE INTENSITY DOWN AT THE BOTTOM BOUNDARY
C       BNDINT :  INTENSITY ATTENUATED AT BOTH BOUNDARIES, STWJ(25-6)
C       DTAU   :  OPTICAL DEPTH OF A COMPUTATIONAL LAYER
C       LYREND :  END LAYER OF INTEGRATION
C       LYRSTR :  START LAYER OF INTEGRATION
C       PALINT :  INTENSITY COMPONENT FROM PARALLEL BEAM
C       PLKINT :  INTENSITY COMPONENT FROM PLANCK SOURCE
C       WK     :  SCRATCH VECTOR FOR SAVING 'EXP' EVALUATIONS
C       ALL THE EXPONENTIAL FACTORS ( EXP1, EXPN,... etc.)
C       COME FROM THE SUBSTITUTION OF CONSTANTS OF INTEGRATION IN
C       EQ. SC(12) INTO EQS. S1(8-9).  THEY ALL HAVE NEGATIVE
C       ARGUMENTS SO THERE SHOULD NEVER BE OVERFLOW PROBLEMS.
C+---------------------------------------------------------------------+
C
      LOGICAL  LAMBER, LYRCUT, NOPLNK, NEGUMU
      INTEGER  LAYRU(*)
      REAL     CMU(*), CWT(*), EMU(*), EXPBEA(0:*), GC( MXCMU,MXCMU,* ),
     $         GU( MXUMU,MXCMU,* ), KK( MXCMU,* ), LL( MXCMU,* ),
     $         RMU( MXUMU,0:* ), TAUCPR( 0:* ), UUM( MXUMU,MXULV,0:* ),
     $         UMU(*), UTAUPR(*), WK(*), Z0U( MXUMU,* ), Z1U( MXUMU,* ),
     $         ZBEAM( MXUMU,* ), ZZ( MXCMU,* ), ZPLK0( MXCMU,* ),
     $         ZPLK1( MXCMU,* )
C
C
      CALL  ZEROIT( UUM, MXUMU*MXULV*(MXCMU+1) )
C
C                          ** INCORPORATE CONSTANTS OF INTEGRATION INTO
C                          ** INTERPOLATED EIGENVECTORS
      DO 10  LC = 1, NCUT
         DO  10  IQ = 1, NSTR
            DO 10  IU = 1, NUMU
               GU(IU,IQ,LC) = GU(IU,IQ,LC) * LL(IQ,LC)
10    CONTINUE
C                           ** LOOP OVER LEVELS AT WHICH INTENSITIES
C                           ** ARE DESIRED ('USER OUTPUT LEVELS')
      DO 200  LU = 1, NTAU
C
         EXP0 = EXP( - UTAUPR(LU) / UMU0 )
         LYU = LAYRU(LU)
C                              ** LOOP OVER POLAR ANGLES AT WHICH
C                              ** INTENSITIES ARE DESIRED
         DO 100  IU = 1, NUMU
            IF ( LYRCUT .AND. LYU.GT.NCUT )  GO TO 100
            NEGUMU = UMU(IU).LT.0.0
            IF( NEGUMU )  THEN
               LYRSTR = 1
               LYREND = LYU - 1
               SGN = - 1.0
            ELSE
               LYRSTR = LYU + 1
               LYREND = NCUT
               SGN = 1.0
            END IF
C                          ** FOR DOWNWARD INTENSITY, INTEGRATE FROM TOP
C                          ** TO 'LYU-1' IN EQ. S1(8); FOR UPWARD,
C                          ** INTEGRATE FROM BOTTOM TO 'LYU+1' IN S1(9)
            PALINT = 0.0
            PLKINT = 0.0
            DO 30  LC = LYRSTR, LYREND
C
               DTAU = TAUCPR(LC) - TAUCPR(LC-1)
               EXP1 =  EXP( (UTAUPR(LU) - TAUCPR(LC-1)) / UMU(IU) )
               EXP2 =  EXP( (UTAUPR(LU) - TAUCPR( LC )) / UMU(IU) )
C
               IF ( .NOT.NOPLNK .AND. MAZ.EQ.0 )
     $           PLKINT = PLKINT + SGN * ( Z0U(IU,LC) * (EXP1 - EXP2) +
     $                    Z1U(IU,LC) * ( (TAUCPR(LC-1) + UMU(IU))*EXP1 -
     $                                   (TAUCPR(LC) + UMU(IU))*EXP2 ) )
C
               IF ( FBEAM.GT.0.0 )  THEN
                  DENOM = 1.0 + UMU(IU) / UMU0
                  IF ( ABS(DENOM).LT.0.0001 ) THEN
C                                                   ** L'HOSPITAL LIMIT
                     EXPN = ( DTAU / UMU0 ) * EXP0
                  ELSE
                     EXPN = ( EXP1 * EXPBEA(LC-1) - EXP2 * EXPBEA(LC) )
     $                      * SGN / DENOM
                  END IF
                  PALINT = PALINT + ZBEAM(IU,LC) * EXPN
               ENDIF
C                                                   ** -KK- IS NEGATIVE
               DO 20  IQ = 1, NN
                  WK(IQ) = EXP( KK(IQ,LC) * DTAU )
                  DENOM = 1.0 + UMU(IU) * KK(IQ,LC)
                  IF ( ABS(DENOM).LT.0.0001 ) THEN
C                                                   ** L'HOSPITAL LIMIT
                     EXPN = DTAU / UMU(IU) * EXP2
                  ELSE
                     EXPN = SGN * ( EXP1 * WK(IQ) - EXP2 ) / DENOM
                  END IF
                  PALINT = PALINT + GU(IU,IQ,LC) * EXPN
20             CONTINUE
C                                                   ** -KK- IS POSITIVE
               DO 21  IQ = NN+1, NSTR
                  DENOM = 1.0 + UMU(IU) * KK(IQ,LC)
                  IF ( ABS(DENOM).LT.0.0001 ) THEN
C                                                   ** L'HOSPITAL LIMIT
                     EXPN = - DTAU / UMU(IU) * EXP1
                  ELSE
                     EXPN = SGN *( EXP1 - EXP2 * WK(NSTR+1-IQ) ) / DENOM
                  END IF
                  PALINT = PALINT + GU(IU,IQ,LC) * EXPN
21             CONTINUE
C
30          CONTINUE
C                           ** CALCULATE CONTRIBUTION FROM USER
C                           ** OUTPUT LEVEL TO NEXT COMPUTATIONAL LEVEL
C
            DTAU1 = UTAUPR(LU) - TAUCPR(LYU-1)
            DTAU2 = UTAUPR(LU) - TAUCPR(LYU)
            IF( ABS(DTAU1).LT.1.E-6 .AND. NEGUMU )  GO TO 50
            IF( ABS(DTAU2).LT.1.E-6 .AND. (.NOT.NEGUMU) )  GO TO 50
            IF( NEGUMU ) EXP1 = EXP( DTAU1 / UMU(IU) )
            IF( .NOT.NEGUMU ) EXP2 = EXP( DTAU2 / UMU(IU) )
C
            IF ( FBEAM.GT.0.0 )  THEN
               DENOM = 1.0 + UMU(IU) / UMU0
               IF ( ABS(DENOM).LT.0.0001 ) THEN
                  EXPN =  ( DTAU1 / UMU0 ) * EXP0
               ELSE IF ( NEGUMU ) THEN
                  EXPN = ( EXP0 - EXPBEA(LYU-1) * EXP1 ) / DENOM
               ELSE
                  EXPN = ( EXP0 - EXPBEA(LYU) * EXP2 ) / DENOM
               END IF
               PALINT = PALINT + ZBEAM(IU,LYU) * EXPN
            ENDIF
C                                                   ** -KK- IS NEGATIVE
            DTAU = TAUCPR(LYU) - TAUCPR(LYU-1)
            DO 40  IQ = 1, NN
               DENOM = 1.0 + UMU(IU) * KK(IQ,LYU)
               IF ( ABS(DENOM).LT.0.0001 ) THEN
                  EXPN = - DTAU2 / UMU(IU) * EXP2
               ELSE IF ( NEGUMU ) THEN
                  EXPN = ( EXP( - KK(IQ,LYU) * DTAU2 ) -
     $                     EXP( KK(IQ,LYU) * DTAU ) * EXP1 ) / DENOM
               ELSE
                  EXPN = ( EXP( - KK(IQ,LYU) * DTAU2 ) - EXP2 ) / DENOM
               END IF
               PALINT = PALINT + GU(IU,IQ,LYU) * EXPN
40          CONTINUE
C                                                   ** -KK- IS POSITIVE
            DO 41  IQ = NN+1, NSTR
               DENOM = 1.0 + UMU(IU) * KK(IQ,LYU)
               IF ( ABS(DENOM).LT.0.0001 ) THEN
                  EXPN = - DTAU1 / UMU(IU) * EXP1
               ELSE IF ( NEGUMU ) THEN
                  EXPN = ( EXP(- KK(IQ,LYU) * DTAU1 ) - EXP1 ) / DENOM
               ELSE
                  EXPN = ( EXP( - KK(IQ,LYU) * DTAU1 ) -
     $                     EXP( - KK(IQ,LYU) * DTAU ) * EXP2 ) / DENOM
               END IF
               PALINT = PALINT + GU(IU,IQ,LYU) * EXPN
41          CONTINUE
C
            IF ( .NOT.NOPLNK .AND. MAZ.EQ.0 )  THEN
              IF ( NEGUMU ) THEN
                 EXPN = EXP1
                 FACT = TAUCPR(LYU-1) + UMU(IU)
              ELSE
                 EXPN = EXP2
                 FACT = TAUCPR( LYU ) + UMU(IU)
              END IF
              PLKINT = PLKINT + Z0U(IU,LYU) * ( 1.- EXPN ) +
     $                 Z1U(IU,LYU) *( UTAUPR(LU) + UMU(IU) - FACT*EXPN )
            END IF
C                            ** CALCULATE INTENSITY COMPONENTS
C                            ** ATTENUATED AT BOTH BOUNDARIES.
C                            ** NOTE:: NO AZIMUTHAL INTENSITY
C                            ** COMPONENT FOR ISOTROPIC SURFACE
50          BNDINT = 0.0
            IF ( NEGUMU .AND. MAZ.EQ.0 ) THEN
              BNDINT = ( FISOT + TPLANK ) * EXP( UTAUPR(LU) / UMU(IU) )
            ELSE IF ( .NOT.NEGUMU ) THEN
              IF ( LYRCUT .OR. (LAMBER .AND. MAZ.GT.0) )  GO TO 90
              DO 60  JQ = NN+1, NSTR
                WK(JQ) = EXP(-KK(JQ,NLYR)*(TAUCPR(NLYR)-TAUCPR(NLYR-1)))
60            CONTINUE
              BNDDFU = 0.0
              DO 80  IQ = NN, 1, -1
                 DFUINT = 0.0
                 DO 70  JQ = 1, NN
                    DFUINT = DFUINT + GC(IQ,JQ,NLYR) * LL(JQ,NLYR)
70               CONTINUE
                 DO 71  JQ = NN+1, NSTR
                    DFUINT = DFUINT + GC(IQ,JQ,NLYR) * LL(JQ,NLYR)
     $                                * WK(JQ)
71               CONTINUE
                 IF ( FBEAM.GT.0.0 )
     $                DFUINT = DFUINT + ZZ(IQ,NLYR) * EXPBEA(NLYR)
                 DFUINT = DFUINT + DELM0 * ( ZPLK0(IQ,NLYR)
     $                                + ZPLK1(IQ,NLYR) * TAUCPR(NLYR) )
                 BNDDFU = BNDDFU + ( 1. + DELM0 ) * RMU(IU,NN+1-IQ)
     $                           * CMU(NN+1-IQ) * CWT(NN+1-IQ) * DFUINT
80            CONTINUE
C
              BNDDIR = 0.0
              IF (FBEAM.GT.0.0) BNDDIR = UMU0*FBEAM/PI * RMU(IU,0)
     $                                   * EXPBEA(NLYR)
              BNDINT = ( BNDDFU + BNDDIR + DELM0 * EMU(IU) * BPLANK )
     $                 * EXP( (UTAUPR(LU)-TAUCPR(NLYR)) / UMU(IU) )
            END IF
C
90          UUM( IU, LU, MAZ ) = PALINT + PLKINT + BNDINT
C
100      CONTINUE
200   CONTINUE
C
      RETURN
      END
      LOGICAL FUNCTION  WRTBAD ( VARNAM )
C
C          WRITE NAMES OF ERRONEOUS VARIABLES AND RETURN 'TRUE'
C
C      INPUT :   VARNAM = NAME OF ERRONEOUS VARIABLE TO BE WRITTEN
C                         ( CHARACTER, ANY LENGTH )
C ----------------------------------------------------------------------
      CHARACTER*(*)  VARNAM
      INTEGER        MAXMSG, NUMMSG
      SAVE  NUMMSG, MAXMSG
      DATA  NUMMSG / 0 /,  MAXMSG / 50 /
C
C
      WRTBAD = .TRUE.
      NUMMSG = NUMMSG + 1
      WRITE ( *, '(3A)' )  ' ****  INPUT VARIABLE  ', VARNAM,
     $                     '  IN ERROR  ****'
      IF ( NUMMSG.EQ.MAXMSG )
     $   CALL  ERRMSG ( 'TOO MANY INPUT ERRORS.  ABORTING...$', .TRUE. )
      RETURN
      END
      LOGICAL FUNCTION  WRTDIM ( DIMNAM, MINVAL )
C
C          WRITE NAME OF TOO-SMALL SYMBOLIC DIMENSION AND
C          THE VALUE IT SHOULD BE INCREASED TO;  RETURN 'TRUE'
C
C      INPUT :  DIMNAM = NAME OF SYMBOLIC DIMENSION WHICH IS TOO SMALL
C                        ( CHARACTER, ANY LENGTH )
C               MINVAL = VALUE TO WHICH THAT DIMENSION SHOULD BE
C                        INCREASED (AT LEAST)
C ----------------------------------------------------------------------
      CHARACTER*(*)  DIMNAM
      INTEGER        MINVAL
C
C
      WRITE ( *, '(3A,I7)' )  ' ****  SYMBOLIC DIMENSION  ', DIMNAM,
     $                     '  SHOULD BE INCREASED TO AT LEAST ', MINVAL
      WRTDIM = .TRUE.
      RETURN
      END
      SUBROUTINE  ZEROAL( AMB, APB, ARRAY, CC, CMU, CWT, EVAL, EVECC,
     $                    GC, GU, HLPR, KK, LL, PSI, WK, XR0, XR1,
     $                    YLM0, YLMC, YLMU, Z, Z0, Z1, ZJ, ZZ, ZPLK0,
     $                    ZPLK1, Z0U, Z1U, ZBEAM, MI, MXCMU, MXCLY,
     $                    NNLYRI, MXUMU, MXULV )
C
C            ZERO ARRAYS
C
      REAL  AMB(MI,*), APB(MI,*), ARRAY(MXCMU,*), CC(MXCMU,*),
     $      CMU(*), CWT(*), EVAL(*), EVECC(MXCMU,*), GC(MXCMU,MXCMU,*),
     $      GU(MXUMU,MXCMU,*), HLPR(0:*), KK(MXCMU,*), LL(MXCMU,*),
     $      PSI(*), WK(*), XR0(*), XR1(*),  YLM0(0:*),
     $      YLMC(0:MXCMU,*), YLMU(0:MXCMU,*), Z(*), Z0(*), Z1(*),
     $      Z0U(MXUMU,*), Z1U(MXUMU,*), ZJ(*), ZZ(MXCMU,*),
     $      ZPLK0(MXCMU,*), ZPLK1(MXCMU,*), ZBEAM(MXUMU,*)
C
C
      CALL  ZEROIT( XR0, MXCLY )
      CALL  ZEROIT( XR1, MXCLY )
      CALL  ZEROIT( CMU, MXCMU )
      CALL  ZEROIT( CWT, MXCMU )
      CALL  ZEROIT( PSI, MXCMU )
      CALL  ZEROIT( EVAL,MXCMU )
      CALL  ZEROIT( WK,  MXCMU )
      CALL  ZEROIT( Z0,  MXCMU )
      CALL  ZEROIT( Z1,  MXCMU )
      CALL  ZEROIT( ZJ,  MXCMU )
      CALL  ZEROIT( HLPR, MXCMU+1 )
      CALL  ZEROIT( YLM0, MXCMU+1 )
      CALL  ZEROIT( ARRAY, MXCMU**2 )
      CALL  ZEROIT( CC,    MXCMU**2 )
      CALL  ZEROIT( EVECC, MXCMU**2 )
      CALL  ZEROIT( YLMC, (MXCMU+1)*MXCMU )
      CALL  ZEROIT( YLMU, (MXCMU+1)*MXUMU )
      CALL  ZEROIT( AMB, MI**2 )
      CALL  ZEROIT( APB, MI**2 )
      CALL  ZEROIT( KK,     MXCMU*MXCLY )
      CALL  ZEROIT( LL,     MXCMU*MXCLY )
      CALL  ZEROIT( ZZ,     MXCMU*MXCLY )
      CALL  ZEROIT( ZPLK0,  MXCMU*MXCLY )
      CALL  ZEROIT( ZPLK1,  MXCMU*MXCLY )
      CALL  ZEROIT( Z0U,   MXUMU*MXCLY )
      CALL  ZEROIT( Z1U,   MXUMU*MXCLY )
      CALL  ZEROIT( ZBEAM, MXUMU*MXCLY )
      CALL  ZEROIT( GC, MXCMU**2*MXCLY )
      CALL  ZEROIT( GU, MXUMU*MXCMU*MXCLY )
      CALL  ZEROIT( Z, NNLYRI )
C
      RETURN
      END
      SUBROUTINE  ZEROIT( A, LENGTH )
C
C         ZEROS A REAL ARRAY -A- HAVING -LENGTH- ELEMENTS
C
C     REAL  A(*)
      DIMENSION A(*)
C
      DO 10  L = 1, LENGTH
         A( L ) = 0.0
10    CONTINUE
C
      RETURN
      END
