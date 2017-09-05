function [ C_out ] = Trtrmm_unb_var1( U, R, C )

  [ UTL, UTR, ...
    UBL, UBR ] = FLA_Part_2x2( U, ...
                               0, 0, 'FLA_TL' );

  [ RTL, RTR, ...
    RBL, RBR ] = FLA_Part_2x2( R, ...
                               0, 0, 'FLA_TL' );

  [ CTL, CTR, ...
    CBL, CBR ] = FLA_Part_2x2( C, ...
                               0, 0, 'FLA_TL' );

  while ( size( UTL, 1 ) < size( U, 1 ) )

    [ U00,  u01,       U02,  ...
      u10t, upsilon11, u12t, ...
      U20,  u21,       U22 ] = FLA_Repart_2x2_to_3x3( UTL, UTR, ...
                                                      UBL, UBR, ...
                                                      1, 1, 'FLA_BR' );

    [ R00,  r01,   R02,  ...
      r10t, rho11, r12t, ...
      R20,  r21,   R22 ] = FLA_Repart_2x2_to_3x3( RTL, RTR, ...
                                                  RBL, RBR, ...
                                                  1, 1, 'FLA_BR' );

    [ C00,  c01,     C02,  ...
      c10t, gamma11, c12t, ...
      C20,  c21,     C22 ] = FLA_Repart_2x2_to_3x3( CTL, CTR, ...
                                                    CBL, CBR, ...
                                                    1, 1, 'FLA_BR' );

    %------------------------------------------------------------%
    
    c01 = laff_trmv( 'Upper triangular', 'No transpose', 'Nonunit diag', U00, r01 );
    c01 = laff_axpy(rho11, u01, c01);
    gamma11 = laff_dot(upsilon11, rho11);
    
    %------------------------------------------------------------%

    [ UTL, UTR, ...
      UBL, UBR ] = FLA_Cont_with_3x3_to_2x2( U00,  u01,       U02,  ...
                                             u10t, upsilon11, u12t, ...
                                             U20,  u21,       U22, ...
                                             'FLA_TL' );

    [ RTL, RTR, ...
      RBL, RBR ] = FLA_Cont_with_3x3_to_2x2( R00,  r01,   R02,  ...
                                             r10t, rho11, r12t, ...
                                             R20,  r21,   R22, ...
                                             'FLA_TL' );

    [ CTL, CTR, ...
      CBL, CBR ] = FLA_Cont_with_3x3_to_2x2( C00,  c01,     C02,  ...
                                             c10t, gamma11, c12t, ...
                                             C20,  c21,     C22, ...
                                             'FLA_TL' );

  end

  C_out = [ CTL, CTR
            CBL, CBR ];

return