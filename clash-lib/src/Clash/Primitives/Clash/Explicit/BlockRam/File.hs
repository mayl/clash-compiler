module Clash.Primitives.Clash.GHC.Word where

import qualified Clash.GHC.Word as P

systemverilogPrimitives =
  [ -- blockRamFile#
    --   :: ( KnownDomain dom         --       ARG[0]
    --      , KnownNat m              --       ARG[1]
    --      , HasCallStack )          --       ARG[2]
    --   => Clock dom                 -- clk,  ARG[3]
    --   => Enable dom                -- en,   ARG[4]
    --   -> SNat n                    -- sz,   ARG[5]
    --   -> FilePath                  -- file, ARG[6]
    --   -> Signal dom Int            -- rd,   ARG[7]
    --   -> Signal dom Bool           -- wren, ARG[8]
    --   -> Signal dom Int            -- wr,   ARG[9]
    --   -> Signal dom (BitVector m)  -- din,  ARG[10]
    --   -> Signal dom (BitVector m)
    (blackbox 'P.blockRamFile#){
      kind=Declaration
    , template=[I.i|
        // blockRamFile begin
        ~SIGDO[~GENSYM[RAM][1]] [0:~LIT[5]-1];
        ~SIGD[~GENSYM[~RESULT_q][2]][10];

        initial begin
          $readmemb(~FILE[~LIT[6]],~SYM[1]);
        end
        ~IF ~ISACTIVEENABLE[4] ~THEN
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~GENSYM[~COMPNAME_blockRamFile][3]~IF ~VIVADO ~THEN
          if (~ARG[4]) begin
            if (~ARG[8]) begin
              ~SYM[1][~ARG[9]] <= ~ARG[10];
            end
            ~SYM[2] <= ~SYM[1][~ARG[7]];
          end~ELSE
          if (~ARG[8] & ~ARG[4]) begin
            ~SYM[1][~ARG[9]] <= ~ARG[10];
          end
          if (~ARG[4]) begin
            ~SYM[2] <= ~SYM[1][~ARG[7]];
          end~FI
        end~ELSE
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~SYM[3]
          if (~ARG[8]) begin
            ~SYM[1][~ARG[9]] <= ~ARG[10];
          end
          ~SYM[2] <= ~SYM[1][~ARG[7]];
        end~FI

        assign ~RESULT = ~SYM[2];
        // blockRamFile end
      |]
    }
  ]
