module Clash.Primitives.Clash.Explicit.ROM.File where

import qualified Clash.Explicit.ROM.File as P

systemverilogPrimitives =
  [ -- romFile# :: ( KnownNat m             --       ARG[0]
    --             , KnownDomain dom )      --       ARG[1]
    --          => Clock dom                -- clk,  ARG[2]
    --          -> Enable dom               -- en,   ARG[3]
    --          -> SNat n                   -- sz,   ARG[4]
    --          -> FilePath                 -- file, ARG[5]
    --          -> Signal dom Int           -- rd,   ARG[6]
    --          -> Signal dom (BitVector m)
    (blackbox 'P.romFile#){
      kind=Declaration
    , template=[I.i|
        // romFile begin
        ~SIGDO[~GENSYM[ROM][0]] [0:~LIT[4]-1];

        initial begin
          $readmemb(~FILE[~LIT[5]],~SYM[0]);
        end

        ~SIGDO[~GENSYM[~RESULT_q][1]];~IF ~ISACTIVEENABLE[3] ~THEN
        always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[2]) begin : ~GENSYM[~COMPNAME_romFile][2]
          if (~ARG[3]) begin
            ~SYM[1] <= ~SYM[0][~ARG[6]];
          end
        end~ELSE
        always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[2]) begin : ~SYM[2]
          ~SYM[1] <= ~SYM[0][~ARG[6]];
        end~FI

        assign ~RESULT = ~SYM[1];
        // romFile end
      |]
    }
  ]