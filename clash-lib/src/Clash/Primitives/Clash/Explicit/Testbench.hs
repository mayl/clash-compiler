module Clash.Primitives.Clash.Explicit.Testbench where

import qualified Clash.Explicit.Testbench as P

systemverilogPrimitives =
  [ -- assert
    --  :: (KnownDomain dom, Eq a, ShowX a)      -- (ARG[0], ARG[1], ARG[2])
    --  => Clock dom                             -- ARG[3]
    --  -> Reset dom                             -- ARG[4]
    --  -> String                                -- ARG[5]
    --  -> Signal dom a                          -- Checked value  (ARG[6])
    --  -> Signal dom a                          -- Expected value (ARG[7])
    --  -> Signal dom b                          -- Return valued  (ARG[8])
    --  -> Signal dom b
    (blackbox 'P.assert){
      kind=Declaration
    , template=[I.i|
        // assert begin
        // pragma translate_off
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin
          if (~ARG[6] !== ~ARG[7]) begin
            $display(\"@%0tns: %s, expected: %b, actual: %b\", $time, ~LIT[5], ~TOBV[~ARG[7]][~TYP[7]], ~TOBV[~ARG[6]][~TYP[6]]);
            $stop;
          end
        end
        // pragma translate_on
        assign ~RESULT = ~ARG[8];
        // assert end
      |]
  , (blackbox 'P.assertBitVector){
      kind=Declaration
    , template=[I.i|
        // assertBitVector begin
        // pragma translate_off
        wire ~TYP[6] ~GENSYM[maskXor][0]  = ~ARG[6] ^ ~ARG[6];
        wire ~TYP[6] ~GENSYM[checked][1]  = ~ARG[5] ^ ~SYM[0];
        wire ~TYP[6] ~GENSYM[expected][2] = ~ARG[6] ^ ~SYM[0];

        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[2]) begin
          if (~SYM[1] !== ~SYM[2]) begin
            $display(\"@%0tns: %s, expected: %b, actual: %b\", $time, ~LIT[4], ~TOBV[~ARG[6]][~TYP[6]], ~TOBV[~ARG[5]][~TYP[5]]);
            $stop;
          end
        end
        // pragma translate_on
        assign ~RESULT = ~ARG[7];
        // assertBitVector end
      |]
    }
  ]