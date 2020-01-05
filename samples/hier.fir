;buildInfoPackage: chisel3, version: 3.3-SNAPSHOT, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit TopOfVisualizer :
  module VizModC :
    input clock : Clock
    input reset : Reset
    output io : {flip in : UInt<16>, out : UInt<16>}

    io.out <= io.in @[HierarchicalModulesExample.scala 25:10]

  module VizModC_1 :
    input clock : Clock
    input reset : Reset
    output io : {flip in : UInt<16>, out : UInt<16>}

    io.out <= io.in @[HierarchicalModulesExample.scala 25:10]

  module VizModB :
    input clock : Clock
    input reset : Reset
    output io : {flip in : UInt<16>, out : UInt<16>}

    inst modC of VizModC_1 @[HierarchicalModulesExample.scala 55:20]
    modC.clock <= clock
    modC.reset <= reset
    modC.io.in <= io.in @[HierarchicalModulesExample.scala 56:14]
    io.out <= modC.io.out @[HierarchicalModulesExample.scala 57:10]

  module VizModC_2 :
    input clock : Clock
    input reset : Reset
    output io : {flip in : UInt<16>, out : UInt<16>}

    io.out <= io.in @[HierarchicalModulesExample.scala 25:10]

  module VizModB_1 :
    input clock : Clock
    input reset : Reset
    output io : {flip in : UInt<16>, out : UInt<16>}

    inst modC of VizModC_2 @[HierarchicalModulesExample.scala 55:20]
    modC.clock <= clock
    modC.reset <= reset
    modC.io.in <= io.in @[HierarchicalModulesExample.scala 56:14]
    io.out <= modC.io.out @[HierarchicalModulesExample.scala 57:10]

  module VizModA :
    input clock : Clock
    input reset : Reset
    output io : {flip in : UInt, out : UInt}

    inst modC of VizModC @[HierarchicalModulesExample.scala 39:20]
    modC.clock <= clock
    modC.reset <= reset
    inst modB of VizModB @[HierarchicalModulesExample.scala 40:20]
    modB.clock <= clock
    modB.reset <= reset
    inst modB2 of VizModB_1 @[HierarchicalModulesExample.scala 41:21]
    modB2.clock <= clock
    modB2.reset <= reset
    modC.io.in <= io.in @[HierarchicalModulesExample.scala 43:14]
    modB.io.in <= io.in @[HierarchicalModulesExample.scala 44:14]
    modB2.io.in <= io.in @[HierarchicalModulesExample.scala 45:15]
    node _T = add(modC.io.out, modB.io.out) @[HierarchicalModulesExample.scala 46:25]
    node _T_1 = tail(_T, 1) @[HierarchicalModulesExample.scala 46:25]
    node _T_2 = add(_T_1, modB2.io.out) @[HierarchicalModulesExample.scala 46:39]
    node _T_3 = tail(_T_2, 1) @[HierarchicalModulesExample.scala 46:39]
    io.out <= _T_3 @[HierarchicalModulesExample.scala 46:10]

  module VizModC_3 :
    input clock : Clock
    input reset : Reset
    output io : {flip in : UInt<32>, out : UInt<32>}

    io.out <= io.in @[HierarchicalModulesExample.scala 25:10]

  module VizModB_2 :
    input clock : Clock
    input reset : Reset
    output io : {flip in : UInt<32>, out : UInt<32>}

    inst modC of VizModC_3 @[HierarchicalModulesExample.scala 55:20]
    modC.clock <= clock
    modC.reset <= reset
    modC.io.in <= io.in @[HierarchicalModulesExample.scala 56:14]
    io.out <= modC.io.out @[HierarchicalModulesExample.scala 57:10]

  module VizModC_4 :
    input clock : Clock
    input reset : Reset
    output io : {flip in : UInt<48>, out : UInt<48>}

    io.out <= io.in @[HierarchicalModulesExample.scala 25:10]

  module TopOfVisualizer :
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : UInt<32>, flip in2 : UInt<32>, flip select : UInt<1>, out : UInt<32>, memOut : UInt<32>}

    reg x : UInt<32>, clock @[HierarchicalModulesExample.scala 69:14]
    reg y : UInt<32>, clock @[HierarchicalModulesExample.scala 70:14]
    cmem myMem : UInt<32>[16] @[HierarchicalModulesExample.scala 72:18]
    io.memOut is invalid @[HierarchicalModulesExample.scala 74:13]
    inst modA of VizModA @[HierarchicalModulesExample.scala 76:20]
    modA.clock <= clock
    modA.reset <= reset
    inst modB of VizModB_2 @[HierarchicalModulesExample.scala 77:20]
    modB.clock <= clock
    modB.reset <= reset
    inst modC of VizModC_4 @[HierarchicalModulesExample.scala 78:20]
    modC.clock <= clock
    modC.reset <= reset
    when io.select : @[HierarchicalModulesExample.scala 80:19]
      x <= io.in1 @[HierarchicalModulesExample.scala 81:7]
      node _T = bits(io.in1, 3, 0) @[HierarchicalModulesExample.scala 82:10]
      infer mport _T_1 = myMem[_T], clock @[HierarchicalModulesExample.scala 82:10]
      _T_1 <= io.in2 @[HierarchicalModulesExample.scala 82:19]
      skip @[HierarchicalModulesExample.scala 80:19]
    else : @[HierarchicalModulesExample.scala 84:14]
      x <= io.in2 @[HierarchicalModulesExample.scala 85:7]
      node _T_2 = bits(io.in1, 3, 0) @[HierarchicalModulesExample.scala 86:23]
      infer mport _T_3 = myMem[_T_2], clock @[HierarchicalModulesExample.scala 86:23]
      io.memOut <= _T_3 @[HierarchicalModulesExample.scala 86:15]
      skip @[HierarchicalModulesExample.scala 84:14]
    modA.io.in <= x @[HierarchicalModulesExample.scala 89:14]
    node _T_4 = add(modA.io.out, io.in2) @[HierarchicalModulesExample.scala 91:20]
    node _T_5 = tail(_T_4, 1) @[HierarchicalModulesExample.scala 91:20]
    node _T_6 = add(_T_5, modB.io.out) @[HierarchicalModulesExample.scala 91:29]
    node _T_7 = tail(_T_6, 1) @[HierarchicalModulesExample.scala 91:29]
    node _T_8 = add(_T_7, modC.io.out) @[HierarchicalModulesExample.scala 91:43]
    node _T_9 = tail(_T_8, 1) @[HierarchicalModulesExample.scala 91:43]
    y <= _T_9 @[HierarchicalModulesExample.scala 91:5]
    io.out <= y @[HierarchicalModulesExample.scala 92:10]
    modB.io.in <= x @[HierarchicalModulesExample.scala 94:14]
    modC.io.in <= x @[HierarchicalModulesExample.scala 95:14]

