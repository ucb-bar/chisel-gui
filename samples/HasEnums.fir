;buildInfoPackage: chisel3, version: 3.3-SNAPSHOT, scalaVersion: 2.12.10, sbtVersion: 1.3.8
circuit HasEnums : 
  module NextState : 
    input clock : Clock
    input reset : Reset
    input state : UInt<3>
    output nextState : UInt<3>
    
    nextState is invalid @[ChiselEnumTest.scala 25:13]
    node _T = asUInt(UInt<1>("h00")) @[Conditional.scala 37:23]
    node _T_1 = asUInt(state) @[Conditional.scala 37:39]
    node _T_2 = eq(_T, _T_1) @[Conditional.scala 37:30]
    when _T_2 : @[Conditional.scala 40:58]
      nextState <= UInt<1>("h01") @[ChiselEnumTest.scala 27:25]
      skip @[Conditional.scala 40:58]
    else : @[Conditional.scala 39:67]
      node _T_3 = asUInt(UInt<1>("h01")) @[Conditional.scala 37:23]
      node _T_4 = asUInt(state) @[Conditional.scala 37:39]
      node _T_5 = eq(_T_3, _T_4) @[Conditional.scala 37:30]
      when _T_5 : @[Conditional.scala 39:67]
        nextState <= UInt<2>("h02") @[ChiselEnumTest.scala 28:25]
        skip @[Conditional.scala 39:67]
      else : @[Conditional.scala 39:67]
        node _T_6 = asUInt(UInt<2>("h02")) @[Conditional.scala 37:23]
        node _T_7 = asUInt(state) @[Conditional.scala 37:39]
        node _T_8 = eq(_T_6, _T_7) @[Conditional.scala 37:30]
        when _T_8 : @[Conditional.scala 39:67]
          nextState <= UInt<2>("h03") @[ChiselEnumTest.scala 29:25]
          skip @[Conditional.scala 39:67]
        else : @[Conditional.scala 39:67]
          node _T_9 = asUInt(UInt<2>("h03")) @[Conditional.scala 37:23]
          node _T_10 = asUInt(state) @[Conditional.scala 37:39]
          node _T_11 = eq(_T_9, _T_10) @[Conditional.scala 37:30]
          when _T_11 : @[Conditional.scala 39:67]
            nextState <= UInt<3>("h04") @[ChiselEnumTest.scala 30:25]
            skip @[Conditional.scala 39:67]
          else : @[Conditional.scala 39:67]
            node _T_12 = asUInt(UInt<3>("h04")) @[Conditional.scala 37:23]
            node _T_13 = asUInt(state) @[Conditional.scala 37:39]
            node _T_14 = eq(_T_12, _T_13) @[Conditional.scala 37:30]
            when _T_14 : @[Conditional.scala 39:67]
              nextState <= UInt<1>("h00") @[ChiselEnumTest.scala 31:25]
              skip @[Conditional.scala 39:67]
    
  module HasEnums : 
    input clock : Clock
    input reset : UInt<1>
    input advance : UInt<1>
    output currentState : UInt<3>
    output nextEatee : UInt<3>
    
    nextEatee is invalid @[ChiselEnumTest.scala 40:13]
    reg stateReg : UInt<3>, clock with : (reset => (reset, UInt<1>("h00"))) @[ChiselEnumTest.scala 45:25]
    reg cycles : UInt<24>, clock with : (reset => (reset, UInt<24>("h00"))) @[ChiselEnumTest.scala 47:23]
    node _T = add(cycles, UInt<1>("h01")) @[ChiselEnumTest.scala 48:20]
    node _T_1 = tail(_T, 1) @[ChiselEnumTest.scala 48:20]
    cycles <= _T_1 @[ChiselEnumTest.scala 48:10]
    inst getNextState of NextState @[ChiselEnumTest.scala 50:28]
    getNextState.clock <= clock
    getNextState.reset <= reset
    getNextState.state <= stateReg @[ChiselEnumTest.scala 51:22]
    when advance : @[ChiselEnumTest.scala 53:17]
      stateReg <= getNextState.nextState @[ChiselEnumTest.scala 54:14]
      skip @[ChiselEnumTest.scala 53:17]
    node _T_2 = asUInt(UInt<1>("h00")) @[Conditional.scala 37:23]
    node _T_3 = asUInt(stateReg) @[Conditional.scala 37:39]
    node _T_4 = eq(_T_2, _T_3) @[Conditional.scala 37:30]
    when _T_4 : @[Conditional.scala 40:58]
      nextEatee <= UInt<1>("h00") @[ChiselEnumTest.scala 58:25]
      skip @[Conditional.scala 40:58]
    else : @[Conditional.scala 39:67]
      node _T_5 = asUInt(UInt<1>("h01")) @[Conditional.scala 37:23]
      node _T_6 = asUInt(stateReg) @[Conditional.scala 37:39]
      node _T_7 = eq(_T_5, _T_6) @[Conditional.scala 37:30]
      when _T_7 : @[Conditional.scala 39:67]
        nextEatee <= UInt<1>("h01") @[ChiselEnumTest.scala 59:25]
        skip @[Conditional.scala 39:67]
      else : @[Conditional.scala 39:67]
        node _T_8 = asUInt(UInt<2>("h02")) @[Conditional.scala 37:23]
        node _T_9 = asUInt(stateReg) @[Conditional.scala 37:39]
        node _T_10 = eq(_T_8, _T_9) @[Conditional.scala 37:30]
        when _T_10 : @[Conditional.scala 39:67]
          nextEatee <= UInt<2>("h02") @[ChiselEnumTest.scala 60:25]
          skip @[Conditional.scala 39:67]
        else : @[Conditional.scala 39:67]
          node _T_11 = asUInt(UInt<2>("h03")) @[Conditional.scala 37:23]
          node _T_12 = asUInt(stateReg) @[Conditional.scala 37:39]
          node _T_13 = eq(_T_11, _T_12) @[Conditional.scala 37:30]
          when _T_13 : @[Conditional.scala 39:67]
            nextEatee <= UInt<2>("h03") @[ChiselEnumTest.scala 61:25]
            skip @[Conditional.scala 39:67]
          else : @[Conditional.scala 39:67]
            node _T_14 = asUInt(UInt<3>("h04")) @[Conditional.scala 37:23]
            node _T_15 = asUInt(stateReg) @[Conditional.scala 37:39]
            node _T_16 = eq(_T_14, _T_15) @[Conditional.scala 37:30]
            when _T_16 : @[Conditional.scala 39:67]
              nextEatee <= UInt<3>("h04") @[ChiselEnumTest.scala 62:25]
              skip @[Conditional.scala 39:67]
    node _T_17 = asUInt(stateReg) @[ChiselEnumTest.scala 65:51]
    node _T_18 = bits(reset, 0, 0) @[ChiselEnumTest.scala 65:9]
    node _T_19 = eq(_T_18, UInt<1>("h00")) @[ChiselEnumTest.scala 65:9]
    when _T_19 : @[ChiselEnumTest.scala 65:9]
      printf(clock, UInt<1>(1), "cycles %d state %d\n", cycles, _T_17) @[ChiselEnumTest.scala 65:9]
      skip @[ChiselEnumTest.scala 65:9]
    currentState <= stateReg @[ChiselEnumTest.scala 67:16]
    
