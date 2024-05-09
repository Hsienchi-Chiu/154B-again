;buildInfoPackage: chisel3, version: 3.2.8, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit Control : 
  module Control : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip opcode : UInt<7>, aluop : UInt<3>, controltransferop : UInt<2>, memop : UInt<2>, op1_src : UInt<1>, op2_src : UInt<2>, writeback_valid : UInt<1>, writeback_src : UInt<2>, validinst : UInt<1>}
    
    node _T = and(io.opcode, UInt<7>("h07f")) @[Lookup.scala 31:38]
    node _T_1 = eq(UInt<6>("h033"), _T) @[Lookup.scala 31:38]
    node _T_2 = and(io.opcode, UInt<7>("h07f")) @[Lookup.scala 31:38]
    node _T_3 = eq(UInt<6>("h03b"), _T_2) @[Lookup.scala 31:38]
    node _T_4 = mux(_T_3, UInt<2>("h03"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_0 = mux(_T_1, UInt<1>("h01"), _T_4) @[Lookup.scala 33:37]
    node _T_5 = mux(_T_3, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_1 = mux(_T_1, UInt<1>("h00"), _T_5) @[Lookup.scala 33:37]
    node _T_6 = mux(_T_3, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_2 = mux(_T_1, UInt<1>("h00"), _T_6) @[Lookup.scala 33:37]
    node _T_7 = mux(_T_3, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_3 = mux(_T_1, UInt<1>("h00"), _T_7) @[Lookup.scala 33:37]
    node _T_8 = mux(_T_3, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_4 = mux(_T_1, UInt<1>("h00"), _T_8) @[Lookup.scala 33:37]
    node _T_9 = mux(_T_3, UInt<1>("h01"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_5 = mux(_T_1, UInt<1>("h01"), _T_9) @[Lookup.scala 33:37]
    node _T_10 = mux(_T_3, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_6 = mux(_T_1, UInt<1>("h00"), _T_10) @[Lookup.scala 33:37]
    node _T_11 = mux(_T_3, UInt<1>("h01"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_7 = mux(_T_1, UInt<1>("h01"), _T_11) @[Lookup.scala 33:37]
    io.aluop <= signals_0 @[control.scala 72:24]
    io.controltransferop <= signals_1 @[control.scala 73:24]
    io.memop <= signals_2 @[control.scala 74:24]
    io.op1_src <= signals_3 @[control.scala 75:24]
    io.op2_src <= signals_4 @[control.scala 76:24]
    io.writeback_valid <= signals_5 @[control.scala 77:24]
    io.writeback_src <= signals_6 @[control.scala 78:24]
    io.validinst <= signals_7 @[control.scala 79:24]
    
