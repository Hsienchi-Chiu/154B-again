;buildInfoPackage: chisel3, version: 3.2.8, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit ControlTransferUnit : 
  module ControlTransferUnit : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip controltransferop : UInt<2>, flip operand1 : UInt<64>, flip operand2 : UInt<64>, flip funct3 : UInt<3>, flip pc : UInt<64>, flip imm : UInt<64>, nextpc : UInt<64>, taken : UInt<1>}
    
    node _T = add(io.pc, UInt<3>("h04")) @[controltransferunit.scala 40:22]
    node _T_1 = tail(_T, 1) @[controltransferunit.scala 40:22]
    io.nextpc <= _T_1 @[controltransferunit.scala 40:13]
    io.taken <= UInt<1>("h00") @[controltransferunit.scala 41:12]
    
