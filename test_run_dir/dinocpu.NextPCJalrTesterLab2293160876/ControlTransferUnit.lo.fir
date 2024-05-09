circuit ControlTransferUnit :
  module ControlTransferUnit :
    input clock : Clock
    input reset : UInt<1>
    input io_controltransferop : UInt<2>
    input io_operand1 : UInt<64>
    input io_operand2 : UInt<64>
    input io_funct3 : UInt<3>
    input io_pc : UInt<64>
    input io_imm : UInt<64>
    output io_nextpc : UInt<64>
    output io_taken : UInt<1>
  
    node _T = add(io_pc, UInt<3>("h4")) @[controltransferunit.scala 40:22]
    node _T_1 = tail(_T, 1) @[controltransferunit.scala 40:22]
    io_nextpc <= _T_1 @[controltransferunit.scala 40:13]
    io_taken <= UInt<1>("h0") @[controltransferunit.scala 41:12]
