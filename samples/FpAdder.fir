;buildInfoPackage: chisel3, version: 3.3-SNAPSHOT, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit FpAdder : 
  module FpAdder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : Fixed<8><<2>>, flip b : Fixed<8><<2>>, c : Fixed<9><<2>>}
    
    node _T = add(io.a, io.b) @[FixedPointTest.scala 20:16]
    node _T_1 = tail(_T, 1) @[FixedPointTest.scala 20:16]
    node _T_2 = asFixedPoint(_T_1, 2) @[FixedPointTest.scala 20:16]
    io.c <= _T_2 @[FixedPointTest.scala 20:8]
    
