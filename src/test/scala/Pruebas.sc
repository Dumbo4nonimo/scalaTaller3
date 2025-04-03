import ManiobrasTrenes._
val estadoInicial: Estado = (
  List("V1", "V2", "V3", "V4"),  // Tren1
  List("V5", "V6"),               // Tren2
  List("V7", "V8")                // Tren3
)

// Mover 2 vagones desde el final de Tren1 a Tren2 (Uno(2))
val estado1 = aplicarMovimiento(estadoInicial, Dos(0))

val estado2= aplicarMovimientos(estadoInicial,List(Uno(2),Uno(-2),Dos(1),Dos(0)))
