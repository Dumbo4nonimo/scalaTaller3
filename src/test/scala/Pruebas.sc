import ManiobrasTrenes._
val principal: Tren = List('a', 'b', 'c', 'd')
val one: Tren = List('e', 'f')
val two: Tren = List('g', 'h')

val m: Movimiento = Uno(2)

val estado: Estado = (principal, one, two)

aplicarMovimiento(estado, m)
