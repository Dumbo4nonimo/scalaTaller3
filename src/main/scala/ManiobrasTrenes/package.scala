package object ManiobrasTrenes {
  type Vagon = Any
  type Tren = List[Vagon]
  type Estado = (Tren, Tren, Tren)

  trait Movimiento
  case class Uno(n:Int) extends Movimiento
  case class Dos(n:Int) extends Movimiento
  type Maniobra = List[Movimiento]

  //Estas funciones seran comentadas en el informe para hacer el codigo menos largo

  def tomarDesdeIzquierda(tren: Tren, n: Int): (Tren, Tren) = (tren, n) match {
    case (vagones, 0) => (Nil, vagones)
    case (Nil, _) => (Nil, Nil)
    case (head :: tail, _) =>
      val (tomados, restantes) = tomarDesdeIzquierda(tail, n - 1)
      (head :: tomados, restantes)
  }
  def tomarDesdeDerecha(tren: Tren, n: Int): (Tren, Tren) = {
    def invertir(t: Tren): Tren = t match {
      case Nil => Nil
      case head :: tail => invertir(tail) ::: List(head)
    }

    val (tomadosInvertidos, restantesInvertidos) = tomarDesdeIzquierda(invertir(tren), n)
    (invertir(tomadosInvertidos), invertir(restantesInvertidos))
  }
  def aplicarMovimiento(e:Estado,m:Movimiento) = m match {
  case Uno(n) if n < 0 =>
  val (vagonesTomados, nuevosTren2) = tomarDesdeIzquierda(e._2, -n)
  (vagonesTomados ::: e._1, nuevosTren2, e._3)

  case Uno(n) if n == 0 =>
  e // No hace nada si n igual a 0

  case Uno(n) if n > 0 =>
  val (vagonesTomados, nuevosTren1) = tomarDesdeDerecha(e._1, n)
  (nuevosTren1, vagonesTomados ::: e._2, e._3)

    // Movimiento Dos
  case Dos(n) if n < 0 =>
  val (vagonesTomados, nuevosTren3) = tomarDesdeIzquierda(e._3, -n)
  (vagonesTomados ::: e._1, e._2, nuevosTren3)

  case Dos(n) if n == 0 =>
  e // No hacer nada

  case Dos(n) if n > 0 =>
  val (vagonesTomados, nuevosTren1) = tomarDesdeDerecha(e._1, n)
  (nuevosTren1, e._2, vagonesTomados ::: e._3 )
  }
  def aplicarMovimientos(e:Estado,movimientos:Maniobra): List[Estado] = movimientos match {
    case Nil => List(e)
    case primerMovimiento::movimientosR =>
      val nuevoE = aplicarMovimiento(e,primerMovimiento)
      e::aplicarMovimientos(nuevoE,movimientosR)


  }
}
  
