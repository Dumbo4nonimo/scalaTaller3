package object ManiobrasTrenes {
  type Vagon = Any
  type Tren = List[Vagon]
  type Estado = (Tren, Tren, Tren)

  trait Movimiento
  case class Uno(n: Int) extends Movimiento
  case class Dos(n: Int) extends Movimiento

  type Maniobra = List[Movimiento]

  def aplicarMovimiento(e: Estado, m: Movimiento): Estado ={
    val (principal, train_one, train_two) = e
    val principal_length = principal.length
    val train_one_length = train_one.length
    val train_two_length = train_two.length

    m match {

      case Uno(n) => n match {
        case x if x > 0 => {  // From principal to train one
          if(!(x > principal_length)){
            val new_principal = remove_from_right(principal, x)
            val new_train_one = add_to_left(extract_from_right(principal, x), train_one)
            (new_principal, new_train_one, train_two)

          }else{
            (Nil, add_to_left(principal, train_one), train_two)
          }

        }
        case x if x < 0 => {
          if(!(x > principal_length)){
            val new_train_one = remove_from_left(train_one, x)
            val new_principal = add_to_right(extract_from_left(train_one, x), principal)
            (new_principal, new_train_one, train_two)

          }else{
            (add_to_right(extract_from_left(train_two, x), principal), remove_from_left(train_one, x*(-1)), train_two)
          }

        }
        case _ => {
          println("Intente de nuevo con diferentes valores")
          (principal, train_one, train_two)
          //Aqui se contempla la posibilidad de que haya mas de n vagones en principal
        }
      }

      case Dos(n) => n match {
        case x if x > 0 => {  // From principal to train one
          if (!(x > principal_length)) {
            val new_principal = remove_from_right(principal, x)
            val new_train_two = add_to_left(extract_from_right(principal, x), train_two)
            (new_principal, train_one, new_train_two)

          }else{
            (Nil, train_one, add_to_left(extract_from_right(principal, x), train_two))
          }

        }
        case x if x < 0 => {
          if (!(x > principal_length)) {
            val new_train_two = remove_from_left(train_two, x)
            val new_principal = add_to_right(extract_from_left(train_two, x), principal)

            (new_principal, train_one, new_train_two)
          }else{
            (add_to_right(extract_from_left(train_two, x), principal), train_one, remove_from_left(train_two, x))
          }

        }
        case _ => {
          println("Intente de nuevo con diferentes valores")
          (principal, train_one,train_two)
          //Aqui se contempla la posibilidad de que haya mas de n vagones en principal
        }
      }
    }
  }

  def add_to_left(list: Tren, target: Tren): Tren = {
    if (list.isEmpty) {
      target
    } else {
      list.head :: add_to_left(list.tail, target)
    }
  }

  def add_to_right(list: Tren, target: Tren): Tren = {
    add_to_left(target, list)
  }



  def remove_from_left(list: Tren, elements_to_remove: Int): Tren = {
    if(elements_to_remove == 0){
      list
    }else{
      remove_from_left(list.tail, elements_to_remove - 1)
    }
  }

  def remove_from_right(train: Tren, elements_to_remove: Int): Tren = {
    val n = train.length - elements_to_remove
    if (n == 0) {
      List()
    } else {
      train.head :: remove_from_right(train.tail, n - 1)
    }
  }

  def extract_from_left(train: Tren, elements_to_extract: Int): Tren = {
    if(elements_to_extract == 0){
      List()
    }else{
      train.head :: extract_from_left(train.tail, elements_to_extract-1)
    }
  }

  def extract_from_right(train: Tren, elements_to_extract: Int): Tren = {
    val n = train.length - elements_to_extract
    if(n == 0){
      train
    }else{
      extract_from_right(train.tail, n - 1)
    }
  }


  /*def aplicarMovimientos(e: Estado, movs: Maniobra): List[Estado] = {
  ...
  }

  def definirManiobra(t1: Tren, t2: Tren): Maniobra ={
  ...
  }*/

}