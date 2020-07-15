
sealed trait Item

case class Arma(danio: Int) extends Item

object Hacha extends Arma(30)

object Maza extends Arma(100)

case class Comestible(porcentajeAlimentacion: Int) extends Item

case class SistemaDeVuelo() extends Item

// TODO si hacen algo así luego la responsabilidad es en el pattern matching y no es más polimórfico