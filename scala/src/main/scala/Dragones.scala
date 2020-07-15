case class CaracteristicasDragon(peso: Int, velocidadBase: Int = 60, danio: Int)

class Dragon(val caracteristicas: CaracteristicasDragon, requisitosExtra: List[Function1[Vikingo , Boolean]] = List()) {

  def velocidad: Int = caracteristicas.velocidadBase - caracteristicas.peso

  def cumplisCondiciones(condiciones: CaracteristicasDragon): Boolean = {
    this.caracteristicas.velocidadBase >= condiciones.velocidadBase &&
      this.caracteristicas.danio >= condiciones.danio &&
      this.caracteristicas.peso >= condiciones.peso
  }
  
  def danio: Int = caracteristicas.danio

  def peso: Double = caracteristicas.peso

  def velocidadBase: Int = caracteristicas.velocidadBase

  def puedeMontar(vikingo: Vikingo): Boolean = (caracteristicas.peso / 5) > vikingo.peso && requisitosExtra.forall(requisito => requisito(vikingo))

  def cargaSoportada: Double = peso * 0.2

  def puedeCargar(cantidadACargar: Double): Boolean = cantidadACargar < cargaSoportada

}

case class FuriaNocturna(caracteristicasFuriaNocturna: CaracteristicasDragon,
                         requisitosFuria : List[Vikingo => Boolean] = List())
  extends Dragon(caracteristicas = caracteristicasFuriaNocturna, requisitosExtra = requisitosFuria){

  override def velocidad: Int = super.velocidad * 3

}

case class NadderMortifero(pesoNadder : Int , velocidaNadder : Int, requisitosNadder: List[Vikingo => Boolean] = List())
  extends Dragon(caracteristicas = CaracteristicasDragon(danio = 150, velocidadBase = velocidaNadder, peso = pesoNadder),
    requisitosExtra = RequisitosExtra.noSuperaMiDanio(150) :: requisitosNadder ) {

  private def vikingoNoSuperaMiDanio(vikingo: Vikingo): Boolean =
    this.danio >= vikingo.danio

}

case class Gronckle(caracteristicasGronkle: CaracteristicasDragon,
                    limitePesoVikingo: Int,
                    requisitosGronckle: List[Vikingo => Boolean] = List())
  extends Dragon(caracteristicasGronkle,
    requisitosExtra = RequisitosExtra.noSuperaPeso(limitePesoVikingo) :: requisitosGronckle) {

  override def velocidad: Int = (caracteristicasGronkle.velocidadBase / 2) - caracteristicas.peso

  override def danio: Int = caracteristicasGronkle.peso * 5

  private def vikingoNoSuperaPeso(vikingo: Vikingo): Boolean =
    vikingo.peso <= limitePesoVikingo

}

object RequisitosExtra {

  type Requisito = Vikingo => Boolean

  def noSuperaMiDanio(danioDragon : Int) : Requisito = _.danio < danioDragon

  def poseeItem(itemNecesario : Item) : Requisito = _.item == Some(itemNecesario)

  def cumpleBarbarosidad(barbarosidadMinima : Int) : Requisito = _.barbarosidad > barbarosidadMinima

  def noSuperaPeso(pesoMaximo: Int): Requisito = _.peso < pesoMaximo

}
