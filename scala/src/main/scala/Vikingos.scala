
trait Participante {
  def velocidad: Int

  def cuantoCarga: Double

  def danio: Int

  def dragon: Option[Dragon]

  def hambre: Double

  def barbarosidad: Int

  def alimentate: Participante = item match {
    case Some(Comestible(porcentajeAlimentacion)) => reducirHambre(porcentajeAlimentacion)
    case _ => this
  }

  def reducirHambre(d: Int): Participante

  def desmontar: Participante

  def terminarPosta: Participante = desmontar.alimentate

  def montar(dragon: Dragon): Participante

  def item: Option[Item]

  def esMejorQue(participanteEnemigo: Participante)(unaPosta: Posta): Boolean = {
    realizaPosta(unaPosta) > participanteEnemigo.realizaPosta(unaPosta)

  }

  def meConvieneHacermeJinete(dragonesDisponibles: Set[Dragon], posta: Posta): Option[Dragon] = {
    val opciones = dragonesDisponibles.map(this.montar) + this
    opciones
      .filter(opcion => posta.cumpleCondiciones(opcion))
      .toList
      .sortWith(_.esMejorQue(_)(posta))
      // TODO cuidado, head falla con exception si la lista está vacía (es decir, si ninguna opción "cumpleCondición"). Lo paso a headOption
      .headOption
      .flatMap(_.dragon)
  }

  def realizaPosta(unaPosta: Posta): Double = unaPosta match {
    case Pesca(_) => cuantoCarga
    case Combate(_) => danio.toDouble
    case Carrera(_, _) => velocidad.toDouble
  }

  def aumentaHambre(procentaje: Int): Participante //Aca en el jinete no usa el porcentaje pasado, lo dejamos asi?


  def aptoParaParticipar: Boolean = hambre < 100

  def puedeCargar(pesoACargar: Int): Boolean = pesoACargar <= cuantoCarga
}


case class VikingoDeEquipo(vikingo: Vikingo, equipo: Equipo) extends Participante {

  override def velocidad: Int = vikingo.velocidad

  override def dragon: Option[Dragon] = vikingo.dragon

  override def desmontar: VikingoDeEquipo = copy(vikingo.desmontar, equipo)

  override def cuantoCarga: Double = vikingo.cuantoCarga

  override def danio: Int = vikingo.danio

  override def hambre: Double = vikingo.hambre

  override def barbarosidad: Int = vikingo.barbarosidad

  override def item: Option[Item] = vikingo.item

  override def aumentaHambre(porcentaje: Int): Participante = copy(vikingo = hambreParaMiVikingo(porcentaje))

  protected def hambreParaMiVikingo(porcentaje: Int): Vikingo = vikingo.aumentaHambre(porcentaje) match {
    case v: Vikingo => v
    case _ => vikingo
  }

  override def reducirHambre(porcentaje: Int): Participante = copy(vikingo = alimentaMiVikingo(porcentaje))

  protected def alimentaMiVikingo(porcentaje: Int): Vikingo = vikingo.reducirHambre(porcentaje) match {
    case v: Vikingo => v
    // TODO creo que no tiene sentido retornar el vikingo en el estado anterior de "reducirHambre" porque es inmutable y nada cambió
    case _ => vikingo
  }

  override def montar(dragon: Dragon): VikingoDeEquipo = copy(vikingo = vikingo.montar(dragon))

}

case class Equipo(integrantes: List[Vikingo]) {
  def miembros: List[VikingoDeEquipo] = integrantes.map(integrante => VikingoDeEquipo(integrante, this))
}


case class Vikingo(peso: Int,
                   _velocidad: Int,
                   _barbarosidad: Int,
                   hambre: Double = 0.0,
                   item: Option[Item] = None,
                   dragon: Option[Dragon] = None) extends Participante {

  val tenesItem: Boolean = item.isDefined

  val cuantoCarga: Double = dragon match {
    case Some(dragon) => (dragon.cargaSoportada - peso).abs
    case None => (peso / 2.0) + (barbarosidad * 2.0)
  }

  def barbarosidad: Int = _barbarosidad

  // TODO falta la validación para los "Jinetes": Un jinete puede cargar tantos kilos de pescado como diferencia haya entre el peso del vikingo y lo que puede cargar el dragón.
  //  Nota: revisen de nuevo si separar al Vikingo y al Dragon en una case class Jinete(vikingo, dragon) los ayuda o no (siendo Jinete y Vikingo polimórficos para lo que necesita la Posta: velocidad, daño, cuanto puede cargar, etc)

  def velocidad: Int = dragon match {
    case Some(d) => d.velocidad - peso
    case None => _velocidad
  }

  def danioDragon: Int = dragon match {
    case Some(d) => d.danio
    case None => 0
  }

  def danio: Int = item match {
    case Some(Arma(danioItem)) => barbarosidad + danioItem + danioDragon
    case _ => barbarosidad + danioDragon
  }

  override def aumentaHambre(porcentaje: Int): Vikingo = dragon match {
    case Some(_) => copy(hambre = hambre + 5.00)
    case None => copy(hambre = hambre + porcentaje)
  }

  override def reducirHambre(d: Int): Participante = copy(hambre = hambre - d)

  def montar(dragonAMontar: Dragon): Vikingo = {
    if (dragonAMontar.puedeMontar(this) && dragon.isEmpty) {
      copy(dragon = Some(dragonAMontar))
    } else {
      this
    }
  }

  def desmontar: Vikingo = copy(dragon = None)

}

object Hipo extends Vikingo(peso = 20, _velocidad = 30, _barbarosidad = 20, item = Some(new SistemaDeVuelo))

object Astrid extends Vikingo(peso = 10, _velocidad = 50, _barbarosidad = 20, item = Some(Hacha))

object Patan extends Vikingo(peso = 10, _velocidad = 30, _barbarosidad = 20, item = Some(Maza))

object Patapez extends Vikingo(peso = 10, _velocidad = 30, _barbarosidad = 20) {
  override def aptoParaParticipar: Boolean = hambre < 50

  override def aumentaHambre(porcentaje: Int): Vikingo =
    super.aumentaHambre(porcentaje * 2)
}