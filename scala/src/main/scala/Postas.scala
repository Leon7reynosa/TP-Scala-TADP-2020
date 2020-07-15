//Todas postas van a heredar de Posta y redefinir el metodo de si cumple condiciones
trait Posta {

  def cumpleCondiciones(participante: Participante): Boolean = participante.aptoParaParticipar && hambreDespuesDePosta(participante) < 100

  def hambreDespuesDePosta(participante: Participante): Double = cansarParticipantes(participante).hambre

  def cansarParticipantes(participante: Participante): Participante

  // Competir implica cansar a los participantes aptos para competir y retornarlos segun su puntuacion en la posta
  def competir(competidores: List[Participante]): List[Participante] = {
    competidores.map(compe => cansarParticipantes(compe))
      .sortWith((participanteAlto, participanteBajo) => participanteAlto.esMejorQue(participanteBajo)(this))
  }

}


case class Pesca(peso_minimo: Option[Int] = None) extends Posta {
  override def cumpleCondiciones(participante: Participante): Boolean = super.cumpleCondiciones(participante) && participante.puedeCargar(peso_minimo.getOrElse(0))

  override def cansarParticipantes(participante: Participante): Participante = participante.aumentaHambre(5)
}


case class Combate(barbarosidadMinima: Int) extends Posta {

  override def cumpleCondiciones(participante: Participante): Boolean =
    super.cumpleCondiciones(participante) && (participante.barbarosidad >= barbarosidadMinima || poseeArma(participante))

  def poseeArma(participante: Participante): Boolean = participante.item match {
    case Some(Arma(_)) => true
    case _ => false
  }

  override def cansarParticipantes(participante: Participante): Participante = participante.aumentaHambre(10)
}


case class Carrera(km: Int, monturaRequerida: Boolean = false) extends Posta {

  override def cumpleCondiciones(participante: Participante): Boolean = participante.dragon match {
    case Some(_) => super.cumpleCondiciones(participante)
    case _ => super.cumpleCondiciones(participante) && !monturaRequerida
  }

  override def cansarParticipantes(participante: Participante): Participante = participante.aumentaHambre(km)

}
