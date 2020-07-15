trait TorneoGenerico[A] {

  def finalizarTorneo: Option[A] // Punto de entrada al torneo

  def determinarGanador(participantes: List[Participante]): Option[A]

  def esFinalizableTorneo(participantes: List[Participante]): Boolean

  def desarrollarTorneo(postas: List[Posta], valientes: List[Participante], dragones: Set[Dragon]): Option[A] = {

    if(hayPrecipitaciones){
      postas match{
        case List() => determinarGanador(valientes)
        case primer :: List() => determinarGanador(valientes)
        case primer :: segundo :: List() => determinarGanador(valientes)
        case primer :: segundo :: tercer :: List() => determinarGanador(valientes)
        case primero :: segundo :: tercero :: cola => desarrollarTorneo(cola, valientes , dragones)
        case _ => determinarGanador(valientes) //No se si hace falta revisar cada caso o si con este solo caso abarca los que tengan lista vacia, por si las dudas pongo todo
      }
    }else{
      (valientes, postas) match {
        case (_, List()) => determinarGanador(valientes) // CASO en el que ya no quedan mas postas
        case (concursantes, postaActual :: postaCola) =>
          val participantesFiltrados = concursantes.filter(postaActual.cumpleCondiciones) // Nos fijamos si los participantes pueden competir
        val participantesGanadores = avanzarPosta(postaActual, participantesFiltrados, dragones) // Hacemos que compitan nuestros participantes

          participantesGanadores match {
            case List() => None // CASO el torneo finaliza sin ganador porque ningun participante pudo pasar la posta
            case participantes if esFinalizableTorneo(participantes) => determinarGanador(participantes) // CASO en que un torneo ya puede finalizar por sus condiciones especificas
            case _ => desarrollarTorneo(postaCola, participantesGanadores, dragones) // Caso recursivo
          }
      }
    }


  }

  def hayPrecipitaciones  = math.random < 0.2

  def avanzarPosta(postaActual: Posta, valientes: List[Participante], dragones: Set[Dragon]): List[Participante] = {
    val competidoresPreparados = prepararCompetidores(postaActual, dragones, valientes) // Preparar a los competidores implica montarlos al dragon de ser necesario
    val competidores = postaActual.competir(competidoresPreparados) // Competir implica cansar a los participantes y ordenarlos segun su puntaje en la posta
    val competidoresDesmontados = competidores.map(vikin => vikin.terminarPosta) // Desmontamos a los vikingos que esten montados a algun dragon
    quienesPasan(competidoresDesmontados)
  }


  def quienesPasan(competidoresAfterPosta: List[Participante]): List[Participante] = {
    mitadDeLalista(competidoresAfterPosta)
  }

  //Segun el tipo de participantes, definimos las funciones que se van a ejecutar en el torneo que corresponde
  protected def prepararCompetidores(postaActual: Posta, dragonesDisponibles: Set[Dragon], participantes: List[Participante]): List[Participante] = {
    participantes match {
      case List() => List()
      case head :: cola => head.meConvieneHacermeJinete(dragonesDisponibles, postaActual) match {
        case Some(d) => montar(head, d) :: prepararCompetidores(postaActual, dragonesDisponibles - d, cola)
        case None => head :: prepararCompetidores(postaActual, dragonesDisponibles, cola) // Caso recursivo
      }
    }
  }

  def montar(participante: Participante, dragon: Dragon): Participante = participante.montar(dragon)

  protected def mitadDeLalista(listaVikingo: List[Participante]): List[Participante] = {
    listaVikingo.take(math.ceil(listaVikingo.size / 2.0).toInt)
  }

}

//este seria el torneo estandar
class Torneo(val participantes: List[Vikingo], val postas: List[Posta], val dragones: Set[Dragon]) extends TorneoGenerico[Vikingo] {

  def realizarTorneo: Option[Participante] = desarrollarTorneo(postas, participantes, dragones) // Punto de entrada

  override def esFinalizableTorneo(participantes: List[Participante]): Boolean = participantes.size == 1 // El torneo finaliza en caso de quedar solo un sobreviviente

  override def finalizarTorneo: Option[Vikingo] = desarrollarTorneo(postas, participantes, dragones)

  // En un torneo estandar gana el participante que mas puntos haya sacado en la ultima posta

  def determinarGanador(finalistas: List[Participante]): Option[Vikingo] =
    finalistas match {
      case List() => None
      case head :: _ => head match{
        case v : Vikingo => Some(v)
        case _ => None
      }

    }

}

case class TorneoEliminatorio(participantesElim: List[Vikingo],
                              postasElim: List[Posta],
                              dragonesElim: Set[Dragon],
                              cuantosSeVan: Int)
  extends Torneo(participantes = participantesElim, postas = postasElim, dragones = dragonesElim) {

  override def quienesPasan(listaVikingo: List[Participante]): List[Participante] =
    listaVikingo.take(listaVikingo.size - cuantosSeVan) // En un torneo eliminaria se indica cuantos se eliminan en cada ronda

  override def determinarGanador(finalistas: List[Participante]): Option[Vikingo] =
    // TODO creo que esto les quedó de más, ya el Torneo saca bien al ganador (y es el primero, no el ultimo). Creo que esto va en el inverso
    finalistas.last match {
      case v: Vikingo => Some(v)
      case _ => None
    }

}

case class TorneoInverso(participantesInv: List[Vikingo],
                         postasInv: List[Posta],
                         dragonesInv: Set[Dragon])
  extends Torneo(participantesInv, postasInv, dragonesInv) {

  override def quienesPasan(listaVikingo: List[Participante]): List[Participante] =
    mitadDeLalista(listaVikingo.reverse)

}

case class TorneoVeto(participantesVeto: List[Vikingo],
                      postasVeto: List[Posta],
                      dragonesVeto: Set[Dragon],
                      condiciones: CaracteristicasDragon)
  extends Torneo(participantesVeto, postasVeto, dragonesVeto) {

  override def prepararCompetidores(postaActual: Posta, dragonesDisponibles: Set[Dragon], participantes: List[Participante]): List[Participante] = {
    val dragonesFiltrados = dragonesDisponibles.filter(d => d.cumplisCondiciones(condiciones))
    super.prepararCompetidores(postaActual, dragonesFiltrados, participantes)
  }


}

case class TorneoHandicap(partipantesHC: List[Vikingo],
                          postasHC: List[Posta],
                          dragonesHC: Set[Dragon]) extends Torneo(partipantesHC, postasHC, dragonesHC) {

  override def prepararCompetidores(postaActual: Posta, dragonesDisponibles: Set[Dragon], participantes: List[Participante]): List[Participante] =
    super.prepararCompetidores(postaActual, dragones, participantes.reverse)


}

case class TorneoEquipos(equipos: List[Equipo], postas: List[Posta], dragones: Set[Dragon])
  extends TorneoGenerico[Equipo] {

  override def finalizarTorneo: Option[Equipo] = desarrollarTorneo(postas, equipos.flatMap(equipo => equipo.miembros), dragones) // Punto de entrada al torneo de equipos

  // El torneo finaliza si todos los vikingos de equipo pertenecen al mismo equipo
  override def esFinalizableTorneo(participantes: List[Participante]): Boolean = 
    // TODO si este método recibiera un "VikingoDeEquipo" no necesitarían castear o matchear (podría hacerse con un segundo type parameter:
    //  TorneoGenerico[TipoDeGanador, TipoDeParticipanteIndividual]
    //  Torneo extends TorneoGenerico[Vikingo, Vikingo]
    //  TorneoEquipos extends TorneoGenerico[Equipo, VikingoDeEquipo]
    agruparVikingosDeEquipos(participantes.asInstanceOf[List[VikingoDeEquipo]]).size == 1

  /*  
  participantes match {
    // TODO esto match no vale por type erasure de la JVM (es una limitación tecnológica de la JVM, los type parameters no se ven en runtime, salvo casos particulares*)
    case ve: List[VikingoDeEquipo] => ve.size == 1 || ve.forall(_.equipo == ve.head.equipo)
    case _ => false
  }
  */

  // Gana el equipo con mas participantes. Caso contrario se define "al azar"
  override def determinarGanador(vikingos: List[Participante]): Option[Equipo] = {
    val groupByEquipos = vikingos
      .groupBy {
        // TODO esto es similar al que hicieron ustedes pero en vez de hacer pattern matching por el type parameter de la collection, lo hago por el tipo de cada elemento
        //   Aca estoy haciendo algo medio sucio, estoy haciendo un matching "parcial". Si viene un elemento que NO es VikingoDeEquipo, esto explota por exception
        //   (no debería pasar pero es una mala solución, es mejor tipar generico y no tener que castear o hacer este tipo de matchings)
        case v: VikingoDeEquipo => v.equipo
      }
    val ordenPorCantidadDeIntegrantes = groupByEquipos.toSeq.sortWith(_._2.length > _._2.length) //Asumo que si hay repetidos, deberia agrupar y ordenar al azar
    ordenPorCantidadDeIntegrantes.headOption match {
      // TODO guarda porque el "v1" es el equipo en el estado original (el TP no lo especifica así que lo dejamos como que esta OK así)
      case Some(v) => Some(v._1)
      case _ => None
    }
  }

  /*
    vikingos match {
    // TODO no se puede por el erasure:
    // warning del compilador: Warning:(155, 18) non-variable type argument VikingoDeEquipo in type pattern List[VikingoDeEquipo] (the underlying of List[VikingoDeEquipo]) is unchecked since it is eliminated by erasure
    case listVE: List[VikingoDeEquipo] =>
      val groupByEquipos = listVE.groupBy(vikingo_deEquipo => vikingo_deEquipo.equipo)
      val ordenPorCantidadDeIntegrantes = groupByEquipos.toSeq.sortWith(_._2.length > _._2.length) //Asumo que si hay repetidos, deberia agrupar y ordenar al azar
      ordenPorCantidadDeIntegrantes.headOption match {
        case Some(v) => Some(v._1)
        case _ => None
      }
    case _ => None
  }
  */

  // Funcion de pablo para volver a agrupar vikingos de equipos
  def agruparVikingosDeEquipos(v: List[VikingoDeEquipo]): Seq[Equipo] =
    v.groupBy(_.equipo)
      .map { case (_, nuevos) => Equipo(nuevos.map(_.vikingo)) }
      .toList

}