import org.scalatest.{FreeSpec, Matchers}


class TorneoSpec extends FreeSpec with Matchers {
  "Tests del torneo" - {
    "Competicion entre vikingos" in {

      val vikingo1 = Vikingo(10, 20, 15) //cuantoCarga = 10/2 + 20*2 = 45
      val vikingo2 = Vikingo(15, 15, 10) //cuantoCarga = 15/2 + 15*2 = 37.5
      val vikingo3 = Vikingo(10,10,10)

      val postaPesca = Pesca(Some(12))
      val postaCarrera = Carrera(10)
      val postaCarrera1 = Carrera(100)
      val postaCarrera2 = Carrera(20)
      val postaCarrera3 = Carrera(100)

      val participantes: List[Vikingo] = List(vikingo1, vikingo2,vikingo3)
      val postas: List[Posta] = List(postaPesca, postaCarrera, postaCarrera1, postaCarrera2)

      val unTorneo = new Torneo(participantes, postas, Set())

      unTorneo.finalizarTorneo shouldBe Some(vikingo1.aumentaHambre(20))
    }

    "Competicion entre vikingos - 2 vikingos basicos, con dragones a eleccion" in {

      val vikingo1 = Vikingo(10, 20, 15) //cuantoCarga = 10/2 + 20*2 = 45
      val vikingo2 = Vikingo(15, 15, 10) //cuantoCarga = 15/2 + 15*2 = 37.5

      val postaPesca = Pesca(Some(12))
      val postaCarrera = Carrera(10)

      val dragon1 = new Dragon(CaracteristicasDragon(peso = 10, danio = 50))
      val dragon2 = new Dragon(CaracteristicasDragon(peso = 5, velocidadBase = 30, danio = 10))
      val dragon3 = new Dragon(CaracteristicasDragon(peso = 1, velocidadBase = 1, danio = 1))

      val participantes: List[Vikingo] = List(vikingo1, vikingo2)
      val postas: List[Posta] = List(postaPesca, postaCarrera)
      val dragones: Set[Dragon] = Set(dragon1, dragon2, dragon3)

      val unTorneo = new Torneo(participantes, postas, dragones)

      unTorneo.finalizarTorneo shouldBe Some(vikingo1.aumentaHambre(5))
    }
  }

  "Tests del torneo de Equipos" - {
    "Probamos el Determinar Ganador" in {

      val vikingo1 = Vikingo(10, 20, 15) //cuantoCarga = 10/2 + 20*2 = 45
      val vikingo2 = Vikingo(15, 15, 10) //cuantoCarga = 15/2 + 15*2 = 37.5
      val vikingo3 = Vikingo(1, 10, 1)
      val vikingo4 = Vikingo(4, 2, 6)
      val vikingo5 = Vikingo(4, 3, 1)

      val postaCarrera = Carrera(10)

      val equipo1 = Equipo(List(vikingo1, vikingo2))
      val equipo2 = Equipo(List(vikingo3, vikingo4, vikingo5))

      val postas: List[Posta] = List(postaCarrera)

      val equiposParticipantes = List(equipo1, equipo2)

      val unTorneo = TorneoEquipos(equiposParticipantes, postas, Set())

      val participatesDeEquipos = equiposParticipantes.flatMap(_.miembros)

      unTorneo.determinarGanador(participatesDeEquipos) shouldBe Some(equipo2)
    }
    "Probamos el finalizar Torneo con una sola posta" in {

      val vikingo1 = Vikingo(10, 20, 15) //cuantoCarga = 10/2 + 20*2 = 45
      val vikingo2 = Vikingo(15, 15, 10) //cuantoCarga = 15/2 + 15*2 = 37.5
      val vikingo3 = Vikingo(1, 10, 1)
      val vikingo4 = Vikingo(4, 2, 6)
      val vikingo5 = Vikingo(4, 3, 1)

      val postaCarrera = Carrera(20)

      val equipo1 = Equipo(List(vikingo1, vikingo2))
      val equipo2 = Equipo(List(vikingo3, vikingo4, vikingo5))

      val postas: List[Posta] = List(postaCarrera)

      val equiposParticipantes = List(equipo2, equipo1)

      val unTorneo = TorneoEquipos(equiposParticipantes, postas, Set())

      unTorneo.finalizarTorneo shouldBe Some(equipo1)
    }

    "Probamos el Desarrollar Torneo con una sola posta y solo puede competir un equipo, por eso gana ese equipo" in {

      val vikingo1 = Vikingo(10, 20, 15, hambre = 100) //cuantoCarga = 10/2 + 20*2 = 45
      val vikingo2 = Vikingo(15, 15, 10, hambre = 100) //cuantoCarga = 15/2 + 15*2 = 37.5
      val vikingo3 = Vikingo(1, 10, 1)
      val vikingo4 = Vikingo(4, 2, 6)
      val vikingo5 = Vikingo(4, 3, 1)

      val postaCarrera = Carrera(20)

      val equipo1 = Equipo(List(vikingo1, vikingo2))
      val equipo2 = Equipo(List(vikingo3, vikingo4, vikingo5))

      val postas: List[Posta] = List(postaCarrera)

      val equiposParticipantes = List(equipo2, equipo1)

      val unTorneo = TorneoEquipos(equiposParticipantes, postas, Set())

      unTorneo.finalizarTorneo shouldBe Some(equipo2)

    }
    "Probamos el Desarrollar Torneo Que no gane nadie" in {

      val vikingo1 = Vikingo(10, 20, 15, hambre = 100) //cuantoCarga = 10/2 + 20*2 = 45
      val vikingo2 = Vikingo(15, 15, 10, hambre = 100) //cuantoCarga = 15/2 + 15*2 = 37.5
      val vikingo3 = Vikingo(1, 10, 1, hambre = 100)
      val vikingo4 = Vikingo(4, 2, 6, hambre = 100)
      val vikingo5 = Vikingo(4, 3, 1, hambre = 100)

      val postaCarrera = Carrera(10)

      val equipo1 = Equipo(List(vikingo1, vikingo2))
      val equipo2 = Equipo(List(vikingo3, vikingo4, vikingo5))

      val postas: List[Posta] = List(postaCarrera)

      val equiposParticipantes = List(equipo2, equipo1)

      val unTorneo = TorneoEquipos(equiposParticipantes, postas, Set())

      unTorneo.finalizarTorneo shouldBe None
    }

    "Probamos la funcion determinarGanador" in {

      val vikingo1 = Vikingo(10, 20, 15, hambre = 100) //cuantoCarga = 10/2 + 20*2 = 45
      val vikingo2 = Vikingo(15, 15, 10, hambre = 100) //cuantoCarga = 15/2 + 15*2 = 37.5
      val vikingo3 = Vikingo(1, 10, 1, hambre = 100)
      val vikingo4 = Vikingo(4, 2, 6, hambre = 100)
      val vikingo5 = Vikingo(4, 3, 1, hambre = 100)

      //val postaCarrera = Carrera(10)

      val equipo1 = Equipo(List(vikingo1, vikingo2))
      val equipo2 = Equipo(List(vikingo3, vikingo4, vikingo5))

      //val postas: List[Posta] = List(postaCarrera)
      val postas: List[Posta] = List()

      val equiposParticipantes = List(equipo2, equipo1)

      val unTorneo = TorneoEquipos(equiposParticipantes, postas, Set())

      unTorneo.finalizarTorneo shouldBe Some(equipo2)
    }
  }
}
