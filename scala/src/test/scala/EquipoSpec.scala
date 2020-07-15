import org.scalatest.{FreeSpec, Matchers}

class EquipoSpec extends FreeSpec with Matchers {

  "Tests de algunos Equipos" - {

    "Equipos de Vikingos" - {
      "Agrego una lista de vikingos a un equipo" in {

        val vikingo1 = Vikingo(4, 30, 10)
        val vikingo2 = Vikingo(4, 30, 10)
        val vikingo3 = Vikingo(4, 30, 10)

        val equipoDeVikingos = Equipo(List(vikingo1, vikingo2, vikingo3))

        equipoDeVikingos.miembros.contains(VikingoDeEquipo(vikingo1, equipoDeVikingos)) shouldBe true

      }
    }
  }

}
