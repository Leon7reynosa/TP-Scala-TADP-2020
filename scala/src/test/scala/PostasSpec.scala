import org.scalatest.{FreeSpec, Matchers}

class PostasSpec extends FreeSpec with Matchers {

  "Tests de algunas Postas" - {

    "Competicion entre vikingos" - {
      "vikingoMuchoPeso es mejor que VikingoPocoPeso" in {

        val nader = NadderMortifero(240, 250)
        val vikingo = Vikingo(4, 30, 10)
        val vikingoJinete = vikingo.montar(nader)

        vikingoJinete.dragon shouldBe Some(nader)
      }

      "Una posta de Carrera con montura requerida solo pueden participar jinetes" in {

        val vikingo1 = Vikingo(20,20,20)
        val vikingo2 = Vikingo(30,10,20)

        val furia = FuriaNocturna(CaracteristicasDragon(500, 30,50))

        val vikingo1Jinete = vikingo1.montar(furia)

        val carrera = Carrera(60, true)

        carrera.cumpleCondiciones(vikingo1Jinete) shouldBe true
        carrera.cumpleCondiciones(vikingo2) shouldBe false

      }

      "Patapez no puede competir en una carrera de 60 km " in {

        val carrera = Carrera(60)
        carrera.cumpleCondiciones(Patapez) shouldBe false

      }

    }
  }

}
