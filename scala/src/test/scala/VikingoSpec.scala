import org.scalatest.{FreeSpec, Matchers}


class VikingoSpec extends FreeSpec with Matchers {

  "Prueba de Vikingos" - {

    "Probamos los Vikingos ya definidos" - {

      "Hipo debería devolvernos una barbarosidad de 20" in {
        Hipo.barbarosidad shouldBe 20
      }

      "El daño de Patan deberia ser 120" in {
        Patan.danio shouldBe 120
      }

      "Posta de Pesca SIN peso minimo: Hipo vs Patan)" in {
        val postaPesca = Pesca()
        postaPesca.cumpleCondiciones(Hipo) shouldBe true
        postaPesca.cumpleCondiciones(Patan) shouldBe true
        Patan.esMejorQue(Hipo)(postaPesca) shouldBe false
      }

      "Posta de Pesca CON peso minimo: Hipo vs Patan)" in {
        val postaPesca = Pesca(Some(46))
        postaPesca.cumpleCondiciones(Hipo) shouldBe true
        postaPesca.cumpleCondiciones(Patan) shouldBe false
      }

      "Un vikingo con un item alimentacion deberia terminar la posta y reducir su hambre" in {

        val itemComestible = Comestible(20)
        val vikingoGloton = Vikingo(20, 30, 40, hambre = 50, Some(itemComestible))

        vikingoGloton.terminarPosta.hambre shouldBe 30

      }

      "Posta de Pesca CON peso minimo: Hipo vs Patan como jinete)" in {
        val postaPesca = Pesca(Some(15))
        val jinetePatan = Patan.montar(FuriaNocturna(CaracteristicasDragon(1000, 100, 100)))
        postaPesca.cumpleCondiciones(Hipo) shouldBe true
        postaPesca.cumpleCondiciones(jinetePatan) shouldBe true
        jinetePatan.esMejorQue(Hipo)(postaPesca) shouldBe true
      }

      "Combate que no Hipo puede participar por barbarosidad y Astrid si por tener item" in {
        val postaCombate50 = Combate(50)
        postaCombate50.cumpleCondiciones(Patapez) shouldBe false
        postaCombate50.cumpleCondiciones(Astrid) shouldBe true
      }

      "Posta de Combate: Astrid vs Patapez" in {
        val postaCombate19 = Combate(19)
        postaCombate19.cumpleCondiciones(Patapez) shouldBe true
        postaCombate19.cumpleCondiciones(Astrid) shouldBe true
        Patapez.esMejorQue(Astrid)(postaCombate19) shouldBe false
      }

      "Posta de Combate: Astrid vs Patapez con dragon" in {
        val postaCombate19 = Combate(19)
        val jinetePatapez = Patapez.montar(NadderMortifero(100, 100))
        postaCombate19.cumpleCondiciones(jinetePatapez) shouldBe true
        postaCombate19.cumpleCondiciones(Astrid) shouldBe true
        jinetePatapez.esMejorQue(Astrid)(postaCombate19) shouldBe true
      }

      val postaCarrera = Carrera(21)
      "Posta de Carrera: Astrid vs Patan" in {
        postaCarrera.cumpleCondiciones(Patan) shouldBe true
        postaCarrera.cumpleCondiciones(Astrid) shouldBe true
        Patan.esMejorQue(Astrid)(postaCarrera) shouldBe false
      }

      "Posta de Carrera: Astrid vs Patan como jinete" in {
        val jinetePatan = Patan.montar(FuriaNocturna(CaracteristicasDragon(100, 600, 1)))
        postaCarrera.cumpleCondiciones(jinetePatan) shouldBe true
        postaCarrera.cumpleCondiciones(Astrid) shouldBe true
        jinetePatan.esMejorQue(Astrid)(postaCarrera) shouldBe true
      }
    }
  }

  "Con quien le conviene hacerse jinete el vikingo" - {
    "Probamos dicho test" in {

      val unVikingo = Vikingo(peso = 10, _velocidad = 20, _barbarosidad = 30)
      val dragon1 = new Dragon(CaracteristicasDragon(peso = 50, danio = 50))
      val dragon2 = new Dragon(CaracteristicasDragon(peso = 10, danio = 30))
      val dragonesDisponibles: Set[Dragon] = Set(dragon1, dragon2)
      val unaPesca = Pesca()

      println(unVikingo.meConvieneHacermeJinete(dragonesDisponibles, unaPesca))

    }

    "Un vikingo no puede montar a un dragon por sus requisitos" in {

      val vikingoPesado = Vikingo(500, 80, 100)
      val vikingoDebil = Vikingo(20, 2, 2)
      val vikingoNormal = Vikingo(20, 10, 60)
      val dragonLivianoYFuerte = new Dragon(CaracteristicasDragon(peso = 600, danio = 500, velocidadBase = 800), List(RequisitosExtra.noSuperaPeso(50), RequisitosExtra.cumpleBarbarosidad(50)))

      val postaCarrera = Carrera(20)

      vikingoDebil.meConvieneHacermeJinete(Set(dragonLivianoYFuerte), postaCarrera) shouldBe None
      vikingoPesado.meConvieneHacermeJinete(Set(dragonLivianoYFuerte), postaCarrera) shouldBe None
      vikingoNormal.meConvieneHacermeJinete(Set(dragonLivianoYFuerte), postaCarrera) shouldBe Some(dragonLivianoYFuerte)

    }
  }

  "Pruebo el aumentaHambre de VikingoDeEquipo" - {
    "Pruebo eso" in {
      val vikingoDebil = Vikingo(20, 2, 2)
      val equipo1 = Equipo(List(vikingoDebil))
      val viki = VikingoDeEquipo(vikingoDebil, equipo1)

      println(viki.aumentaHambre(10))
    }

  }

}
