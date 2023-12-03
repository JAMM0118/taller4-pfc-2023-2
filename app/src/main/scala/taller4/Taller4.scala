/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.util.Random
import implAlgoritmos._
import common._

object Taller4{

 type Matriz = Vector [ Vector [ Int ] ]

  def saludo() = "Taller 4 2023-II"
 


  // PRUEBAS DE RENDIMIENTOS
  def main(args: Array[String]): Unit = {

    val tamanomatriz = 8

    val promedioParv2 = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val m1 = matrizAlAzar(tamanomatriz, 2)
      val m2 = matrizAlAzar(tamanomatriz, 2)
      val time = withWarmer(new Warmer.Default) measure {
        multMatrizParV2(m1, m2)
      }
      promedioParv2(i) = time.value
      println(" Repetición " + i + " tiempo: " + time)
    }

    val promedioParv1 = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val m1 = matrizAlAzar(tamanomatriz, 2)
      val m2 = matrizAlAzar(tamanomatriz, 2)
      val time = withWarmer(new Warmer.Default) measure {
        multMatrizPar(m1, m2)
      }
      promedioParv1(i) = time.value
      println(" Repetición " + i + " tiempo: " + time)
    }

    val promedioSeq = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val m1 = matrizAlAzar(tamanomatriz, 2)
      val m2 = matrizAlAzar(tamanomatriz, 2)
      val time = withWarmer(new Warmer.Default) measure {
        multMatriz(m1, m2)
      }
      promedioSeq(i) = time.value
      println(" Repetición " + i + " tiempo: " + time)
    }


    val promedioSeqRec = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val m1 = matrizAlAzar(tamanomatriz, 2)
      val m2 = matrizAlAzar(tamanomatriz, 2)
      val time = withWarmer(new Warmer.Default) measure {
        multMatrizRec(m1, m2)
      }
      promedioSeqRec(i) = time.value
      println(" Repetición " + i + " tiempo: " + time)
    }

    // val promedioRecPar = (1 to 100).map(_ => 0.0).toArray
    // for (i <- 0 until 100) {
    //   val m1 = matrizAlAzar(tamanomatriz, 2)
    //   val m2 = matrizAlAzar(tamanomatriz, 2)
    //   val time = withWarmer(new Warmer.Default) measure {
    //     multMatrizRecPar(m1, m2)
    //   }
    //   promedioRecPar(i) = time.value
    //   println(" Repetición " + i + " tiempo: " + time)
    // }


      val promedioStrassen = (1 to 100).map(_ => 0.0).toArray
      for (i <- 0 until 100) {
        val m1 = matrizAlAzar(tamanomatriz, 2)
        val m2 = matrizAlAzar(tamanomatriz, 2)
        val time = withWarmer(new Warmer.Default) measure {
          strassen(m1, m2)
        }
        promedioStrassen(i) = time.value
        println(" Repetición " + i + " tiempo: " + time)
      }

      val promedioStrassenPar = (1 to 100).map(_ => 0.0).toArray
      for (i <- 0 until 100) {
        val m1 = matrizAlAzar(tamanomatriz, 2)
        val m2 = matrizAlAzar(tamanomatriz, 2)
        val time = withWarmer(new Warmer.Default) measure {
          strassenParallel(m1, m2)
        }
        promedioStrassenPar(i) = time.value
        println(" Repetición " + i + " tiempo: " + time)
      }

    println("El promedio secuencial es: " + promedioSeq.sum / 100 + " ms")
    println("El promedio paralelo 1 es: " + promedioParv1.sum / 100 + " ms")
    println("El promedio paralelo 2 es: " + promedioParv2.sum / 100 + " ms")
    println("El promedio recursivo es: " + promedioSeqRec.sum / 100 + " ms")
    // println("El promedio recursivo paralelo es: " + promedioRecPar.sum / 100 + " ms")
    println("El promedio Strassen es: " + promedioStrassen.sum / 100 + " ms")
    println("El promedio Strassen paralelo es: " + promedioStrassenPar.sum / 100 + " ms")




    //println("La aceleracion es de: " + (promedioParv1.sum) / (promedioParv2.sum))




    // val m1 = Vector(Vector(3, 2, 0), Vector(6, 7, 8), Vector(1, 9, 2))

    // val m2 = Vector(Vector(3, 2, 9), Vector(5, 2, 4), Vector(5, 6, 5))
    // println("Resultado secuencial:")
    // println(multMatriz(m1, m2))

    // println("resultado recursivo:")
    // println(multMatrizRec(m1, m2))

    // println("resultado recursivo paralelo:")
    // println(multMatrizRecPar(m1, m2))
  }

 }
