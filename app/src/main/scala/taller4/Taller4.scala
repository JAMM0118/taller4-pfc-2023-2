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

  def matrizAlAzar(Long: Int, vals: Int): Matriz = {
    val v = Vector.fill(Long, Long){Random.nextInt(vals)}
    v
  }

  def vectorAlAzar(Long: Int, vals: Int): Vector[Int] = {
    val v = Vector.fill(Long){Random.nextInt(vals)}
    v
  }
  
  // Funciones auxiliares para la operación de matrices
  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map( { case (i, j) => (i * j) }).sum
  }

  def transpuesta(m: Matriz): Matriz = {
    val l=m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    //calculo usando prodPunto y la transpuesta
    val l=m1.length
    Vector.tabulate(l, l)((i, j) => prodPunto(m1(i), transpuesta(m2)(j)))
  } 
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l=m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j)+m2(i)(j))
  }

  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val l=m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j)-m2(i)(j))
  }

  def subMatriz (m:Matriz , i: Int , j : Int , l : Int ) : Matriz ={
    val sub = Vector.tabulate(l/l, l/l)((is, js) => m(i)(j))
    sub
  }
 
  def main(args: Array[String]): Unit = {

    // println(matrizAlAzar(2,3))
    // println(matrizAlAzar(2,5))
    // println(prodPunto(vectorAlAzar(3,3),vectorAlAzar(3,5)))
    // println(transpuesta(matrizAlAzar(8,2)))
    // println(multMatriz(matrizAlAzar(3,3),matrizAlAzar(3,5)))
    // println(sumMatriz(matrizAlAzar(3,3),matrizAlAzar(3,5)))
    // println(restaMatriz(matrizAlAzar(3,3),matrizAlAzar(3,5)))
    // println(subMatriz(matrizAlAzar(4,10),0,1,2))


    // val m1 = Vector(Vector(3, 2, 0), Vector(6, 7, 8), Vector(1, 9, 2))
    // val m2 = Vector(Vector(3, 2, 9), Vector(5, 2, 4), Vector(5, 6, 5))

    // val resultado = multMatrizPar(m1, m2)
    // println(resultado)
    println("Multiplicación de matrices secuencial") 
    val promedioSeq = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {

      val m1 = matrizAlAzar(16, 2)
      val m2 = matrizAlAzar(16, 2)
      val time = withWarmer(new Warmer.Default) measure {
        multMatriz(m1, m2)
      }
      promedioSeq(i) = time.value
      println(" Repetición " + i + " tiempo: " + time)

    }

    val promedioPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val m1 = matrizAlAzar(16, 2)
      val m2 = matrizAlAzar(16, 2)
      val time = withWarmer(new Warmer.Default) measure {
        multMatrizPar(m1, m2)
      }
      promedioPar(i) = time.value
      println(" Repetición " + i + " tiempo: " + time)
    }


    println("El promedio secuencial es: " + promedioSeq.sum / 100 + " ms")
    println("El promedio paralelo es: " + promedioPar.sum / 100 + " ms")
    println("La aceleracion es de: " + (promedioSeq.sum) / (promedioPar.sum))
  }

 }
