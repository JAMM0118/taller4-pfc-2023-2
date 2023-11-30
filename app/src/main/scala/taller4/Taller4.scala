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

    println(matrizAlAzar(2,3))
    println(matrizAlAzar(2,5))
    println(prodPunto(vectorAlAzar(3,3),vectorAlAzar(3,5)))
    println(transpuesta(matrizAlAzar(8,2)))
    println(multMatriz(matrizAlAzar(3,3),matrizAlAzar(3,5)))
    println(sumMatriz(matrizAlAzar(3,3),matrizAlAzar(3,5)))
    println(restaMatriz(matrizAlAzar(3,3),matrizAlAzar(3,5)))
    println(subMatriz(matrizAlAzar(4,10),0,1,2))

    // println(saludo())
    // println(
    //   withWarmer(new Warmer.Default) measure {
    //     (1 to 100000000).toArray
    //   } // ) 
  }

 }
