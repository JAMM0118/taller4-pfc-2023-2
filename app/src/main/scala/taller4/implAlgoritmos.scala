package taller4
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.util.Random
import common._

object implAlgoritmos {     
    type Matriz = Vector [ Vector [ Int ] ]

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
    val l1 = m1.length //Numero de filas
    val l2 = m2.head.length //Numero de columnas en el vector
    val m2t = transpuesta(m2)
    val m3: Matriz = Vector.tabulate(l1, l2)((i, j) => prodPunto(m1(i), m2t(j)))
    m3
  }

  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val filasM1 = m1.length
    val columnasM2 = m2(0).length

    def calcularElemento(fila: Vector[Int], columnaTranspuesta: Vector[Int]): Int = {
      prodPunto(fila, columnaTranspuesta)
    }

    def crearTarea(i: Int, j: Int): Int = {
      val tarea = task {
        val fila = m1(i)
        val columnaTranspuesta = transpuesta(m2)(j)
        calcularElemento(fila, columnaTranspuesta)
      }
      tarea.join
    }

    val tareas = for {
      i <- 0 until filasM1
      j <- 0 until columnasM2
    } yield crearTarea(i, j)

    val resultados = tareas.map(_.toInt).toVector
    resultados.grouped(m1.length).toVector
   }

  def multMatrizParV2(m1: Matriz, m2: Matriz): Matriz = {
    // calcular la multipliacion de forma paralela con otro enfoque, dividir en 4 partes y mandar 4 tareas 
    
    def bloquesTarea (bloqueM1: Matriz, transpuesta: Matriz): Matriz = {
       multMatriz(bloqueM1, transpuesta)
    }

    def partirMatrizPorMitad(matriz: Matriz): (Matriz, Matriz) = {
      val mitadFilas = matriz.length / 2
      val (primeraMitad, segundaMitad) = matriz.splitAt(mitadFilas)
      (primeraMitad, segundaMitad)
    }

    // val filasM1 = m1.length
    // val columnasM2 = m2(0).length
    val tamanoMatrices = m1.length
    val mitadTamano = tamanoMatrices / 2


    if (m1.length == 1){
      val l=m1.length
      Vector.tabulate(l, l)((i, j) => prodPunto(m1(i), transpuesta(m2)(j)))

    } else if (m1.length == 2) {
      // una tarea para la primera fila y otra para la segunda fila
      val resultado = parallel((prodPunto(m1(0), transpuesta(m2)(0)), prodPunto(m1(0), transpuesta(m2)(1)))
      ,(prodPunto(m1(1), transpuesta(m2)(0)), prodPunto(m1(1), transpuesta(m2)(1))))

       (Vector(Vector(resultado._1._1, resultado._1._2), Vector(resultado._2._1, resultado._2._2)))
    } else if (m1.length == 4){
      // el tamaño 4 sigue siendo un caso especial, 4 vectores que tienen que ser multiplicados, 4 tareas o 2??
      // modelo de 2 tareas 
      //val matriz2Transpuesta = transpuesta(m2)

      val matrizSeparada = partirMatrizPorMitad(m1)
      val par = parallel(bloquesTarea(matrizSeparada._1, m2), bloquesTarea(matrizSeparada._2, m2))
      (par._1 ++ par._2)
    
    } else {
      // dividir en 4 partes y mandar 4 tareas
      val matrizEn2 = partirMatrizPorMitad(m1)
      val matriz4Partes = (partirMatrizPorMitad(matrizEn2._1), partirMatrizPorMitad(matrizEn2._2))
      val par = parallel(bloquesTarea(matriz4Partes._1._1, m2), bloquesTarea(matriz4Partes._1._2, m2), 
      bloquesTarea(matriz4Partes._2._1, m2), bloquesTarea(matriz4Partes._2._2, m2))
      (par._1 ++ par._2 ++ par._3 ++ par._4)
    }
    
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
}