package taller4
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.util.Random
import common._
import scala.collection.parallel.CollectionConverters._

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

  def prodPuntoParD(v1: Vector[Int], v2: Vector[Int]): Int = {
    val resultadoParalelo = v1.par.zip(v2.par).map { case (x, y) => x * y }.sum

    resultadoParalelo
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


  def subMatriz2(m: Matriz, filaFrom: Int, filaTo: Int, columnaFrom: Int,columnaTo: Int): Matriz = {
    val sub = m.slice(filaFrom, filaTo).map(_.slice(columnaFrom, columnaTo))
    sub
  }
  
  

  def multMatrizRec (m1:Matriz , m2: Matriz ) : Matriz = {

    def auxSumaVectorMatriz(matriz: Matriz, posicion: Int, acumuladorSuma : Vector[Vector[Int]]): Vector[Int]={
      if (posicion == matriz.length) {acumuladorSuma(0)}
      else{
        val suma = sumMatriz(subMatriz(matriz,(posicion),0,matriz.length),acumuladorSuma.appended(Vector(0)))
        auxSumaVectorMatriz(matriz,posicion+1,suma)
      }
    }

    def modificarVector(vector:Vector[Int], posicion:Int,tamanio:Int, acumulador:Vector[Vector[Int]]) :Matriz={
      if (posicion == tamanio) {acumulador}
      else{
        val nuevoVector = acumulador:+Vector(vector(posicion))
        modificarVector(vector,posicion+1,tamanio,nuevoVector)
      }
    }

    def auxMultMatrizRec (m1:Matriz , m2: Matriz,auxMatriz: Matriz, tamanio : Int,posicionQuieta:Int,posicionCambiante:Int,auxVector: Vector[Int] ) : Matriz ={
     if (auxMatriz.length == m1.length ) {auxMatriz}
     else{
      val vectorSuma = Vector.tabulate(tamanio)((i) => prodPunto(subMatriz(m1,posicionQuieta,i,m1.length)(0), transpuesta(subMatriz(m2,i,posicionCambiante,m2.length))(0)))
      val nuevoVector= modificarVector(vectorSuma,0,vectorSuma.length,Vector())
      val aux2 = auxVector ++ auxSumaVectorMatriz(nuevoVector,0,Vector())
      
      if(aux2.length == tamanio){
        val nuevaMatriz = auxMatriz:+aux2
        auxMultMatrizRec(m1,m2,nuevaMatriz,tamanio,posicionQuieta+1,0,Vector())

      }else{
        if (posicionCambiante == tamanio-1) {auxMultMatrizRec(m1,m2,auxMatriz,tamanio,posicionQuieta+1,0,aux2)}
      else{ auxMultMatrizRec(m1,m2,auxMatriz,tamanio,posicionQuieta,posicionCambiante+1,aux2)}
      
      } 
    }
    }
    val matriz: Matriz= Vector()

    auxMultMatrizRec(m1,m2,matriz,m1.length,0,0,Vector())
  }


  def multMatrizRecPar(m1:Matriz , m2: Matriz ): Matriz = {

    def auxSumaVectorMatriz(matriz: Matriz, posicion: Int, acumuladorSuma : Vector[Vector[Int]]): Vector[Int]={
      if (posicion == matriz.length) {acumuladorSuma(0)}
      else{
        val suma = sumMatriz(subMatriz(matriz,(posicion),0,matriz.length),acumuladorSuma.appended(Vector(0)))
        auxSumaVectorMatriz(matriz,posicion+1,suma)
      }
    }

    def modificarVector(vector:Vector[Int], posicion:Int,tamanio:Int, acumulador:Vector[Vector[Int]]) :Matriz={
      if (posicion == tamanio) {acumulador}
      else{
        val nuevoVector = acumulador:+Vector(vector(posicion))
        modificarVector(vector,posicion+1,tamanio,nuevoVector)
      }
    }

    def auxMultMatrizRec (m1:Matriz , m2: Matriz,auxMatriz: Matriz, tamanio : Int,posicionQuieta:Int,posicionCambiante:Int,auxVector: Vector[Int] ) : Matriz ={
     if (auxMatriz.length == m1.length ) {auxMatriz}
     else{
        val vectorSuma = Vector.tabulate(tamanio) { i =>
          val parResul = parallel(
            subMatriz(m1, posicionQuieta, i, m1.length)(0),
            transpuesta(subMatriz(m2, i, posicionCambiante, m2.length))(0)
          )
          prodPunto(parResul._1, parResul._2)
        }
      val nuevoVector= modificarVector(vectorSuma,0,vectorSuma.length,Vector())
      val aux2 = auxVector ++ auxSumaVectorMatriz(nuevoVector,0,Vector())
      
      if(aux2.length == tamanio){
        val nuevaMatriz = auxMatriz:+aux2
        auxMultMatrizRec(m1,m2,nuevaMatriz,tamanio,posicionQuieta+1,0,Vector())

      }else{
        if (posicionCambiante == tamanio-1) {auxMultMatrizRec(m1,m2,auxMatriz,tamanio,posicionQuieta+1,0,aux2)}
      else{ auxMultMatrizRec(m1,m2,auxMatriz,tamanio,posicionQuieta,posicionCambiante+1,aux2)}
      
      } 
    }
    }
    val matriz: Matriz= Vector()

    auxMultMatrizRec(m1,m2,matriz,m1.length,0,0,Vector())
  }


  def strassen(A: Matriz, B: Matriz): Matriz = {
    if (A.length == 1) {
      Vector(Vector(A(0)(0) * B(0)(0)))
    } else {

      val newSize = A.length

      //Divide las matrices en 4 bloques
      val mitad = newSize / 2
      val A11 = subMatriz2(A,0, mitad,0, mitad)
      val A12 = subMatriz2(A,0, mitad,mitad, newSize)      
      val A21 = subMatriz2(A,mitad, newSize,0, mitad)      
      val A22 = subMatriz2(A,mitad, newSize,mitad, newSize)

      val B11 = subMatriz2(B,0, mitad,0, mitad)
      val B12 = subMatriz2(B,0, mitad,mitad, newSize)
      val B21 = subMatriz2(B,mitad, newSize,0, mitad)
      val B22 = subMatriz2(B,mitad, newSize,mitad, newSize)


      val P1 = strassen(A11, restaMatriz(B12, B22))
      val P2 = strassen(sumMatriz(A11, A12), B22)
      val P3 = strassen(sumMatriz(A21,A22), B11)
      val P4 = strassen(A22, restaMatriz(B21, B11))
      val P5 = strassen(sumMatriz(A11, A22), sumMatriz(B11,B22))
      val P6 = strassen(restaMatriz(A12, A22), sumMatriz(B21, B22))
      val P7 = strassen(restaMatriz(A11, A21), sumMatriz(B11, B12))


      val C11 = sumMatriz(sumMatriz(P5, P4), restaMatriz(P6, P2))
      val C12 = sumMatriz(P1, P2)
      val C21 = sumMatriz(P3, P4)
      val C22 = restaMatriz(sumMatriz(P5, P1), sumMatriz(P7, P3))

    val result: Matriz = Vector.tabulate(newSize) { i =>
      if (i < mitad) {
        Vector.concat(C11(i), C12(i))
      } else {
        Vector.concat(C21(i - mitad), C22(i - mitad))
      }
    }
      result
    }
  }


  def strassenParallel(A: Matriz, B: Matriz): Matriz = {
      if (A.length == 1) {
        Vector(Vector(A(0)(0) * B(0)(0)))
      } else {

        val newSize = A.length

        //Divide las matrices en 4 bloques
        val mitad = newSize / 2
        val A11 = subMatriz2(A,0, mitad,0, mitad)
        val A12 = subMatriz2(A,0, mitad,mitad, newSize)      
        val A21 = subMatriz2(A,mitad, newSize,0, mitad)      
        val A22 = subMatriz2(A,mitad, newSize,mitad, newSize)

        val B11 = subMatriz2(B,0, mitad,0, mitad)
        val B12 = subMatriz2(B,0, mitad,mitad, newSize)
        val B21 = subMatriz2(B,mitad, newSize,0, mitad)
        val B22 = subMatriz2(B,mitad, newSize,mitad, newSize)


        val P1 = task(strassenParallel(A11, restaMatriz(B12, B22)))
        val P2 = task(strassenParallel(sumMatriz(A11, A12), B22))
        val P3 = task(strassenParallel(sumMatriz(A21,A22), B11))
        val P4 = task(strassenParallel(A22, restaMatriz(B21, B11)))
        val P5 = task(strassenParallel(sumMatriz(A11, A22), sumMatriz(B11,B22)))
        val P6 = task(strassenParallel(restaMatriz(A12, A22), sumMatriz(B21, B22)))
        val P7 = task(strassenParallel(restaMatriz(A11, A21), sumMatriz(B11, B12)))

        
        val C11 = sumMatriz(sumMatriz(P5.join(), P4.join()), restaMatriz(P6.join(), P2.join()))
        val C12 = sumMatriz(P1.join(), P2.join())
        val C21 = sumMatriz(P3.join(), P4.join())
        val C22 = restaMatriz(sumMatriz(P5.join(), P1.join()), sumMatriz(P7.join(), P3.join()))

      val result: Matriz = Vector.tabulate(newSize) { i =>
        if (i < mitad) {
          Vector.concat(C11(i), C12(i))
        } else {
          Vector.concat(C21(i - mitad), C22(i - mitad))
        }
      }
        result
      }
  }

}