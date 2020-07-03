package com.rotation.arrays

import scala.reflect.ClassTag

object RotationExercises extends App{
  /**
   * 1. Get all possible left rotations of an array
   */
  getAllRotations(Array(1,2,3,4)).map(_.mkString(",")).foreach(println)

  def getAllRotations[T: ClassTag](arr: Array[T]): Array[Array[T]] = {
    var res: Array[Array[T]] = Array()
    for(i <- arr.indices){
      res = res :+ leftRotate(arr,i)
    }; res
  }

 leftRotate(Array(2,3,4,1),1).mkString(",").foreach(print)
  println
  def leftRotate[T:ClassTag](arr: Array[T], count: Int): Array[T] = {
    val splits = arr.splitAt(count)
    splits._2 ++ splits._1
  }

  rightRotate(Array(2,3,4,1),1).mkString(",").foreach(print)
  println
  def rightRotate[T:ClassTag](arr: Array[T], count: Int): Array[T] = {
    val splits = arr.splitAt(arr.length - count)
    splits._2 ++ splits._1
  }

  /**
   * 2. get no of left rotations to sort an array
   */
  println(noOfRotationsToSort(Array(3,4,1,2)))
  def noOfRotationsToSort(arr: Array[Int]): Int =
    arr.zipWithIndex.sliding(2)
      .filter(tup => tup(0)._1 > tup(1)._1)
      .map(tup => tup(1)._2).next
}
