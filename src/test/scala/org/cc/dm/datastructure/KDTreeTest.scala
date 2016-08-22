package org.cc.dm.datastructure

import org.scalatest.FunSuite
import collection.immutable.Seq
import collection.immutable.IndexedSeq

import org.cc.dm.data._
import KDTree._

class KDTreeTest extends FunSuite {

  test("Building a KDTree from an empty Dataset should produce an empty KDTree") {
    assert(build(Seq()) === Empty)
  }

  test("Building a KDTree from a non-empty Dataset should produce a non-empty KDTree") {
    val ds = dataset(
      vec(1.0,2.0),
      vec(2.0,2.0),
      vec(3.0,1.0),
      vec(4.0,3.0)
    )

    val kdtree = build(ds)
    assert(kdtree !== Empty)
    kdtree match {
      case KDNode(v,_,_,_) => assert(v === vec(3.0,1.0))
      case _ => throw new AssertionError()
    }
  }

  test("Contains on a KDTree should return true if a point/vector is part of the nodes") {
    val ds = dataset(
      vec(1.0,2.0),
      vec(2.0,2.0),
      vec(3.0,1.0),
      vec(4.0,3.0)
    )

    val kdtree = build(ds)
    assert(kdtree !== Empty)
    ds.foreach(pt => assert(contains(kdtree)(pt)))
  }

  test("Contains on a KDTree should return false if a point/vector is NOT part of the nodes") {
    val ds = dataset(
      vec(1.0,2.0),
      vec(2.0,2.0),
      vec(3.0,1.0),
      vec(4.0,3.0)
    )

    val kdtree = build(ds)
    assert(kdtree !== Empty)
    assert(!contains(kdtree)(vec(2.0,1.0)))
    assert(!contains(kdtree)(vec(3.0,4.0)))
    assert(!contains(kdtree)(vec(1.0,3.0)))
    assert(!contains(kdtree)(vec(3.0,4.0)))
  }

  // Orthogonal range query

  test("Orthonal range query should return points inside an hyper-cube") {
    val ds = dataset(
      // Outside
      vec(-1.0,0.5,0.5),
      vec(0.5,-1.0,0.5),
      vec(0.5,0.5,-1.0),
      vec(0.5,0.5,2.0),
      // Inside
      vec(0.5,0.5,0.5),
      vec(0.75,0.75,0.75),
      // Corners
      vec(0.0,0.0,0.0),
      vec(1.0,0.0,0.0),
      vec(0.0,1.0,0.0),
      vec(0.0,0.0,1.0),
      vec(1.0,1.0,0.0),
      vec(0.0,1.0,1.0),
      vec(1.0,0.0,1.0),
      vec(1.0,1.0,1.0),
      // On a face
      vec(0.5,0.0,0.5),
      vec(0.75,0.0,0.75)
    )
    val cube = (vec(0.0,0.0,0.0), vec(1.0,1.0,1.0))
    val kdtree = build(ds)
    assert(kdtree !== Empty)
    val inside = orthogonalQuery(kdtree)(cube)
    assert(inside.size === (8 /* corners */ + 2 /* inside */ + 2 /* on a face */))
  }


  // Orthogonal range query

  test("Circular range query should return points inside an hyper-sphere") {
    val ds = dataset(
      // Outside
      vec(-1.0,0.5,1.1),
      // Inside the sphere
      vec(0.0,0.0,0.0),
      vec(0.5,0.5,0.5),
      // Middle of faces
      vec(-1.0,0.0,0.0),
      vec(0.0,-1.0,0.0),
      vec(0.0,0.0,-1.0),
      vec(1.0,0.0,0.0),
      vec(0.0,1.0,0.0),
      vec(0.0,0.0,1.0),
      // Wrapping Cube Corners (outside)
      vec(-1.0,-1.0,-1.0),
      vec(-1.0,-1.0,1.0),
      vec(-1.0,1.0,-1.0),
      vec(1.0,-1.0,-1.0),
      vec(-1.0,1.0,0.0),
      vec(1.0,-1.0,1.0),
      vec(1.0,1.0,-1.0),
      vec(1.0,1.0,1.0),
      // Outside
      vec(0.75,0.75,0.75)
    )
    val euclidean = (v1: Vec, v2: Vec) => math.sqrt((v1 zip v2).map { case (x,y) => (x-y) * (x-y)}.sum)
    val sphere = (vec(0.0,0.0,0.0), 1.0)
    val kdtree = build(ds)
    assert(kdtree !== Empty)
    val inside = circularQuery(kdtree)(euclidean)(sphere)
    assert(inside.size === 2 /* inside */ + 6 /* middle of faces */)
  }


  // K-nearest neighbours query

  test("Nearest Neighbours query") {
    val ds = dataset(
      vec(2.0,2.0),
      vec(0.0,5.0),
      vec(8.0,0.0),
      vec(9.0,8.0),
      vec(7.0,14.0),
      vec(13.0,12.0),
      vec(14.0,13.0)
    )
    val euclidean = (v1: Vec, v2: Vec) => math.sqrt((v1 zip v2).map { case (x,y) => (x-y) * (x-y)}.sum)
    val kdtree = build(ds)
    assert(kdtree !== Empty)
    val nn = nnQuery(kdtree)(distfn=euclidean)(k=4)(target=vec(5,5)).reverse.map(_._1).toIndexedSeq
    assert(nn === dataset(vec(2.0,2.0),vec(0.0,5.0),vec(9.0,8.0),vec(8.0,0.0)))
  }


  // Helper

  def dataset(xs: Vec*): Seq[Vec] = IndexedSeq(xs:_*)
  def vec(xs: Double*): Seq[Double] = IndexedSeq(xs:_*)

}
