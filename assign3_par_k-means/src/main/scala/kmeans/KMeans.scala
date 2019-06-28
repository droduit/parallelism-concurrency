package kmeans

import scala.collection._
import scala.util.Random
import org.scalameter._
import common._

class KMeans {

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to[mutable.ArrayBuffer]
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to[mutable.ArrayBuffer]
  }

  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    assert(means.size > 0)
    var minDistance = p.squareDistance(means(0))
    var closest = means(0)
    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    closest
  }

  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
    // pour chaque point, regarder laquelle croix est la plus proche du point
    // faire ceci a l'aide de groupBy and findClosest et retourner une
    // Map[Croix (Mean), Points associés à cette croix]
    val cluster : GenMap[Point, GenSeq[Point]] = points.groupBy { p => findClosest(p, means) }
    
    means.foldLeft(cluster)(
     (acc:GenMap[Point, GenSeq[Point]], mean:Point) => {
         if(!acc.contains(mean))
           acc + ( (mean, GenSeq.empty) )
         else
           acc
     }
    )
  }

  def findAverage(oldMean: Point, points: GenSeq[Point]): Point = if (points.length == 0) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    
    for(oldM <- oldMeans) yield
      findAverage(oldM, classified.apply(oldM))
    
  }

  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    oldMeans.zip(newMeans).forall{
      case (oldMean:Point, newMean:Point) =>
        oldMean.squareDistance(newMean) <= eta
    }
  }

  /**
   *
   * points : Séquence de points
   * means : séquence de means calculé précédemment
   * return la séquence des means, chacun correspondant a un cluster spécifique
   */
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    // Step 1
    // Prendre k points appelés "means" (Initialisation)
 
    // Step 2 -- Associate each input point with the closest mean. Yield k clusters of points.
    val k : GenMap[Point, GenSeq[Point]] = classify(points, means)
    
    // Step 3 -- Update each mean to have the average value od the corresponding cluster
    var newMeans : GenSeq[Point] = update(k, means)
    
    // Step 4 -- If the k means have significantly changed, go back to step 2. Else, the algo converged
    if(!converged(eta)(means, newMeans))
      kMeans(points, newMeans, eta)
    
    // Step 5 -- The k means represent different clusters -- every point is in the cluster corresponding to the closest mean
    newMeans
    
  }
}

/** Describes one point in three-dimensional space.
 *
 *  Note: deliberately uses reference equality.
 */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      val parPoints = points.par
      val parMeans = means.par
      kMeans.kMeans(parPoints, parMeans, eta)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}
