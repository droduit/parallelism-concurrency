
package wikipedia

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.apache.spark.RangePartitioner
import org.apache.spark.Partitioner

case class Customer(
  customerId: Int,
  homeCity: String,
  purchaseHistory: List[Ticket]
)
case class Ticket(
  customer: Option[Int],
  origin: String,
  destination: String,
  price: Double
)

object testSpark {

  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Test")
  val sc: SparkContext = new SparkContext(conf)
  
  val customers: RDD[Customer] = sc.parallelize(
    List(
      Customer(1, "Lausanne", List(
        Ticket(Some(1), "Lausanne", "Zurich", 70.4),
        Ticket(Some(1), "Zurich", "Lausanne", 70.4)
      )),
      
      Customer(2, "Montreux", List(
        Ticket(Some(2), "Montreux", "Vevey", 3.50),
        Ticket(Some(2), "Montreux", "Martigny", 5.40)
      ))
    )
  )
  
  val sales : Double = customers.map{
    case Customer(_, _, history) =>
      history.map(h => h.price).sum
  }.reduce(_ + _)
  
  val cityGross : Seq[(String, Double)] = customers.map({
    case Customer(_, homeCity, history) =>
      (homeCity, history.map(_.price).sum)
  }).reduceByKey(_ + _).collect.toSeq
    
  

  
  // (personnes qui quittent leur homeCity) / (nombre total de gens quittant cette ville)
  def leavingTheCityRatio(
    customers: RDD[(Option[Int], Customer)], 
    recentPurchases: RDD[(Option[Int], Ticket)],
    city: String): Double = {
      
    val leavingHomeCity = customers.filter(c => c._2.homeCity == city).count()
   
    val overallLeavingCity = recentPurchases.filter( t => t._2.origin == city)
                              .map(x => (x._1, x._2.origin))
                              .groupByKey()
                              .count()
    
    leavingHomeCity / overallLeavingCity
    
  }
  
  def leavingTheCityRatio2(
    customers: RDD[(Option[Int], Customer)],
    recentPurchases: RDD[(Option[Int], Ticket)],
    city: String): Double = {
    
    val joined : RDD[(Option[Int], (Ticket, Option[Customer]))] =
      (recentPurchases leftOuterJoin customers).persist()
    
    val leavingHome =
      joined.filter(_._2._2.isDefined)
      .filter(c => c._2._2.get.homeCity == city)
      .count()
    
    val leaving = joined.filter(c => c._2._1.origin == city).count()
    
    leavingHome/leaving
  }
  
  
  val idCustomers: RDD[(Option[Int], Customer)] =
    customers.map(c => (Option(c.customerId), c))
    
  val partitioner: Partitioner =
    new RangePartitioner(10, idCustomers)
    
  val persistedCustomers: RDD[(Option[Int], Customer)] =
    idCustomers.partitionBy(partitioner).persist()
  
  // RDD containing all purchases made in the last 10 minutes
  // RDD containing all purchases made in the last 10 minutes
  val recentPurchases: RDD[Ticket] = sc.parallelize(List(
    Ticket(None, "Bern", "Zurich", 15.20), // paper ticket: no customer info
    Ticket(Some(1), "Lausanne", "Zurich", 70.4),
    Ticket(Some(1), "Zurich", "Lausanne", 70.4),
    Ticket(Some(2), "Montreux", "Vevey", 3.50),
    Ticket(Some(2), "Montreux", "Martigny", 5.40)
  ))
  // assume that persistedCustomers, partitioner, and leavingTheCityRatio are visible
  def leavingLausanneRatio(recentPurchases: RDD[Ticket]): Double = {
    val purchases = recentPurchases.map(t => (t.customer, t))
                    .partitionBy(persistedCustomers.partitioner.get)
                    
    leavingTheCityRatio(persistedCustomers, purchases, "Lausanne")
  }
}
