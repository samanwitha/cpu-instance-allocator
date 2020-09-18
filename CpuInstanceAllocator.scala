//Written in Scala

//Result will be returned as List of RegionWiseResult objects
case class RegionWiseResult(region: String, total_cost: String, servers: List[(String, Int)])

//Object CpuInstanceAllocator can be used to call the get_costs method.
object CpuInstanceAllocator extends App {

  //Initializing the server to CPU count Map so that it can be used whenever called for.
  lazy val serverToCpuMap: Map[String, Int] = Map("large" -> 1, "xlarge" -> 2, "2xlarge" -> 4, "4xlarge" -> 8, "8xlarge" -> 16, "10xlarge" -> 32)

  //Initializing area wise server to cost per hour so that it can be used whenever called for.
  lazy val areaToServerAndserverToCostMap: Map[String, Map[String, Double]] = Map(
    "us-east" -> Map(
      "large" -> 0.12, "xlarge" -> 0.23, "2xlarge" -> 0.45, "4xlarge" -> 0.774, "8xlarge" -> 1.4, "10xlarge" -> 2.82),
    "us-west" -> Map(
      "large" -> 0.14, "2xlarge" -> 0.413, "4xlarge" -> 0.89, "8xlarge" -> 1.3, "10xlarge" -> 2.97),
    "asia" -> Map(
      "large" -> 0.11, "xlarge" -> 0.20, "4xlarge" -> 0.67, "8xlarge" -> 1.18))

  //get_costs method is used to get the result according to the requirements
  def get_costs(hours: Int, cpus: Option[Int], price: Option[Double]): List[Result] = {

    //When the input is combination of cpu count and the maximum cost we call the allocateMaxCpusWithMinCost method
    if ((cpus.isDefined && cpus.get > 0) && (price.isDefined && price.get > 0)) allocateMaxCpusWithMinCost(hours, cpus.get, price.get)

    //When only the number of cpus is specified in the input we call allocateMaxCpusWithNoCostGiven method
    else if ((cpus.isDefined && cpus.get > 0)) allocateMaxCpusWithNoCostGiven(hours, cpus.get)

    //When only the maximum cost is specified in the input we call allocateCpusWithProvidedMaxCost method
    else if ((price.isDefined && price.get > 0)) allocateCpusWithProvidedMaxCost(hours, price.get)

    //When both cpu count and maximum cost are not given we return empty result list
    else List()
  }

  //allocateMaxCpusWithMinCost is the method that uses both the cpu count and the maximum price given and calculates the server types and count required according to the input
  private def allocateMaxCpusWithMinCost(hours: Int, cpus: Int, price: Double) = {
    import scala.util.control._
    val loop = new Breaks;

    var cpuCount = cpus;
    var minPrice = price;
    var totalCost: Double = 0;

    //serversCountMap to store the server types and count required for each area separately
    var serversCountMap = scala.collection.mutable.Map[String, Int]()

    //isApplicable is used to check if the specified serverType can be used to allocate cpus
    def isApplicable(serverType: String)(implicit serverToCostMap: Map[String, Double]) = {

      //check if the servertype is available in the area and if it can fit with the minimum cost and cpu count
      serverToCpuMap.contains(serverType) && serverToCostMap.contains(serverType) && cpuCount > serverToCpuMap.get(serverType).get && minPrice >= serverToCostMap.get(serverType).get * hours
    }

    //calculateAndAdd method is used to check the number of cpus that can be used considering the serverType and change the cpuCount,minPrice,serversCountMap,totalCost accordingly
    def calculateAndAdd(serverType: String)(implicit serverToCostMap: Map[String, Double]) = {

      //cpuNums is the cpu count that the specified server type contains
      val cpuNums = serverToCpuMap.get(serverType).get

      //serverTypeCpucount is the number of severs of the specified type  that can be used  according to the remaning price and the cpu count
      val serverTypeCpucount = Math.min(cpuCount / cpuNums, (minPrice / (serverToCostMap.get(serverType).get * hours)).toInt);

      //Decrementing the remaining cpu count by number of cpus from serverTypeCpucount
      cpuCount = cpuCount - serverTypeCpucount * cpuNums;

      //Decrementing the remaining price by the cost of the servers from serverTypeCpucount
      minPrice = minPrice - serverTypeCpucount * serverToCostMap.get(serverType).get * hours;

      //adding  number of servers of the specified type from serverTypeCpucount  to the result
      serversCountMap.put(serverType, serversCountMap.getOrElse(serverType, 0) + serverTypeCpucount);

      //adding the cost of servers from serverTypeCpucount to the total cost for the region
      totalCost = totalCost + serverToCostMap.get(serverType).get * serverTypeCpucount * hours;
    }

    areaToServerAndserverToCostMap.map { a =>

      //serverToCostMap is the server to cost as given in the areaToServerAndserverToCostMap
      implicit val serverToCostMap = a._2

      //Reset the serversCountMap,cpuCount,minPrice,totalCost for each region
      serversCountMap = scala.collection.mutable.Map[String, Int]()
      cpuCount = cpus;
      minPrice = price;
      totalCost = 0;

      loop.breakable {

        //While the cpucount and the price are remaining, we will check the availablity of the servers according to the number of cpus
        //and their cost and add then to the result in the calculateAndAdd method.
        while (cpuCount > 0 && minPrice > 0) {
          if (isApplicable("10xlarge")) calculateAndAdd("10xlarge")
          else if (isApplicable("8xlarge")) calculateAndAdd("8xlarge")
          else if (isApplicable("4xlarge")) calculateAndAdd("4xlarge")
          else if (isApplicable("2xlarge")) calculateAndAdd("2xlarge")
          else if (isApplicable("xlarge")) calculateAndAdd("xlarge")
          else if (isApplicable("large")) calculateAndAdd("large")
          else loop.break
        }
      }
      //Build the result object for each area
      Result(a._1, "$" + totalCost, serversCountMap.toList)

      //Sort the result list by totalcost
    }.toList.sortBy(_.total_cost)
  }

  private def allocateMaxCpusWithNoCostGiven(hours: Int, cpus: Int) = {
    import scala.util.control._
    val loop = new Breaks;

    var serversCountMap = scala.collection.mutable.Map[String, Int]()
    var cpuCount = cpus;
    var totalCost: Double = 0;

    //isApplicable is used to check if the specified serverType can be used to allocate cpus
    def isApplicable(serverType: String)(implicit serverToCostMap: Map[String, Double]) = {

      //check if the servertype is available in the area and if it can fit with the cpu count
      serverToCostMap.contains(serverType) && serverToCpuMap.contains(serverType) && cpuCount > serverToCpuMap.get(serverType).get
    }

    //calculateAndAdd method is used to check the number of cpus that can be used considering the serverType and change the cpuCount,serversCountMap,totalCost accordingly
    def calculateAndAdd(serverType: String)(implicit serverToCostMap: Map[String, Double]) = {

      //cpuNums is the cpu count that the specified server type contains
      val cpuNums = serverToCpuMap.get(serverType).get

      //serverTypeCpucount is the number of severs of the specified type  that can be used  according to the cpu count
      val serverTypeCpuCount = cpuCount / cpuNums;

      //Considering the remaining cpu count after substracting the cpus from the serverTypeCpucount
      cpuCount = cpuCount % cpuNums;

      //adding  number of servers of the specified type from serverTypeCpucount  to the result
      serversCountMap.put(serverType, serversCountMap.getOrElse(serverType, 0) + serverTypeCpuCount);

      //adding the cost of servers from serverTypeCpucount to the total cost for the region
      totalCost = totalCost + serverToCostMap.get(serverType).get * serverTypeCpuCount * hours;
    }

    areaToServerAndserverToCostMap.map { a =>

      //serverToCostMap is the server to cost as given in the areaToServerAndserverToCostMap
      implicit val serverToCostMap = a._2

      //Reset the serversCountMap,cpuCount,totalCost for each region
      serversCountMap = scala.collection.mutable.Map[String, Int]()
      cpuCount = cpus;
      totalCost = 0;

      loop.breakable {

        //While the cpucount is remaining, we will check the availablity of the servers according to the number of cpus
        //and their cost and add then to the result in the calculateAndAdd method.
        while (cpuCount > 0) {
          if (isApplicable("10xlarge")) calculateAndAdd("10xlarge")
          else if (isApplicable("8xlarge")) calculateAndAdd("8xlarge")
          else if (isApplicable("4xlarge")) calculateAndAdd("4xlarge")
          else if (isApplicable("2xlarge")) calculateAndAdd("2xlarge")
          else if (isApplicable("xlarge")) calculateAndAdd("xlarge")
          else if (isApplicable("large")) calculateAndAdd("large")
          else loop.break
        }
      }
      //Build the result object for each area
      Result(a._1, "$" + totalCost, serversCountMap.toList)

      //Sort the result list by totalcost
    }.toList.sortBy(_.total_cost)

  }

  private def allocateCpusWithProvidedMaxCost(hours: Int, price: Double) = {
    import scala.util.control._
    val loop = new Breaks;

    var minPrice = price;
    var totalCost: Double = 0;
    var serversCountMap = scala.collection.mutable.Map[String, Int]()

    //isApplicable is used to check if the specified serverType can be used to allocate cpus
    def isApplicable(serverType: String)(implicit serverToCostMap: Map[String, Double]) = {

      //check if the servertype is available in the area and if it can fit with the minimum cost
      serverToCostMap.contains(serverType) && minPrice >= serverToCostMap.get(serverType).get * hours
    }

    //calculateAndAdd method is used to check the number of cpus that can be used considering the serverType and change the minPrice,serversCountMap,totalCost accordingly
    def calculateAndAdd(serverType: String)(implicit serverToCostMap: Map[String, Double]) = {

      //cost of the specified server in the particular region
      val serverCost = serverToCostMap.get(serverType).get

      //serverTypeCpucount is the number of severs of the specified type that can be used according to the minPrice
      val serverCpuCount = (minPrice / (serverCost * hours)).toInt;

      //Decrementing the remaining price by the cost of the servers from serverTypeCpucount
      minPrice = minPrice - serverCpuCount * serverCost * hours;

      //adding  number of servers of the specified type from serverTypeCpucount  to the result
      serversCountMap.put(serverType, serversCountMap.getOrElse(serverType, 0) + serverCpuCount);

      //adding the cost of servers from serverTypeCpucount to the total cost for the region
      totalCost = totalCost + serverCost * serverCpuCount * hours;
    }

    areaToServerAndserverToCostMap.map { a =>
      implicit val serverToCostMap = a._2

      //Reset the serversCountMap,minPrice,totalCost for each region
      serversCountMap = scala.collection.mutable.Map[String, Int]()
      minPrice = price;
      totalCost = 0;

      loop.breakable {

        //While the minPrice is remaining, we will check the availablity of the servers according to the number of cpus
        //and their cost and add then to the result in the calculateAndAdd method.
        while (minPrice > 0) {
          if (isApplicable("10xlarge")) calculateAndAdd("10xlarge")
          else if (isApplicable("8xlarge")) calculateAndAdd("8xlarge")
          else if (isApplicable("4xlarge")) calculateAndAdd("4xlarge")
          else if (isApplicable("2xlarge")) calculateAndAdd("2xlarge")
          else if (isApplicable("xlarge")) calculateAndAdd("xlarge")
          else if (isApplicable("large")) calculateAndAdd("large")
          else loop.break
        }
      }
      //Build the result object for each area
      Result(a._1, "$" + totalCost, serversCountMap.toList)

      //Sort the result list by totalcost
    }.toList.sortBy(_.total_cost)
  }
}
/*
 Test cases:
 1. hours = 0;
  Result =>  List()

 2. hours < 0;
  Result =>  List()

 3. hours > 0; cpus < 0; price = None
  Result =>  List()

 4. hours > 0; cpus = None; price < 0
  Result =>  List()

 5. hours > 0; cpus < 0; price <0
  Result =>  List()

 6. hours > 0; cpus = None; price = None
  Result =>  List()

 7. hours = 5 ; cpus = None ; price = Some(100)
  Result =>  List(RegionWiseResult(us-west,$99.72999999999999,List((2xlarge,2), (10xlarge,6), (8xlarge,1))), RegionWiseResult(asia,$99.74999999999999,List((xlarge,2), (8xlarge,16), (4xlarge,1))), RegionWiseResult(us-east,$99.85,List((xlarge,1), (10xlarge,7))))

 8. hours = 5 ; cpus = Some(100) ; price = None
	Result => List(RegionWiseResult(asia,$37.4,List((xlarge,2), (8xlarge,6))), RegionWiseResult(us-east,$44.599999999999994,List((xlarge,2), (10xlarge,3))), RegionWiseResult(us-west,$47.349999999999994,List((large,4), (10xlarge,3))))

 9. hours = 5 ; cpus = Some(100) ; price = Some(100)
  Result => List(RegionWiseResult(asia,$37.4,List((xlarge,2), (8xlarge,6))), RegionWiseResult(us-east,$44.599999999999994,List((xlarge,2), (10xlarge,3))), RegionWiseResult(us-west,$47.349999999999994,List((large,4), (10xlarge,3))))

 10. hours = 5 ; cpus = Some(100) ; price = Some(30)
  Result =>  List(RegionWiseResult(asia,$29.499999999999996,List((8xlarge,5))), RegionWiseResult(us-west,$29.700000000000003,List((10xlarge,2))), RegionWiseResult(us-east,$29.95,List((large,1), (xlarge,1), (10xlarge,2))))
 */
