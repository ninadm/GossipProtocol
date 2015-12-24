import akka.actor._
import akka.actor.ActorSystem
import akka.actor.Props
import scala.util._
import scala.collection.mutable.Map
import scala.math._
import java.io._

case object SpreadGossip
case class SpreadPushSum(sum: Double, weight: Double)
case class InitializeGossiper(gossiperID: Int, gossipers: List[ActorRef], neighbours: List[Int], gossipLimit: Int, monitor: ActorRef,system: ActorSystem)
case object StartTimer
case class MonitorGossiperStatus(id: Int, count: Int)
case class Quit(system: ActorSystem, numOfNodes: Int)

object MyGossip {
    def main(args: Array[String]) {
        val system = ActorSystem("GossipCommunication")
        // default Actor constructor
        //val helloActor = system.actorOf(Props[HelloActor], name = "helloactor")
        var networkNodes: List[ActorRef] = Nil
        var startTime: Long = 0
        var endTime: Long = 0
        var maximumGossipHearingLimit = 10 //is it necessary here? defined in Gossiper too
        var networkTopology: String = ""
        var algorithm: String = ""
        var numOfNodes: Int = 0
        var monitor = system.actorOf(Props[RumorMonitor])
        if (args.length == 0 || args.length != 3) {
            println("Please enter the correct number of arguments")
        } else if (args(0) forall Character.isDigit) {
            //Arguments are proper start working
            numOfNodes = args(0).toInt
            networkTopology = args(1)
            algorithm = args(2)
            var i: Int = 0
            for (i <- 1 to (numOfNodes) by +1) { 
                networkNodes ::= system.actorOf(Props[Gossiper])
            }
            if(networkTopology.equalsIgnoreCase("line")) {
                println("inside line creation")
                i = 0
                while (i < numOfNodes) {
                    var neighbours: List[Int] = Nil
                    if (i > 0) neighbours ::= i - 1
                    if (i < numOfNodes - 1) neighbours ::= i + 1
                    networkNodes(i) ! InitializeGossiper(i, networkNodes, neighbours, maximumGossipHearingLimit, monitor, system)
                    i += 1
                }
                //system.shutdown
            } else if(networkTopology.equalsIgnoreCase("full")) {
                println("inside full creation")
                i = 0
                while (i < numOfNodes) {
                    var neighbours: List[Int] = Nil
                    var j: Int = 0
                    while(j < numOfNodes)
                    {
                        if (j != i) neighbours ::= j 
                        j += 1
                    }
                    networkNodes(i) ! InitializeGossiper(i, networkNodes, neighbours, maximumGossipHearingLimit, monitor, system)
                    i += 1
                }
            } else if(networkTopology.equalsIgnoreCase("3d")) {
                var currentNode: Int = 0
                networkNodes = Nil
                //println("inside 3d creation")
                val nearestCubeRoot = round(cbrt(numOfNodes))
                //println("nearestCubeRoot:" + nearestCubeRoot)
                numOfNodes = (nearestCubeRoot * nearestCubeRoot * nearestCubeRoot).toInt
                //println("numOfNodes:" + numOfNodes)
                i = 0
                var j: Int = 0
                var k: Int = 0
                networkNodes = Nil
                for (i <- 1 to (numOfNodes) by +1) { 
                    networkNodes ::= system.actorOf(Props[Gossiper])
                }
                for (i ← 0 until nearestCubeRoot.toInt)
                {
                    for (j ← 0 until nearestCubeRoot.toInt)
                    {
                        for (k ← 0 until nearestCubeRoot.toInt)
                        {
                            var neighbours: List[Int] = Nil
                            //print(i + ", " + j + ", " + k)
                            currentNode = k * pow(nearestCubeRoot, 0).toInt + j * pow(nearestCubeRoot, 1).toInt + i * pow(nearestCubeRoot, 2).toInt
                            neighbours = neighbours ::: calculateNeighbours(i, j, k, numOfNodes, nearestCubeRoot.toInt, false)
                            //println("currentNode : " + currentNode)
                            //neighbours.foreach { println }
                            networkNodes(currentNode) ! InitializeGossiper(currentNode, networkNodes, neighbours, maximumGossipHearingLimit, monitor, system)
                        }
                    }
                }
            }
            else if(networkTopology.equalsIgnoreCase("imp3d")) {
                var currentNode: Int = 0
                //println("inside 3d creation")
                val nearestCubeRoot = round(cbrt(numOfNodes))
                println("nearestCubeRoot:" + nearestCubeRoot)
                numOfNodes = (nearestCubeRoot * nearestCubeRoot * nearestCubeRoot).toInt
                //println("numOfNodes:" + numOfNodes)
                i = 0
                var j: Int = 0
                var k: Int = 0
                networkNodes = Nil
                for (i <- 1 to (numOfNodes) by +1) { 
                    networkNodes ::= system.actorOf(Props[Gossiper])
                }
                for (i ← 0 until nearestCubeRoot.toInt)
                {
                    for (j ← 0 until nearestCubeRoot.toInt)
                    {
                        for (k ← 0 until nearestCubeRoot.toInt)
                        {
                            var neighbours: List[Int] = Nil
                            //print(i + ", " + j + ", " + k)
                            currentNode = k * pow(nearestCubeRoot, 0).toInt + j * pow(nearestCubeRoot, 1).toInt + i * pow(nearestCubeRoot, 2).toInt
                            neighbours = neighbours ::: calculateNeighbours(i, j, k, numOfNodes, nearestCubeRoot.toInt, true)
                            //println("\ncurrentNode : " + currentNode)
                            //neighbours.foreach { println }
                            networkNodes(currentNode) ! InitializeGossiper(currentNode, networkNodes, neighbours, maximumGossipHearingLimit, monitor, system)
                        }
                    }
                }
            }
            if(algorithm.equalsIgnoreCase("gossip")) {
                networkNodes(0) ! SpreadGossip
                //system.shutdown
            }  
            else if(algorithm.equalsIgnoreCase("push-sum")) {
                networkNodes(0) ! SpreadPushSum(0, 1)
            }
        }
        else {
            println("Number of nodes must be an integer")
        }         
    }

    def calculateNeighbours(i: Int, j: Int, k: Int, numOfNodes: Int, nearestCubeRoot: Int, imp3D: Boolean): List[Int] = {
        var possibleNeighbour: Int = 0
        var neighbours: List[Int] = Nil
        if (!((k - 1) < 0)) {
            //println("\nCalculating for " + (k-1) + ", " + j + ", " + i)
            neighbours ::= ((k - 1) * pow(nearestCubeRoot, 0) + j * pow(nearestCubeRoot, 1) + i * pow(nearestCubeRoot, 2)).toInt
        }
        if (!((j - 1) < 0)) {
            //println("\nCalculating for " + (k) + ", " + (j-1) + ", " + i)
            neighbours ::= (k * pow(nearestCubeRoot, 0) + ((j - 1) * pow(nearestCubeRoot, 1)) + i * pow(nearestCubeRoot, 2)).toInt
        }
        if (!((i - 1) < 0)) {
            //println("\nCalculating for " + (k) + ", " + j + ", " + (i-1))
            neighbours ::= (k * pow(nearestCubeRoot, 0) + j * pow(nearestCubeRoot, 1) + ((i - 1) * pow(nearestCubeRoot, 2))).toInt
        }
        if (!((k + 1) > (nearestCubeRoot - 1))) {
            //println("\nCalculating for " + (k+1) + ", " + j + ", " + i)
            neighbours ::= ((k + 1) * pow(nearestCubeRoot, 0) + j * pow(nearestCubeRoot, 1) + i * pow(nearestCubeRoot, 2)).toInt
        }
        if (!((j + 1) > (nearestCubeRoot - 1))) {
            //println("\nCalculating for " + (k) + ", " + (j+1) + ", " + i)
            neighbours ::= (k * pow(nearestCubeRoot, 0) + ((j + 1)* pow(nearestCubeRoot, 1)) + i * pow(nearestCubeRoot, 2)).toInt
        }
        if (!((i + 1) > (nearestCubeRoot - 1))) {
            //println("\nCalculating for " + (k) + ", " + j + ", " + (i+1))
            neighbours ::= ((k * pow(nearestCubeRoot, 0) + j * pow(nearestCubeRoot, 1) + ((i + 1) * pow(nearestCubeRoot, 2)))).toInt
        }
        if (imp3D) {
            val rnd = new Random()
            var randomNeighbour: Int = 0
            do
            {
                randomNeighbour = rnd.nextInt(numOfNodes - 1)                
            } while(neighbours.contains(randomNeighbour))
            neighbours ::= randomNeighbour
        }
        return neighbours
    }
}

class Gossiper extends Actor {
    var receivedGossipCounter = 0
    var gossipNeighbours: List[Int] = Nil
    var sum: Double = 0
    var weight: Double = 0
    var gossiperID: Int = 0
    var allGossipers: List[ActorRef] = Nil
    var gossipLimit: Int = 0
    val rnd = new Random()
    var rumorStatusMonitor: ActorRef = null
    var mainSystem: ActorSystem = null
    var currentConverganceRatio: Double = 0
    var converganceCounter: Int = 0
    def receive = {
        case InitializeGossiper(id, gossipers, neighbours, limit, monitor, system) => {
            
            gossipNeighbours = neighbours
            gossiperID = id
            allGossipers = gossipers
            gossipLimit = limit
            rumorStatusMonitor = monitor
            mainSystem = system
            sum = id
            weight = 1
            //println("For gossiperId: " + gossiperID)
            //println("\nduring InitializeGossiper sum is " + sum + " and weight is " + weight + " and neighbours ...")
            //neighbours.foreach {
            //    println
            //}
        } 
        case SpreadGossip => {
            if(receivedGossipCounter < gossipLimit) {
                //println("inside gossip condition satisfied")
                receivedGossipCounter += 1
                val randomNeighbour: Int = gossipNeighbours(rnd.nextInt(gossipNeighbours.length))
                allGossipers(randomNeighbour) ! SpreadGossip
                rumorStatusMonitor ! MonitorGossiperStatus(gossiperID, receivedGossipCounter)
                //println("random neighbour: " + neighbours(randomNeighbour) + "\n")
            } else {
                //println("inside quit passed")
                //println(rumorStatusMonitor)
                rumorStatusMonitor ! Quit(mainSystem, allGossipers.length)
            }
        }
        case SpreadPushSum(newSum, newWeight) => {
            receivedGossipCounter += 1
            currentConverganceRatio = sum / weight
            sum = sum + newSum
            weight = weight + newWeight
            //println("\nsum " + sum + "weight " + weight)
            //println("\nnewSum " + newSum + "newWeight " + newWeight)
            var newConverganceRatio: Double = newSum / newWeight
            if(abs(currentConverganceRatio - newConverganceRatio) < pow(10, -10)) {
                //println("inside gossip condition satisfied")
                converganceCounter += 1
                if (converganceCounter < 3)
                {
                    //println("\ncurrentConverganceRatio" + currentConverganceRatio)
                    //println("\nnewConverganceRatio" + newConverganceRatio)
                    val randomNeighbour: Int = gossipNeighbours(rnd.nextInt(gossipNeighbours.length))
                    allGossipers(randomNeighbour) ! SpreadPushSum(sum / 2.0, weight / 2.0)
                    rumorStatusMonitor ! MonitorGossiperStatus(gossiperID, receivedGossipCounter)
                    //println("random neighbour: " + neighbours(randomNeighbour) + "\n")
                }    
                else {
                    //println("inside quit passed")
                    //println(rumorStatusMonitor)
                    rumorStatusMonitor ! Quit(mainSystem, allGossipers.length)
                }
            }
            else {
                //println("\ncurrentConverganceRatio when difference is bigger" + currentConverganceRatio)
                //println("\nnewConverganceRatio when difference is bigger" + newConverganceRatio)
                val randomNeighbour: Int = gossipNeighbours(rnd.nextInt(gossipNeighbours.length))
                rumorStatusMonitor ! MonitorGossiperStatus(gossiperID, receivedGossipCounter)
                allGossipers(randomNeighbour) ! SpreadPushSum(sum / 2.0, weight / 2.0)
            }
        }   
    }
}

class RumorMonitor extends Actor {
    var rumorCounter = 0
    var z:Array[Int] = new Array[Int](3)
    var endTime: Long = 0
    var startTime: Long = System.currentTimeMillis
    var rumorTerminationCount = 10
    var gossipSystem: ActorSystem = null

    var statusMap: Map[Int, Int] = Map()
    def receive = {
        case MonitorGossiperStatus(gossiperID, count) => {
            //println("inside monitorGossip : " + gossiperID + " : " + count)
            if(statusMap.contains(gossiperID)) statusMap = statusMap - gossiperID
            statusMap = statusMap + (gossiperID -> count)
            //println("Monitor gossip: " + statusMap.size)
        }
        case Quit(system, numOfNodes) => {
            var endTime = (System.currentTimeMillis - startTime).toString
            //startTime = startTimeOfGossip
            //println("inside quit actual")
            gossipSystem = system
            //println("asdasd" + statusMap.size)        
            //statusMap.keys.foreach{ i => print( "Key = " + i )
            //   println(" Value = " + statusMap(i))
            //}
            
            var i = 0
            var count = 0
            for ( i <- 0 until numOfNodes)
            {
                if (!statusMap.contains(i)) count += 1
            }

            println("Nodes that have received a message " + statusMap.size)
            println("\nNodes that havent received a message " + count)
            println("\nTotal time: " + endTime)
            val fw = new FileWriter("full_pushsum.txt", true)
            try {
              fw.write(" << " + endTime + "\t" + statusMap.size + "\t" + count + "\t" + (statusMap.size + count) + " >> ")
              
            }
            finally fw.close() 

            gossipSystem.shutdown
        }
    }
}