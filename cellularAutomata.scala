import scala.util.Random
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.collection.mutable.Map

class Creature(maxLifespanc: Int = 0, fitnessc: Double = 0, namec: String = "-", xc: Int, yc: Int) {
    val x: Int = xc
    val y: Int = yc
    val maxLifespan: Int = maxLifespanc
    val deathTime: Int = maxLifespanc + getActualLifespan(maxLifespanc)
    val fitness: Double = fitnessc
    val name: String = namec
    val birthTime : Long = System.currentTimeMillis()

    def getActualLifespan(upperLimit: Int): Int = {
        if (upperLimit < 1) {
            return 0
        }
        val r = scala.util.Random
        val maxLifespan = r.nextInt(upperLimit)
        return maxLifespan
    }

    override 
    def toString(): String = {
        "Creature: maxLifespan(" + maxLifespan + 
        "); fitness(" + fitness + 
        "); name(" + name + 
        "); birthTime(" + birthTime + ")"
    }
}

class NodeManager(xc: Int, yc: Int) extends Actor {
    var resident: Creature = new Creature(0,0,"-",xc,yc)

    def receive = {
        case "requestName" =>
            sender ! resident.name
        case "checkStatus" =>
            if (resident.maxLifespan != 0) {
                val currentTime: Long = System.currentTimeMillis()
                if (currentTime > resident.deathTime) {
                    sender ! resident 
                } else {
                    val returnVal = new Creature(0,0,"-",resident.x,resident.y)
                    sender ! returnVal
                }
            } else {
                val returnVal = new Creature(0,0,"-",resident.x,resident.y)
                sender ! returnVal
            }
        case attacker: Creature => 
            val r = scala.util.Random
            if (r.nextFloat <= (attacker.fitness - resident.fitness)) {
                resident = new Creature(attacker.maxLifespan, attacker.fitness, attacker.name, resident.x, resident.y)
            }
        case "speciesOne" => 
            val speciesOneInstance = new Creature(1000,0.8,"1",resident.x,resident.y)
            resident = speciesOneInstance
        case _ => println("Unknown message received")
    }
}

object cellularAutomata  {
    def main(args: Array[String]) {
        val worldMap = generateWorld()
        //runSimulation(world)
        populateWorld(worldMap)
        updateWorldState(worldMap,10,20)
    }

    def populateWorld(worldMap: scala.collection.immutable.Map[String,ActorRef]) = {
        worldMap("5_5") ! "speciesOne"
    }

    def runSimulation(worldMap: scala.collection.immutable.Map[String,ActorRef]) = {
        while (true) {
            Thread.sleep(2000)
            //printWorld(world)
        }
    }

    def updateWorldState(worldMap: scala.collection.immutable.Map[String,ActorRef], rows: Int, cols: Int) = {
        val printFrequency: Int = 2000
        var startTime: Long = System.currentTimeMillis() - printFrequency
        while (true) {
            for ( i <- 0 until rows; j <- 0 until cols ) {
                implicit val timeout = Timeout(5 seconds)
                val key = "" + i + "_" + j
                val nodeManager = worldMap(key)
                val future = nodeManager ? "checkStatus"
                val creature = Await.result(future, timeout.duration).asInstanceOf[Creature]
                if (creature.maxLifespan != 0) {
                    val moves = List(-1, 0, 1)
                    for ( k <- 0 until moves.length; l <- 0 until moves.length) {
                        val neighbourKey: String = "" + (creature.x + i) + "_" + (creature.y + j)
                        if (worldMap isDefinedAt neighbourKey) {
                            worldMap(neighbourKey) ! creature
                        }
                        
                    }
                }
            }
            val currentTime: Long = System.currentTimeMillis()
            if (currentTime > startTime + printFrequency) {
                printWorldMap(worldMap, rows, cols)
                startTime = System.currentTimeMillis()
            }
        }
    }

    def checkCreatureStatus(world: Array[Array[ActorRef]]) = {
        while (true) {
            val outerLength : Int = world.length
            val innerLength : Int = world(0).length
            for (i <- 0 until outerLength; j <- 0 until innerLength) {
                val nodeManager = world(i)(j)
                nodeManager ! "checkStatus"
            }
        }
    }

    def generateWorld() : scala.collection.immutable.Map[String,ActorRef] = {
    //def generateWorld() : Array[Array[ActorRef]] = {
        val rows = 10
        val cols = 20
        val world = Array.ofDim[ActorRef](rows,cols)
        val system = ActorSystem("CellularAutomataSystem")
        var nodeManagerMap: Map[String,ActorRef] =  Map()
        for ( i <- 0 until rows; j <- 0 until cols ) {
            val key = "" + i + "_" + j
            val nodeManager = system.actorOf(Props(new NodeManager(i,j)), name = "nodeManager_" + i + "_" + j)
            nodeManagerMap += (key -> nodeManager)
            //val nodeManager = system.actorOf(Props[NodeManager], name = "nodeManager_" + i + "_" + j)
            //world(i)(j) = nodeManager
        }
        val immutableNodeManagerMap = nodeManagerMap.toMap
        //return world
        return immutableNodeManagerMap
    }

    def printWorldMap(worldMap : scala.collection.immutable.Map[String,ActorRef], rows: Int, cols: Int) = {
        for (i <- 0 until rows; j <- 0 until cols) {
            implicit val timeout = Timeout(5 seconds)
            val key: String = "" + i + "_" + j
            val nodeManager = worldMap(key)
            val future = nodeManager ? "requestName"
            val name = Await.result(future, timeout.duration).asInstanceOf[String]
            print(name + " ")
            if (j == cols - 1) {
                print("\n")
            }
        }
        println()
    }

    def printWorld(world : Array[Array[ActorRef]]) = {
        val outerLength : Int = world.length
        val innerLength : Int = world(0).length
        for (i <- 0 until outerLength; j <- 0 until innerLength) {
            implicit val timeout = Timeout(5 seconds)
            val nodeManager = world(i)(j)
            val future = nodeManager ? "requestName"
            val name = Await.result(future, timeout.duration).asInstanceOf[String]
            print(name + " ")
            if (j == innerLength - 1) {
                print("\n")
            }
        }
        println()
    }
}


