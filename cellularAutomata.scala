import scala.util.Random
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

class Creature(maxLifespanc: Int = 0, fitnessc: Double = 0, namec: String = "-", xc: Int, yc: Int) {
    val x: Int = xc
    val y: Int = yc
    val maxLifespan: Int = maxLifespanc
    val fitness: Double = fitnessc
    val name: String = namec
    val birthTime : Long = System.currentTimeMillis()
    val deathTime: Long = birthTime + getActualLifespan(maxLifespanc)

    def getActualLifespan(upperLimit: Int): Int = {
        if (upperLimit < 1) {
            return 0
        }
        val r = scala.util.Random
        val maxLifespan = r.nextInt(upperLimit) + 1
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
    var neighours: ArrayBuffer[ActorRef] = ArrayBuffer[ActorRef]()

    def receive = {
        case "requestName" =>
            sender ! resident.name
        case "checkStatus" =>
            val currentTime: Long = System.currentTimeMillis()
            if (resident.maxLifespan != 0 && currentTime > resident.deathTime) {
                for (i <- 0 until neighours.length) {
                    neighours(i) ! resident
                }
                resident = new Creature(0,0,"-",xc,yc)
            } 
        case attacker: Creature => 
            val r = scala.util.Random
            if (r.nextFloat <= (attacker.fitness - resident.fitness)) {
                resident = new Creature(attacker.maxLifespan, attacker.fitness, attacker.name, resident.x, resident.y)
            }
        case "speciesOne" => 
            val speciesOneInstance = new Creature(10000,0.8,"1",resident.x,resident.y)
            resident = speciesOneInstance
        case "speciesTwo" => 
            val speciesTwoInstance = new Creature(5000,0.4,"2",resident.x,resident.y)
            resident = speciesTwoInstance
        case nbs: ArrayBuffer[ActorRef] => 
            neighours = nbs
        case _ => println("Unknown message received")
    }
}

object cellularAutomata  {
    def main(args: Array[String]) {
        val rows: Int = 15
        val cols: Int = 30
        val worldMap = generateWorld(rows,cols)
        //runSimulation(world)
        populateWorld(worldMap,rows,cols)
        allocateNeighbours(worldMap,rows,cols)
        updateWorldState(worldMap,rows,cols)
    }

    def populateWorld(worldMap: scala.collection.immutable.Map[String,ActorRef], rows: Int, cols: Int) = {
        val bottomCorner = "0_0"
        val topCorner = "" + (rows - 1) + "_" + (cols - 1)
        worldMap(bottomCorner) ! "speciesOne"
        worldMap(topCorner) ! "speciesTwo"
    }

    def runSimulation(worldMap: scala.collection.immutable.Map[String,ActorRef]) = {
        while (true) {
            Thread.sleep(2000)
            //printWorld(world)
        }
    }

    def allocateNeighbours(worldMap: scala.collection.immutable.Map[String,ActorRef], rows: Int, cols: Int) = {
        for ( i <- 0 until rows; j <- 0 until cols) {
            val creatureKey: String = "" + i + "_" + j
            if (worldMap isDefinedAt creatureKey) {
                val creature = worldMap(creatureKey)
                val moves = List(-1, 0, 1)
                var neighours: ArrayBuffer[ActorRef] = ArrayBuffer[ActorRef]()
                for ( k <- 0 until moves.length; l <- 0 until moves.length) {
                    val x: Int = i + moves(k)
                    val y: Int = j + moves(l)
                    val neighourKey: String = "" + x + "_" + y
                    if (worldMap isDefinedAt neighourKey) {
                        if ( x != i || y != j) {
                            neighours += worldMap(neighourKey)
                        }
                    }
                }
                creature ! neighours
            } 
        }
    }

    def updateWorldState(worldMap: scala.collection.immutable.Map[String,ActorRef], rows: Int, cols: Int) = {
        val printFrequency: Int = 500
        var startTime: Long = System.currentTimeMillis() - printFrequency
        while (true) {
            for ( i <- 0 until rows; j <- 0 until cols ) {
                implicit val timeout = Timeout(5 seconds)
                val key = "" + i + "_" + j
                val nodeManager = worldMap(key)
                nodeManager ! "checkStatus"
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

    def generateWorld(rows: Int, cols: Int) : scala.collection.immutable.Map[String,ActorRef] = {
    //def generateWorld() : Array[Array[ActorRef]] = {
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


