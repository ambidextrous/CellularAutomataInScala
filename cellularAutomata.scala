import scala.util.Random
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

/**
 * Represents a creature with a maximum lifespan, an acutal lifespan
 * a fitness score, a name and x/y co-ordinates
 */
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
}

/**
 * An actor which runs on its own thread and manages a given node.
 * Instance variables are a resident Creature and other neighouring
 * Creatures. Initial constructor creates a "null" resident creatures
 * with 0 lifespan and fitness, represented by a "-"
 */
class NodeManager(xc: Int, yc: Int) extends Actor {
    // *** STATEFUL VARIABLES ***
    var resident: Creature = new Creature(0,0,"-",xc,yc)
    var neighours: ArrayBuffer[ActorRef] = ArrayBuffer[ActorRef]()

    def receive = {
        // Returns resident Creature name
        case "requestName" =>
            sender ! resident.name
        // Checks if resident Creature lifetime finished and attempts to reproduce in neighbouring nodes if so
        case "checkStatus" =>
            val currentTime: Long = System.currentTimeMillis()
            if (resident.maxLifespan != 0 && currentTime > resident.deathTime) {
                for (i <- 0 until neighours.length) {
                    neighours(i) ! resident
                }
                resident = new Creature(0,0,"-",xc,yc)
            } 
        // Recieves an attack by a Creature
        case attacker: Creature => 
            val r = scala.util.Random
            if (r.nextFloat <= (attacker.fitness - resident.fitness)) {
                resident = new Creature(attacker.maxLifespan, attacker.fitness, attacker.name, resident.x, resident.y)
            }
        // Allocates neighbours to variable
        case nbs: ArrayBuffer[ActorRef] => 
            neighours = nbs
        // Allocates value to new resident Creature
        case stringArray: Array[String] =>
            val life = stringArray(0).toInt
            val fit = stringArray(1).toDouble
            val nam = stringArray(2)
            val newCreature = new Creature(life,fit,nam,resident.x,resident.y)
            resident = newCreature
        // Error message
        case _ => println("Unknown message received")
    }
}

/**
 * Cellular automata simulation in an arbitrarily sized grid
 */
object cellularAutomata  {
    def main(args: Array[String]) {
        var r: Int = 15
        var c: Int = 30
        // If args present allocates world dimensions to them
        if (args.length == 2) {
            r = args(0).toInt
            c = args(1).toInt
            if (r < 1) {
                r = 1
            }
            if (c < 1) {
                c = 1
            }
        }
        val rows = r
        val cols = c
        val worldMap = generateWorld(rows,cols)
        populateWorld(worldMap,rows,cols)
        allocateNeighbours(worldMap,rows,cols)
        updateWorldState(worldMap,rows,cols)
    }

    /**
     * Allocates initial population to worldMap
     */
    def populateWorld(worldMap: scala.collection.immutable.Map[String,ActorRef], rows: Int, cols: Int) = {
        val bottomCorner = "0_0"
        val topCorner = "" + (rows - 1) + "_" + (cols - 1)
        val creatureOne = Array("10000","0.8","1")
        val creatureTwo = Array("5000","0.4","2")
        worldMap(bottomCorner) ! creatureOne
        worldMap(topCorner) ! creatureTwo
    }

    /**
     * Allocates neighouring ActorRefs to each ActorRef in worldMap
     */
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
                        neighours += worldMap(neighourKey)
                    }
                }
                creature ! neighours
            } 
        }
    }

    /**
     * Updates worldMap state by sending messages to all nodeManagers
     * and periodically printing to console
     */
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
            // Update only every 10 milliseconds, to reduce system workload
            Thread.sleep(10)
        }
    }

    /**
     * Generates worldMap hashmap of nodeManagers
     */
    def generateWorld(rows: Int, cols: Int) : scala.collection.immutable.Map[String,ActorRef] = {
        val world = Array.ofDim[ActorRef](rows,cols)
        val system = ActorSystem("CellularAutomataSystem")
        var nodeManagerMap: Map[String,ActorRef] =  Map()
        for ( i <- 0 until rows; j <- 0 until cols ) {
            val key = "" + i + "_" + j
            val nodeManager = system.actorOf(Props(new NodeManager(i,j)), name = "nodeManager_" + i + "_" + j)
            nodeManagerMap += (key -> nodeManager)
        }
        val immutableNodeManagerMap = nodeManagerMap.toMap
        return immutableNodeManagerMap
    }

    /**
     * Prints current worldMap state to console
     */
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
}
