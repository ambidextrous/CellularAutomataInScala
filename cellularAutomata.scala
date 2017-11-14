import scala.util.Random
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

class Creature(maxLifespanc: Int = 0, fitnessc: Double = 0, namec: String = "-") {
    val maxLifespan: Int = getMaxLifespan(maxLifespanc)
    val fitness: Double = fitnessc
    val name: String = namec
    val birthTime : Long = System.currentTimeMillis()

    def getMaxLifespan(upperLimit: Int): Int = {
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

class NodeManager() extends Actor {
    var resident: Creature = new Creature()

    def receive = {
        case "requestName" =>
            sender ! resident.name
            //val r = scala.util.Random
            //if (r.nextFloat <= (attacker.fitness - resident.fitness)) {
            //    resident = new Creature(attacker.maxLifespan, attacker.fitness, attacker.name)
            //}
    }
}

object cellularAutomata  {
    def main(args: Array[String]) {
        val world = generateWorld()
        printWorld(world)
    }

    def generateWorld() : Array[Array[ActorRef]] = {
        val rows = 10
        val cols = 20
        val world = Array.ofDim[ActorRef](rows,cols)
        val system = ActorSystem("CellularAutomataSystem")
        for ( i <- 0 until rows; j <- 0 until cols ) {
            val nodeManager = system.actorOf(Props[NodeManager], name = "nodeManager_" + i + "_" + j)
            world(i)(j) = nodeManager
        }
        return world
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
    }
}


