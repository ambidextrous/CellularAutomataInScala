import scala.util.Random
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

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

class NodeManager extends Actor {
    // Simulation state isolated in this variable, each
    var resident: Creature = new Creature()

    def receive = {
        case attacker: Creature => attackResident(attacker) 
        case _ => println("Unknown command")
    }

    def attackResident(attacker: Creature) = {
        val r = scala.util.Random
        if (r.nextFloat <= (attacker.fitness - resident.fitness)) {
            resident = new Creature(attacker.maxLifespan, attacker.fitness, attacker.name)
        }
    }
}

//val world = Array.ofDim[Creature](10,10) 

object cellularAutomata {
    def main(args: Array[String]) {
        val world = generateWorld()
        //world.foreach(populate)
        //println(world(0)(0))
        printWorld(world)
    }

    def generateWorld() : Array[Array[Creature]] = {
        val rows = 10
        val cols = 20
        //for (i <- 0 until rows) 
        //    println(i)
        val world = Array.ofDim[Creature](rows,cols)
        //new Creature(1000, 0.8, "X")
        for ( i <- 0 until rows; j <- 0 until cols ) {
            //val creature = new Creature(1.1, 2.2, "X")
            val creature = new Creature()
            world(i)(j) = creature
        }
        world(0)(0) = new Creature(1000, 2.2, "X")
        //for ( i <- 0 until rows; j <- 0 until cols )
        //    println(s"($i)($j) = ${world(i)(j)}")
        return world
    }

    def printWorld(world : Array[Array[Creature]]) = {
        val outerLength : Int = world.length
        val innerLength : Int = world(0).length
        for (i <- 0 until outerLength; j <- 0 until innerLength) {
            print(world(i)(j).name + " ")
            if (j == innerLength - 1) {
                print("\n")
            }
        }
    }
}


