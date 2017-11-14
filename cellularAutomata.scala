class Creature(lifespanc: Int = 0, fitnessc: Double = 0, namec: String = "-") {
    val lifespan: Int = lifespanc
    val fitness: Double = fitnessc
    val name: String = namec
    val birthTime : Long = System.currentTimeMillis()

    override 
    def toString(): String = {
        "Creature: lifespan(" + lifespan + 
        "); fitness(" + fitness + 
        "); name(" + name + 
        "); birthTime(" + birthTime + ")"
    }
}

//val world = Array.ofDim[Creature](10,10) 


object cellularAutomata {
    def main(args: Array[String]) {
        val world = generateNewWorld()
        //world.foreach(populate)
        println(world)
        //println(world(0)(0))
        printWorld(world)
    }

    def generateNewWorld() : Array[Array[Creature]] = {
        val rows = 10
        val cols = 20
        //for (i <- 0 until rows) 
        //    println(i)
        val world = Array.ofDim[Creature](rows,cols)
        //new Creature(1000, 2.2, "X")
        for ( i <- 0 until rows; j <- 0 until cols ) {
            //val creature = new Creature(1.1, 2.2, "X")
            val creature = new Creature()
            world(i)(j) = creature
        }
        world(0)(0) = new Creature(1000, 2.2, "X")
        for ( i <- 0 until rows; j <- 0 until cols )
            println(s"($i)($j) = ${world(i)(j)}")
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


