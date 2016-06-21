package de.csmath.psa

import scala.util._
import com.typesafe.config._


/**
 *  PSAConfig encapsulates the parameters for the simulated annealing process.
 *  It also contains the file names for the input and output.
 *  @param inFileName  The name of the input file which describes the problem.
 *  @param outFileName The name of the output file which describes the computed
 *                     solution.
 *  @param startTemp   The start temperature of the simulated annealing process.
 *  @param endTemp     The end temperature of the simulated annealing process.
 *  @param numIter     The number of solutions to create at each
 *                     temperature state.
 *  @param coolingRate The cooling rate. of the simulated annealing process.
 */
class PSAConfig(val inFileName: String,
                val outFileName: String,
                val startTemp: Double,
                val endTemp: Double,
                val numIter: Int,
                val coolingRate: Double) {

    /**
     *  The auxiliary constructor to gather the information from a
     *  configuration file or from command arguments. (Later is not yet
     *  implemented).
     *  @param config A Config object of a configuration file.
     *  @param args   The command arguments.
     */
    def this(config: Config, args: Array[String]) =
        this(config.getString("psa.inFileName"),
             config.getString("psa.outFileName"),
             config.getDouble("psa.startTemp"),
             config.getDouble("psa.endTemp"),
             config.getInt("psa.numIter"),
             config.getDouble("psa.coolingRate"))


    /**
     *  The predicate to check the validity of the parameters.
     */
    def valid = inFileName.length > 0 &&
                startTemp > 0 &&
                endTemp > 0 &&
                startTemp > endTemp &&
                numIter > 0 &&
                coolingRate < 1 &&
                coolingRate > 0

}


/**
 *  The companion object to create a new PSAConfig instance.
 */
object PSAConfig {

    /**
     *  A new configuration object.
     *  @param args The command arguments.
     *  @return A new configuration object.
     */
    def apply(args: Array[String]) = {
        val config = ConfigFactory.load()
        val conf = new PSAConfig(config,args)
        if (conf.valid)
            Success(conf)
        else
            Failure(new IllegalArgumentException("ConfigurationError!"))
    }


    /**
     *  A destructed configuration object.
     *  @param c A configuration object.
     *  @return Some(a,b,c,d,e,f) where a is the input file name, b is the
     *          output file name, c is the start temperature, d is the end
     *          temperature, e is the number of iterations and f is the
     *          cooling rate.
     */
    def unapply(c: PSAConfig) =
        Some(c.inFileName,c.outFileName,c.startTemp,c.endTemp,c.numIter,c.coolingRate)

}
