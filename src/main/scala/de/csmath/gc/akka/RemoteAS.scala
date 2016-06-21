package de.csmath.gc.akka

import com.typesafe.config._
import akka.actor._
import de.csmath.gc._


/**
 *  The application object of a remote actor system to offer the execution
 *  of actors.
 */
object RemoteAS {

    /**
     *  The main method of the remote actor system.
     *  @param args The argument list.
     */
    def main(args: Array[String]) {
        val config = ConfigFactory.load()
        val remconf = config.getConfig("remoteas").withFallback(config)
        val system = ActorSystem("RemoteAS",remconf)
        system.awaitTermination()
    }

}
