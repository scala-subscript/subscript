package subscript.example.taskprocessor.ssactors

import subscript.akka._
import scala.concurrent.duration._
import akka.actor._
import subscript.example.taskprocessor.ActorFamily

object SSActorFamily extends ActorFamily {

  var started = false

  def preStart() {
    if (!started) {
      SSARunnerV1Scheduler.execute(null)
      started = true
    }
  }

  def frontProcessor[Di, Df, Ri, Rf] = classOf[FrontProcessor[Di, Df, Ri, Rf]]
  
  def processor[Df, Rf]              = classOf[Processor[Df, Rf]             ]
  
  def proxy                          = classOf[Proxy                         ]
  
  override def toString() = "SubScript Actors"
  
}