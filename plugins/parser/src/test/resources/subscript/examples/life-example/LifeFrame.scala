package subscript.example.life
import subscript.file

import scala.math._
import scala.swing._
import scala.swing.event._
import subscript.Predef._
import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._
import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._
import subscript.vm.executor._

object LifeFrame extends LifeFrameApplication
class LifeFrameApplication extends BasicLifeFrameApplication {
  import scala.language.implicitConversions

    //////////////////////////////////////////////
    // speed control
    //////////////////////////////////////////////
    
    def getSleep_ms = pow(2, 12-speed).toInt // logarithmic scale
    
    def sleep = 
      try {
        val sleepPart_ms = 10
        val startTime_ms = System.currentTimeMillis
        while (System.currentTimeMillis - startTime_ms < getSleep_ms) {
          Thread.sleep(sleepPart_ms)
        }
      }
      catch { case e: InterruptedException => /*println("sleep interrupted")*/}

    def sleep_ms(time_ms: Int) = 
      try {
          Thread.sleep(time_ms)
      }
      catch { case e: InterruptedException => /*println("sleep interrupted")*/}

    //////////////////////////////////////////////
    // confirm exit dialog
    //////////////////////////////////////////////
    def confirmExit: Boolean = Dialog.showConfirmation(top.contents.head, "Are you sure?", "About to exit")==Dialog.Result.Yes
     
    board.listenTo(board.mouse.clicks)
    board.listenTo(board.mouse.moves)

    //////////////////////////////////////////////
    // handle MouseDown events
    //////////////////////////////////////////////
     def resetLastMousePos: Unit = board.resetLastMousePos // so that mouseDragSet will initially not draw a line
     
     def handleMouseSingleClick(p: java.awt.Point): Unit = {
       selectedPattern match {
          case None => board.mouseDownToggle(p)
          case Some(s) => val ec = Coord(p.x/board.cellSizeX, p.y/board.cellSizeY)
                          for (pc <- ConwayPatterns.moveTo(s,ec)) {board.setCellValue(pc.x,pc.y,true)}
       }
     }
     def handleMouseMove(p: java.awt.Point): Unit = board.mouseDragSet(p)
     def handleMouseDrag(p: java.awt.Point): Unit = board.mouseDragSet(p)
    
     def char2Value(ac: Any) = {var c=chr(ac); int2Value(c-'0')}
     def int2Value ( i: Int) = if (i==0) maxSpeed else minSpeed+i-1

     // conversion method, unfortunately needed since the SubScript compiler does not handle var types well
     def chr(c:Any) = c.asInstanceOf[Int].toChar
     
  implicit script..
  
    key(??c: Char     ) =  key2: top, ??c
   vkey(??k: Key.Value) = vkey2: top, ??k

  script ..
	 randomizeCommand  = randomizeButton + 'r'
	     clearCommand  =     clearButton + 'c'
	      stepCommand  =      stepButton + ' '
	      exitCommand  =      exitButton + windowClosing: top
	multiStepStartCmd  =     startButton + Key.Enter
	 multiStepStopCmd  =      stopButton + Key.Enter
	
	doExit             =   exitCommand @gui: {!confirmExit!} ~~(r:Boolean)~~> while (!r)
	                  //   exitCommand @gui: {confirmExit} ==> while(!_)
      boardControl     = ...; noise / [.. singleStep] multiStep || clear || randomize

      do1Step          = {*board.calculateGeneration*} @gui: board.validate
      
      noise            = 'n'; ... @gui: board.doRandomize() {*sleep*}
      randomize        =   randomizeCommand @gui: board.doRandomize()
      clear            =       clearCommand @gui: board.doClear
      singleStep       =        stepCommand do1Step
       multiStep       = multiStepStartCmd; ... do1Step {*sleep*} 
                       / multiStepStopCmd

      speedControl     = ...; speedKeyInput + speedButtonInput + speedSliderInput
                    
    setSpeed(s: Int)   = @gui: setSpeedValue: s

      speedKeyInput    = while(here.pass<10)
                       + [ val c: Char = (pass_up1+'0').toChar; key: c setSpeed: char2Value(c)] // TBD: make here an implicit parameter
                          
                       // Note: first "("...")" pair is needed because else the ifs would be nested
   speedButtonInput =   [if speed>minSpeed then speedDecButton]
                      + [if speed<maxSpeed then speedIncButton]
    
     speedDecButton = minSpeedButton setSpeed: minSpeed + slowerButton setSpeed: (speed-1)
     speedIncButton = maxSpeedButton setSpeed: maxSpeed + fasterButton setSpeed: (speed+1)
     
   speedSliderInput = speedSlider setSpeed: speedSlider.value

      mouseInput    = [mouseClickInput & mouseDragInput]
                      /  doubleClick [mouseMoveInput / doubleClick resetLastMousePos]
                      ...

 //mouseClickInput  = mouseSingleClick (board, ?p:java.awt.Point) {! doMouseSingleClick(p) !} ... 
//                     !@#%^&$ mouseSingleClick also reacts on double clicks!!! 
//                     So wait 220 ms; if by then no mouseDoubleClick as arrived, do the singleClick action:
   mouseClickInput  = var p:java.awt.Point=null
                      mouseSingleClick: board, ?p
                        ...
                      ; resetLastMousePos
                      ; [ {*sleep_ms(220)*} here.break_up(2) / mouseDoubleClick: board, ?p ]
                      ; ...
                      handleMouseSingleClick: p
                      ...
/*  
TBD as soon as mouseSingleClick has a java.awt.Point as result:
                 
   mouseClickInput  = mouseSingleClick(board) 
                      ~~~(p:java.awt.Point)~~~> 
                      {! resetLastMousePos !}
                        {*sleep_ms(220)*} {! handleMouseSingleClick(p) !} 
                      / mouseDoubleClick(board)
*/
                      
                    
   doubleClick      = var p:java.awt.Point=null; mouseDoubleClick(board, ?p)
   mouse_Released   = var p:java.awt.Point=null; mouseReleased(   board, ?p)
 //mouse_Released   = var p:java.awt.Point=null mouseReleased(   board, ActualOutputParameter(p, (v:java.awt.Point)=>p=v)) // TBD: "?p"; mouseReleased instead of mouse_Released yields "too many arguments for method" error
    mouseDragInput  = mouseDraggings(board, (e: MouseEvent) => handleMouseDrag(e.point)) / [mouse_Released  resetLastMousePos]; ...
    mouseMoveInput  = mouseMoves(    board, (e: MouseEvent) => handleMouseMove(e.point)) 
  //mouseMoveToggle = var p:java.awt.Point=null mouseMove(board, p?) // doMouseDraw(p))  

override script..
    liveScript      = || boardControl mouseInput speedControl doExit
                   
}


