package net.michaelripley.minesweeperer

import java.awt.MouseInfo
import java.awt.Point
import java.awt.PointerInfo
import java.awt.Rectangle
import java.awt.Robot
import java.awt.Toolkit

import Board.ScreenData

object Minesweeperer {

  def main(args: Array[String]): Unit = {

    println(getScreenSize())
    
    val sweeper = new Minesweeperer()
    sweeper.sweep()
  }
  
  def getMouseLocation() = {
    MouseInfo.getPointerInfo().getLocation()
  }
  
  def getScreenSize() = {
    Toolkit.getDefaultToolkit().getScreenSize()
  }
}

// allow use of Object functions
import Minesweeperer._

class Minesweeperer {
  val robot = new Robot()
  val screenSize = new Rectangle(getScreenSize())
  
  def getScreenshot() = {
    robot.createScreenCapture(screenSize)
  }
  
  def sweep() = {
    val screenState = ScreenData(getScreenshot(), getMouseLocation())
    
    // for all possible coordinates
    val boardCorners = Board.findBoards(screenState)
    println(boardCorners)
  }
}
