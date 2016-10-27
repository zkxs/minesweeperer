package net.michaelripley.minesweeperer

import java.awt.Color
import java.awt.Point

import java.awt.image.BufferedImage

object Board {
  
  val tileSize = 16
  
  val tileSamplePixelX1 = 8
  val tileSamplePixelX2 = 10
  val tileSamplePixelY  = 12
  
  object Colors {
    val border         = new Color(  0,   0,   0)
    val nwBoardInset   = new Color(128, 128, 128)
    val nwTileOutset   = new Color(255, 255, 255)
    val tileBackground = new Color(227, 227, 227)
    val flag           = new Color(128, 128, 128)
    val eight          = new Color(  0,   0,   0)
    val seven          = new Color(128,   0, 128) // only pixel 1 is set
    val six            = new Color(  0, 128, 128)
    val five           = new Color(128,   0,   0)
    val four           = new Color(  0,   0, 128) // only pixel 2 is set
    val three          = new Color(255,   0,   0)
    val two            = new Color(  0, 128,   0)
    val one            = new Color(  0,   0, 255)
    val empty          = tileBackground
  }
  
  case class ScreenData(screen: BufferedImage, mousePos: Point)
  
  def getColor(screen: BufferedImage, x: Int, y:Int) = {
    new Color(screen.getRGB(x, y))
  }
  
  // finds the gray pixel just to the NW of the game board
  def findBoards(screenState: ScreenData) = {
    // the original x,y are aligned for easier checking of the corner pixels
    (for {x <- 0 until screenState.screen.getWidth; y <- 0 until screenState.screen.getHeight} yield (x, y))
      .withFilter{case (x, y) => checkCorner(screenState.screen, x, y)}
      .map{case (x, y) => Board(screenState, x + 3, y + 3)} // +3 is to adjust coordinates to be grid aligned
      .collect{case Some(board) => board}
  }
  
  /* x and y are the corners of the board found in findBoardCorners()
   * tries to find the dimensions of the board. if it can't, this implies
   * that the board is partially obscured by something
   * 
   * Note that if the board's lower right corner is obscured, it will simply be measured
   * as a smaller board. this isn't necessarily a bad thing
   */
  private def findBoardDimensions(screen: BufferedImage, x: Int, y: Int): Option[(Int, Int)] = {
    var xdim = 0
    while (getColor(screen, x + tileSize * (xdim + 1), y + tileSize) == Colors.border) {
      xdim += 1
    }
    
    var ydim = 0
    while (getColor(screen, x + tileSize, y + tileSize * (ydim + 1)) == Colors.border) {
      ydim += 1
    }
    
    // make sure all expected tiles exist by checking the black pixel to their SE
    for (xIdx <- 1 to xdim; yIdx <- 1 to ydim) {
      if (getColor(screen, x + tileSize * xIdx, y + tileSize * yIdx) != Colors.border) {
        return None
      }
    }
    
    return Some((xdim, ydim))
  }
  
  // check to see if this is the corner of a board
  private def checkCorner(screen: BufferedImage, x: Int, y: Int): Boolean = {
    // we will be checking a 6x6 rectangle
    // make sure we have enough room to actually do that
    if (x + 6 >= screen.getWidth() || y + 6 >= screen.getHeight()) {
      return false
    }
    
    // check tile pixel
    if (getColor(screen, x + 5, y + 5) != Colors.tileBackground) {
      return false
    }
    
    // check top row
    for (xx <- x until x + 6) {
      if (getColor(screen, xx, y) != Colors.tileBackground) {
        return false
      }
    }
    
    // check left column
    for (yy <- y + 1 until y + 6) {
      if (getColor(screen, x, yy) != Colors.tileBackground) {
        return false
      }
    }
    
    // check horizontal gray band
    for (xx <- x + 1 until x + 6; yy <- y + 1 until y + 4) {
      val mycolor = getColor(screen, xx, yy)
      if ( mycolor != Colors.nwBoardInset) {
        return false
      }
    }
    
    // check rest of gray band
    for (xx <- x + 1 until x + 4; yy <- y + 4 until y + 6) {
      val mycolor = getColor(screen, xx, yy)
      if ( mycolor != Colors.nwBoardInset) {
        return false
      }
    }
    
    return true
  }
  
  def apply(screenState: ScreenData, xPos: Int, yPos: Int) = {
    val dimensions = findBoardDimensions(screenState.screen, xPos, yPos)
    
    dimensions match {
      case Some((xDim, yDim)) => {
        // ok, it has dimensions. now to check each square
        Some(new Board(screenState.mousePos, xPos, yPos, xDim, yDim))
      }
      case _ => None
    }
    
  }
}

import Board._

case class Board (val mousePos: Point, val xPos: Int, val yPos: Int, val xDim: Int, val yDim: Int) {
  
}
