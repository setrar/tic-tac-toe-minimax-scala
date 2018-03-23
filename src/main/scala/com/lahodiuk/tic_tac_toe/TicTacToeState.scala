/**
 * Copyright 2017 Yurii Lahodiuk (yura.lagodiuk@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.lahodiuk.tic_tac_toe

class TicTacToeState(val playerOnePositions : Set[Position],
                     val playerTwoPositions : Set[Position],
                     val availablePositions : Set[Position],
                     val isPlayerOneTurn : Boolean,
                     val winLength : Int) extends State[TicTacToeState] {

  import TicTacToeRules._

  implicit val state: TicTacToeState = this

  lazy val isGameOver: Boolean =
    availablePositions.isEmpty || playerOneWin || playerTwoWin

  lazy val playerOneWin: Boolean = checkWin(playerOnePositions)

  lazy val playerTwoWin: Boolean = checkWin(playerTwoPositions)

  def generateStates: Seq[TicTacToeState] =
    for(pos <- availablePositions.toSeq) yield makeMove(pos)

}

object TicTacToeState {

  // Just additional constructor for convenience
  def apply(dimension: Int, winLength: Int) = new TicTacToeState(
      Set(), Set(), // positions of the players are empty initially
      (for{row <- 1 to dimension // initialize available positions
           col <- 1 to dimension} yield Position(row, col)).toSet,
      true, winLength)
}
