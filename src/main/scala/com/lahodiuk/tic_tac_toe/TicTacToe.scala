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

class TicTacToe(playerOne: BestMoveFinder[TicTacToeState],
                playerTwo: BestMoveFinder[TicTacToeState]) {

  def play = {
    @annotation.tailrec
    def go(players: List[BestMoveFinder[TicTacToeState]], game: TicTacToeState, moveNumber: Int): Unit = {
      if (game.isGameOver) {
        if(game.playerOneWin) println("Player One win!")
        else if(game.playerTwoWin) println("Player Two win!")
        else println("Draw!")
      } else {
        println(s"Player ${moveNumber % 2 + 1} makes move:")
        val player = players.head
        val newGame =   player.move(game)
        println(display(newGame))
        go(players.tail :+ player, newGame, moveNumber + 1)
      }
    }
    go(List(playerOne, playerTwo), new TicTacToeState(DIMENSION, DIMENSION), 0)
  }

  def display(game: TicTacToeState) =
    (1 to DIMENSION).map(row =>
      (1 to DIMENSION).map(col => {
        val p = Position(row, col)
        if(game.playerOnePositions contains p) X_PLAYER
        else if(game.playerTwoPositions contains p) O_PLAYER
        else EMPTY_CELL
      }).mkString).mkString("\n") + "\n"

  final val DIMENSION  = 3
  final val X_PLAYER   = "X"
  final val O_PLAYER   = "O"
  final val EMPTY_CELL = "."
}
