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

case class Position(row: Int, col: Int)

trait Rules {

  type StepOffsetGen = (Position, Int) => Position

  def leftDiagonal: StepOffsetGen =
    (pos, offset) => Position(pos.row + offset, pos.col + offset)

  def rightDiagonal: StepOffsetGen =
    (pos, offset) => Position(pos.row - offset, pos.col + offset)

  def row: StepOffsetGen =
    (pos, offset) => Position(pos.row + offset, pos.col)

  def column: StepOffsetGen =
    (pos, offset) => Position(pos.row, pos.col + offset)

  def winConditionsSatisfied(step: StepOffsetGen)
                            (positions: Set[Position], winLength: Int): Boolean =
    positions exists (position =>
      (0 until winLength) forall (offset =>
        positions contains step(position, offset)))
}


object TicTacToeRules extends Rules {

  final val directions = List(leftDiagonal, rightDiagonal, row, column)

  def checkWin(positions: Set[Position])(implicit state: TicTacToeState): Boolean =
    directions.exists(winConditionsSatisfied(_)(positions, state.winLength))

  def makeMove(p: Position)(implicit state: TicTacToeState): TicTacToeState = {
    assert(state.availablePositions.contains(p))
    if(state.isPlayerOneTurn)
      new TicTacToeState(
        state.playerOnePositions + p,
        state.playerTwoPositions,
        state.availablePositions - p,
        !state.isPlayerOneTurn,
        state.winLength)
    else new TicTacToeState(
      state.playerOnePositions,
      state.playerTwoPositions + p,
      state.availablePositions - p,
      !state.isPlayerOneTurn,
      state.winLength)}

}
