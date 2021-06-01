# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
               rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 0]]), # My Big Thumb 5
               [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]], # My Big 5 (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
               rotations([[0, 0], [1, 0], [0, 1]])] # My bending 3

  # your enhancements here
  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.next_cheat_piece(board)
    MyPiece.new([[[0, 0]]], board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize(game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @score = 0
    @game = game
    @delay = 500
    @current_block = MyPiece.next_piece(self)
    @cheating = false
  end

  def activate_cheating
    if @score >= 100 && !@cheating
        @cheating = true
        @score -= 100
    end
  end

  def rotate_twice
    rotate_counter_clockwise
    rotate_counter_clockwise
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    size = locations.size - 1
    (0..(size)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def next_piece
    if @cheating
      @current_block = MyPiece.next_cheat_piece(self)
      @cheating = false
    else
      @current_block = MyPiece.next_piece(self)

    @current_pos = nil
    end
  end

end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @board = MyBoard.new(self)
    @canvas = TetrisCanvas.new
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_twice})
    @root.bind('c', proc {@board.activate_cheating})
  end

end


