# https://www.codingame.com/training/medium/there-is-no-spoon-episode-1
# The interesting thing about this solution is I use a second array.
# The first array is normal row by columns but the second is column-oriented.
# That means second_matrix[N] returns an array of elements for the column and not row.

STDOUT.sync = true # DO NOT REMOVE
# Don't let the machines win. You are humanity's last hope...

width = gets.to_i # the number of cells on the X axis
height = gets.to_i # the number of cells on the Y axis

m = Array.new(height)
m_transposed = Array.new(width) { Array.new(height) }

class Point
  def initialize(x,y)
    @x = x
    @y = y
  end

  attr_reader :x, :y
end

# NOTE: This is called find_right because it's always looking within an
# array of elements, to the right. This is true for the row-wise and column-wise matrices
def find_right(row_i, col_i, matrix)
  col_start_index = [col_i + 1, matrix[row_i].length].min
  search_space = matrix[row_i][col_start_index..-1]
  closest_index = search_space.find_index { |cell| cell == "0" }

  # STDERR.puts("matrix:#{matrix} #{col_start_index} #{search_space}, #{closest_index}")

  if closest_index
    Point.new(row_i, col_start_index + closest_index)
  else
    Point.new(-1, -1)
  end
end

height.times do |i|
  line = gets.chomp # width characters, each either 0 or .
  m[i] = line.split("")
end

height.times do |row|
  width.times do |col|
    STDERR.print(m[row][col])
    m_transposed[col][row] = m[row][col]
  end
  STDERR.puts
end

STDERR.puts("m: #{m}")
STDERR.puts("m_transposed: #{m_transposed}")

height.times do |row|
  width.times do |col|
    if m[row][col] == "0"
      STDERR.puts("processing row:#{row} col:#{col} (#{col},#{row})")
      right_point = find_right(row, col, m)
      down_point = find_right(col, row, m_transposed)
      puts "#{col} #{row} #{right_point.y} #{right_point.x} #{down_point.x} #{down_point.y}"
    end
  end
end