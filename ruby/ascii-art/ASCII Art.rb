# https://www.codingame.com/training/easy/ascii-art
# Auto-generated code below aims at helping you parse
# the standard input according to the problem statement.

l = gets.to_i
h = gets.to_i
t = gets.chomp

characters = %w{A B C D E F G H I J K L M N O P Q R S T U V W X Y Z ?}
characters << " "
column_indices = characters.map.with_index do |letter, idx|
    [letter, idx * l]
end
column_hash = column_indices.to_h

STDERR.puts l,h,t
STDERR.puts column_hash

ascii_matrix = Array.new(h)

# create the matrix
h.times do |i|
    row = gets.chomp
    ascii_matrix[i] = row.split("")
end

# print the ascii given just for info purposes
# h.times do |row|
#     (27 * l).times do |column|
#         STDERR.print ascii_matrix[row][column]
#     end
#     STDERR.puts
# end

STDERR.puts

# get characters from the matrix
entire_solution = h.times.map do |row|
    row_solution = t.length.times.inject("") do |memo, i|
        key = column_hash.has_key?(t[i].upcase) ? t[i].upcase : "?"
        column_index = column_hash[key]
        memo += ascii_matrix[row][column_index, l].join
    end
end

puts entire_solution