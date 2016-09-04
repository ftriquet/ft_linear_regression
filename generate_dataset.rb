#!/usr/bin/ruby

$stderr.puts 'Enter dataset size:'

size = gets.chomp.to_i
exit if size == 0

$stderr.puts 'Enter constant:'
constant = gets.chomp.to_i

$stderr.puts 'Enter parameters:'

params = gets.chomp.split(' ').map(&:to_f)
exit unless params.any?

$stderr.puts 'Enter error range:'

error_rng = gets.chomp.to_f

r = Random.new

size.times do
  features = 1.upto(params.count).map do
    bool = r.rand(1.0) > 0.5
    if bool
      r.rand(10_000)
    else
      -r.rand(10_000)
    end
  end
  sum = constant

  features.each_with_index do |f, i|
    sum += f * params[i] + r.rand(error_rng)
  end
  puts features.join(',') + ',' + sum.to_s
end
