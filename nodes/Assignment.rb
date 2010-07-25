class Assignment
	def initialize(lhs, rhs)
		@identifier = lhs
		@rhs = rhs
	end
	def to_s
		@identifier + ': ' + @rhs.to_s + ';'
	end
end
