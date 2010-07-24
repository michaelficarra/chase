class ExistentialRule
	def initialize(terminals,expression)
		@terminals = terminals
		@expression = expression
	end
	def to_s
		'∃' + @terminals.join('') + ' ' + @expression.to_s
	end
end
