class Predicate
	def initialize(symbol,terminals)
		@symbol = symbol
		@terminals = terminals
		@notted = false
	end
	def to_s
		(@notted ? '¬' : '') + @symbol + @terminals.join('')
	end
	def not
		@notted = !@notted
	end
end

