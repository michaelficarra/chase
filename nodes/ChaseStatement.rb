class ChaseStatement
	def initialize(*identifiers)
		@identifiers = identifiers
	end
	def to_s
		"chase " + @identifiers.join(', ') + ';'
	end
end
