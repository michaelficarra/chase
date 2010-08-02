class ExistentialQuantifier
	def initialize(terminals,predicate)
		@terminals = terminals
		@predicate = predicate
	end
	def to_s
		'∃' + @terminals.join('') + ' ' + @predicate.to_s
	end
end
