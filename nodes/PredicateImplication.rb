class PredicateImplication
	def initialize(explicit,implicit)
		@explicit = explicit
		@implicit = implicit
	end
	def to_s
		@explicit.to_s + ' → ' + @implicit.to_s
	end
end

