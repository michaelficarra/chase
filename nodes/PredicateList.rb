class PredicateList
	def initialize *predicates
		@predicates = predicates || []
	end
	def push *args
		@predicates.push *args
	end
	def to_s
		@predicates.join ' '
	end
end
