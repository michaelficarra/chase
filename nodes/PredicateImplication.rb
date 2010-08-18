class PredicateImplication
	def initialize(provided,implied)
		@provided = provided
		@implied = implied
		if ParentheticalExpression === @provided
			@provided = @provided.expression
		end
	end
	def to_s
		@provided.to_s + ' → ' + @implied.to_s
	end
end
