class ParentheticalExpression
	attr_reader :expression
	def initialize expression
		@expression = expression
		if self.class === expression
			@expression = expression.expression
		end
	end
	def to_s
		'(' + @expression.to_s + ')'
	end
end
