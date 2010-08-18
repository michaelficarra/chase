class TerminalUnaryExpression
	def initialize operator, expression
		@operator = operator
		@expression = expression
	end
	def operator
		operators = FirstOrderLogic.operators
		case @operator
			when operators[:PLUS]: '+'
			when operators[:MINUS]: '-'
			else '◇'
		end
	end
	def to_s
		self.operator + @expression.to_s
	end
end
