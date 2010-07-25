class TerminalExpression
	def initialize(operator,lhs,rhs)
		@operator = operator
		@lhs = lhs
		@rhs = rhs
		@notted = false
	end
	def to_s
		parenthesize = @notted
		op = '◇'
		operators = FirstOrderLogic.operators
		case @operator
			when operators[:LT]: op = '<'
			when operators[:GT]: op = '>'
			when operators[:LTE]: op = '≤'
			when operators[:GTE]: op = '≥'
			when operators[:NE]: op = '≠'
			when operators[:EQ]: op = '='
			when operators[:MULTIPLY]: parenthesize = true; op = '*'
			when operators[:DIVIDE]: parenthesize = true; op = '/'
			when operators[:PLUS]: op = '+'
			when operators[:MINUS]: op = '-'
		end
		ret = @lhs.to_s + ' ' + op + ' ' + @rhs.to_s
		ret = '('+ret+')' if parenthesize
		ret = '¬'+ret if @notted
		ret
	end
	def not
		@notted = !@notted
	end
end

