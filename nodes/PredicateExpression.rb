class PredicateExpression
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
			when operators[:AND]: parenthesize = true; op = '∧'
			when operators[:XOR]: parenthesize = true; op = '⊕'
			when operators[:OR]: op = '∨'
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

