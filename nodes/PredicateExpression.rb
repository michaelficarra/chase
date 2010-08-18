require 'nodes/ParentheticalExpression.rb'
class PredicateExpression
	def initialize(operator,lhs,rhs)
		@operator = operator
		@lhs = lhs
		@rhs = rhs
		@notted = false
	end
	def parenthesize?
		return true if @notted
		operators = FirstOrderLogic.operators
		@operator == operators[:XOR] || @operator == operators[:OR]
	end
	def operator
		operators = FirstOrderLogic.operators
		case @operator
			when operators[:AND]: '∧'
			when operators[:XOR]: '⊕'
			when operators[:OR]: '∨'
			else '◇'
		end
	end
	def to_s
		ret = @lhs.to_s + ' ' + self.operator + ' ' + @rhs.to_s
		ret = ParentheticalExpression.new(ret).to_s if self.parenthesize?
		ret = '¬'+ret if @notted
		ret
	end
	def not
		@notted = !@notted
	end
end
