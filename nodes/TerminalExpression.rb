require 'nodes/ParentheticalExpression.rb'
class TerminalExpression
	def initialize(operator,lhs,rhs)
		@operator = operator
		@lhs = lhs
		@rhs = rhs
		@notted = false
	end
	def parenthesize?
		return true if @notted
		operators = FirstOrderLogic.operators
		@operator == operators[:MULTIPLY] || @operator == operators[:DIVIDE]
	end
	def operator
		operators = FirstOrderLogic.operators
		case @operator
			when operators[:LT]: '<'
			when operators[:GT]: '>'
			when operators[:LTE]: '≤'
			when operators[:GTE]: '≥'
			when operators[:NE]: '≠'
			when operators[:EQ]: '='
			when operators[:EXP]: '^'
			when operators[:MULTIPLY]: '*'
			when operators[:DIVIDE]: '/'
			when operators[:PLUS]: '+'
			when operators[:MINUS]: '-'
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

