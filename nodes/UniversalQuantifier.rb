require 'nodes/ParentheticalExpression.rb'
class UniversalQuantifier
	attr_reader :terminals
	attr_reader :body
	def initialize(terminals,body)
		@terminals = terminals
		@body = body
		if self.class === @body
			@terminals.push *@body.terminals
			@body = @body.body
		end
		if ParentheticalExpression === @body
			@body = @body.expression
		end
	end
	def to_s
		'∀' + @terminals.join('') + ': ' + @body.to_s
	end
end
