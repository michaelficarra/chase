class ExistentialQuantifier
	def initialize(terminals,body,options={})
		@terminals = terminals
		@body = body
		@one = options.include? :one
		if ParentheticalExpression === @body
			@body = @body.expression
		end
	end
	def to_s
		'∃' + (@one ? '!' : '') + @terminals.join('') + ': ' + @body.to_s
	end
end
